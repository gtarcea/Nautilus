;;;; ESRAP -- a packrat parser for Common Lisp
;;;; by Nikodemus Siivola, 2007
;;;;
;;;; In addition to regular Packrat / Parsing Grammar / TDPL features
;;;; ESRAP supports:
;;;;
;;;;  - dynamic redefinition of nonterminals
;;;;  - inline grammars
;;;;  - semantic predicates
;;;;  - introspective facilities
;;;;
;;;; References:
;;;;
;;;;   * Bryan Ford, 2002, "Packrat Parsing: a Practical Linear Time
;;;;     Algorithm with Backtracking".
;;;;     http://pdos.csail.mit.edu/~baford/packrat/thesis/
;;;;
;;;; Licence:
;;;;
;;;;  Permission is hereby granted, free of charge, to any person
;;;;  obtaining a copy of this software and associated documentation files
;;;;  (the "Software"), to deal in the Software without restriction,
;;;;  including without limitation the rights to use, copy, modify, merge,
;;;;  publish, distribute, sublicense, and/or sell copies of the Software,
;;;;  and to permit persons to whom the Software is furnished to do so,
;;;;  subject to the following conditions:
;;;;
;;;;  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;;  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;;;  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;;;  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;;;  CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;;;  TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;;;  SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;;;
;;;; Syntax overview:
;;;;
;;;;  character                 -- any single character
;;;;  (string length)           -- any string of length
;;;;  (and &rest sequence)
;;;;  (or &rest ordered-choises)
;;;;  (* greedy-repetition)
;;;;  (+ greedy-positive-repetition)
;;;;  (? optional)
;;;;  (& followed-by)           -- does not consume
;;;;  (! not-followed-by)       -- does not consume
;;;;  (<predicate> expr)        -- semantic parsing
;;;;
;;;; Examples:
;;;;
;;;;  (parse '(or "foo" "bar") "foo")         => "foo", NIL
;;;;
;;;;  (add-rule 'foo+ (make-instance 'rule :expression '(+ "foo"))) => FOO+
;;;;
;;;;  (parse 'foo+ "foofoofoo")               => ("foo" "foo" "foo"), NIL
;;;;
;;;;  (add-rule 'decimal
;;;;            (make-instance 'rule
;;;;             :expression '(+ (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))
;;;;             :transform (lambda (list) 
;;;;                          (parse-integer (format nil "~{~A~}" list)))))
;;;;   => DECIMAL
;;;;
;;;;  (parse '(oddp decimal) "123")                  => 123
;;;;
;;;;  (parse '(evenp decimal) "123" :junk-allowed t) => NIL, 0
;;;;
;;;; TODO:
;;;;  - character classes
;;;;  - nicer error messages
;;;;  - setting breaks on rules
;;;;  - proper tests
;;;;  - states
;;;;  - documentation
;;;;  - transform-production vs. transform-subseq:
;;;;    (add-rule 'decimal :expression '(+ (or "0" "1" ...))
;;;;                       :transform-subseq #'parse-integer)
;;;;  - optimizing single-character alternatives: store in a string,
;;;;    not in a list.

(eval-when (:compile-toplevel :load-toplevel :execute)
  
  (require :alexandria)
  
  (defpackage :esrap
    (:use :cl :alexandria)
    (:export 
     #:! #:? #:+ #:*
     #:add-rule
     #:concat
     #:describe-grammar
     #:defrule
     #:esrap-version
     #:find-rule
     #:parse 
     #:rule
     #:rule-dependencies
     #:remove-rule
     )))

(in-package :esrap)

;;; Miscellany

(defun esrap-version ()
  "0.1")

(defun concat (&rest arguments)
  "Arguments must be strings, or lists whose leaves are strings.
Catenates all the strings in arguments into a single string."
  (with-output-to-string (s)
    (labels ((cat-list (list)
               (dolist (elt list)
                 (etypecase elt
                   (string (write-string elt s))
                   (list (cat-list elt))))))
      (cat-list arguments))))

(deftype nonterminal ()
  "Any symbol except CHARACTER and NIL can be used as a nonterminal symbol."
  '(and symbol (not (member character nil))))

(deftype terminal ()
  "Strings and characters are used as terminal symbols."
  '(or string character))

;;; RULE REPRESENTATION AND STORAGE
;;;
;;; For each rule, there is a cons cell in *RULES*, which has the
;;; function that implements the rule in car, and the rule object
;;; in CDR. A RULE object can be attaches to only one non-terminal
;;; at a time, which is accessible via RULE-SYMBOL.

(defvar *rules* (make-hash-table))

(defun clear-rules ()
  (clrhash *rules*)
  nil)

(deftype rule-cell ()
  '(cons function (cons t t)))

(declaim (inline cell-function))
(defun cell-function (cell) 
  (car cell))

(defun (setf cell-function) (function cell)
  (declare (function function) (rule-cell cell))
  (setf (car cell) function))

(defun cell-referents (cell)
  (car (cdr cell)))

(defun (setf cell-referents) (list cell)
  (declare (rule-cell cell))
  (setf (car (cdr cell)) list))

(defun cell-rule (cell)
  (cdr (cdr cell)))

(defun (setf cell-rule) (rule cell)
  (declare (rule-cell cell))
  (setf (cdr (cdr cell)) rule))

(defun ensure-rule-cell (symbol)
  (check-type symbol nonterminal)
  ;; FIXME: Need to lock *RULES*.
  (or (gethash symbol *rules*)
      (setf (gethash symbol *rules*)
            (cons (lambda (&rest args)
                    (declare (ignore args))
                    (error "Undefined rule: ~S" symbol))
                  (cons nil nil)))))

(defun delete-rule-cell (symbol)
  (remhash symbol *rules*))

(defun reference-rule-cell (symbol referent)
  (let ((cell (ensure-rule-cell symbol)))
    (pushnew referent (cell-referents cell))
    cell))

(defun dereference-rule-cell (symbol referent)
  (let ((cell (ensure-rule-cell symbol)))
    (setf (cell-referents cell) (delete referent (cell-referents cell)))
    cell))

(defun find-rule-cell (symbol)
  (check-type symbol nonterminal)
  (gethash symbol *rules*))

(defclass rule ()
  ((%symbol
    :initform nil
    :reader rule-symbol)
   (%expression 
    :initarg :expression
    :initform (required-argument :expression)
    :reader rule-expression)
   (%transform
    :initarg :transform
    :initform nil
    :reader rule-transform)))

(defun detach-rule (rule)
  (dolist (dep (rule-direct-dependencies rule))
    (dereference-rule-cell dep (rule-symbol rule)))
  (setf (slot-value rule '%symbol) nil))

(defmethod shared-initialize :after ((rule rule) slots &key)
  (validate-expression (rule-expression rule)))

(defmethod print-object ((rule rule) stream)
  (print-unreadable-object (rule stream :type t :identity nil)
    (let ((symbol (rule-symbol rule)))
      (if symbol
          (format stream "~S <- " symbol)
          (format stream "(detached) ")))
    (write (rule-expression rule) :stream stream)))

(defun sort-dependencies (symbol dependencies)
  (let ((symbols (delete symbol dependencies))
        (defined nil)
        (undefined nil))
    (dolist (sym symbols)
      (if (find-rule sym)
          (push sym defined)
          (push sym undefined)))
    (values defined undefined)))

(defun rule-dependencies (rule)
  "Returns the dependencies of the RULE: primary value is a list of defined
nonterminal symbols, and secondary value is a list of undefined nonterminal
symbols."
  (sort-dependencies 
   (rule-symbol rule) (%expression-dependencies (rule-expression rule) nil)))

(defun rule-direct-dependencies (rule)
  (sort-dependencies
   (rule-symbol rule) (%expression-direct-dependencies (rule-expression rule) nil)))

;;; Expression destructuring and validation

(defmacro with-expression ((expr lambda-list) &body body)
  (let* ((type (car lambda-list))
         (car-var (gensym "CAR"))
         (fixed-list (cons car-var (cdr lambda-list))))
    (once-only (expr)
      `(destructuring-bind ,fixed-list ,expr
         ,(if (eq t type)
              `(declare (ignore ,car-var))
              `(unless (eq ',type ,car-var)
                 (error "~S-expression expected, got: ~S" ',type ,expr)))
         (locally ,@body)))))

;;; MEMOIZATION CACHE
;;;
;;; Because each [rule, position] tuple has an unambiguous
;;; result per source text, we can cache this result -- this is what
;;; makes packrat parsing O(N).
;;;
;;; For now we just use EQUAL hash-tables, but a specialized
;;; representation would probably pay off.

(defvar *cache*)

(defun make-cache ()
  (make-hash-table :test #'equal))

(defun get-cached (symbol position cache)
  (gethash (cons symbol position) cache))

(defun (setf get-cached) (result symbol position cache)
  (setf (gethash (cons symbol position) cache) result))

(defvar *nonterminal-stack* nil)

;;; SYMBOL, POSITION, and CACHE must all be lexical variables!
(defmacro with-cached-result ((symbol position) &body forms)
  (with-gensyms (cache result)
    `(let* ((,cache *cache*)
            (,result (get-cached ,symbol ,position ,cache))
            (*nonterminal-stack* (cons ,symbol *nonterminal-stack*)))
       (cond ((eq t ,result)
              (error "Left recursion in nonterminal ~S, at ~S.~%Path: ~{~S~^ -> ~}" 
                     ,symbol ,position (nreverse *nonterminal-stack*)))
             (,result
              ,result)
             (t
              ;; First mark this pair with T to detect left-recursion,
              ;; then compute the result and cache that.
              (setf (get-cached ,symbol ,position ,cache) t
                    (get-cached ,symbol ,position ,cache) (locally ,@forms)))))))

;;; RESULT REPRESENTATION
;;;
;;; We always return a result -- ERROR-RESULT for failed parses, and
;;; RESULT for successes.
;;;
;;; We implement a simple lazy evaluation for the productions. This is
;;; used to perform semantic actions only when necessary -- either
;;; when we call a semantic predicate or once parse has finished.

(defstruct error-result
  ;; Expression that failed to match.
  expression
  ;; Position at which match was attempted.
  (position (required-argument) :type array-index)
  ;; A nested error, closer to actual failure site.
  detail)

(defstruct (result (:constructor %make-result))
  ;; Either a list of results, whose first element is the production, or a
  ;; function to call that will return the production.
  %production
  ;; Position after the match.
  (position (required-argument) :type array-index))

(defmacro make-result (&rest arguments &key production &allow-other-keys)
  (if production
      (let ((args (copy-list arguments)))
        (remf args :production)
        `(%make-result ,@args
                       :%production ,(if (symbolp production)
                                         `(list ,production)
                                         `(lambda () ,production))))
      `(%make-result ,@arguments)))

(defun result-production (result)
  (let ((thunk (result-%production result)))
    (if (functionp thunk)
        (let ((value (funcall thunk)))
          (setf (result-%production result) (list value))
          value)
        (car thunk))))

;;; MAIN INTERFACE

(defun parse (expression text &key (start 0) end junk-allowed)
  ;; There is no backtracking in the toplevel expression -- so there's
  ;; no point in compiling it as it will be executed only once -- unless
  ;; it's a constant, for which we have a compiler-macro.
  (let ((end (or end (length text))))
    (process-parse-result
     (let ((*cache* (make-cache)))
       (eval-expression expression text start end))
     end
     junk-allowed)))

(define-compiler-macro parse (&whole form expression &rest arguments
                              &environment env)
  (if (constantp expression env)
      (with-gensyms (expr-fun)
        `(let ((,expr-fun (load-time-value (compile-expression ,expression))))
           ;; This inline-lambda here provides keyword defaults and
           ;; parsing, so the compiler-macro doesn't have to worry
           ;; about evaluation order.
           ((lambda (text &key (start 0) end junk-allowed)
              (let ((*cache* (make-cache))
                    (end (or end (length text))))
                (process-parse-result
                 (funcall ,expr-fun text start end)
                 end
                 junk-allowed)))
            ,@arguments)))
      form))

(defun process-parse-result (result end junk-allowed)
  (if (error-result-p result)
      (if junk-allowed
          (values nil 0)
          (error (with-output-to-string (s)
                   (format s "Expression ~S failed at ~S~:[.~;:~]" 
                           (error-result-expression result)
                           (error-result-position result)
                           (error-result-detail result))
                   (labels ((rec (e)
                              (when e
                                (format s "~& subexpression ~S failed at ~S."
                                        (error-result-expression e)
                                        (error-result-position e)) 
                                (rec (error-result-detail e)))))
                     (rec (error-result-detail result))))))
      (let ((position (result-position result)))
        (values (result-production result) 
                (when (< position end)
                  (if junk-allowed
                      position
                      (error "Incomplete parse, stopped at ~S." position)))))))

(defmacro defrule (&whole form symbol expression &body options)
  (let (transform)
    (when options
      (dolist (option options)
        (when transform
          (error "Multiple transforms in DEFRULE:~% ~S" form))
        (ecase (car option)
          (:constant 
           (setf transform `(lambda (x) (declare (ignore x)) ,(second option))))
          (:concat
           (when (second option)
             (setf transform '#'concat)))
          (:lambda
           (destructuring-bind (lambda-list &body forms) (cdr option)
             (setf transform `(lambda ,lambda-list ,@forms))))
          (:destructure
           (destructuring-bind (lambda-list &body forms) (cdr option)
             (setf transform
                   (with-gensyms (production)
                     `(lambda (,production)
                        (destructuring-bind ,lambda-list ,production
                          ,@forms)))))))))
    `(eval-when (:load-toplevel :execute)
       (add-rule ',symbol (make-instance 'rule
                                         :expression ',expression
                                         :transform ,transform)))))

(defun add-rule (symbol rule)
  "Associates RULE with the nonterminal SYMBOL. Signals an error if the
rule is already associated with a nonterminal. If the symbol is already
associated with a rule, the old rule is removed first."
  ;; FIXME: This needs locking and WITHOUT-INTERRUPTS.
  (check-type symbol nonterminal)
  (when (rule-symbol rule)
    (error "~S is already associated with the nonterminal ~S -- remove it first."
           rule (rule-symbol rule)))
  (let ((cell (ensure-rule-cell symbol))
        (function (compile-rule symbol 
                                (rule-expression rule)
                                (rule-transform rule))))
    (setf (cell-function cell) function
          (cell-rule cell) rule
          (slot-value rule '%symbol) symbol)
    symbol))

(defun find-rule (symbol)
  "Returns rule designated by SYMBOL, if any. Symbol must be a nonterminal
symbol."
  (check-type symbol nonterminal)
  (let ((cell (find-rule-cell symbol)))
    (when cell
      (cddr cell))))

(defun remove-rule (symbol &key force)
  "Makes the nonterminal SYMBOL undefined. If the nonterminal is defined an
already referred to by other rules, an error is signalled unless :FORCE is
true."
  (check-type symbol nonterminal)
  ;; FIXME: Lock and WITHOUT-INTERRUPTS.
  (let* ((cell (find-rule-cell symbol))
         (rule (cell-rule cell)))
    (when cell
      (cond ((and rule (cell-referents cell))
             (unless force
               (error "Nonterminal ~S is used by other nonterminal~P:~% ~{~S~^, ~}"
                      symbol (length (cell-referents cell)) (cell-referents cell)))
             (setf (cell-function cell) (lambda (text position end)
                                          (declare (ignore text position end))
                                          (error "Undefined rule: ~S" symbol))
                   (cell-rule cell) nil)
             (detach-rule rule))
            ((not (cell-referents cell))
             (when rule
               (detach-rule rule))
             ;; There are no references to the rule at all, so
             ;; we can remove the cell.
             (delete-rule-cell symbol)))
      rule)))

(defun symbol-length (x)
  (length (symbol-name x)))

(defun describe-grammar (symbol &optional (stream *standard-output*))  
  "Prints the grammar tree rooted at nonterminal SYMBOL to STREAM for human
inspection."
  (check-type symbol nonterminal)
  (let ((rule (find-rule symbol)))
    (cond ((not rule)
           (format stream "Symbol ~S is not a defined nonterminal." symbol))
          (t
           (format stream "~&Grammar ~S:~%" symbol)
           (multiple-value-bind (defined undefined) (rule-dependencies rule)
             (let ((length 
                    (+ 4 (max (reduce #'max (mapcar #'symbol-length defined)
                                      :initial-value 0)
                              (reduce #'max (mapcar #'symbol-length undefined)
                                      :initial-value 0)))))
               (format stream "~3T~S~VT<- ~S~%" 
                       symbol length (rule-expression rule))
               (when defined
                 (dolist (s defined)
                   (format stream "~3T~S~VT<- ~S~%" 
                           s length (rule-expression (find-rule s)))))
               (when undefined
                 (format stream "~%Undefined nonterminal~P:~%~{~3T~S~%~}"
                         (length undefined) undefined))))))))

;;; COMPILING RULES

(defvar *current-rule*)

(defun compile-rule (symbol expression transform)
  (let* ((*current-rule* symbol)
         (function (compile-expression expression)))
    (if transform
        (named-lambda rule (text position end)
          (with-cached-result (symbol position)
            (let ((result (funcall function text position end)))
              (if (error-result-p result)
                  (make-error-result 
                   :expression symbol
                   :position position
                   :detail result)
                  (make-result
                   :position (result-position result)
                   :production (funcall transform (result-production result)))))))
        (lambda (text position end)
          (with-cached-result (symbol position)
            (funcall function text position end))))))

;;; EXPRESSION COMPILER & EVALUATOR

(defun invalid-expression-error (expression)
  (error "Invalid expression: ~S" expression))

(defun validate-expression (expression)
  (or (typecase expression
        ((eql character)
         t)
        (terminal
         t)
        (nonterminal
         t)
        (cons
         (case (car expression)
           ((and or)
            (and (every #'validate-expression (cdr expression)) t))
           ((nil)
            nil)
           (string
            (and (cdr expression) (not (cddr expression))
                 (typep (second expression) 'array-length)))
           (t
            (and (symbolp (car expression))
                 (cdr expression) (not (cddr expression))
                 (validate-expression (second expression))))))
        (t
         nil))
      (invalid-expression-error expression)))

(defun %expression-dependencies (expression seen)
  (etypecase expression
    ((member character)
     seen)
    (terminal
     seen)
    (nonterminal
     (if (member expression seen :test #'eq)
         seen
         (let ((rule (find-rule expression))
               (seen (cons expression seen)))
           (if rule
               (%expression-dependencies (rule-expression rule) seen)
               seen))))
    (cons
     (case (car expression)
       (string
        seen)
       ((and or)
        (dolist (subexpr (cdr expression) seen)
          (setf seen (%expression-dependencies subexpr seen))))
       ((* + ? & !)
        (%expression-dependencies (second expression) seen))
       (t
        (%expression-dependencies (second expression) seen))))))

(defun %expression-direct-dependencies (expression seen)
  (etypecase expression
    ((member character)
     seen)
    (terminal
     seen)
    (nonterminal
     (cons expression seen))
    (cons
     (case (car expression)
       (string
        seen)
       ((and or)
        (dolist (subexpr (cdr expression) seen)
          (setf seen (%expression-direct-dependencies subexpr seen))))
       ((* + ? & !)
        (%expression-direct-dependencies (second expression) seen))
       (t
        (%expression-direct-dependencies (second expression) seen))))))

(defun eval-expression (expression text position end)
  (typecase expression
    ((eql character)
     (eval-character text position end))
    (terminal
     (eval-terminal (string expression) text position end))    
    (nonterminal
     (eval-nonterminal expression text position end))
    (cons
     (case (car expression)
       (string
        (eval-string expression text position end))
       (and
        (eval-sequence expression text position end))
       (or
        (eval-ordered-choise expression text position end))
       (*
        (eval-greedy-repetition expression text position end))
       (+
        (eval-greedy-positive-repetition expression text position end))
       (?
        (eval-optional expression text position end))
       (&
        (eval-followed-by expression text position end))
       (!
        (eval-not-followed-by expression text position end))
       (t
        (if (symbolp (car expression))
            (eval-semantic-predicate expression text position end)
            (invalid-expression-error expression)))))
    (t
     (invalid-expression-error expression))))

(defun compile-expression (expression)
  (etypecase expression
    ((eql character)
     (compile-character))
    (terminal
     (compile-terminal (string expression)))
    (nonterminal
     (compile-nonterminal expression))
    (cons
     (case (car expression)
       (string
        (compile-string expression))
       (and 
        (compile-sequence expression))
       (or
        (compile-ordered-choise expression))
       (*
        (compile-greedy-repetition expression))
       (+
        (compile-greedy-positive-repetition expression))
       (?
        (compile-optional expression))
       (&
        (compile-followed-by expression))
       (!
        (compile-not-followed-by expression))
       (t
        (if (symbolp (car expression))
            (compile-semantic-predicate expression)
            (invalid-expression-error expression)))))
    (t
     (invalid-expression-error expression))))

;;; Characters and strings

(declaim (inline exec-string))
(defun exec-string (length text position end)
  (let ((limit (+ length position)))
    (if (<= limit end)
        (make-result
         :production (subseq text position limit)
         :position limit)
        (make-error-result
         :expression 'character
         :position position))))

(defun eval-character (text position end)
  (exec-string 1 text position end))

(defun compile-character ()
  #'eval-character)

(defun eval-string (expression text position end)
  (with-expression (expression (string length))
    (exec-string length text position end)))

(defun compile-string (expression)
  (with-expression (expression (string length))
    (named-lambda compiled-string (text position end)
      (exec-string length text position end))))

;;; Terminals
;;;
;;; FIXME: It might be worth it to special-case terminals of length 1.

(declaim (inline exec-terminal))
(defun exec-terminal (string length text position end)
  (if (and (<= (+ length position) end)
           (dotimes (i length t)
             (unless (eql (char string i) (char text (+ i position)))
               (return nil))))
      (make-result
       :position (+ length position)
       :production string)
      (make-error-result
       :expression string
       :position position)))

(defun eval-terminal (string text position end)
  (exec-terminal string (length string) text position end))

(defun compile-terminal (string)
  (let ((length (length string)))
    (named-lambda compiled-terminal (text position end)
      (exec-terminal string length text position end))))

;;; Nonterminals

(defun eval-nonterminal (symbol text position end)
  (funcall (cell-function (ensure-rule-cell symbol)) text position end))

(defun compile-nonterminal (symbol)
  (let ((cell (if (boundp '*current-rule*)
                  (reference-rule-cell symbol *current-rule*)
                  (find-rule-cell symbol))))
    (declare (rule-cell cell))
    (named-lambda compile-nonterminal (text position end)
      (funcall (cell-function cell) text position end))))

;;; Sequences
;;;
;;; FIXME: It might be better if we actually chained the closures
;;; here, instead of looping over them -- benchmark first, though.

(defun eval-sequence (expression text position end)
  (with-expression (expression (and &rest subexprs))
    (let (results)
      (dolist (expr subexprs
               (make-result
                :position position
                :production (mapcar #'result-production (nreverse results))))
        (let ((result (eval-expression expr text position end)))
          (if (error-result-p result)
              (return (make-error-result
                       :expression expression
                       :position position
                       :detail result))
              (setf position (result-position result)))
          (push result results))))))

(defun compile-sequence (expression)
  (with-expression (expression (and &rest subexprs))
    (let ((functions (mapcar #'compile-expression subexprs)))
      (named-lambda compiled-sequence (text position end)
          (let (results)
            (dolist (fun functions 
                     (make-result
                      :position position
                      :production (mapcar #'result-production (nreverse results))))
              (let ((result (funcall fun text position end)))
                (if (error-result-p result)
                    (return (make-error-result 
                             :expression expression
                             :position position
                             :detail result))
                    (setf position (result-position result)))
                (push result results))))))))

;;; Ordered choises

(defun eval-ordered-choise (expression text position end)
  (with-expression (expression (or &rest subexprs))
    (let (last-error)
      (dolist (expr subexprs
               (make-error-result 
                :expression expression
                :position position
                :detail last-error))
        (let ((result (eval-expression expr text position end)))
          (if (error-result-p result)
              (when (or (not last-error)
                        (< (error-result-position last-error) 
                           (error-result-position result)))
                (setf last-error result))
              (return result)))))))

(defun compile-ordered-choise (expression)
  (with-expression (expression (or &rest subexprs))
    (let ((functions (mapcar #'compile-expression subexprs)))
      (named-lambda compiled-ordered-choise (text position end)
        (let (last-error)
          (dolist (fun functions 
                   (make-error-result 
                    :expression expression
                    :position position
                    :detail last-error))
            (let ((result (funcall fun text position end)))
              (if (error-result-p result)
                  (when (or (not last-error) 
                            (< (error-result-position last-error) 
                               (error-result-position result)))
                    (setf last-error result))
                  (return result)))))))))

;;; Greedy repetitions

(defun eval-greedy-repetition (expression text position end)
  (funcall (compile-greedy-repetition expression) text position end))

(defun compile-greedy-repetition (expression)
  (with-expression (expression (* subexpr))
    (let ((function (compile-expression subexpr)))
      (named-lambda compiled-greedy-repetition (text position end)
        (let ((results 
               (loop for result = (funcall function text position end)
                     until (error-result-p result)
                     do (setf position (result-position result))
                     collect result)))
          (make-result
           :position position
           :production (mapcar #'result-production results)))))))

;;; Greedy positive repetitions

(defun eval-greedy-positive-repetition (expression text position end)
  (funcall (compile-greedy-positive-repetition expression)
           text position end))

(defun compile-greedy-positive-repetition (expression)
  (with-expression (expression (+ subexpr))
    (let ((function (compile-expression subexpr)))
      (named-lambda compiled-greedy-positive-repetition (text position end)
        (let* ((last nil)
               (results
                (loop for result = (funcall function text position end)
                     until (error-result-p (setf last result))
                     do (setf position (result-position result))
                     collect result)))
          (if results
              (make-result
               :position position
               :production (mapcar #'result-production results))
              (make-error-result
               :position position
               :expression expression
               :detail last)))))))

;;; Optionals

(defun eval-optional (expression text position end)
  (with-expression (expression (? subexpr))
    (let ((result (eval-expression subexpr text position end)))
      (if (error-result-p result)
          (make-result :position position)
          result))))

(defun compile-optional (expression)
  (with-expression (expression (? subexpr))
    (let ((function (compile-expression subexpr)))
      (named-lambda compiled-optional (text position end)
        (let ((result (funcall function text position end)))
          (if (error-result-p result)
              (make-result :position position)
              result))))))

;;; Followed-by's

(defun eval-followed-by (expression text position end)
  (with-expression (expression (& subexpr))
    (let ((result (eval-expression subexpr text position end)))
      (if (error-result-p result)
          (make-error-result 
           :position position
           :expression expression
           :detail result)
          (make-result
           :position position
           :production (result-production result))))))

(defun compile-followed-by (expression)
  (with-expression (expression (& subexpr))
    (let ((function (compile-expression subexpr)))
      (named-lambda compiled-followed-by (text position end)
        (let ((result (funcall function text position end)))
          (if (error-result-p result)
              (make-error-result
               :position position
               :expression expression
               :detail result)
              (make-result
               :position position
               :production (result-production result))))))))

;;; Not followed-by's

(defun eval-not-followed-by (expression text position end)
  (with-expression (expression (! subexpr))
    (let ((result (eval-expression subexpr text position end)))
      (if (error-result-p result)
          (make-result 
           :position position)
          (make-error-result
           :expression expression
           :position position)))))

(defun compile-not-followed-by (expression)
  (with-expression (expression (! subexpr))
    (let ((function (compile-expression subexpr)))
      (named-lambda compiled-not-followed-by (text position end)
        (let ((result (funcall function text position end)))
          (if (error-result-p result)
              (make-result
               :position position)
              (make-error-result
               :expression expression
               :position position)))))))

;;; Semantic predicates

(defun eval-semantic-predicate (expression text position end)
  (with-expression (expression (t subexpr))
    (let ((result (eval-expression subexpr text position end)))
      (if (error-result-p result)
          (make-error-result
           :position position
           :expression expression
           :detail result)
          (let ((production (result-production result)))
            (if (funcall (symbol-function (car expression)) production)
                result
                (make-error-result 
                 :position position
                 :expression expression)))))))

(defun compile-semantic-predicate (expression)
  (with-expression (expression (t subexpr))
    (let* ((function (compile-expression subexpr))
           (predicate (car expression))
           ;; KLUDGE: Calling via a variable symbol can be slow, and if we
           ;; grab the SYMBOL-FUNCTION here we will not see redefinitions.
           (semantic-function 
            (if (eq (symbol-package predicate) (load-time-value (find-package :cl)))
                (symbol-function predicate)
                (compile nil `(lambda (x) (,predicate x))))))
      (named-lambda compiled-semantic-predicate (text position end)
        (let ((result (funcall function text position end)))
          (if (error-result-p result)
              (make-error-result
               :position position
               :expression expression
               :detail result)
              (let ((production (result-production result)))
                (if (funcall semantic-function production)
                    result
                    (make-error-result
                     :position position
                     :expression expression)))))))))
