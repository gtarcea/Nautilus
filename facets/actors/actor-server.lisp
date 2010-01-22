(in-package :com.facets.actors)

;;;;
;;;; This server implements the remote actors interface. It allows actors
;;;; on remote hosts or in other processes to communicate with actors on
;;;; this host/process. It processes all the remote mailbox calls and
;;;; sends them to the appropriate actor.
;;;;

;;;;
;;;; Remote API:
;;;;
;;;;  send-msg
;;;;  receive-msg
;;;;  receive-msg-if
;;;;