(in-package :skipdb-tests)

(fiveam:def-suite skipdb-support-test-suite :description "Tests for skipdb support utilities.")

(fiveam:in-suite skipdb-support-test-suite)

(fiveam:test no-such-directory
  (fiveam:is (null (skipdb::db-dir-open "/does/not/exist"))))

(fiveam:test directory-exists
  (fiveam:is (skipdb::db-dir-open "/tmp")))

(fiveam:test key-ranges
  (let ((dbdir (skipdb::db-dir-open "/tmp" :key-start 1 :key-range 10)))
    (multiple-value-bind (start end) (skipdb::get-keys dbdir 1)
      (fiveam:is (= start 1))
      (fiveam:is (= end 10)))
    (multiple-value-bind (start end) (skipdb::get-keys dbdir 10)
      (fiveam:is (= start 1))
      (fiveam:is (= end 10)))
    (multiple-value-bind (start end) (skipdb::get-keys dbdir 11)
      (fiveam:is (= start 11))
      (fiveam:is (= end 20)))
    (multiple-value-bind (start end) (skipdb::get-keys dbdir 15)
      (fiveam:is (= start 11))
      (fiveam:is (= end 20)))))
