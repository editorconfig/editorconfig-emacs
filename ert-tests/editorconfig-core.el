(require 'editorconfig-core)

(ert-deftest test-editorconfig-core--remove-duplicate ()
  (should (equal (editorconfig-core--remove-duplicate '(("a" . 1) ("b" . 2) ("c" . 3) ("b" . 4)))
            '(("a" . 1) ("b" . 4) ("c" . 3))))
  (should (equal (editorconfig-core--remove-duplicate '(("a" . 1) ("b" . 2) ("c" . 3)))
            '(("a" . 1) ("b" . 2) ("c" . 3))))
  (should (equal (editorconfig-core--remove-duplicate nil)
            nil))
  )


(ert-deftest test-editorconfig-core--get-handles ()
  (let* ((fixtures (concat default-directory
                     "/ert-tests/fixtures/"))
          (dir (concat fixtures
                 "dir1"))
          (confname "parent.ini")
          (handles (editorconfig-core--get-handles dir
                     confname)))
    (should (= 2
              (length handles)))
    (should (editorconfig-core-handle-p (car handles)))
    (should (editorconfig-core-handle-p (cadr handles)))))
