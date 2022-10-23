(require 'editorconfig-core)

(set-variable 'vc-handled-backends nil)

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
