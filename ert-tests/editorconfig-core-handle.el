(require 'editorconfig-core-handle)

(ert-deftest editorconfig-core-handle ()
  ;; handle.ini
  (let* ((fixtures (concat default-directory
                           "ert-tests/fixtures/"))
         (conf (concat fixtures
                       "handle.ini"))
         (handle (editorconfig-core-handle conf)))
    (should (editorconfig-core-handle-root-p handle))
    (should (equal (editorconfig-core-handle-get-properties handle
                                                            (concat fixtures
                                                                    "b.js"))
                   '((("key2" . "value2")))))
    (should (equal (editorconfig-core-handle-get-properties handle
                                                            (concat fixtures
                                                                    "a.js"))
                   '((("key1" . "value1")) (("key2" . "value2"))))))
  ;; Test twice for checking cache
  (let* ((fixtures (concat default-directory
                           "ert-tests/fixtures/"))
         (conf (concat fixtures
                       "handle.ini"))
         (handle (editorconfig-core-handle conf)))
    (should (editorconfig-core-handle-root-p handle))
    (should (equal (editorconfig-core-handle-get-properties handle
                                                            (concat fixtures
                                                                    "b.js"))
                   '((("key2" . "value2")))))
    (should (equal (editorconfig-core-handle-get-properties handle
                                                            (concat fixtures
                                                                    "a.js"))
                   '((("key1" . "value1")) (("key2" . "value2"))))))

  ;; handle2.ini
  (let* ((fixtures (concat default-directory
                           "ert-tests/fixtures/"))
         (conf (concat fixtures
                       "handle2.ini"))
         (handle (editorconfig-core-handle conf)))
    (should-not (editorconfig-core-handle-root-p handle))
    (should (equal (editorconfig-core-handle-get-properties handle
                                                            (concat fixtures
                                                                    "b.js"))
                   nil))
    (should (equal (editorconfig-core-handle-get-properties handle
                                                            (concat fixtures
                                                                    "a.js"))
                   '((("key" . "value"))))))
  )
