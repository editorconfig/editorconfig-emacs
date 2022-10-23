(require 'editorconfig-core-handle)

(defconst fixtures (concat (file-name-directory load-file-name)
                           "fixtures/"))

(set-variable 'vc-handled-backends nil)

(ert-deftest test-editorconfig-core-handle ()
  ;; handle.ini
  (let* ((conf (concat fixtures
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
  (let* ((conf (concat fixtures
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
  (let* ((conf (concat fixtures
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

  ;; For checking various normal whitespace (line breaks, horizontal space, vertical space, etc.)
  (let* ((conf (concat default-directory
                       "ert-tests/whitespaces/example-editorconfig.txt"))
         (handle (editorconfig-core-handle conf)))
    (should (editorconfig-core-handle-p handle)))
  )
