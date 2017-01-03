(require 'package)

(defvar metadata-el-files nil)

(ert-deftest test-metadata ()
  (dolist (el metadata-el-files)
    (message "Loading info: %s"
             el)
    (with-temp-buffer
      (insert-file-contents el)
      (message "%S"
               (package-buffer-info)))))
