(defun set-indentation (style &optional width)
  "Set indentation type from given style and tab width"
  (if (equal style "space")
      (setq indent-tabs-mode nil
            tab-width (string-to-number width)
            tab-stop-list (let ((stops (cons tab-width ())))
                            (while (< (car stops) 120)
                              (setq stops (cons
					   (+ tab-width (car stops))
					   stops)))
                            (nreverse stops)))
    (setq indent-tabs-mode t))
  )

(defun get-properties ()
  "Call EditorConfig core and return output"
  (let ((oldbuf (current-buffer)))
    (call-process "editorconfig" nil "ecbuffer" nil (buffer-file-name oldbuf))
    (set-buffer (get-buffer "ecbuffer"))
    (let (props-string)
      (setq props-string (buffer-string))
      (set-buffer oldbuf)
      props-string)))

(defun parse-properties (props-string)
  "Create properties hash table from string of properties"
  (let (props-list properties)
    (setq props-list (split-string props-string "\n")
	  properties (make-hash-table :test 'equal))
    (dolist (prop props-list properties)
      (let ((key-val (split-string prop " *= *")))
	(if (> (length key-val) 1)
	    (let (key val)
	      (setq key (car key-val)
		    val (mapconcat 'identity (cdr key-val) ""))
	    (puthash key val properties)))))))

(add-hook 'find-file-hook
	  (function (lambda ()
		      (let (props indent_style tab_width)
			(setq props (parse-properties (get-properties))
			      indent_style (gethash "indent_style" props)
			      tab_width (gethash "tab_width" props))
			(set-indentation indent_style tab_width)))))
