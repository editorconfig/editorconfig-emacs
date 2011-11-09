(defun set-indentation (style &optional size)
  "Set indentation type from given style and size"
  (if (equal style "space")
      (setq indent-tabs-mode nil
	    size (string-to-number size)
	    c-basic-offset size
	    python-indent size
	    py-indent-offset size
	    perl-indent-level size
	    cperl-indent-level size
	    tab-stop-list (let ((stops (cons size ())))
			    (while (< (car stops) 120)
			      (setq stops (cons
					   (+ size (car stops))
					   stops)))
			    (nreverse stops)))
    (setq indent-tabs-mode t)))

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
		      (let (props indent_style indent_size)
			(setq props (parse-properties (get-properties))
			      indent_style (gethash "indent_style" props)
			      indent_size (gethash "indent_size" props))
			(set-indentation indent_style indent_size)))))
