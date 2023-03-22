;; -*- lexical-binding: t; -*-

(eval-and-compile
  (require 'doc-scroll))

;;;###autoload
(define-minor-mode doc-mupdf-mode
  "MuPDF backend for Doc-Scroll."
  :lighter " MuPDF"
  ;; (doc-mupdf-create-pages doc-scroll-overlay-width)

  (setq-local doc-scroll-number-of-pages              (doc-mupdf-number-of-pages)
              doc-scroll-internal-page-sizes          (doc-mupdf-page-sizes)
	      doc-scroll-image-data-function          #'doc-mupdf-get-image-data
	      doc-scroll-create-image-files-async-fun #'doc-mupdf-create-image-files-async
	      doc-scroll-async t))
              ;; doc-scroll-internal-page-sizes (doc-mupdf-page-sizes)
              ;; doc-scroll-last-page (length doc-scroll-internal-page-sizes)
              ;; doc-scroll-structured-contents (doc-poppler-structured-contents nil nil t)

              ;;  ;; doc-scroll-display-page-function #'doc-scroll-djvu-display-page
              ;;  doc-scroll-image-type 'png
              ;;  doc-scroll-image-data-function #'doc-mupdf-get-image-data

              ;;  imenu-create-index-function #'doc-scroll-mupdf--imenu-create-index
              ;;  imenu-default-goto-function (lambda (_name position &rest _rest)
              ;;                                ;; NOTE WEIRD, the first result is
              ;;                                ;; a number, while the other
              ;;                                ;; results are markers
              ;;                                (doc-scroll-goto-page (if (markerp position)
              ;;                                                     (marker-position position)
              ;;                                                   position)))
              ;;  doc-scroll-info-function #'doc-mupdf-info))

(defvar doc-mupdf-info-commands '(doc-mupdf-page-sizes
                              doc-mupdf-structured-contents))

(defun doc-mupdf-number-of-pages (&optional file)
  (setq file (or file buffer-file-name))
  (call-process "mutool" nil "mupdf" nil
		"info" "-M" file)
  (with-current-buffer "mupdf"
    (goto-char (point-min))
    (search-forward-regexp "Pages: ")
    (prog1
	(string-to-number (nth 1 (split-string (thing-at-point 'line))))
      (kill-buffer))))

(defun doc-mupdf-page-sizes (&optional file)
  (setq file (or file buffer-file-name))
  (call-process "mutool" nil "mupdf" nil
		"pages" file)
  (with-current-buffer "mupdf"
    (goto-char (point-min))
    (delete-line)
    (prog1 (let-alist (libxml-parse-html-region)
	     (let* ((pages (cdr .body))
		    (page-dims (mapcar (lambda (p)
				    (car (alist-get 'mediabox p)))
				  pages))
		    size results)
	       (dolist (dims page-dims)
		 (when-let (r (mapcar (lambda (d)
					(string-to-number (cdr d)))
				      dims))
		   (setq size r))
		 (push (cons (nth 2 size) (nth 3 size)) results))
	       (nreverse results)))
      (kill-buffer)))) 

  ;; (kill-buffer))))

(defun doc-mupdf-info (function &optional arg)
  (interactive (if (member (file-name-extension (buffer-file-name))
                           '("pdf" "epub"))
                   (list (intern-soft (completing-read "Select info type: "
                                                       doc-mupdf-info-commands))
                         current-prefix-arg)
                 (user-error "Buffer file not of `pdf' or `epub' type")))
  (pp (pcase function
        ('doc-mupdf-structured-contents (call-interactively #'doc-mupdf-structured-contents))
        (var (funcall var)))
      (when arg
        (get-buffer-create "*doc-mupdf-info*")))
  (when arg (pop-to-buffer "*doc-mupdf-info*")))

;;      (let* ((lines (process-lines "mutool" "info" "-M"
;;                                         file))
;;                   (split-lines (mapcar (lambda (l)
;;                                              (split-string l " " t))
;;                                            lines))
;;                   (pages (or (string-to-number (cadr (nth 5 split-lines)))
;;                              (user-error "Output does not have 11 lines;
;; the function `doc-mupdf-page-sizes' should get modified/generalized")))
;;                   (box-line (nth 9 split-lines)))
;;              (make-list pages (cons (string-to-number (nth 5 box-line))
;;                                     (string-to-number (nth 6 box-line))))))))

(defun doc-mupdf-get-image-data (page width &optional file)
  (setq file (or file (buffer-file-name)))
  (call-process "mutool" nil nil nil
                "draw"
                "-o" "/tmp/pdf-temp-img"
                "-w" (number-to-string width)
                file
                (number-to-string page))
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (setq coding-system-for-read 'binary)
    (insert-file-contents-literally "/tmp/pdf-temp-img")
    (buffer-substring-no-properties (point-min) (point-max))))

(defun doc-mupdf-create-image-files-async (outdir &optional width &rest pages)
  (start-process "mutool" "mutool" "mutool"
		 "draw"
		 "-o" (concat outdir "page-%d.png")
                 "-w" (number-to-string (or width doc-scroll-cache-file-image-width))
		 ;; "-I"
		 buffer-file-name
		 (mapconcat #'number-to-string pages ",")))

;; (defun doc-mupdf-create-pages (width &optional file force)
;;   (setq file (or file buffer-file-name))
;;   (let ((outdir (concat "/tmp/doc-tools/" (file-name-as-directory (file-name-base file)) "pages/")))
;;     (when (or (not (file-exists-p outdir)) force)
;;       (unless (file-exists-p outdir)
;;         (make-directory outdir t))
;;       (let ((proc (start-process "mutool" "mutool create page files" "mutool"
;;                                  "draw"
;;                                  "-o" (concat outdir "page-%d.png")
;;                                  "-w" (number-to-string width)
;;                                  file)))
;;         (set-process-sentinel proc (lambda (process event)
;;                                      (message "Create pages process %s" event)))))))

(defun doc-mupdf-create-thumbs (&optional file force)
  (setq file (or file buffer-file-name))
  (let ((outdir (concat "/tmp/doc-tools/"
                        (file-name-as-directory (file-name-base file))
                        "thumbs/")))
    (when (or (not (file-exists-p outdir)) force)
      (unless (file-exists-p outdir)
        (make-directory outdir t))
      (let ((proc (start-process "mutool" "mutool create thumbs" "mutool"
                                 "draw"
                                 "-o" (concat outdir "thumb%d.png")
                                 "-w" "200"
                                 file)))
        (set-process-sentinel proc (lambda (process event)
                                     (message "Create pages process %s" event)))))))


(defun doc-mupdf-parse-coords (coords-string)
  (mapcar #'string-to-number (split-string coords-string)))

(defun doc-mupdf-parse-line (line-contents)
  (append (doc-mupdf-parse-coords (alist-get 'bbox (nth 1 line-contents)))
          (list (mapcan (lambda (e)
                          (mapconcat (lambda (c)
                                       (alist-get 'c (nth 1 c)))
                                     (nthcdr 2 e)))
                        (nthcdr 2 line-contents)))))

(defun doc-mupdf-structured-contents (&optional page detail file)
  (interactive "nEnter page number: ")
  (setq file (or file buffer-file-name))
  (let (text)
    (with-temp-buffer
      (apply #'call-process "mutool" nil t nil
               "draw" "-F" "stext" file
               (when page (list (number-to-string page))))
               ;; (when pages (list (mapconcat #'number-to-string pages ","))))
      (goto-char (point-min))
      (flush-lines "^warning")
      (setq text (libxml-parse-xml-region)))
    text))
    ;; (when detail ;i.e. page or more detail
    ;;   (setq text (nthcdr 2 text)))
    ;; (when (memq detail '(block line char))
    ;;   (setq text (mapcan (apply-partially #'nthcdr 2) text)))
    ;; (when (memq detail '(line char))
    ;;   (setq text (mapcan (apply-partially #'nthcdr 2) text)))
    ;; (when (eq detail 'char)
    ;;   (setq text (mapcan (apply-partially #'nthcdr 2)
    ;;                      (mapcan (apply-partially #'nthcdr 2) text))))
    ;; (mapcar #'doc-mupdf-parse-line text)))

(defun doc-mupdf-outline (&optional file)
  (setq file (or file (buffer-file-name)))
  (mapcar (lambda (l)
            (let* ((parts (split-string l "\\(#page=\\|&zoom\\)"))
                   (p1 (split-string (car parts) "\""))
                   (level (- (length (car p1)) 2))
                   (title (string-join (nbutlast (cdr p1)))))
              (cons level (cons title (string-to-number (nth 1 parts))))))
          (process-lines "mutool" "show" file "outline")))

  ;; (with-current-buffer (get-buffer-create "*pdf-outline*")
  ;;   (call-process "mutool" nil t nil "show" file "outline")
  ;;   (while (not (bobp))
  ;;     (forward-line -1)
  ;;     (delete-char 2))

  ;;   (save-excursion
  ;;     (search-forward-regexp "^[[:space:]]*\""))
  ;;   (let ((current-level (length (match-string 0))))
  ;;     (insert "((")
  ;;     (forward-line)
  ;;     (while (not (eobp))
  ;;       (save-excursion
  ;;         (search-forward-regexp "^[[:space:]]*\""))
  ;;       (let ((new-level (length (match-string 0))))
  ;;         (cond ((= new-level current-level)
  ;;                (insert ")("))
  ;;               ((> new-level current-level)
  ;;                (insert "("))
  ;;               (t
  ;;                (dotimes (_ (1+ (- current-level new-level)))
  ;;                        (insert ")"))
  ;;                (insert "(")))
  ;;         (setq current-level new-level)
  ;;         (forward-line)))
  ;;     (dotimes (_ (1+ current-level))
  ;;       (insert ")")))))

