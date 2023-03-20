;;; doc-pymupdf.el --- Manipulate pdf files with doc-pymupdf-epc-server  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Daniel Nicolai

;; Author: Daniel Nicolai <dalanicolai@fedora>
;; Keywords: multimedia, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(require 'epc)
(require 'svg)

(defconst doc-pymupdf-dir
  (file-name-directory (or load-file-name buffer-file-name)))

(defcustom doc-pymupdf-epc-executable
  (expand-file-name "doc-pymupdf-epc-server.py" doc-pymupdf-dir)
  "Name of doc-pymupdf-epc-server python file.")

(defvar doc-pymupdf-epc-server nil)

(defvar doc-pymupdf-text-page-methods
  "See https://pymupdf.readthedocs.io/en/latest/textpage.html#textpage"
  '(text blocks words xml))

(defvar doc-pymupdf-epc-info-commands '(doc-pymupdf-epc-number-of-pages
                                    doc-pymupdf-epc-toc
                                    doc-pymupdf-epc-metadata
                                    doc-pymupdf-epc-page-structured-text))

(defun doc-svg-embed-base64 (svg data image-type &rest args)
  "Insert IMAGE into the SVG structure.
IMAGE should be a file name if DATAP is nil, and a binary string
otherwise.  IMAGE-TYPE should be a MIME image type, like
\"image/jpeg\" or the like."
  (svg--append
   svg
   (dom-node
    'image
    `((xlink:href . ,(concat "data:" image-type ";base64," data))
      ,@(svg--arguments svg args)))))

(defsubst list-to-cons (pair-list)
  (cons (nth 0 pair-list)
        (nth 1 pair-list)))

(defun doc-pymupdf-kill-server ()
  (epc:stop-epc doc-pymupdf-epc-server))

;;;###autoload
(define-minor-mode doc-pymupdf-mode
  "Minor mode for activating the DocScroll PyMuPDF backend."
  :lighter " PyMuPDF"
  (setq doc-pymupdf-epc-server (doc-pymupdf-epc-start-server t))
  (doc-pymupdf-epc-init)
  (add-hook 'kill-buffer-hook #'doc-pymupdf-kill-server nil t)

  (add-hook 'write-file-functions #'doc-pymupdf-epc-save nil t)

  (setq doc-scroll-number-of-pages (doc-pymupdf-epc-number-of-pages)
        doc-scroll-internal-page-sizes (doc-pymupdf-epc-page-sizes)
	doc-scroll-image-data-function #'doc-pymupdf-epc-page-base64-image-data
	doc-scroll-async t))
	;; 	doc-scroll-structured-text-fun (lambda ()
  ;; 					 (run-with-idle-timer
  ;; 					  1 nil (lambda ()
  ;; 						  (dotimes (i doc-scroll-number-of-pages)
  ;; 						    (let ((p (1+ i)))
  ;; 						      (doc-pymupdf-epc-structured-text 'words p p t t)))))))

  ;; imenu-create-index-function #'doc-backend-pymupdf--imenu-create-index
  ;; imenu-default-goto-function (lambda (_name position &rest _rest)
  ;;                               ;; NOTE VERY WEIRD, the first
  ;;                               ;; result is a number, while the
  ;;                               ;; other results are markers
  ;;                               (doc-scroll-goto-page (if (markerp position)
  ;;                                                         (marker-position position)
  ;;                                                       position))))

;;; EPC
(defun doc-pymupdf-epc-info (function &optional arg)
  (interactive (list (completing-read "Select info type: "
                                      doc-pymupdf-epc-info-commands)
                     current-prefix-arg))
  (pp (pcase (intern function)
        ('doc-pymupdf-epc-page-structured-text
         (call-interactively #'doc-pymupdf-epc-page-structured-text))
        (var (funcall var)))
      (when arg
        (get-buffer-create "*doc-pymupdf-epc-info*")))
  (when arg (pop-to-buffer "*doc-pymupdf-epc-info*")))

(defun doc-pymupdf-epc-start-server (&optional local)
  (interactive)
  (let ((server (epc:start-epc "python" (list doc-pymupdf-epc-executable))))
    (unless local
      (setq doc-pymupdf-epc-server server))
    server))

(defun doc-pymupdf-epc-kill-server (&optional local)
  (interactive)
  (epc:stop-epc doc-pymupdf-epc-server))

(defun doc-pymupdf-epc-restart-server (&optional local)
  (interactive)
  (epc:manager-restart-process doc-pymupdf-epc-server))

(defun doc-pymupdf-epc-test ()
  (interactive)
  (pp (epc:call-sync doc-pymupdf-epc-server 'test (list 'hello))))

(defun doc-pymupdf-epc-init (&optional file)
  (interactive "fSelect pdf file: ")
  (epc:call-sync doc-pymupdf-epc-server 'open (list (or file (buffer-file-name)))))

(defun doc-pymupdf-epc-number-of-pages ()
  (interactive)
  (epc:call-sync doc-pymupdf-epc-server 'number_of_pages nil))

(defun doc-scroll-structured-text-normalize (page words)
  "WORDS should be a page structured text as obtained from the server."
  (pcase-let ((`(,w . ,h) (nth (1- page) doc-scroll-internal-page-sizes)))
    (mapcar (lambda (word)
	      (append (list (/ (nth 0 word) w)
			    (/ (nth 1 word) h)
			    (/ (nth 2 word) w)
			    (/ (nth 3 word) h))
		      (last word 4)))
	    words)))

(defun doc-pymupdf-epc-structured-text (&optional detail start-page end-page async normalize-push)
  "If PUSH non-nil then push to (buffer-local) `doc-scroll-structured-text'."
  (let ((buffer (current-buffer)))
    (if async
	(deferred:$
	 (epc:call-deferred doc-pymupdf-epc-server
			    'structured_text
			    (list (1- (or start-page 1)) end-page (if detail (symbol-name detail))))
	 (deferred:nextc it
			 (lambda (x)
			   (if normalize-push
			       (with-current-buffer buffer
				 ;; (push (doc-scroll-structured-text-normalize 5 (car x)) doc-scroll-structured-text)
				 (push (car x) doc-scroll-structured-text)
				 (when (= (print (length doc-scroll-structured-text)) doc-scroll-number-of-pages)
				 ;; (when (= (length doc-scroll-structured-text) doc-scroll-number-of-pages)
				   (setq doc-scroll-structured-text (nreverse doc-scroll-structured-text))
				   (message "Text layer retrieval finished")))
			     x))
			   ;; (setq-local doc-scroll-structured-contents x)
			   ;; (message (concat (propertize "doc-scroll-structured-contents" 'face 'bold) " set"))))))
			   ))
      (epc:call-sync doc-pymupdf-epc-server
		     'structured_text
		     (list (1- (or start-page 1)) end-page (if detail (symbol-name detail)))))))

;; (defun doc-pymupdf-epc-page-structured-text (&optional page detail)
;;   (interactive (let ((last-page (doc-pymupdf-epc-number-of-pages)))
;;                  (list (read-number (format "Select page(s) (max %s): " last-page)
;;                                     (or (doc-scroll-current-page) 1))
;;                        (intern (completing-read "Select detail: "
;;                                                 '(plain djvu blocks words xml))))))
;;   (when (eq detail 'plain) (setq detail nil))
;;   (let ((text (epc:call-sync doc-pymupdf-epc-server
;;                              'page_structured_text
;;                              (list page (symbol-name detail)))))
;;     (if (eq detail 'xml)
;;         (with-temp-buffer
;;           (insert text)
;;           (xml-parse-region))
;;       text)))

(defun doc-pymupdf-epc-restructure-text (text)
  "Convert structured text to djvu text structure."
  (mapcar (lambda (e)
            (let ((type (car e)))
              (pcase type
                ('page (let-alist (nth 1 e)
                         (append (list type 0 0
                                       (string-to-number .width) (string-to-number .height))
                                 (doc-pymupdf-epc-restructure-text
                                  (delete "\n" (nthcdr 3 e))))))
                ((or 'line 'block) (append (cons type (mapcar #'string-to-number
                                                              (split-string (cdar (nth 1 e)))))
                                           (doc-pymupdf-epc-restructure-text
                                            (pcase type
                                              ('block (delete "\n" (nthcdr 3 e)))
                                              ('line (delete "\n" (nthcdr 3 (nth 3 e))))))))
                ('char (let-alist (nth 1 e)
                         (let ((coord-string (split-string .quad)))
                           `(,type
                             ,@(mapcar (lambda (n)
                                         (string-to-number (nth n coord-string)))
                                       '(0 1 6 7))
                             ,.c)))))))
          text))

(defun doc-pymupdf-epc-page-sizes ()
  (interactive)
  (let ((sizes (epc:call-sync doc-pymupdf-epc-server 'pagesizes nil)))
        (mapcar #'list-to-cons sizes)))

(defun doc-pymupdf-epc-page-svg-data (page text)
  (interactive "nEnter page number: ")
  (epc:call-sync doc-pymupdf-epc-server 'renderpage_svg (list page text)))

(defun doc-pymupdf-epc-page-base64-image-data (page width)
  (interactive "nEnter page number: ")
  (epc:call-sync doc-pymupdf-epc-server 'renderpage_base64 (list page width)))

(defun doc-pymupdf-epc-display-image-async (page width &optional window)
  (interactive "nEnter page number: ")
  (deferred:$ (epc:call-deferred doc-pymupdf-epc-server 'renderpage_base64 (list page width))
	      (deferred:nextc it (lambda (x)
				   (with-selected-window (or window (selected-window))
				     (doc-scroll-display-image (doc-scroll-page-overlay page)
							       ;; (base64-decode-string x)
							       x t t))))))

(defun doc-pymupdf-epc-page-image-file (page width path)
  (interactive "nEnter page number: ")
  (epc:call-sync doc-pymupdf-epc-server 'renderpage_file (list page width path)))

(defun doc-pymupdf-epc-toc ()
  (interactive)
  (epc:call-sync doc-pymupdf-epc-server 'toc nil))

(defun doc-pymupdf-epc-metadata ()
  (interactive)
  (epc:call-sync doc-pymupdf-epc-server 'metadata nil))

(defun doc-pymupdf-epc-get-annots (page)
  (interactive "nEnter page number: ")
  (epc:call-sync doc-pymupdf-epc-server 'get_annots (list page)))

(defun doc-pymupdf-epc-add-annot (page
				  edges
				  style
				  &optional display width)
  (epc:call-sync doc-pymupdf-epc-server
			     'addannot
			     (list page edges style display width)))
    

(defun doc-pymupdf-epc-search (pattern)
  (interactive "sEnter pattern: ")
  (epc:call-sync doc-pymupdf-epc-server 'search (list pattern)))

(defun doc-pymupdf-epc-list-colors ()
  (interactive "nEnter pattern: ")
  (epc:call-sync doc-pymupdf-epc-server 'list_colors nil))

(defun doc-pymupdf-epc-text-blocks ()
  (interactive "nEnter pattern: ")
  (epc:call-sync doc-pymupdf-epc-server 'text_blocks nil))

(defun doc-pymupdf-epc-save ()
  (interactive)
  (let ((status (epc:call-sync doc-pymupdf-epc-server 'save (list buffer-file-name))))
    (pcase status
      ('t (set-buffer-modified-p nil)
	  (message "Document saved succesfully"))
      (_ status))))

(provide 'doc-pymupdf-epc)
;;; doc-pymupdf.el ends here
