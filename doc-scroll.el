;;; doc-scroll.el --- Let's do it!             -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Daniel Nicolai

;; Author: Daniel Nicolai <dalanicolai@fedora>
;; Keywords: tools
;; Version: 0.1

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

(require 'image-mode)
(require 'svg)

;; (defun doc-svg-embed-base64 (svg data image-type &rest args)
;;   "Insert IMAGE into the SVG structure.
;; IMAGE should be a file name if DATAP is nil, and a binary string
;; otherwise.  IMAGE-TYPE should be a MIME image type, like
;; \"image/jpeg\" or the like."
;;   (svg--append
;;    svg
;;    (dom-node
;;     'image
;;     `((xlink:href . ,(concat "data:" image-type ";base64," data))
;;       ,@(svg--arguments svg args)))))

(defvar doc-scroll-incompatible-modes '(visual-line-mode
                                        global-hl-line-mode))

(defvar-local doc-scroll-step-size 80)

(defmacro doc-scroll-overlays (&optional winprops)
  "List of overlays that make up a scroll.
Setf-able (macro)."
  `(image-mode-window-get 'overlays ,winprops))

(defun doc-scroll-page-overlay (page)
  (nth (1- page) (image-mode-window-get 'overlays)))

(defun doc-scroll-overlay-size (page)
  "Size of overlay containing PAGE image."
  (overlay-get (nth (1- page) (doc-scroll-overlays)) 'size))

(defsubst doc-scroll-current-overlay ()
  (seq-find (lambda (o)
              (eq (overlay-get o 'window) (selected-window)))
            (overlays-at (point))))

(defun doc-scroll-current-page ()
  (1+ (overlay-get (doc-scroll-current-overlay) 'i)))

(defun doc-scroll-current-overlay-height ()
  (cdr (overlay-get (doc-scroll-current-overlay) 'size)))

(defun doc-scroll-create-overlays (number
                                   &optional columns hspace vspace text
                                   &rest overlay-props)
  "This is a general function for creating a grid of overlays.
This function is used by `doc-scroll' to create the scroll,
however, it is written in a general way so that it can be used to
quickly create overlay grid of different forms.

NUMBER is the number of overlays in the grid.

COLUMNS (number) is the number of columns in the grid.

HSPACE/VSPACE (number) are the number of horizontal/vertical
spaces/newlines added between the overlays.

TEXT is the text that is used as a placeholder for the overlay."
  (setq columns (or columns 1))
  (dolist (m doc-scroll-incompatible-modes)
    (funcall m -1))
  (toggle-truncate-lines 1) ; also disables visual-mode
  (let (overlays)
    ;; (ldbg "OVERLAYS")
    (dotimes (i number)
      (let* ((n (1+ i))
             (o (make-overlay
                 (point)
                 (progn (insert (cond ((stringp text) text)
                                      ((functionp text) (funcall text n))
                                      (t " ")))
                        (point))
                 (unless (= (% columns 3) 0)
                   (insert (make-string (or hspace 0) (string-to-char " ")))))))
        (when (and (= (% n columns) 0)
                   (not (= n number)))
          (insert "\n")
          (insert (make-string (or vspace 0) (string-to-char "\n"))))
        (overlay-put o 'i i)
        (dotimes (j (/ (length overlay-props) 2))
          (let ((m (* j 2)))
            (overlay-put o (nth m overlay-props) (nth (+ m 1) overlay-props))))
        (push o overlays)))
    (goto-char (point-min))
    ;; (setq-local doc-scroll-columns columns) ; TODO make a winprop
    ;; (doc-scroll-mode)
    (nreverse overlays)))

;; The `image-mode-new-window-functions' are called from `image-mode-winprops'
;; (with the single argument `winprops'). To obtain the winprops for the correct
;; window (e.g. when using `(car winprops)' below), `image-mode-winprops' should
;; somehow (i.e. directly or indirectly) get called from the
;; `window-configuration-change-hook'.
(defun doc-scroll-new-window-function (winprops)
  ;; (ldbg "NEW WINDOW")
  ;; (ldbg (car winprops))
  ;; (ldbg  image-mode-winprops-alist)
  (if (not (overlays-at 1))
      (let ((inhibit-read-only t)
            overlays) ; required because we derive mode (inherit) from
                                        ; `special-mode'

        (erase-buffer)
        (setq overlays (doc-scroll-create-overlays doc-scroll-number-of-pages
                                                   nil nil nil
                                                   (make-string 120 (string-to-char " "))
                                                   'window (car winprops)))
        (image-mode-window-put 'overlays overlays)
        (image-mode-window-put 'columns 1))

    ;; To independently update overlays in different windows, we create "window
    ;; local" overlays using the overlay `window' property (see `Overlay
    ;; Properties' documentation), and store them as winprops to enable easy
    ;; access.
    ;; When splitting some window, the new window inherits the winprops.
    ;; Therefore, we can just use (image-mode-window-get 'overlays) to retrieve
    ;; the "original" overlays (from the new window).
    (let ((overlays (mapcar (lambda (o)
                         (let ((oc (copy-overlay o)))
                           (overlay-put oc 'window (car winprops))
                           oc))
                       (image-mode-window-get 'overlays))))
      (image-mode-window-put 'overlays overlays winprops)
      (when (image-mode-window-get 'win-width)
        (doc-scroll-redisplay 'force))

      (goto-char (point-min)))))

(defun doc-scroll-redisplay (&optional force)
  ;; (ldbg "WINDOW CONFIGURATION CHANGE (redisplay)")
  (when (or force
            (/= (or (image-mode-window-get 'win-width) -1)
                (window-pixel-width)))
    (image-mode-window-put 'win-width (window-pixel-width))
    (let* ((w (doc-scroll-overlay-base-width (image-mode-window-get 'columns) 0))
           (h (doc-scroll-overlay-base-height w))
           (overlays (image-mode-window-get 'overlays)))
      (dolist (o overlays)
        (overlay-put o 'display `(space . (:width (,w) :height (,h))))
        (overlay-put o 'size (cons w h)))
      ;; NOTE this seems not required when a non-nil UPDATE argument is passes
      ;; to the `window-end' function (however, the outcommenting might lead
      ;; errors)
      ;; (redisplay)
      (dolist (o (doc-scroll-visible-overlays))
        (doc-scroll-display-page o nil t)))))

(defun doc-scroll-overlay-base-width (columns hspace)
  (let ((win-width (- (nth 2 (window-inside-pixel-edges))
                      (nth 0 (window-inside-pixel-edges)))))
    (/ (- win-width (* (1- columns) (* hspace (frame-char-width))))
       columns)))

(defun doc-scroll-overlay-base-height (base-width)
  (let* ((widest-page-w (apply #'max (mapcar #'car doc-scroll-internal-page-sizes)))
         (ratio (/ (float base-width) widest-page-w)))
    (floor (* ratio (apply #'max (mapcar #'cdr doc-scroll-internal-page-sizes))))))

(defun doc-scroll-visible-overlays ()
  (let* ((visible (overlays-in (window-start) (window-end nil t)))
         (start (apply #'min (mapcar (lambda (o) (overlay-get o 'i)) visible)))
         (end (apply #'max (mapcar (lambda (o) (overlay-get o 'i)) visible)))
         ;; include previous/next rows for 'smoother' displaying
         (new-start (max (- start (image-mode-window-get 'columns)) 0))
         (new-end (min (+ end (image-mode-window-get 'columns)) (1- doc-scroll-number-of-pages))))
         ;; start and end should be limited to index start/end page
    ;; (seq-subseq overlays (max new-start 0) (1+ (min new-end (length overlays))))))
    (seq-subseq (image-mode-window-get 'overlays) new-start (1+ new-end))))

(defun doc-scroll-undisplay-page (overlay)
  (let ((size (overlay-get overlay 'size)))
    (overlay-put overlay 'display `(space . (:width (,(car size)) :height (,(cdr size)))))))

(defun doc-scroll-display-page (overlay &optional async svg)
  ;; (ldbg "EPC")
  (let* ((page (if (numberp overlay)
		   (prog1 overlay
		     (setq overlay (doc-scroll-page-overlay overlay)))
		 (1+ (overlay-get overlay 'i))))
	 (size (overlay-get overlay 'size))
	 data)

    (unless async
      (setq data (funcall doc-scroll-image-data-function page (car size))))

    (if async
	(doc-djvu-page-data page (car size) t)
      (doc-scroll-display-image overlay data t))))
    ;; 	 (data (funcall (if async #'epc:call-deferred #'epc:call-sync)
    ;; 			doc-pymupdf-epc-server
    ;; 			'renderpage_base64
    ;; 			(list page (car size))))
    ;; 	 ;; (text (doc-scroll-structured-text-denormalize
    ;; 	 ;; 	size
    ;; 	 ;; 	(nth (1- page) doc-scroll-structured-text)))
    ;; 	 (display (lambda (x)
    ;; 		    (doc-scroll-display-image overlay
    ;; 					      ;; (base64-decode-string x)
    ;; 					      x t
    ;; 					      ))))
    ;; ;; (when text (overlay-put overlay 'text text))
    ;; (if async
    ;; 	(deferred:$ data
    ;; 		    (deferred:nextc it display))
    ;;   (funcall display data))))

(defun doc-scroll-display-image (overlay data &optional svg)
  (when svg
    (setq data (let* ((size (overlay-get overlay 'size))
		      (svg (svg-create (car size) (cdr size))))
		 (svg-embed svg data "image/png" t)
		 ;; (svg-rectangle svg 0 0 200 200 :fill "red")
		 svg)))

  (let* ((fn (if svg
		 (apply-partially #'svg-image data)
	       (apply-partially #'create-image data 'png t)))
	 (image (funcall fn :page (1+ (overlay-get overlay 'i)))))

    (overlay-put overlay 'display image))
  (overlay-put overlay 'data data))

(setq doc-scroll-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "C-n") 'doc-scroll-forward)
        (define-key map (kbd "<down>") 'doc-scroll-forward)
        (define-key map (kbd "C-p") 'doc-scroll-backward)
        (define-key map (kbd "<up>") 'doc-scroll-backward)
        (define-key map (kbd "<wheel-down>") 'doc-scroll-forward)
        (define-key map (kbd "<wheel-up>") 'doc-scroll-backward)
        ;; (define-key map (kbd "<mouse-5>") 'doc-scroll--scroll-forward)
        ;; (define-key map (kbd "<mouse-4>") 'doc-scroll-backward)
        (define-key map "n" 'doc-scroll-next-page)
        (define-key map (kbd "<next>") 'doc-scroll-next-page)
        (define-key map "p" 'doc-scroll-previous-page)
        (define-key map (kbd "<prior>") 'doc-scroll-previous-page)
        (define-key map (kbd "S-<next>") 'doc-scroll-screen-forward)
        (define-key map (kbd "S-<prior>") 'doc-scroll-screen-backward)
        (define-key map [remap goto-line] 'doc-scroll-goto-page)
        (define-key map "f" 'doc-scroll-fit-toggle)
        (define-key map "c" 'doc-scroll-set-columns)
        (define-key map "t" 'doc-scroll-thumbs)
        (define-key map "T" 'doc-scroll-page-text)
        (define-key map "i" 'doc-scroll-info)
        (define-key map "y" 'doc-scroll-kill-new)
        (define-key map (kbd "C-s") 'doc-scroll-search)
        (define-key map [down-mouse-1] 'doc-scroll-mouse-drag-action)
        ;; (define-key map [S-down-mouse-1] 'doc-scroll-select-region-free)
        map))

(when (featurep 'evil)
  (evil-define-key 'motion doc-scroll-mode-map
    "j" 'doc-scroll-forward
    "k" 'doc-scroll-backward
    (kbd "<down>") 'doc-scroll-forward
    (kbd "<up>") 'doc-scroll-backward
    (kbd "<wheel-down>") 'doc-scroll-forward
    (kbd "<wheel-up>") 'doc-scroll-backward
    ;; (kbd "<mouse-5>") 'doc-scroll-forward
    ;; (kbd "<mouse-4>") 'doc-scroll-backward
    "J" 'doc-scroll-next-page
    "K" 'doc-scroll-previous-page
    (kbd "<next>") 'doc-scroll-next-page
    (kbd "<prior>") 'doc-scroll-previous-page
    (kbd "C-j") 'doc-scroll-screen-forward
    (kbd "C-k") 'doc-scroll-screen-backward
    (kbd "S-<next>") 'doc-scroll-screen-forward
    (kbd "S-<prior>") 'doc-scroll-screen-backward
    "G" 'doc-scroll-goto-page
    "f" 'doc-scroll-fit-toggle
    "c" 'doc-scroll-set-columns
    "t" 'doc-scroll-thumbs
    "T" 'doc-scroll-page-text
    "/" 'doc-scroll-search
    "n" 'doc-scroll-search-next
    "i" 'doc-scroll-info
    "y" 'doc-scroll-kill-new
    "o" 'imenu-list-smart-toggle
    [down-mouse-1] 'doc-scroll-mouse-drag-action)

  (when (featurep 'imenu-list)
  (evil-define-key 'motion imenu-list-major-mode-map
    "o" 'imenu-list-smart-toggle)))


;;;###autoload
(define-derived-mode doc-scroll-mode special-mode "DocScroll"
  (dolist (m doc-scroll-incompatible-modes)
    (funcall m -1))

  ;; (setq-local doc-scroll-internal-page-sizes (doc-djvu-page-sizes)
  ;;             doc-scroll-number-of-pages (length doc-scroll-internal-page-sizes))
  (pcase (file-name-extension buffer-file-name)
    ("pdf" (doc-mupdf-mode))
    ;; ("pdf" (doc-poppler-mode))
    ("djvu" (doc-djvu-mode)))

  (add-hook 'window-configuration-change-hook 'doc-scroll-redisplay nil t)
  (add-hook 'image-mode-new-window-functions 'doc-scroll-new-window-function nil t)

  ;; get structured text from file or asynchronously from server
  ;; (let ((stext-file (concat doc-scroll-dir (file-name-base buffer-file-name) ".el")))
  ;;   (if (file-exists-p stext-file)
  ;; 	(setq doc-scroll-structured-text (with-temp-buffer
  ;; 					   (insert-file-contents-literally stext-file)
  ;; 					   (read (current-buffer))))
  ;;     ;; reading all text at once blocks Emacs, and can crash the server
  ;;     (funcall doc-scroll-structured-text-fun)))

  (setq image-mode-winprops-alist nil)
  (image-mode-winprops)

  (set-buffer-modified-p nil)

  ;; (when (featurep 'imenu-list)
  ;;   (setq-local imenu-list-focus-after-activation t))

  ;; (ldbg "doc-scroll-mode")
  )

;; (setq magic-mode-alist (remove '("%PDF" . pdf-view-mode) magic-mode-alist))
(dolist (ext '("\\.pdf\\'" "\\.djvu\\'"))
  (add-to-list 'auto-mode-alist (cons ext 'doc-scroll-mode)))

(defun doc-scroll-vscroll-to-pscroll (&optional vscroll)
  "Scroll in units of page size."
  (/ (float (or vscroll (window-vscroll nil t)))
     (cdr (doc-scroll-overlay-size (doc-scroll-current-page)))))

(defun doc-scroll-pscroll-to-vscroll (pscroll &optional page)
  (* pscroll
     (cdr (doc-scroll-overlay-size (if page
                                       (cdr (doc-scroll-overlay-size page))
                                     (doc-scroll-current-page))))))

(defun doc-scroll-goto-page (page &optional window)
  "Go to PAGE in document."
  (interactive
   (list (if current-prefix-arg
             (prefix-numeric-value current-prefix-arg)
           (read-number "Page: "))))
  ;; (unless (and (>= page 1)
  ;; (<= page doc-scroll-last-page))
  ;; (error "No such page: %d" page))
  (unless window
    (setq window
          ;; (if (pdf-util-pdf-window-p)
          (selected-window)
          ;; t)))
          ))
  (save-selected-window
    ;; Select the window for the hooks below.
    (when (window-live-p window)
      (select-window window 'norecord))
    (let ((changing-p
           (not (eq page (doc-scroll-current-page)))))
      ;; (when changing-p
      ;;   (run-hooks 'doc-scroll-before-change-page-hook)
      ;;   (run-hooks 'doc-scroll-change-page-hook))
      (when (window-live-p window)
	(let ((page-overlay (doc-scroll-page-overlay page)))
          (doc-scroll-display-page page-overlay)
          (doc-scroll-display-page (doc-scroll-page-overlay (1+ page))) ; TODO remove
          (goto-char (overlay-start page-overlay))))
      ;; (when changing-p
      ;;   (run-hooks 'doc-scroll-after-change-page-hook))
      ;; (when changing-p
      ;;   (pdf-view-deactivate-region)
      ;;   (force-mode-line-update)
      ;;   (run-hooks 'pdf-view-after-change-page-hook))))
      nil)))

(defun doc-scroll--forward (&optional n row)
  "Scroll forward N units.
Default unit is pixels. If ROW is non-nil then unit is row, which
is equivalent to page if the value of `doc-scroll-columns` is 1.
If N is nil, the value of `doc-scroll-step-size` is used."
  (let ((old-vscroll (window-vscroll nil t))
	(old-overlays (doc-scroll-visible-overlays)))

    (if row
	(let ((pscroll (doc-scroll-vscroll-to-pscroll)))
	  (if (= (doc-scroll-current-page) (length (image-mode-window-get 'overlays)))
	      (message "End of buffer")
	    (forward-line n)
	    (image-set-window-vscroll old-vscroll)))

      (let ((new-vscroll (+ old-vscroll (or n doc-scroll-step-size)))
            (current-overlay-height (doc-scroll-current-overlay-height)))
	(cond ((> new-vscroll current-overlay-height)
	       (forward-line)
               (set-window-vscroll nil (floor (- new-vscroll current-overlay-height)) t))
              (t (if (and (= (doc-scroll-current-page) (length (image-mode-window-get 'overlays)))
			  (> (+ new-vscroll (window-text-height nil t)) (doc-scroll-current-overlay-height)))
                     (message "End of buffer")
		   (image-set-window-vscroll new-vscroll))))))

    (redisplay)
    (let ((new-overlays (doc-scroll-visible-overlays)))
      (dolist (o (seq-difference old-overlays new-overlays))
	(doc-scroll-undisplay-page o))
      (dolist (o (seq-difference new-overlays old-overlays))
	(doc-scroll-display-page o nil t)))))
      
(defun doc-scroll--backward (&optional n row)
  (let ((old-vscroll (window-vscroll nil t))
	(old-overlays (doc-scroll-visible-overlays)))

    (if row
	(let ((pscroll (doc-scroll-vscroll-to-pscroll)))
	  (if (= (doc-scroll-current-page) 1)
              (progn (image-set-window-vscroll 0)
		     (message "Beginning of buffer"))
	    (forward-line (- (or n 1)))
	    (image-set-window-vscroll old-vscroll)))

      (let ((new-vscroll (- old-vscroll (or n doc-scroll-step-size)))
            (current-overlay-height (doc-scroll-current-overlay-height)))
	(cond ((< new-vscroll 0)
	       (if (= (doc-scroll-current-page) 1)
                     (message "Beginning of buffer")
		 (forward-line -1)
		 (set-window-vscroll nil (floor (- current-overlay-height old-vscroll)) t)))
              (t (image-set-window-vscroll new-vscroll)))))

    (redisplay)
    (let ((new-overlays (doc-scroll-visible-overlays)))
      (dolist (o (seq-difference old-overlays new-overlays))
	(doc-scroll-undisplay-page o))
      (dolist (o (seq-difference new-overlays old-overlays))
	(doc-scroll-display-page o nil t)))))
      
(defun doc-scroll-forward (n)
  (interactive "p")
  (dotimes (_ n)
    (doc-scroll--forward)))

(defun doc-scroll-backward (n)
  (interactive "p")
  (dotimes (_ n)
    (doc-scroll--backward)))

(defun doc-scroll-screen-forward (n)
  (interactive "p")
  (dotimes (_ n)
    (doc-scroll--forward (window-text-height nil t))))

(defun doc-scroll-next-page (n)
  (interactive "p")
  (doc-scroll--forward n t))

(defun doc-scroll-previous-page (n)
  (interactive "p")
  (doc-scroll--backward n t))


(provide 'doc-scroll)
;;; doc-scroll.el ends here
