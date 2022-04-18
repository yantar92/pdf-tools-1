;; -*- lexical-binding: t; -*-
;; (require 'image-roll)
(require 'pdf-tools)
(load "~/git/papyrus.el/papyrus.el")

(defun pdf-scroll-page-sizes ()
  (let (s)
    (dotimes (i (pdf-info-number-of-pages) (nreverse s))
      (push (pdf-view-desired-image-size (1+ i)) s))))

(defun pdf-set-redisplay-flag-function ()
  (setf (pdf-view-window-needs-redisplay) t))

(defun pdf-scroll-set-functions ()
(setq image-roll-display-page 'pdf-view-display-page
      image-roll-number-of-pages-function 'pdf-cache-number-of-pages
      image-roll-page-sizes-function 'pdf-scroll-page-sizes
      image-roll-set-redisplay-flag-function 'pdf-set-redisplay-flag-function)
)

(add-hook 'pdf-view-mode-hook 'pdf-scroll-set-functions)

(defalias 'pdf-view-new-window-function 'image-roll--new-window-function)
(defalias 'pdf-view-redisplay #'image-roll--redisplay)

(defalias 'pdf-view-next-line-or-next-page #'image-roll-scroll-forward)
(defalias 'pdf-view-previous-line-or-previous-page #'image-roll-scroll-backward)

(defalias 'pdf-view-next-page #'image-roll-next-page)
(defalias 'pdf-view-previous-page #'image-roll-previous-page)

(defun pdf-view-display-page (page &optional window)
  "Display page PAGE in WINDOW."
  (setf (pdf-view-window-needs-redisplay window) nil)
  (pdf-view-display-image
   page
   (pdf-view-create-page page window)
   window))

(defun pdf-view-display-image (page image &optional window inhibit-slice-p)
  ;; TODO: write documentation!
  (let ((ol (image-roll-page-overlay page))
        (gol (image-roll-gap-overlay page)))
    (when (window-live-p (overlay-get ol 'window))
      (let* ((size (image-size image t))
             (slice (if (not inhibit-slice-p)
                        (pdf-view-current-slice window)))
             (displayed-width (floor
                               (if slice
                                   (* (nth 2 slice)
                                      (car (image-size image)))
                                 (car (image-size image))))))
        (setf (pdf-view-current-image window) image)
        ;; (move-overlay ol (point-min) (point-max))
        ;; In case the window is wider than the image, center the image
        ;; horizontally.
        (dolist (o (list ol gol))
          (overlay-put o 'before-string
                       (when (> (window-width window)
                                displayed-width)
                         (propertize " " 'display
                                     `(space :align-to
                                             ,(/ (- (window-width window)
                                                    displayed-width) 2))))))
        (overlay-put ol 'display
                     (if slice
                         (list (cons 'slice
                                     (pdf-util-scale slice size 'round))
                               image)
                       image))))))
        ;; (let* ((win (overlay-get ol 'window))
        ;;        (hscroll (image-mode-window-get 'hscroll win))
        ;;        (vscroll (image-mode-window-get 'vscroll win)))
        ;;   ;; Reset scroll settings, in case they were changed.
        ;;   (if hscroll (set-window-hscroll win hscroll))
        ;;   (if vscroll (set-window-vscroll win vscroll pdf-view-have-image-mode-pixel-vscroll)))))))

;; TODO investigate how to fix these 'fit' features
;; (defun pdf-view-fit-page-to-window ()
;;   "Fit PDF to window.

;; Choose the larger of PDF's height and width, and fits that
;; dimension to window."
;;   (interactive)
;;   (setq pdf-view-display-size 'fit-page)
;;   ;; (image-set-window-vscroll 0)
;;   (image-set-window-hscroll 0)
;;   (pdf-view-redisplay t))

;; (defun pdf-view-fit-height-to-window ()
;;   "Fit PDF height to window."
;;   (interactive)
;;   (setq pdf-view-display-size 'fit-height)
;;   ;; (image-set-window-vscroll 0)
;;   (pdf-view-redisplay t))


;;; Fix `pdf-occur'

(defun pdf-isearch-hl-matches (current matches &optional occur-hack-p)
  "Highlighting edges CURRENT and MATCHES."
  (cl-check-type current pdf-isearch-match)
  (cl-check-type matches (list-of pdf-isearch-match))
  (cl-destructuring-bind (fg1 bg1 fg2 bg2)
      (pdf-isearch-current-colors)
    (let* ((width (car (pdf-view-image-size)))
           (page (pdf-view-current-page))
           (window (selected-window))
           (buffer (current-buffer))
           (tick (cl-incf pdf-isearch--hl-matches-tick))
           (pdf-info-asynchronous
            (lambda (status data)
              (when (and (null status)
                         (eq tick pdf-isearch--hl-matches-tick)
                         (buffer-live-p buffer)
                         (window-live-p window)
                         (eq (window-buffer window)
                             buffer))
                (with-selected-window window
                  (when (and (derived-mode-p 'pdf-view-mode)
                             (or isearch-mode
                                 occur-hack-p)
                             (eq page (pdf-view-current-page)))
                    (pdf-view-display-image
                     page
                     (pdf-view-create-image data :width width))))))))
      (pdf-info-renderpage-text-regions
       page width t nil
       `(,fg1 ,bg1 ,@(pdf-util-scale-pixel-to-relative
                      current))
       `(,fg2 ,bg2 ,@(pdf-util-scale-pixel-to-relative
                      (apply 'append
                        (remove current matches))))))))

(defun pdf-util-required-vscroll (edges &optional eager-p context-pixel)
  "Return the amount of scrolling necessary, to make image EDGES visible.

Scroll as little as necessary.  Unless EAGER-P is non-nil, in
which case scroll as much as possible.

Keep CONTEXT-PIXEL pixel of the image visible at the bottom and
top of the window.  CONTEXT-PIXEL defaults to an equivalent pixel
value of `next-screen-context-lines'.

Return the required vscroll in pixels or nil, if scrolling is not
needed.

Note: For versions of emacs before 27 this will return lines instead of
pixels. This is because of a change that occurred to `image-mode' in 27."
  (pdf-util-assert-pdf-window)
  (let* ((win (window-inside-pixel-edges))
         (image-height (cdr (pdf-view-image-size)))
         (image-top (window-vscroll nil t))
         (edges (pdf-util-translate
                 edges
                 (pdf-view-image-offset) t)))
    (pdf-util-with-edges (win edges)
      (let* ((context-pixel (or context-pixel
                                (* next-screen-context-lines
                                   (frame-char-height))))
             ;;Be careful not to modify edges.
             (edges-top (- edges-top context-pixel))
             (edges-bot (+ edges-bot context-pixel))
             (vscroll
              (cond ((< edges-top image-top)
                     (max 0 (if eager-p
                                (- edges-bot win-height)
                              edges-top)))
                    ((> (min image-height
                             edges-bot)
                        (+ image-top win-height))
                     (min (- image-height win-height)
                          (if eager-p
                              edges-top
                            (- edges-bot win-height)))))))


        (when vscroll
          (round
           ;; `image-set-window-vscroll' changed in version 27 to using
           ;; pixels, not lines.
           (if (version< emacs-version "27")
               (/ vscroll (float (frame-char-height)))
               vscroll)))))))

(defun pdf-util-scroll-to-edges (edges &optional eager-p)
  "Scroll window such that image EDGES are visible.

Scroll as little as necessary.  Unless EAGER-P is non-nil, in
which case scroll as much as possible."

  (let ((vscroll (pdf-util-required-vscroll edges eager-p)))
    ;; (hscroll (pdf-util-required-hscroll edges eager-p)))
    (when vscroll
      (image-set-window-vscroll (+ (car (image-roll-page-overlay-get (image-roll-current-page) 'vpos)) vscroll)))))


;;; Fix `pdf-links-minor-mode'

(with-eval-after-load 'pdf-links

  (define-minor-mode pdf-links-minor-mode
    "Handle links in PDF documents.\\<pdf-links-minor-mode-map>

If this mode is enabled, most links in the document may be
activated by clicking on them or by pressing \\[pdf-links-action-perform] and selecting
one of the displayed keys, or by using isearch limited to
links via \\[pdf-links-isearch-link].

\\{pdf-links-minor-mode-map}"
    :group 'pdf-links
    ;; (pdf-util-assert-pdf-buffer)
    (cond
     (pdf-links-minor-mode
      (pdf-view-add-hotspot-function 'pdf-links-hotspots-function 0))
     (t
      (pdf-view-remove-hotspot-function 'pdf-links-hotspots-function))))

  (defun pdf-links-read-link-action (prompt)
    "Using PROMPT, interactively read a link-action.

See `pdf-links-action-perform' for the interface."

    (pdf-util-assert-pdf-window)
    (let* ((links (pdf-cache-pagelinks
                   (pdf-view-current-page)))
           (keys (pdf-links-read-link-action--create-keys
                  (length links)))
           (key-strings (mapcar (apply-partially 'apply 'string)
                                keys))
           (alist (cl-mapcar 'cons keys links))
           (size (pdf-view-image-size))
           (colors (pdf-util-face-colors
                    'pdf-links-read-link pdf-view-dark-minor-mode))
           (args (list
                  :foreground (car colors)
                  :background (cdr colors)
                  :formats
                  `((?c . ,(lambda (_edges) (pop key-strings)))
                    (?P . ,(number-to-string
                            (max 1 (* (cdr size)
                                      pdf-links-convert-pointsize-scale)))))
                  :commands pdf-links-read-link-convert-commands
                  :apply (pdf-util-scale-relative-to-pixel
                          (mapcar (lambda (l) (cdr (assq 'edges l)))
                                  links)))))
      (unless links
        (error "No links on this page"))
      (unwind-protect
          (let ((image-data
                 (pdf-cache-get-image
                  (pdf-view-current-page)
                  (car size) (car size) 'pdf-links-read-link-action)))
            (unless image-data
              (setq image-data (apply 'pdf-util-convert-page args ))
              (pdf-cache-put-image
               (pdf-view-current-page)
               (car size) image-data 'pdf-links-read-link-action))
            (pdf-view-display-image (pdf-view-current-page)
                                    (create-image image-data (pdf-view-image-type) t))
            (pdf-links-read-link-action--read-chars prompt alist))
        (pdf-view-redisplay))))



;; ;;;###autoload ; and remove autoload
  (defun pdf-links-action-perform (link)
    "Follow LINK, depending on its type.

This may turn to another page, switch to another PDF buffer or
invoke `pdf-links-browse-uri-function'.

Interactively, link is read via `pdf-links-read-link-action'.
This function displays characters around the links in the current
page and starts reading characters (ignoring case).  After a
sufficient number of characters have been read, the corresponding
link's link is invoked.  Additionally, SPC may be used to
scroll the current page."
    (interactive
     (list (or (pdf-links-read-link-action "Activate link (SPC scrolls): ")
               (error "No link selected"))))
    (let-alist link
      (cl-case .type
        ((goto-dest goto-remote)
         (let ((window (selected-window)))
           (cl-case .type
             (goto-dest
              (unless (> .page 0)
                (error "Link points to nowhere")))
             (goto-remote
              (unless (and .filename (file-exists-p .filename))
                (error "Link points to nonexistent file %s" .filename))
              (setq window (display-buffer
                            (or (find-buffer-visiting .filename)
                                (find-file-noselect .filename))))))
           (with-selected-window window
             (when (derived-mode-p 'pdf-view-mode)
               (when (> .page 0)
                 (pdf-view-goto-page .page))

               ;; TODO fix pdf-util-tooltip-arrow function for image-roll
               ;; compatibility

               ;; (when .top
               ;;   ;; Showing the tooltip delays displaying the page for
               ;;   ;; some reason (sit-for/redisplay don't help), do it
               ;;   ;; later.
               ;;   (run-with-idle-timer 0.001 nil
               ;;     (lambda ()
               ;;       (when (window-live-p window)
               ;;         (with-selected-window window
               ;;           (when (derived-mode-p 'pdf-view-mode)
               ;;             (pdf-util-tooltip-arrow .top)))))))
               ))))
        (uri
         (funcall pdf-links-browse-uri-function .uri))
        (t
         (error "Unrecognized link type: %s" .type)))
      nil)))

;;; Fix `pdf-annot-minor-mode'

(with-eval-after-load 'pdf-annot
  (define-minor-mode pdf-annot-minor-mode
    "Support for PDF Annotations.

\\{pdf-annot-minor-mode-map}"
    :group 'pdf-annot
    (cond
     (pdf-annot-minor-mode
      (when pdf-annot-tweak-tooltips
        (when (boundp 'x-gtk-use-system-tooltips)
          (setq x-gtk-use-system-tooltips nil))
        (setq tooltip-hide-delay 3600))
      (pdf-view-add-hotspot-function 'pdf-annot-hotspot-function 9)
      (add-hook 'pdf-info-close-document-hook
                #'pdf-annot-attachment-delete-base-directory nil t)
      (when (featurep 'savehist)
        (add-to-list 'savehist-minibuffer-history-variables
                     'pdf-annot-color-history)))
     (t
      (pdf-view-remove-hotspot-function 'pdf-annot-hotspot-function)
      (remove-hook 'pdf-info-close-document-hook
                   #'pdf-annot-attachment-delete-base-directory t)))))

;;   (defun pdf-annot-show-annotation (a &optional highlight-p window)
;;   "Make annotation A visible.

;; Turn to A's page in WINDOW, and scroll it if necessary.

;; If HIGHLIGHT-P is non-nil, visually distinguish annotation A from
;; other annotations."

;;   (save-selected-window
;;     (when window (select-window window 'norecord))
;;     (pdf-util-assert-pdf-window)
;;     (let ((page (pdf-annot-get a 'page))
;;           (size (pdf-view-image-size)))
;;       (unless (= page (pdf-view-current-page))
;;         (pdf-view-goto-page page))
;;       (let ((edges (pdf-annot-get-display-edges a)))
;;         (when highlight-p
;;           (pdf-view-display-image
;;            page
;;            (pdf-view-create-image
;;                (pdf-cache-renderpage-highlight
;;                 page (car size)
;;                 `("white" "steel blue" 0.35 ,@edges))
;;              :map (pdf-view-apply-hotspot-functions
;;                    window page size)
;;              :width (car size))))
;;         (pdf-util-scroll-to-edges
;;          (pdf-util-scale-relative-to-pixel (car edges))))))))

;;; Fix annots

(with-eval-after-load 'pdf-view
  (defun pdf-view-display-region (&optional region rectangle-p)
    ;; TODO: write documentation!
    (unless region
      (pdf-view-assert-active-region)
      (setq region pdf-view-active-region))
    (let ((colors (pdf-util-face-colors
                   (if rectangle-p 'pdf-view-rectangle 'pdf-view-region)
                   (bound-and-true-p pdf-view-dark-minor-mode)))
          (page (pdf-view-current-page))
          (width (car (pdf-view-image-size))))
      (pdf-view-display-image
       page
       (pdf-view-create-image
           (if rectangle-p
               (pdf-info-renderpage-highlight
                page width nil
                `(,(car colors) ,(cdr colors) 0.35 ,@region))
             (pdf-info-renderpage-text-regions
              page width nil nil
              `(,(car colors) ,(cdr colors) ,@region)))
         :width width))))

  (defun pdf-view-mouse-set-region (event &optional allow-extend-p
                                        rectangle-p)
  "Select a region of text using the mouse with mouse event EVENT.

Allow for stacking of regions, if ALLOW-EXTEND-P is non-nil.

Create a rectangular region, if RECTANGLE-P is non-nil.

Stores the region in `pdf-view-active-region'."
  (interactive "@e")
  (setq pdf-view--have-rectangle-region rectangle-p)
  (unless (and (eventp event)
               (mouse-event-p event))
    (signal 'wrong-type-argument (list 'mouse-event-p event)))
  (unless (and allow-extend-p
               (or (null (get this-command 'pdf-view-region-window))
                   (equal (get this-command 'pdf-view-region-window)
                          (selected-window))))
    (pdf-view-deactivate-region))
  (put this-command 'pdf-view-region-window
       (selected-window))
  (let* ((window (selected-window))
         (pos (event-start event))
         (begin-inside-image-p t)
         (begin (if (posn-image pos)
                    (posn-object-x-y pos)
                  (setq begin-inside-image-p nil)
                  (posn-x-y pos)))
         (abs-begin (posn-x-y pos))
         pdf-view-continuous
         region)
    (when (pdf-util-track-mouse-dragging (event 0.05)
            (let* ((pos (event-start event))
                   (end (posn-object-x-y pos))
                   (end-inside-image-p
                    (and (eq window (posn-window pos))
                         (posn-image pos))))
              (when (or end-inside-image-p
                        begin-inside-image-p)
                (cond
                 ((and end-inside-image-p
                       (not begin-inside-image-p))
                  ;; Started selection outside the image, setup begin.
                  (let* ((xy (posn-x-y pos))
                         (dxy (cons (- (car xy) (car begin))
                                    (- (cdr xy) (cdr begin))))
                         (size (pdf-view-image-size t)))
                    (setq begin (cons (max 0 (min (car size)
                                                  (- (car end) (car dxy))))
                                      (max 0 (min (cdr size)
                                                  (- (cdr end) (cdr dxy)))))
                          ;; Store absolute position for later.
                          abs-begin (cons (- (car xy)
                                             (- (car end)
                                                (car begin)))
                                          (- (cdr xy)
                                             (- (cdr end)
                                                (cdr begin))))
                          begin-inside-image-p t)))
                 ((and begin-inside-image-p
                       (not end-inside-image-p))
                  ;; Moved outside the image, setup end.
                  (let* ((xy (posn-x-y pos))
                         (dxy (cons (- (car xy) (car abs-begin))
                                    (- (cdr xy) (cdr abs-begin))))
                         (size (pdf-view-image-size t)))
                    (setq end (cons (max 0 (min (car size)
                                                (+ (car begin) (car dxy))))
                                    (max 0 (min (cdr size)
                                                (+ (cdr begin) (cdr dxy)))))))))
                (let ((iregion (if rectangle-p
                                   (list (min (car begin) (car end))
                                         (min (cdr begin) (cdr end))
                                         (max (car begin) (car end))
                                         (max (cdr begin) (cdr end)))
                                 (list (car begin) (cdr begin)
                                       (car end) (cdr end)))))
                  (setq region
                        (pdf-util-scale-pixel-to-relative iregion))
                  (pdf-view-display-region
                   (cons region pdf-view-active-region)
                   rectangle-p)))))
                  ;; (pdf-util-scroll-to-edges iregion)))))
      (setq pdf-view-active-region
            (append pdf-view-active-region
                    (list region)))
      (pdf-view--push-mark))))

  (defun pdf-view-redisplay-pages (&rest pages)
    "Redisplay PAGES in all windows."
    (pdf-util-assert-pdf-buffer)
    (dolist (window (get-buffer-window-list nil nil t))
      (when (memq (pdf-view-current-page window)
                  pages)
        (pdf-view-redisplay window t)))))
