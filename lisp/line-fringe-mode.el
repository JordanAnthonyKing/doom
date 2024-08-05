;;; line-fringe-mode.el -*- lexical-binding: t; -*-

(defvar line-fringe-mode nil)

(defvar line-fringe:bitmap-references nil
  "List of overlays for bitmaps in the fringe.")

(defface line-fringe:face
  '((t (:inherit fringe)))
  "Default face for line fringe bitmaps."
  :group 'line-fringe-mode)

(define-fringe-bitmap 'line-fringe:top-left-bitmap
  [#b11111111
   #b11111111
   #b11000000
   #b11000000
   #b11000000
   #b11000000
   #b11000000
   #b11000000] 8 8 'top)

(define-fringe-bitmap 'line-fringe:top-right-bitmap
  [#b11111111
   #b11111111
   #b00000011
   #b00000011
   #b00000011
   #b00000011
   #b00000011
   #b00000011] 8 8 'top)

(define-fringe-bitmap 'line-fringe:bottom-left-bitmap
  [#b11000000
   #b11000000
   #b11000000
   #b11000000
   #b11000000
   #b11000000
   #b11111111
   #b11111111] 8 8 'bottom)

(define-fringe-bitmap 'line-fringe:bottom-right-bitmap
  [#b00000011
   #b00000011
   #b00000011
   #b00000011
   #b00000011
   #b00000011
   #b11111111
   #b11111111] 8 8 'bottom)

(defun line-fringe:add-bitmaps (window _new-start)
  "Add fringe bitmaps to the visible top and bottom lines of WINDOW."
  (line-fringe:clear)
  (with-selected-window window
    (save-excursion
      (let ((top-line (line-number-at-pos (window-start)))
            (bottom-line (- (line-number-at-pos (window-end nil t)) 1))) ; update window-end with t
        (line-fringe:add-corner-bitmap top-line 'line-fringe:top-left-bitmap)
        (line-fringe:add-corner-bitmap top-line 'line-fringe:top-right-bitmap t)
        (line-fringe:add-corner-bitmap bottom-line 'line-fringe:bottom-left-bitmap)
        (line-fringe:add-corner-bitmap bottom-line 'line-fringe:bottom-right-bitmap t)))))


(defun line-fringe:add-corner-bitmap (line bitmap &optional right)
  "Add a fringe bitmap at the specified LINE.
BITMAP is the bitmap to display.
If RIGHT is non-nil, add to the right fringe, otherwise to the left."
  (goto-char (point-min))
  (forward-line (1- line))
  (let* ((ov (make-overlay (point) (point)))
         (side (if right 'right-fringe 'left-fringe))
         (prop 'display))
    (overlay-put ov side bitmap)
    (if right
        (overlay-put ov 'before-string (propertize " " prop `(right-fringe ,bitmap line-fringe:face)))
      (overlay-put ov 'before-string (propertize " " prop `(left-fringe ,bitmap line-fringe:face))))
    (push ov line-fringe:bitmap-references)))

(defun line-fringe:clear ()
  "Clear fringe bitmaps."
  (dolist (ov line-fringe:bitmap-references)
    (delete-overlay ov))
  (setq line-fringe:bitmap-references nil))

(defun line-fringe:turn-on ()
  "Turn on `line-fringe-mode' in the current buffer."
  (unless (minibufferp)
    (line-fringe-mode 1)))

(defvar-local line-fringe:scroll-hook nil
  "Buffer-local variable to store scroll hook status.")

(defun line-fringe:add-scroll-hook ()
  "Add the scroll hook for `line-fringe-mode'."
  (unless line-fringe:scroll-hook
    (setq line-fringe:scroll-hook
          (lambda (window start)
            (when line-fringe-mode
              (line-fringe:add-bitmaps window start))))
    (add-hook 'window-scroll-functions line-fringe:scroll-hook nil t)))

(defun line-fringe:remove-scroll-hook ()
  "Remove the scroll hook for `line-fringe-mode'."
  (when line-fringe:scroll-hook
    (remove-hook 'window-scroll-functions line-fringe:scroll-hook t)
    (setq line-fringe:scroll-hook nil)))

;;;###autoload
(define-minor-mode line-fringe-mode
  "Minor mode to display a fringe bitmap at the visible top and bottom lines of the window."
  :lighter " LineFringe"
  (if line-fringe-mode
      (progn
        (fringe-mode '(8 . 8))
        (line-fringe:add-bitmaps (selected-window) (window-start))
        (line-fringe:add-scroll-hook))
    (line-fringe:clear)
    (line-fringe:remove-scroll-hook)))

;;;###autoload
(define-globalized-minor-mode global-line-fringe-mode
  line-fringe-mode line-fringe:turn-on
  :group 'line-fringe-mode)

(provide 'line-fringe-mode)

;;; line-fringe-mode.el ends here
