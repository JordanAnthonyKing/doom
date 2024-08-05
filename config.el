;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq default-frame-alist '((internal-border-width . 12)
                            (drag-with-header-line . t)))
;; (scroll-bar-mode 1)
(setq window-divider-default-right-width 12)
(setq window-divider-default-bottom-width 12)
;; (setq right-fringe-width 8)
;; (fringe-mode '(8 . 8))


(remove-hook! '(prog-mode-hook text-mode-hook conf-mode-hook)
  #'display-line-numbers-mode)



(use-package! line-fringe-mode
  :load-path "~/.config/doom/lisp/"
  :config
  (global-line-fringe-mode t))

(use-package! doom-themes
  ;; improve integration w/ org-mode
  :hook (doom-load-theme . doom-themes-org-config)
  :init (setq doom-theme 'doom-two))

;;(setq doom-font (font-spec :family "Berkeley Mono Trial Regular" :size 18 :weight 'regular))
(setq doom-font (font-spec :family "Berkeley Mono" :size 18 :weight 'regular))
;; doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;; (add-to-list 'custom-theme-load-path (expand-file-name "~/.config/doom/themes/"))

;; (setq display-line-numbers-type 'relative)
(setq org-directory "~/.doom.d/org/")

(setq-default left-margin-width 2)
(setq-default right-margin-width 2)
(setq-default fringes-outside-margins nil)


(add-hook 'prog-mode-hook
          (lambda ()
            (setq mode-line-format nil)
            (setq header-line-format
                  '((:eval (propertize "%b" 'face '(:line-height 1.5)))))))





;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys

;; (setq flycheck-checker-error-threshold 9999)
(setq scroll-margin 3)
;; mouse-wheel-scroll-amount '(1 ((shift) . hscroll)))
;; TODO: Disable via hooks
(electric-pair-mode -1)
(electric-indent-mode -1)
(electric-layout-mode -1)
(electric-quote-mode -1)
(smartparens-global-mode -1)

;; Evil
(after! evil
  (map! (:leader
         :desc "M-x" "SPC" #'execute-extended-command)
        :n "gj" #'evil-next-visual-line
        :n "gk" #'evil-previous-visual-line
        :n "C-a" #'evil-end-of-line
        :n "C-i" #'doom/backward-to-bol-or-indent))

;; (use-package! evil-motion-trainer
;; :ensure t
;; :config
;; (global-evil-motion-trainer-mode 1))

;; LSP
(after! lsp-mode
  :config
  (setq lsp-auto-execute-action nil
        ;;lsp-modeline-diagnostics-enable nil
        lsp-modeline-code-actions-enable nil
        lsp-modeline-workspace-status-enable nil
        lsp-file-watch-threshold 9999
        ;;lsp-headerline-breadcrumb-enable nil
        lsp-typescript-indent-level 2
        lsp-clients-typescript-plugins
        (vector (list
                 :name "@vsintellicode/typescript-intellicode-plugin"
                 :location "C:/Users/jking/.vscode/extensions/visualstudioexptteam.vscodeintellicode-1.3.1"))))

(defun bring-emacs-frame-to-foreground (&rest _args)
  "Bring the current Emacs frame to the foreground on Windows."
  (interactive)
  (raise-frame)) ; 0xf120 is the command to bring the window to the foreground



(after! dap-ui
  (setq dap-ui-locals-expand-depth t)
  (remove-hook 'dap-ui-mode 'dap-ui-controls-mode))

(after! typescript-mode
  (setq typescript-indent-level 2))

(add-hook 'typescript-mode-hook
          (lambda ()
            (setq tab-width 2)
            (setq indent-tabs-mode nil)))

;; Guides
(after! highlight-indent-guides
  ;;(setq highlight-indent-guides-character 124
  (setq highlight-indent-guides-responsive 'top))


(setq-default header-line-format mode-line-format)
;; (setq header-line-format mode-line-format)


;; Modeline
(after! doom-modeline
  (setq doom-modeline-buffer-file-name-style 'buffer-name
        doom-modeline-check-simple-format t
        doom-modeline-checker-simple-format t)

  (setq header-line-format '("%e"
                             (:eval
                              (doom-modeline-format--main))))
  (setq mode-line-format nil))
;;doom-modeline-buffer-encoding nil
;;doom-modeline-percent-position nil
;;doom-modeline-position-column-line-format nil))

;; DOOG
;;(setq fancy-splash-image "~/.config/doom/doog2.png")
;;(setq fancy-splash-image "~/.config/doom/Rip_and_tear.png")
(setq fancy-splash-image "~/.config/doom/doom_1.png")
(add-hook! +doom-dashboard-mode
  (setq +doom-dashboard-banner-padding '(2 . 2)))

;;(global-yascroll-bar-mode 1)

(defun my/strip-error-prefix (str)
  "Strip the 'Error: ' prefix from STR if present."
  (if (string-prefix-p "Error: " str)
      (substring str 7)
    str))

(defun my/compilation-find-file (orig-fun marker filename directory &rest formats)
  "Advice to strip 'Error: ' prefix from FILENAME and DIRECTORY in `compilation-find-file`."
  (let* ((clean-filename (my/strip-error-prefix filename))
         (clean-directory (my/strip-error-prefix directory))
         ;; Apply `my/strip-error-prefix` to each format string if formats is not empty.
         (clean-formats (if formats
                            (mapcar #'my/strip-error-prefix formats)
                          nil))
         ;; Construct arguments list.
         (args (append (list marker clean-filename clean-directory) clean-formats)))
    (apply orig-fun args)))

;;(advice-add 'compilation-find-file :around #'my/compilation-find-file)

;; (map! :leader
;; :desc "Run npm-mode-npm-run in project root" "p n" #'my/npm-run-in-root-project)

;; Helper function to truncate file paths
(defun truncate-file-path (file-path max-length)
  "Truncate FILE-PATH if it exceeds MAX-LENGTH characters with ellipsis."
  (if (> (length file-path) max-length)
      (concat "…" (substring file-path (- (length file-path) max-length 1)))
    file-path))

;; Define the new function with truncation logic
(cl-defgeneric my-vertico--format-candidate (cand prefix suffix index _start)
  "Format CAND given PREFIX, SUFFIX and INDEX."
  (let ((max-length (- (frame-width) 42)))
    (when (and (stringp cand) (string-match-p "/" cand))
      (setq cand (truncate-file-path cand max-length)))
    (setq cand (vertico--display-string (concat prefix cand suffix "\n")))
    (when (= index vertico--index)
      (add-face-text-property 0 (length cand) 'vertico-current 'append cand))
    cand))

;; Add the advice to override the original function
(advice-add 'vertico--format-candidate :override #'my-vertico--format-candidate)

(after! marginalia
  (setq marginalia-align 'right))

;;(setq-default explicit-shell-file-name "c:/Program Files/Git/bin/bash.exe")

;; (use-package! indent-bars
;; :hook ((prog-mode typescript-mode html-mode scss-mode) . indent-bars-mode))

(use-package! treesit-auto
  :config
  (setq treesit-auto-install t)
  (global-treesit-auto-mode))

;; Probably axe this
                                        ;(use-package! shx
                                        ;(shx-global-mode 1))

;;(use-package! ts-fold
;;:config
  ;;;; we want to use our own face so we nullify this one to have no effect and
  ;;;; make it more similar to hideshows
;;(custom-set-faces! '(ts-fold-replacement-face :foreground unspecified
;;:box nil
;;:inherit font-lock-comment-face
;;:weight light))
;;(setq ts-fold-replacement "  [...]  ")
;;(global-ts-fold-mode +1))

(use-package! treesit-fold
  :ensure t
  :config
  (global-treesit-fold-mode +1))

(add-hook 'compilation-mode-hook
          (lambda ()
            (setq-local compilation-scroll-output t)
            (setq-local scroll-conservatively most-positive-fixnum)
            (setq-local scroll-margin 0)))




(use-package! pulsar
  :ensure t
  :config
  (pulsar-global-mode +1))
