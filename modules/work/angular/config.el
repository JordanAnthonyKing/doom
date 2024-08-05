;;; $DOOMDIR/modules/work/angular/config.el -*- lexical-binding: t; -*-

(add-to-list 'auto-mode-alist '(".ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '(".html\\'" . html-ts-mode))

(after! eglot
  (add-hook! 'typescript-ts-mode-hook #'eglot-ensure)
  (add-hook! 'html-ts-mode-hook #'eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs
               '(html-ts-mode
                 "ngserver"
                 "C:/Program Files/nodejs/node_modules/@angular/language-server"
                 "--ngProbeLocations"
                 "C:/Program Files/nodejs/node_modules"
                 "--tsProbeLocations"
                 "C:/Program Files/nodejs/node_modules"
                 "--stdio")))

(defun my/npm-compilation-error-settings ()

  (defconst npm-error-regexp
    (rx "Error: "
        (group (+ (not (any ":")))) ; filepath
        ":"
        (group (+ digit))            ; line number
        ":"
        (group (+ digit))))
  (setq compilation-error-regexp-alist-alist nil)

  ;; Set new `compilation-error-regexp-alist-alist`
  (setq compilation-error-regexp-alist-alist
        `((npm-error ,npm-error-regexp 1 2 3 2)))

  ;; Set new `compilation-error-regexp-alist`
  (setq compilation-error-regexp-alist
        '(npm-error)))

;; TODO: bindings
(use-package! npm-mode
  :hook (typescript-ts-mode . npm-mode)
  :hook (npm-mode . my/npm-compilation-error-settings))

(use-package! eslintd-fix
  :hook (typescript-ts-mode . eslintd-fix-mode)
  ;; :config
  ;; (setq eslintd-fix-executable "c:/Program Files/nodejs/eslint_d")
  )

(defun ng2--counterpart-name (file)
  "Return the file name of FILE's counterpart, or FILE if there is no counterpart."
  (when (not (ng2--is-component file)) file)
  (let ((ext (file-name-extension file))
        (base (file-name-sans-extension file)))
    (if (equal ext "ts")
        (concat base ".html")
      (concat base ".ts"))))

(defun ng2--is-component (file)
  "Return whether FILE is a component file."
  (equal (file-name-extension (file-name-sans-extension file)) "component"))

(defun ng2-open-counterpart ()
  "Opens the corresponding template or component file to this one."
  (interactive)
  (find-file (ng2--counterpart-name (buffer-file-name))))

(defun remove-ts-suffix (name)
  "Remove the '-ts' suffix from the mode name."
  (s-replace "-ts" "" name))

(defun advice-dumb-jump-get-mode-base-name (orig-fun &rest args)
  "Advice for `dumb-jump-get-mode-base-name` to also remove '-ts'."
  (remove-ts-suffix (apply orig-fun args)))

(defun advice-dumb-jump-get-language-from-mode (orig-fun &rest args)
  "Advice for `dumb-jump-get-language-from-mode` to use the modified mode base name."
  (let* ((dumb-jump-get-mode-base-name #'advice-dumb-jump-get-mode-base-name))
    (apply orig-fun args)))

(defun remove-javascript-html-entry (entry)
  "Remove entry with :language 'javascript' and :ext 'html' from dumb-jump-language-file-exts."
  (not (and (equal (plist-get entry :language) "javascript")
            (equal (plist-get entry :ext) "html"))))

(after! dumb-jump
  (setq dumb-jump-selector 'completing-read)
  (setq dumb-jump-language-file-exts
        (cl-remove-if-not #'remove-javascript-html-entry dumb-jump-language-file-exts))
  (setq dumb-jump-language-file-exts
        (append dumb-jump-language-file-exts
                '((:language "typescript" :ext "html" :agtype "html" :rgtype "html"))))

  (advice-add 'dumb-jump-get-mode-base-name :around #'advice-dumb-jump-get-mode-base-name)
  (advice-add 'dumb-jump-get-language-from-mode :around #'advice-dumb-jump-get-language-from-mode))








(defun my/parrot-animate-when-compile-success (buffer result)
  (message result)
  (if (string-match "exited abnormally" result)
      (progn
        (parrot-set-parrot-type 'rotating)
        (parrot-start-animation))
    (progn
      (parrot-set-parrot-type 'thumbsup)
      (parrot-start-animation))))

(use-package! parrot
  :config
  ;; (parrot-set-parrot-type 'emacs)
  ;; (parrot-mode)
  ;; (add-to-list 'compilation-finish-functions 'my/parrot-animate-when-compile-success)
  )

(after! doom-modeline
  ;; Customize the default 'main' modeline
  (doom-modeline-def-modeline 'main
    '(eldoc bar workspace-name window-number modals matches follow buffer-info remote-host word-count parrot selection-info)
    '(compilation objed-state misc-info persp-name battery grip irc mu4e gnus github debug repl lsp minor-modes input-method indent-info buffer-encoding major-mode process vcs check time)))

(defvar my/auto-recompile-enabled nil
  "Non-nil if auto-recompilation is enabled.")

(defvar my/last-compilation-command nil
  "Stores the command used for the last compilation.")

(defun my/track-compilation-command (proc msg)
  "Track the command used for the compilation."
  (setq my/last-compilation-command
        (with-current-buffer (process-buffer proc)
          (save-excursion
            (goto-char (point-min))
            (if (re-search-forward "Running \\(.*\\)" nil t)
                (match-string 1)
              "Unknown")))))

(defun my/recompile-on-save ()
  "Kill the current compilation if it is running and start a new one if auto-recompilation is enabled."
  (when my/auto-recompile-enabled
    (let ((compilation-buffer (my/current-project-compilation-buffer)))
      (when (and compilation-buffer (buffer-live-p compilation-buffer))
        (with-current-buffer compilation-buffer
          (compilation-kill))
        (message "Previous compilation killed. Starting a new one..."))
      ;; Wait a bit to ensure the compilation is killed before starting a new one
      (run-at-time "0.5 sec" nil 'recompile))))

(defun my/current-project-compilation-buffer ()
  "Return the compilation buffer for the current project, or nil if not found."
  (let ((project-root (project-root (project-current))))
    (when project-root
      (cl-find-if (lambda (buf)
                    (and (buffer-live-p buf)
                         (string-match-p (regexp-quote project-root)
                                         (buffer-file-name buf))
                         (string-match-p "compilation" (buffer-name buf))))
                  (buffer-list)))))

(defun my/toggle-auto-recompile ()
  "Toggle auto-recompilation on file save."
  (interactive)
  (setq my/auto-recompile-enabled (not my/auto-recompile-enabled))
  (if my/auto-recompile-enabled
      (progn
        (add-hook 'after-save-hook 'my/recompile-on-save nil t)
        (message "Auto-recompilation enabled."))
    (remove-hook 'after-save-hook 'my/recompile-on-save t)
    (message "Auto-recompilation disabled.")))

;; Optional: Automatically enable auto-recompilation in specific modes or projects
(defun my/setup-auto-recompile-for-project ()
  "Enable auto-recompilation for the current project."
  (when (project-current)  ;; Check if a project is active
    (my/toggle-auto-recompile)))

;; Add this to `prog-mode-hook` or any specific major mode hook where you want auto-recompile
;; (add-hook 'prog-mode-hook 'my/setup-auto-recompile-for-project)
