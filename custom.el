;;; -*- lexical-binding: t -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   '((eval setq-local compilation-error-regexp-alist '(npm-error)) (eval setq-local
                                                                    compilation-error-regexp-alist-alist
                                                                    '((npm-error (\

                                                                                  (\,
                                                                                   (rx
                                                                                    "Error: "
                                                                                    (group
                                                                                     (+
                                                                                      (not
                                                                                       (any
                                                                                        ":"))))
                                                                                    ":"
                                                                                    (group
                                                                                     (+
                                                                                      digit))
                                                                                    ":"
                                                                                    (group
                                                                                     (+
                                                                                      digit))))))
                                                                      1 2 3 2))))
 '(warning-suppress-types '((use-package) (lsp-mode) (defvaralias) (lexical-binding))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
