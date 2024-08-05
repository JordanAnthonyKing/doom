((nil . ((compile-command . "npm run build-")))
(nil . ((eval . (setq-local
                   compilation-error-regexp-alist-alist
                   '((npm-error
                      ,(rx "Error: "
                           (group (+ (not (any ":")))) ; filepath
                           ":"
                           (group (+ digit))            ; line number
                           ":"
                           (group (+ digit))))          ; column number
                      1 2 3 2)))
          (eval . (setq-local
                   compilation-error-regexp-alist
                   '(npm-error))))))