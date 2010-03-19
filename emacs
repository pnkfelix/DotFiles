(setq load-path (cons "~/ConfigFiles/Elisp" load-path))
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(global-whitespace-mode t)
 '(whitespace-style (quote (trailing lines tabs space-before-tab empty indentation))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

(server-start)
(setenv "EDITOR" "emacsclient")
