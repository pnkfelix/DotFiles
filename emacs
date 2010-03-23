(setq load-path (cons "~/ConfigFiles/Elisp" load-path))
(require 'whitespace)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(whitespace-style (quote (trailing lines tabs space-before-tab empty indentation))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

(global-whitespace-mode 1)
(column-number-mode 1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(server-start)
(setenv "EDITOR" "emacsclient")

;; EmacsWiki explains that this fixes whitespace character
;; rendering for Fedora and OS X 
(setq whitespace-display-mappings 
      '((space-mark ?\  [?\u00B7]) 
	(newline-mark ?\n [?$ ?\n]) 
	(tab-mark ?\t [?\u00BB ?\t])))
