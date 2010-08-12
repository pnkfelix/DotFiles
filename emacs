;; -*- mode: lisp; indent-tabs-mode: nil -*-

(setq load-path (cons "~/ConfigFiles/Elisp" load-path))
(require 'whitespace)
(require 'uniquify)

;; too slow
;(require 'js2-mode)
(require 'javascript-mode)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(comint-completion-fignore nil)
 '(completion-ignored-extensions (quote (".svn/" "CVS/" ".o" "~" ".bin" ".lbin" ".so" ".a" ".ln" ".blg" ".bbl" ".elc" ".lof" ".glo" ".idx" ".lot" ".dvi" ".fmt" ".tfm" ".pdf" ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".fasl" ".ufsl" ".fsl" ".dxl" ".pfsl" ".dfsl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo" ".abc")))
 '(gdb-enable-debug t)
 '(truncate-partial-width-windows nil)
 '(uniquify-buffer-name-style (quote post-forward-angle-brackets) nil (uniquify))
 '(whitespace-style (quote (trailing tabs space-before-tab empty))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(whitespace-line ((t (:background "alice blue")))))

(global-whitespace-mode 1)
(column-number-mode 1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(transient-mark-mode 1)
(show-paren-mode 1)

;; From: http://www.emacswiki.org/emacs/IndentingC
;; (makes left-curly line up with start of if token, among other things)
(setq c-default-style "linux" c-basic-offset 4)

(require 'server)
(when (not (server-running-p))
  (server-start)
  (setenv "EDITOR" "emacsclient"))

;; EmacsWiki explains that this fixes whitespace character
;; rendering for Fedora and OS X 
(setq whitespace-display-mappings 
      '((space-mark ?\  [?\u00B7]) 
        (newline-mark ?\n [?$ ?\n]) 
        (tab-mark ?\t [?\u00BB ?\t])))

(defvar my-fullscreen-p t "Check if fullscreen is on or off")
(defun my-toggle-fullscreen ()
  (interactive)
  (setq my-fullscreen-p (not my-fullscreen-p))
  (if my-fullscreen-p
      (set-frame-parameter nil 'fullscreen 'fullboth)
      (set-frame-parameter nil 'fullscreen 'nil)))

(defun ormap (pred l)
  (cond ((consp l) (cond ((funcall pred (car l))
                          (cons (car l) (ormap pred (cdr l))))
                         (t
                          (ormap pred (cdr l)))))
        ((null l) nil)))

(defun compile-including-xcode ()
  "Compile first looking for Xcode support in current directory."
  (interactive)
  (let* ((suffix ".xcodeproj")
         (dirent-is-xcodeproj
          (lambda (filename)
            (and (> (length filename) (length suffix))
                 (string-equal (substring filename (- (length suffix)))
                               suffix))))
         (has-proj-file (ormap dirent-is-xcodeproj
                               (directory-files "."))))
    (if has-proj-file
        ; then
        (compile "xcodebuild")
      ; else
      (compile "make"))))

(defun compile-in-compilation-buffer ()
  "Reattempt current compilation."
  (interactive)
  (switch-to-buffer "*compilation*")
  (compile-including-xcode))

;; (global-set-key (kbd "<f5>") 'compile-including-xcode)
(global-set-key (kbd "<f5>") 'compile-in-compilation-buffer)

;; Disable WordWrap (Aquamacs) if present
(if (boundp 'global-visual-line-mode)
    (global-visual-line-mode 0))
(if (boundp 'turn-off-word-wrap)
    (turn-off-word-wrap) ;(setq word-wrap nil)
  )

;; In Emacs defaults, .st is used for ESS Transcript files, but
;; Tamarin uses the extension for Selftest (preprocessor generating C++)
(add-to-list 'auto-mode-alist '("\\.st\\'" . c++-mode))

;; GNU GLOBAL is more recent source tag system
;; minor gtags-mode will bind M-. to gtags-find-tag
;(require 'gtags)

;; To prep, % cd SRCROOT; export GTAGSFORCECPP= gtags

(setq font-lock-maximum-decoration
      '((c-mode . 2) (c++-mode . 2)))

(set-frame-height last-event-frame 120)

(defun frame-80 ()
  "Resize current frame to be 80 characters."
  (interactive)
  (set-frame-width (selected-frame) 80))
