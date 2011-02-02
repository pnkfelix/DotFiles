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
 '(debug-on-error t)
 '(gdb-enable-debug t)
 '(line-move-visual nil)
 '(truncate-partial-width-windows nil)
 '(uniquify-buffer-name-style (quote post-forward-angle-brackets) nil (uniquify))
 '(whitespace-style (quote (trailing tabs space-before-tab empty))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(diff-added ((t (:foreground "DarkGreen"))) 'now)
 '(diff-removed ((t (:foreground "DarkRed"))) 'now)
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

(defvar system-processor-count
  (let ((name (system-name)))
    (cond ((string= name "fklockii-MacBookPro") 2)
          ((string= name "fklockii-MacPro")     8)
          ((string= name "FKLOCKII-MACPRO")     8)
          ((string= name "fklockii-iMac")       4)
          (else 1)))
  "Absurd guess at the number of host processors; e.g., to guide make invokes.")

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
                               (directory-files ".")))
         (core-count-guess (number-to-string system-processor-count))
         (xcode-invoke "xcodebuild")
         (make-invoke (concat "make -j" core-count-guess)))
    (if has-proj-file
        ; then
        (compile (concat "time " xcode-invoke))
      ; else
      (compile (concat "time " make-invoke)))))

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
  "Resize current frame to be 80 characters width."
  (interactive)
  (set-frame-width (selected-frame) 80))
(defun frame-163 ()
  "Resize current frame to be 163 characters width (for two cols)."
  (interactive)
  (set-frame-width (selected-frame) 163))

(defun frame-60 ()
  "Resize current frame to be 60 characters high."
  (interactive)
  (set-frame-height (selected-frame) 60))
(defun frame-43 ()
  "Resize current frame to be 43 characters high."
  (interactive)
  (set-frame-height (selected-frame) 43))
(defun frame-25 ()
  "Resize current frame to be 25 characters high."
  (interactive)
  (set-frame-height (selected-frame) 25))

(defun search-cpp-access ()
  "Search forward for C++ access modifier"
  (interactive)
  (search-forward-regexp "\\(private\\|protected\\|public\\):"))

(defconst vc-hg-annotate-re
  "^[ \t]*\\([0-9]+\\) \\(.\\{30\\}\\)\\(?:\\(: \\)\\|\\(?: +\\(.+\\): \\)\\)")

;(defconst vc-hg-annotate-re
;  "^[ \t]*\\([0-9]+\\) \\(.\\{30\\}\\)")

(defun vc-hg-annotate-command (file buffer &optional revision)
  "Execute \"hg annotate\" on FILE, inserting the contents in BUFFER.
Optional arg REVISION is a revision to annotate from."
  (vc-hg-command buffer 0 file "annotate" "-d" "-n" ;;"--follow" ;; (follow prints filenames which I do not want)
                 (when revision (concat "-r" revision))))

(require 'comint)

(setq comint-completion-addsuffix '("\\" . " "))

;; Windows support:
;; "Fix" comint filename path handling.
(defun comint-match-partial-filename ()
  "Return the filename at point, or nil if none is found.
Environment variables are substituted.  See `comint-word'."
  (let ((filename (comint-word comint-file-name-chars)))
    ;;FSK:(insert "comint-match-partial-filename.filename: ") (insert filename)
    (and filename (comint-substitute-in-file-name
                   (comint-unquote-filename filename)))
    (and filename (comint-unquote-filename filename))
    ))

(defun comint-dynamic-complete-as-filename ()
  "Dynamically complete at point as a filename.
See `comint-dynamic-complete-filename'.  Returns t if successful."
  (let* ((completion-ignore-case read-file-name-completion-ignore-case)
         (completion-ignored-extensions comint-completion-fignore)
         ;; If we bind this, it breaks remote directory tracking in rlogin.el.
         ;; I think it was originally bound to solve file completion problems,
         ;; but subsequent changes may have made this unnecessary.  sm.
         ;;(file-name-handler-alist nil)
         (minibuffer-p (window-minibuffer-p (selected-window)))
         (success t)
         (dirsuffix (cond ((not comint-completion-addsuffix)
                           "")
                          ((not (consp comint-completion-addsuffix))
                           "/")
                          (t
                           (car comint-completion-addsuffix))))
         (filesuffix (cond ((not comint-completion-addsuffix)
                            "")
                           ((not (consp comint-completion-addsuffix))
                            " ")
                           (t
                            (cdr comint-completion-addsuffix))))
         (filename (comint-match-partial-filename))
         (filename-beg (if filename (match-beginning 0) (point)))
         (filename-end (if filename (match-end 0) (point)))
         (filename (or filename ""))
         (filedir (file-name-directory filename))
         (filenondir (file-name-nondirectory filename))
         (directory (if filedir (comint-directory filedir) default-directory))
         (completion (file-name-completion filenondir directory)))
    (cond ((null completion)
           (if minibuffer-p
               (minibuffer-message "No completions of %s" filename)
             (message "No completions of %s" filename))
           (setq success nil))
          ((eq completion t)            ; Means already completed "file".
           (insert filesuffix)
           (unless minibuffer-p
             (message "Sole completion")))
          ((string-equal completion "") ; Means completion on "directory/".
           (comint-dynamic-list-filename-completions))
          (t ; Completion string returned.
           (not (string-equal dirsuffix "\\")) ;; alt predicate
           (let ((file.v0 (concat (file-name-as-directory directory) completion))
                 (file.v1 (concat directory dirsuffix completion))
                 (filedir (if filedir (substring filename 0 (- (length filename) (length filenondir))) filedir))
                 (file (if filedir (concat filedir completion)
                         (concat (file-name-as-directory directory)
                                 completion))))
             ;; Insert completion.  Note that the completion string
             ;; may have a different case than what's in the prompt,
             ;; if read-file-name-completion-ignore-case is non-nil,
             (delete-region filename-beg filename-end)
             ;:FSK: (insert "filename: ") (insert filename) (insert " ")
             (if filedir (insert (comint-quote-filename filedir)))
             (insert (comint-quote-filename (directory-file-name completion)))
             (cond ((symbolp (file-name-completion completion directory))
                    ;; We inserted a unique completion.
                    (insert (if (file-directory-p file) dirsuffix filesuffix))
                    (unless minibuffer-p
                      (message "Completed")))
                   ((and comint-completion-recexact comint-completion-addsuffix
                         (string-equal filenondir completion)
                         (file-exists-p file))
                    ;; It's not unique, but user wants shortest match.
                    (insert (if (file-directory-p file) dirsuffix filesuffix))
                    (unless minibuffer-p
                      (message "Completed shortest")))
                   ((or comint-completion-autolist
                        (string-equal filenondir completion))
                    ;; It's not unique, list possible completions.
                    (comint-dynamic-list-filename-completions))
                   (t
                    (unless minibuffer-p
                      (message "Partially completed")))))))
    success))
