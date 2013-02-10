;; -*- mode: lisp; indent-tabs-mode: nil -*-

(defvar emacs-is-felixs-worklog
  (and (getenv "EMACSLOADPATH") (string-match "WorkLog" (getenv "EMACSLOADPATH")))
  (concat
   "non-nil only if this Emacs is named something like EmacsWorkLog.  "
   "Used to specializing environment for independent worklog emacs instance."))

;; Coding system stuff is discussed in Info node
;; Interational .. Coding Systems

(add-to-list 'load-path "~/ConfigFiles/Elisp")
(require 'whitespace)
(require 'uniquify)
(require 'comint) ; so that I can override some its fcns below.

;; too slow
;(require 'js2-mode)
(require 'javascript-mode)
;; too ugly?  and besides, I don't have it on all my machines (yet).
;(require 'actionscript-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(comint-completion-fignore nil)
 '(comint-password-prompt-regexp "\\(^ *\\|\\( SMB\\|'s\\|Bad\\|CVS\\|Enter\\(?: \\(?:\\(?:sam\\|th\\)e\\)\\)?\\|Kerberos\\|LDAP\\|New\\|Old\\|Repeat\\|UNIX\\|\\[sudo]\\|enter\\(?: \\(?:\\(?:sam\\|th\\)e\\)\\)?\\|login\\|new\\|old\\) +\\)\\(?:Pass\\(?: phrase\\|phrase\\|word\\)\\|Response\\|pass\\(?: phrase\\|phrase\\|word\\)\\)\\(?:\\(?:, try\\)? *again\\| (empty for no passphrase)\\| (again)\\)?\\(?: for \\(?:'[^']*'\\|[^:]+\\)\\)?:\\s *\\'")
 '(completion-ignored-extensions (quote (".svn/" "CVS/" ".o" "~" ".bin" ".lbin" ".so" ".a" ".ln" ".blg" ".bbl" ".elc" ".lof" ".glo" ".idx" ".lot" ".dvi" ".fmt" ".tfm" ".pdf" ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".fasl" ".ufsl" ".fsl" ".dxl" ".pfsl" ".dfsl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo" ".abc")))
 '(debug-on-error t)
 '(gdb-enable-debug t)
 '(gud-gud-gdb-command-name "gdb --fullname")
 '(js2-basic-offset 2)
 '(js2-bounce-indent-p t)
 '(line-move-visual nil)
 '(truncate-partial-width-windows nil)
 '(uniquify-buffer-name-style (quote post-forward-angle-brackets) nil (uniquify))
 '(whitespace-style (quote (face trailing tabs space-before-tab empty))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diff-added ((t (:foreground "DarkGreen"))) t)
 '(diff-removed ((t (:foreground "DarkRed"))) t)
 '(whitespace-line ((t (:background "alice blue"))))
 '(whitespace-tab ((t (:background "light goldenrod" :foreground "lightgray")))))

(global-whitespace-mode 1)
(column-number-mode 1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(transient-mark-mode 1)
(show-paren-mode 1)

;; From: http://www.emacswiki.org/emacs/IndentingC
;; (makes left-curly line up with start of if token, among other things)
(setq c-default-style "linux" c-basic-offset 4)

(cond ((not emacs-is-felixs-worklog)
       (require 'server)
       (when (not (server-running-p))
         (server-start)
         (setenv "EDITOR" "~/bin/emacsclient")
         ;; (setenv "EDITOR" (concat exec-directory "/emacsclient"))
         )))

(setenv "GIT_PAGER" "cat")

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

'(defun ormap (pred l)
  (cond ((consp l) (cond ((funcall pred (car l))
                          (cons (car l) (ormap pred (cdr l))))
                         (t
                          (ormap pred (cdr l)))))
        ((null l) nil)))

(defun ormap (pred lst)
  (let (accum)
    (dolist (element lst accum)
      (cond ((funcall pred element)
             (setq accum (cons element accum)))))
    (reverse accum)))

(defvar system-processor-count
  (let ((name (system-name)))
    (cond ((string-match "mac" name)
           (read (car (process-lines "sysctl" "-n" "hw.ncpu"))))
          ((or (string-match "linux" name)
               (string-match "ubuntu" name))
           (length (process-lines "grep" "processor" "/proc/cpuinfo")))
          (t
           1))))

(defvar system-processor-count-old
  (let ((name (system-name)))
    (cond ((string= name "fklockii-MacBookPro")   2)
          ((string= name "fklockii-MacPro")       8)
          ((string= name "fklockii-MacPro.local") 8)
          ((string= name "FKLOCKII-MACPRO")       8)
          ((string= name "fklockii-iMac")         4)
          ((string= name "fklockii-iMac.local")   4)
          (t 1)))
  "Absurd guess at the number of host processors; e.g., to guide make invokes.")

(defvar ASCPATH
  (let ((name (system-name)))
    (cond
     ;;((string= name "fklockii-MacBookPro") 2)
     ;;((string= name "fklockii-MacPro")     8)
     ;;((string= name "FKLOCKII-MACPRO")     8)
     ((or (string= name "fklockii-iMac")
          (string= name "fklockii-iMac.local"))
      "/Users/fklockii/Dev/FlashPFarce/tools/asc/asc.jar"))))

;; Perhaps predicate on whether ASC is already set in environment?
(setenv "ASC" ASCPATH)

(defun xcodebuild-list-shell ()
  "Prints xcodebuild help and target/configuration list to a shell buffer."
  (interactive)
  (shell-command "xcodebuild -help ; xcodebuild -list"))

(defun xcodebuild-list (proj)
  "Returns Xcode target/configuration lines for project in current directory."
  (interactive (list (call-interactively 'determine-xcode-project)))
  (process-lines "xcodebuild" "-project" proj "-list"))

;; until-drop : String [Listof String] -> [Listof String]
;; (until-drop c (list aa bb cc dd ee ff)) => (dd ee ff)
(defun until-drop (name l)
  "Drops every element up to and including the element matching name."
  (cond ((null l) nil)
        ((string-match name (car l)) (cdr l))
        (t (until-drop name (cdr l)))))

;; until-take : String [Listof String] -> [Listof String]
;; (until-take d (list aa bb cc dd ee ff)) => (aa bb cc)
(defun until-take (name l)
  "Accumulates every element up to but excluding the element matching name"
  (cond ((null l) nil)
        ((string-match name (car l)) nil)
        (t (cons (car l) (until-take name (cdr l))))))

;; extract-subsequence : String String [Listof String] -> [Listof String]
;; (extract-subsequence b e (list aa bb cc dd ee ff)) => (cc dd)
(defun extract-subsequence (start-marker end-marker l)
  "Produces subsequence exclusively bounded by start and end markers"
  (until-drop start-marker (until-take end-marker l)))

;; remove-spaces-and-active-marker : String -> String
(defun remove-spaces-and-active-marker (line)
  "Removes space characters and occurrences of \"(Active)\" from line."
  (replace-regexp-in-string
   " " "" (replace-regexp-in-string "(Active)" "" line)))

;; extract-xcode-targets : [Listof String] -> [Listof String]
(defun extract-xcode-targets (lines)
  "Extracts the list of targets from lines."
  (mapcar 'remove-spaces-and-active-marker
          (extract-subsequence "Targets:"
                               "Build Configurations:"
                               lines)))

;; extract-xcode-configs : [Listof String] -> [Listof String]
(defun extract-xcode-configs (lines)
  "Extracts the list of Xcode configurations from lines."
  (mapcar 'remove-spaces-and-active-marker
          (extract-subsequence "Build Configurations:"
                               "If no build configuration is specified"
                               lines)))

(defun xcodebuild-list-targets ()
  "Returns list of Xcode targets for project in current directory."
  (interactive)
  (extract-xcode-targets (xcodebuild-list)))

(defun xcodebuild-list-configs ()
  "Returns list of Xcode configurations for project in current directory."
  (interactive)
  (extract-xcode-configs (xcodebuild-list)))

;; insert-everywhere : forall X . X [Listof X] -> [Listof [Listof X]]
;; (insert-everywhere a (list b c)) => ((a b c) (b a c) (b c a))
(defun insert-everywhere (first lst)
  "Inserts first at every point before and after the elements of lst."
  (cond ((null lst) (list (list first)))
        (t (cons (cons first lst)
                 (mapcar (lambda (x) (cons (car lst) x))
                         (insert-everywhere first (cdr lst)))))))

;; no-empties : [Listof X] -> [Listof X]
;; (no-empties (list a b "" nil c d "" e nil f)) => (a b c d e f)
(defun no-empties (lst)
  "Removes empty strings and lists from lst."
  (cond ((null lst) nil)
        (t (cond ((null (car lst))     (no-empties (cdr lst)))
                 ((equal "" (car lst)) (no-empties (cdr lst)))
                 (t (cons (car lst) (no-empties (cdr lst))))))))

;; list-combinations : [Listof X] -> [Listof [Listof X]]
;; (list-combinations (list a b)) => ((a b) (b a) (a) (b) ())
;; [[ really should be called list-powerlist or list-sublist-permutations ]]
(defun list-combinations (args)
  "Returns list of all permutations of all subcollections of args."
  (cond ((null args) (list nil))
        (t (let ((cs (list-combinations (cdr args))))
             (append (apply 'append
                            (mapcar (lambda (c) (insert-everywhere (car args) c))
                             cs))
                     cs)))))

;; string-combinations : [Listof String] -> [Listof String]
;; (string-combinations (list "a" "b")) => ("a b" "b a" "a" "b" "")
(defun string-combinations (args)
  "Returns list of all concatentations of subcollections of args."
  (mapcar (lambda (c) (mapconcat 'identity c " "))
          (list-combinations args)))

(defun xcode-project-files ()
  (interactive)
  (let* ((suffix ".xcodeproj")
         (dirent-is-xcodeproj
          (lambda (filename)
            (and (> (length filename) (length suffix))
                 (string-equal (substring filename (- (length suffix)))
                               suffix)))))
    (ormap dirent-is-xcodeproj (directory-files "."))))

(defun determine-xcode-project (proj-files)
  "Determines, interactively if necessary, which .xcodeproj to use to build."
  (interactive (list (xcode-project-files)))
  (if (null (cdr proj-files))
      (car proj-files)
    (completing-read "project: " proj-files)))


(defun xcodebuild (proj archs cfg tgt cmds)
  "Compile via xcodebuild prompting for ARCHS, config, target and commands."
  ; (interactive "sARCHS: \nsconfiguration: \nstarget: ")
  (interactive (progn
                 ;; Can fall back on this if necessary; it prints output to window
                 ;; (xcodebuild-list-shell)
                 (let* ((proj (call-interactively 'determine-xcode-project))
                        (xcode-lines (xcodebuild-list proj)))
                   (let ((archs '("x86_64" "i386"))
                         (configs (no-empties (extract-xcode-configs xcode-lines)))
                         (targets (no-empties (extract-xcode-targets xcode-lines))))
                     (list
                      proj
                      (completing-read (format "ARCHS %s: " archs)
                                       (no-empties (string-combinations archs)))
                      (completing-read (format "configuration %s: " configs)
                                       (no-empties configs))
                      (completing-read "target: " (no-empties targets))
                      (completing-read "commands (build): "
                                       (list "build" "clean" "clean build"))
                      )))))
  (xcodebuild-impl proj archs cfg tgt cmds))

(defun xcodebuild-impl (proj archs cfg tgt cmds)
  (let ((archs-arg (concat " ONLY_ACTIVE_ARCH=NO ARCHS=\"" archs "\""))
        (cfg-arg (concat " -configuration " cfg))
        (tgt-arg (concat " -target " tgt))
        (tmp-file (make-temp-file "xcodebuild" nil ".log")))
    (let ((cmd (concat "time ( xcodebuild"
                       " -project " proj
                       archs-arg cfg-arg tgt-arg
                       " " cmds
                       " | tee " tmp-file
                       " | grep --before-context=5 -e '^[^\"]*:' "
                       "&& tail -5 " tmp-file " )")))
      (compile cmd))))

(defun compile-including-xcode ()
  "Compile first looking for Xcode support in current directory."
  (interactive)
  (let* ((has-proj-file (xcode-project-files))
         (core-count-guess (number-to-string system-processor-count))
         (make-invoke (concat "make -j" core-count-guess)))
    (if has-proj-file
        ; then
        (call-interactively 'xcodebuild)
      ; else
      (compile (concat "time " make-invoke)))))

(defun compile-in-compilation-buffer ()
  "Reattempt current compilation."
  (interactive)
  (if (not (string-match "*compil*" (buffer-name)))
      (switch-to-buffer "*compilation*"))
  (compile-including-xcode))

;; (global-set-key (kbd "<f5>") 'compile-including-xcode)
(global-set-key (kbd "<f5>") 'compile-in-compilation-buffer)

(defun set-indent-tabs-mode ()
  "Toggle setting for indent-tabs-mode variable."
  (interactive)
  (setq indent-tabs-mode (not indent-tabs-mode)))

;; Disable WordWrap (Aquamacs) if present
(if (boundp 'global-visual-line-mode)
    (global-visual-line-mode 0))
(if (boundp 'turn-off-word-wrap)
    (turn-off-word-wrap) ;(setq word-wrap nil)
  )

;; In Emacs defaults, .st is used for ESS Transcript files, but
;; Tamarin uses the extension for Selftest (preprocessor generating C++)
(add-to-list 'auto-mode-alist '("\\.st\\'" . c++-mode))

(add-to-list 'auto-mode-alist '("\\.as\\'" . javascript-mode))
;(add-to-list 'auto-mode-alist '("\\.as\\'" . actionscript-mode))

;; GNU GLOBAL is more recent source tag system
;; minor gtags-mode will bind M-. to gtags-find-tag
;(require 'gtags)

;; To prep, % cd SRCROOT; export GTAGSFORCECPP= gtags

(setq font-lock-maximum-decoration
      '((c-mode . 2) (c++-mode . 2)))

(require 'maxframe)

;(set-frame-height last-event-frame 120)
(set-frame-height last-event-frame 50)

(defun frame-80 ()
  "Resize current frame to be 80 characters width."
  (interactive)
  (set-frame-width (selected-frame) 80))
(defun frame-163 ()
  "Resize current frame to be 163 characters width (for two cols)."
  (interactive)
  (set-frame-width (selected-frame) 163))
(defun frame-246 ()
  "Resize current frame to be 246 characters width (for three cols)."
  (interactive)
  (set-frame-width (selected-frame) 246))

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

(defun search-cpp-access-backward ()
  "Search forward for C++ access modifier"
  (interactive)
  (search-backward-regexp "\\(private\\|protected\\|public\\):"))

(defconst vc-hg-annotate-re
  "^[ \t]*\\([0-9]+\\) \\(.\\{30\\}\\)\\(?:\\(: \\)\\|\\(?: +\\(.+\\): \\)\\)")

;(defconst vc-hg-annotate-re
;  "^[ \t]*\\([0-9]+\\) \\(.\\{30\\}\\)")

(defun vc-hg-annotate-command (file buffer &optional revision)
  "Execute \"hg annotate\" on FILE, inserting the contents in BUFFER.
Optional arg REVISION is a revision to annotate from."
  (vc-hg-command buffer 0 file "annotate" "-d" "-n" ;;"--follow" ;; (follow prints filenames which I do not want)
                 (when revision (concat "-r" revision))))

;; Org mode material
(require 'org)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; All of the below is for Windows only; want to conditionally run it
(cond
 ((eq window-system 'w32)

  ;; comint package is required above

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
               ;;:FSK: (insert "filename: ") (insert filename) (insert " ")
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
  ))

(defun kill-emacs-signal-error ()
  (interactive)
  (kill-emacs 1))

(defun insert-octothorpe ()
  (interactive)
  (insert-char ?# 1))

;; This is a way to attempt to get the effect of M-3 expressing "#"
;; but it will not work in the context of interactive search.
;; (See also: http://ergoemacs.org/emacs/emacs_key-translation-map.html
;; http://www.masteringemacs.org/articles/2011/02/08/mastering-key-bindings-emacs/
; (global-set-key (kbd "M-3") 'insert-octothorpe)
; (global-set-key (kbd "s-3") 'insert-octothorpe)

(keyboard-translate ?£ ?#)

(require 'etags-select)
(global-set-key "\M-?" 'etags-select-find-tag-at-point)
(global-set-key "\M-." 'etags-select-find-tag)

(add-to-list 'load-path "~/ConfigFiles/Elisp/ack-el")
(require 'ack)
(autoload 'pcomplete/ack "pcmpl-ack")
(autoload 'pcomplete/ack-grep "pcmpl-ack")

(add-to-list 'load-path "~/ConfigFiles/Elisp/exec-path-from-shell")
(require 'exec-path-from-shell)
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; See: http://www.nongnu.org/color-theme/
(add-to-list 'load-path "~/ConfigFiles/Elisp/color-theme-6.6.0")
(require 'color-theme)
;; See: http://ethanschoonover.com/solarized
(add-to-list 'custom-theme-load-path "~/ConfigFiles/Elisp/emacs-color-theme-solarized")
(cond (emacs-is-felixs-worklog
       (load-theme 'solarized-light t))
      (t
       (load-theme 'solarized-dark t)))

;; http://code.google.com/p/js2-mode/wiki/InstallationInstructions
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; See http://js-comint-el.sourceforge.net/
(require 'js-comint)
(setq inferior-js-program-command
      "/Users/fklock/Dev/Mozilla/iontrail/objdir-dbg-js/js")
(add-hook 'js2-mode-hook '(lambda ()
                            (local-set-key "\C-x\C-e" 'js-send-last-sexp)
                            (local-set-key "\C-\M-x"  'js-send-last-sexp-and-go)
                            (local-set-key "\C-cb"    'js-send-buffer)
                            (local-set-key "\C-c\C-b" 'js-send-buffer-and-go)
                            (local-set-key "\C-cl"    'js-load-file-and-go)
                            ))

;; See https://github.com/mozilla/rust/tree/master/src/etc/emacs
(add-to-list 'load-path "~/ConfigFiles/Elisp/rust-mode")
(require 'rust-mode)

;; See git://jblevins.org/git/markdown-mode.git
(add-to-list 'load-path "~/ConfigFiles/Elisp/markdown-mode")
(require 'markdown-mode)

;; http://stackoverflow.com/questions/1817370/using-ediff-as-git-mergetool/4512729#4512729
;;
;; Setup for ediff.
;;
(require 'ediff)

(defvar ediff-after-quit-hooks nil
  "* Hooks to run after ediff or emerge is quit.")

(defadvice ediff-quit (after edit-after-quit-hooks activate)
  (run-hooks 'ediff-after-quit-hooks))

(setq git-mergetool-emacsclient-ediff-active nil)

(defun local-ediff-frame-maximize ()
  (let* ((bounds (display-usable-bounds))
     (x (nth 0 bounds))
     (y (nth 1 bounds))
     (width (/ (nth 2 bounds) (frame-char-width)))
     (height (/ (nth 3 bounds) (frame-char-height))))
    (set-frame-width (selected-frame) width)
    (set-frame-height (selected-frame) height)
    (set-frame-position (selected-frame) x y)))

(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

(defun local-ediff-before-setup-hook ()
  (setq local-ediff-saved-frame-configuration (current-frame-configuration))
  (setq local-ediff-saved-window-configuration (current-window-configuration))
  ;; (local-ediff-frame-maximize)
  (if git-mergetool-emacsclient-ediff-active
      (raise-frame)))

(defun local-ediff-quit-hook ()
  (set-frame-configuration local-ediff-saved-frame-configuration)
  (set-window-configuration local-ediff-saved-window-configuration))

(defun local-ediff-suspend-hook ()
  (set-frame-configuration local-ediff-saved-frame-configuration)
  (set-window-configuration local-ediff-saved-window-configuration))

(add-hook 'ediff-before-setup-hook 'local-ediff-before-setup-hook)
(add-hook 'ediff-quit-hook 'local-ediff-quit-hook 'append)
(add-hook 'ediff-suspend-hook 'local-ediff-suspend-hook 'append)

;; Useful for ediff merge from emacsclient.
(defun git-mergetool-emacsclient-ediff (local remote base merged)
  (setq git-mergetool-emacsclient-ediff-active t)
  (if (file-readable-p base)
      (ediff-merge-files-with-ancestor remote local base nil merged)
    (ediff-merge-files local remote nil merged))
  (recursive-edit))

(defun git-mergetool-emacsclient-ediff-after-quit-hook ()
  (exit-recursive-edit))

(add-hook 'ediff-after-quit-hooks
          'git-mergetool-emacsclient-ediff-after-quit-hook 'append)

(defun tell-emacsclients-for-buffer-to-die ()
  "Sends error exit command to every client for the current buffer."
  (interactive)
  (dolist (proc server-buffer-clients)
    (server-send-string proc "-error die")))

(defun tell-all-emacsclients-to-die ()
  "Sends error exit command to every client for all buffers."
  (interactive)
  (dolist (proc server-clients)
    (server-send-string proc "-error die")))

;;; This doesn't work; the hooks are run too late within the server code.
;; (add-hook 'kill-buffer-hook 'tell-emacsclients-for-buffer-to-die)

(defun kill-buffer-with-special-emacsclient-handling ()
  "Wrapper around kill-buffer that ensures tell-emacsclients-for-buffer-to-die is on the hooks"
  (interactive)
  (add-hook 'kill-buffer-hook 'tell-emacsclients-for-buffer-to-die nil t)
  (kill-buffer))

;; (global-set-key (kbd "C-x k") 'kill-buffer)

(defun install-emacsclient-wrapped-kill-buffer ()
  "Installs wrapped kill-buffer with special emacsclient handling.
Best not to install it unconditionally because the server is not
necessarily running."
  (interactive)
  (global-set-key (kbd "C-x k") 'kill-buffer-with-special-emacsclient-handling))

(add-hook 'server-switch-hook 'install-emacsclient-wrapped-kill-buffer)

(defvar worklog-directory "~/Documents/WorkLog")
(cond (emacs-is-felixs-worklog
       (setq inhibit-splash-screen t)

       ;(setq-default default-directory worklog-directory)
       (let ((default-directory worklog-directory))
         ;; (call-interactively 'find-file)
         ;; (call-interactively 'dired)
         (dired worklog-directory)
         )))

(require 'gud)
(defun gud-pjs (command-line)
  "Wrapper around gud-gdb that runs firefox using my pjs-alpha profile."
  ;; --P pjs-alpha
  (interactive (list (gud-query-cmdline 'gud-gdb)))
  (let ((new-command-line
         (cond ((string-match " --args " command-line)
                (concat command-line " -P pjs-alpha"))
               (t
                (concat command-line " --args firefox -P pjs-alpha")))))
    (gud-gdb new-command-line)))

;; http://www.emacswiki.org/emacs/AnsiColor
;; Note many programs won't emit color codes, because M-x shell sets
;; TERM to "dumb"; use e.g. TERM=xterm-color on case-by-case basis.
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; https://github.com/magnars/multiple-cursors.el
(add-to-list 'load-path "~/ConfigFiles/Elisp/multiple-cursors")
(require 'multiple-cursors)

;; Adds a cursor to each line in an active region.
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; https://github.com/victorhge/iedit
(add-to-list 'load-path "~/ConfigFiles/Elisp/iedit")
(require 'iedit)
