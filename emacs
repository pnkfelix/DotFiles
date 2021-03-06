;; -*- mode: emacs-lisp; indent-tabs-mode: nil -*-
(setq start-time (float-time))

(let ((emacs-priv (concat (getenv "HOME") "/.emacs_private")))
  (cond ((file-exists-p emacs-priv)
         (load emacs-priv))))

;; Give into the tyranny of the majority.
(setq sentence-end-double-space nil)

;; Note: this does not yet seem to work as I had expected it to.
;; See also https://stackoverflow.com/questions/637351/emacs-how-to-delete-text-without-kill-ring/8887741#comment84616397_8887741
(defun ruthlessly-kill-line (&optional arg)
  "Delete the rest of the current line; if there are no nonblanks there, delete thru newline.
With prefix argument ARG, delete that many lines from point.
Negative arguments delete lines backward.
With zero argument, deletes the text before point on the current line.

This is just like `kill-line' but it restores the kill-ring to its prior state."
  (interactive "P")
  (kill-line arg)
  (current-kill 1))

;; (global-set-key (kbd "C-S-k") 'ruthlessly-kill-line)

(defun filter (pred lst)
  (let (accum)
    (dolist (element lst accum)
      (cond ((funcall pred element)
             (setq accum (cons element accum)))))
    (reverse accum)))

(add-to-list 'load-path "~/ConfigFiles/Elisp")
(setq load-path (append load-path
                        (mapcar
                         (lambda (x) (concat "~/ConfigFiles/Elisp/" x))
                         (filter
                          (lambda (x) (not (= (aref x 0) (aref "." 0))))
                          (directory-files "~/ConfigFiles/Elisp/")))))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;;;; Trying to move to straight.el
;;
;; ;; From watching "Emacs Chat: Magnar Sveen (@emacsrocks)
;; ;; http://www.youtube.com/watch?v=87tjF_mYvpE
;; ;;
;; ;; This is a way to 
;; (require 'setup-package)
;; (defun init--install-packages ()
;;   (packages-install
;;    '(flx
;;      ;flx-ido
;;      ;ido-vertical-mode
;;      guide-key
;;      )))
;; 
;; (condition-case nil
;;     (init--install-packages)
;;   (error
;;    (package-refresh-contents)
;;    (init--install-packages)))

(defun ormap (pred lst)
  (let (accum)
    (dolist (element lst accum)
      (cond ((funcall pred element)
             (setq accum (cons element accum)))))
    (reverse accum)))

(defun any-with-twin (s)
  (string-match "EmacsTwin" s))

(defun any-with-red-twin (s)
  (string-match "EmacsRedTwin" s))

(defun any-with-worklog (s)
  (string-match "WorkLog" s))

(defun any-with-irc (s)
  (string-match "EmacsIRC" s))


(defvar emacs-is-felixs-irc
  (and (ormap 'any-with-irc load-path)
       (concat
        "non-nil only if this Emacs is named something like EmacsIRC.  "
        "Used to specializing environment for independent rcirc emacs instance.")))

(defvar emacs-is-felixs-worklog
  (and (ormap 'any-with-worklog load-path)
       (concat
        "non-nil only if this Emacs is named something like EmacsWorkLog.  "
        "Used to specializing environment for independent worklog emacs instance.")))

(defvar emacs-is-twin
  (and (ormap 'any-with-twin load-path)
       (concat
        "non-nil only if this Emacs is named something like EmacsTwin.  "
        "Used to specializing environment for independent worklog emacs instance.")))

(defvar emacs-is-red-twin
  (and (ormap 'any-with-red-twin load-path)
       (concat
        "non-nil only if this Emacs is named something like EmacsRedTwin.  "
        "Used to specializing environment for independent worklog emacs instance.")))

;; Coding system stuff is discussed in Info node
;; Interational .. Coding Systems


(defvar fsk-use-cedet nil)
 
;; http://cedet.sourceforge.net/setup.shtml
(when (and fsk-use-cedet  (not (featurep 'cedet-devel-load)))
  (load "~/ConfigFiles/Elisp/cedet/cedet-devel-load.el"))

;; Workaround bug
;;;; But not these ways
;;;; (add-to-list 'load-path "~/ConfigFiles/Elisp/cedet/lisp/cedet/srecode")
;;;; (autoload 'srecode/m3 "~/ConfigFiles/Elisp/cedet/lisp/cedet/srecode/m3.el")
;;;; (load-file "~/ConfigFiles/Elisp/cedet/lisp/cedet/srecode/loaddefs.el")
;; gave up, just turned off srecode minor mode below

(setq time-103 (- (float-time) start-time))

(straight-use-package 'whitespace)
(require 'uniquify)
(require 'comint) ; so that I can override some its fcns below.

;; too slow
;(require 'js2-mode)
(straight-use-package 'javascript-mode)
;; too ugly?  and besides, I don't have it on all my machines (yet).
;(require 'actionscript-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ack-command "rg")
 '(background-color "#042028")
 '(background-mode dark)
 '(circe-network-options
   (quote
    (("irc.mozilla.org" :nick "pnkfelix" :user "pnkfelix" :realname "Felix S. Klock II" :host "irc.mozilla.org" :service 6697 :channels
      (("#rust-fr" :after-auth))
      :nickserv-password mozilla-nickserv-password))))
 '(comint-completion-fignore nil)
 '(comint-password-prompt-regexp
   "\\(^ *\\|\\( SMB\\|'s\\|Bad\\|CVS\\|Enter\\(?: \\(?:\\(?:sam\\|th\\)e\\)\\)?\\|Kerberos\\|LDAP\\|New\\|Old\\|Repeat\\|UNIX\\|\\[sudo]\\|enter\\(?: \\(?:\\(?:sam\\|th\\)e\\)\\)?\\|login\\|new\\|old\\) +\\)\\(?:Pass\\(?: phrase\\|phrase\\|word\\)\\|Response\\|pass\\(?: phrase\\|phrase\\|word\\)\\)\\(?:\\(?:, try\\)? *again\\| (empty for no passphrase)\\| (again)\\)?\\(?: for \\(?:'[^']*'\\|[^:]+\\)\\)?:\\s *\\'")
 '(compilation-search-path (quote ("../src" nil)))
 '(compile-command "infer-remake.sh")
 '(completion-ignored-extensions
   (quote
    (".svn/" "CVS/" ".o" "~" ".bin" ".lbin" ".so" ".a" ".ln" ".blg" ".bbl" ".elc" ".lof" ".glo" ".idx" ".lot" ".dvi" ".fmt" ".tfm" ".pdf" ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".fasl" ".ufsl" ".fsl" ".dxl" ".pfsl" ".dfsl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo" ".abc")))
 '(cursor-color "#708183")
 '(custom-safe-themes
   (quote
    ("fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default)))
 '(debug-on-error t)
 '(egg-git-diff-options (quote ("--patience")))
 '(explicit-shell-file-name "bash")
 '(fill-column 80)
 '(foreground-color "#708183")
 '(gdb-enable-debug t)
 '(ido-default-buffer-method (quote selected-window))
 '(js2-basic-offset 2)
 '(js2-bounce-indent-p t)
 '(line-move-visual nil)
 '(lsp-rust-server (quote rust-analyzer))
 '(my-rcirc-notify-timeout 5)
 '(package-selected-packages (quote (eglot guide-key ido-vertical-mode flx-ido flx)))
 '(rcirc-log-flag t)
 '(rcirc-server-alist
   (quote
    (("irc.mozilla.org" :nick "pnkfelix" :port 6697 :user-name "pnkfelix" :full-name "Felix S. Klock II" :channels
      ("#rust-fr" "#rust" "#rust-internals" "#rust-bots" "#rustc" "#rust-lang" "#rust-libs" "#rust-unregistered" "#research" "#developers" "#devtools" "#introduction" "#lagaule")
      :encryption tls)
     ("irc.freenode.net" :nick "pnkfelix" :user-name "pnkfelix" :full-name "Felix S. Klock II" :channels
      ("#rcirc" "#scheme" "#emacs")
      nil nil))))
 '(rcirc-time-format "%Y%b%d %H:%M ")
 '(safe-local-variable-values (quote ((buffer-file-coding-system . utf-8-unix))))
 '(scheme-program-name "~/bin/larceny")
 '(semantic-default-submodes
   (quote
    (global-semantic-idle-completions-mode global-semantic-idle-scheduler-mode global-semanticdb-minor-mode global-semantic-idle-summary-mode global-semantic-idle-local-symbol-highlight-mode)))
 '(sentence-end-double-space nil)
 '(tool-bar-mode nil)
 '(truncate-partial-width-windows nil)
 '(uniquify-buffer-name-style (quote post-forward-angle-brackets) nil (uniquify))
 '(whitespace-style (quote (face trailing tabs space-before-tab empty))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diff-added ((t (:foreground "DarkGreen"))))
 '(diff-removed ((t (:foreground "DarkRed"))))
 '(ediff-current-diff-A ((t (:background "#553333" :foreground "#AAAAAA"))))
 '(ediff-current-diff-B ((t (:background "#335533" :foreground "#AAAAAA"))))
 '(ediff-fine-diff-A ((t (:background "#aa2222" :foreground "black"))))
 '(ediff-fine-diff-B ((t (:background "#22aa22" :foreground "black"))))
 '(org-agenda-restriction-lock ((t (:background "skyblue4" :foreground "black"))))
 '(org-clock-overlay ((t (:background "SkyBlue4" :foreground "black"))))
 '(org-column ((t (:background "grey90" :foreground "black" :strike-through nil :underline nil :slant normal :weight normal :height 120 :family "Monaco"))))
 '(org-column-title ((t (:background "grey30" :foreground "black" :underline t :weight bold))))
 '(smerge-base ((t (:background "saddle brown"))))
 '(smerge-refined-added ((t (:inherit smerge-refined-change :background "dark green"))))
 '(whitespace-line ((t (:background "alice blue"))))
 '(whitespace-tab ((t (:background "light goldenrod" :foreground "lightgray")))))

(column-number-mode 1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(transient-mark-mode 1)
(show-paren-mode 1)

; circe-network-options
; :nickserv-password mozilla-nickserv-password

;; From: http://www.emacswiki.org/emacs/IndentingC
;; (makes left-curly line up with start of if token, among other things)
(setq c-default-style "linux" c-basic-offset 4)

(cond ((and (not emacs-is-felixs-worklog) (not emacs-is-felixs-irc))
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

(setq time-206 (- (float-time) start-time))


(defvar my-fullscreen-p t "Check if fullscreen is on or off")
(defun my-toggle-fullscreen ()
  (interactive)
  (setq my-fullscreen-p (not my-fullscreen-p))
  (if my-fullscreen-p
      (set-frame-parameter nil 'fullscreen 'fullboth)
      (set-frame-parameter nil 'fullscreen 'nil)))

(defun second-toggle-fullscreen ()
  "Toggle full screen on X11"
  (interactive)
  (when (eq window-system 'x)
    (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth))))

'(defun ormap (pred l)
  (cond ((consp l) (cond ((funcall pred (car l))
                          (cons (car l) (ormap pred (cdr l))))
                         (t
                          (ormap pred (cdr l)))))
        ((null l) nil)))

(defvar system-processor-count
  (let ((name (system-name)))
    (cond ((or (string-match "mac" name) (string-match "Oenone" name)
               (string-match "Eris" name))
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


(setq time-300 (- (float-time) start-time))

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

(setq time-412 (- (float-time) start-time))

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

(defvar compile-remake-uses-trace nil
  "Controls whether invocation of remake uses --trace option or not.")

(defvar compile-including-xcode-command
  '(let* ((has-proj-file (xcode-project-files))
          (core-count-guess (number-to-string system-processor-count))
          (make-invoke (concat (cond (compile-remake-uses-trace "remake --trace ")
                                     (t "make"))
                               " -j" core-count-guess)))
     (if has-proj-file
                                        ; then
         "xcodebuild"
       (concat "time " make-invoke))))

(defun compile-including-xcode (command &optional comint)
  "Compile first looking for Xcode support in current directory."
  (interactive
   (list
    (let ((command (eval compile-including-xcode-command)))
      (if (or compilation-read-command current-prefix-arg)
          (compilation-read-command command)
        command))
    (consp current-prefix-arg)))
  (if (equal "xcodebuild" command)
      ;; then
      (call-interactively 'xcodebuild)
    ;; else
    (compile command)))

(defun compile-in-compilation-buffer (&optional arg)
  "Reattempt current compilation."
  (interactive (list ; (if current-prefix-arg (compilation-read-command compile-command) nil)))
                current-prefix-arg))
  ;; (interactive)
  (if (not (string-match "*compil*" (buffer-name)))
      (switch-to-buffer "*compilation*"))
  ;; (compile-including-xcode)
  ;; (if command (recompile command) (recompile))
  (recompile arg)
  )

;; run compile with the default command line
(defun recompile-including-xcode (&optional edit-command)
  "Re-compile the program including the current buffer.
If this is run in a Compilation mode buffer, re-use the arguments from the
original use.  Otherwise, recompile using `compile-command'.
If the optional argument `edit-command' is non-nil, the command can be edited."
  (interactive "P")
  (save-some-buffers (not compilation-ask-about-save)
                     compilation-save-buffers-predicate)
  (let ((default-directory (or compilation-directory (current-directory))))
    (when edit-command
      (setcar compilation-arguments
              (compilation-read-command (car compilation-arguments))))
    (apply 'compile-including-xcode (or compilation-arguments
                                        `(,(eval compile-command))))))

;; (global-set-key (kbd "<f5>") 'compile-including-xcode)
(global-set-key (kbd "<f5>") 'compile-in-compilation-buffer)
;; (global-set-key (kbd "<f5>") 'recompile-including-xcode)

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

(setq time-501 (- (float-time) start-time))

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

(straight-use-package 'maxframe)

;(set-frame-height last-event-frame 120)
;(set-frame-height last-event-frame 50)

(defun frame-80 ()
  "Resize current frame to be 80 characters width."
  (interactive)
  (set-frame-width (selected-frame) 80))
(defun frame-100 ()
  "Resize current frame to be 100 characters width."
  (interactive)
  (set-frame-width (selected-frame) 100))
(defun frame-163 ()
  "Resize current frame to be 163 characters width (for two cols)."
  (interactive)
  (set-frame-width (selected-frame) 163))
(defun frame-203 ()
  "Resize current frame to be 203 characters width (for two cols of 100 chars)."
  (interactive)
  (set-frame-width (selected-frame) 203))

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
(straight-use-package 'org)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(setq time-588 (- (float-time) start-time))

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

(setq time-689 (- (float-time) start-time))

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

(straight-use-package 'etags-select)
; (global-set-key "\M-?" 'etags-select-find-tag-at-point)
; (global-set-key "\M-." 'etags-select-find-tag)

(cond ((file-exists-p "~/ConfigFiles/Elisp/emacs-w3m/w3m-load.el")
       (require 'w3m-load)))

(cond ((file-exists-p "~/ConfigFiles/Elisp/egg/egg.el")
       (require 'egg)))

;; (add-to-list 'load-path "~/ConfigFiles/Elisp/ack-el")
(straight-use-package 'ack)
(autoload 'pcomplete/ack "pcmpl-ack")
(autoload 'pcomplete/ack-grep "pcmpl-ack")

;; Note that if this stops working, double-check the github
;; repo; e.g. frankpzh's pull request to clear PROMPT_COMMAND
;; (add-to-list 'load-path "~/ConfigFiles/Elisp/emacs-bash-completion")
(straight-use-package 'bash-completion)
(bash-completion-setup)

;; (add-to-list 'load-path "~/ConfigFiles/Elisp/exec-path-from-shell")
(straight-use-package 'exec-path-from-shell)
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; ;; See: http://www.nongnu.org/color-theme/
;; ;; (add-to-list 'load-path "~/ConfigFiles/Elisp/color-theme-6.6.0")
;; (straight-use-package 'color-theme)
(straight-use-package 'color-theme-modern)
;; ;; See: http://ethanschoonover.com/solarized
(add-to-list 'custom-theme-load-path "~/ConfigFiles/Elisp/emacs-color-theme-solarized")
;; (cond (emacs-is-felixs-worklog
;;        (load-theme 'solarized-light t))
;;       (emacs-is-felixs-irc
;;        (load-theme 'wombat t))
;;       (emacs-is-twin
;;        (color-theme-initialize)
;;        ; (color-theme-arjen)
;;        (color-theme-jsc-dark)
;;        )
;;       (emacs-is-red-twin
;;        (color-theme-initialize)
;;        ; (color-theme-arjen)
;;        (color-theme-tty-dark)
;;        )
;;       ((memq window-system '(mac ns))
;;        (load-theme 'solarized-dark t)
;;        )
;;       (nil ;; desparately trying to find a reliable theme for use in a ssh-ptty
;;        (color-theme-initialize)p
;;        (color-theme-jsc-light2)
;;        ))
(load-theme 'solarized-dark t)

;; http://code.google.com/p/js2-mode/wiki/InstallationInstructions
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; See http://js-comint-el.sourceforge.net/
(straight-use-package 'js-comint)
(let ((wip-js "/Users/fklock/bin/js"))
  (cond ((file-exists-p wip-js)
         (setq inferior-js-program-command wip-js))))
(add-hook 'js2-mode-hook '(lambda ()
                            (local-set-key "\C-x\C-e" 'js-send-last-sexp)
                            (local-set-key "\C-\M-x"  'js-send-last-sexp-and-go)
                            (local-set-key "\C-cb"    'js-send-buffer)
                            (local-set-key "\C-c\C-b" 'js-send-buffer-and-go)
                            (local-set-key "\C-cl"    'js-load-file-and-go)
                            ))
(setenv "MOZ_SHOW_ALL_JS_FRAMES"  "1")

;; See https://github.com/mozilla/rust/tree/master/src/etc/emacs
;; (add-to-list 'load-path "~/ConfigFiles/Elisp/rust-mode")

;; (straight-use-package 'rust-mode)
(straight-use-package
 '(rust-mode :type git :host github :repo "rust-lang/rust-mode"))

;; See git://jblevins.org/git/markdown-mode.git
;; (add-to-list 'load-path "~/ConfigFiles/Elisp/markdown-mode")
(straight-use-package 'markdown-mode)

(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; See https://github.com/holtzermann17/linepad
;(add-to-list 'load-path "~/ConfigFiles/Elisp/linepad")
;(require 'linepad)

;; http://stackoverflow.com/questions/1817370/using-ediff-as-git-mergetool/4512729#4512729
;;
;; Setup for ediff.
;;
(straight-use-package 'ediff)

(defvar ediff-after-quit-hooks nil
  "* Hooks to run after ediff or emerge is quit.")

(defadvice ediff-quit (after edit-after-quit-hooks activate)
  (run-hooks 'ediff-after-quit-hooks))

(setq time-800 (- (float-time) start-time))

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

;; http://www.emacswiki.org/emacs/AnsiColor
;; Note many programs won't emit color codes, because M-x shell sets
;; TERM to "dumb"; use e.g. TERM=xterm-color on case-by-case basis.
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; Niko notes that with M-x ansi-term, one can use C-x C-j to go into
;; "line mode", which makes it temporarily more like M-x shell, and
;; then C-c C-k to go back.

;; https://github.com/magnars/multiple-cursors.el
;; (add-to-list 'load-path "~/ConfigFiles/Elisp/multiple-cursors")
(straight-use-package 'multiple-cursors)

;; Adds a cursor to each line in an active region.
(global-set-key (kbd "C-c C-c") 'mc/edit-lines)

(global-set-key (kbd "C-c C-n") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c C-p") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-a") 'mc/mark-all-like-this)

(setq time-902 (- (float-time) start-time))

;; ;; https://github.com/victorhge/iedit
;; ;; (add-to-list 'load-path "~/ConfigFiles/Elisp/iedit")
;; (require 'iedit)

;; https://github.com/technomancy/clojure-mode
;; (add-to-list 'load-path "~/ConfigFiles/Elisp/clojure-mode")
;(require 'clojure-mode)

;; https://github.com/rolandwalker/unicode-fonts
;; (add-to-list 'load-path "~/ConfigFiles/Elisp/unicode-fonts")
;; (add-to-list 'load-path "~/ConfigFiles/Elisp/font-utils")
;; (add-to-list 'load-path "~/ConfigFiles/Elisp/ucs-utils")
;; (add-to-list 'load-path "~/ConfigFiles/Elisp/persistent-soft")
;; (add-to-list 'load-path "~/ConfigFiles/Elisp/pcache")
;(require 'persistent-soft) ; be ready to disable this...

(defun fsk-unicode-support ()
  "Loads the unicode support code and sets it up.  Not run on startup due to slowness."
  (interactive)
  (progn
    (straight-use-package 'persistent-soft)
    (straight-use-package 'ucs-utils)
    (straight-use-package 'font-utils)
    (straight-use-package 'unicode-fonts)
    (unicode-fonts-setup)))

;; To test, do M-x list-charset-chars and look for chess pieces circa line 265x

;(add-to-list 'load-path "~/ConfigFiles/Elisp/org-mode/contrib/oldexp")
;(require 'org-export-generic)
;(add-to-list 'load-path "~/ConfigFiles/Elisp/orgmode-markdown")
;(require 'markdown)
;(require 'org-export)

;; Note to self:
;; Useful: M-x set-input-mode TeX
;; but to back out: M-x toggle-input-method

;;; This is Felix trying (and failing) to resolve
;;; http://stackoverflow.com/questions/15180175/
;;;      how-to-disable-underscore-subscripting-in-emacs-tex-input-method
;;; on his own.
;;
;;(defun insert-underscore ()
;;  (interactive)
;;  (insert "_"))
;;(global-set-key "_" 'insert-underscore)
;;

(defun insert-double-colon-equal ()
  (interactive)
  (ucs-insert #x2A74))

(register-input-method
 "FSK-TeX" "UTF-8" 'quail-use-package
 "\\" "FSK-customized LaTeX-like input method for many characters."
 "~/ConfigFiles/Elisp/leim/latin-ltx")

;; http://community.schemewiki.org/?emacs-indentation
;; http://emacswiki.org/emacs/AddKeywords#toc6
(defun scheme-add-keywords (face-name keyword-rules)
  (let* ((keyword-list (mapcar #'(lambda (x)
                                   (symbol-name (cdr x)))
                               keyword-rules))
         (keyword-regexp (concat "(\\("
                                 (regexp-opt keyword-list)
                                 "\\)[ \n]")))
    (font-lock-add-keywords 'scheme-mode
                            `((,keyword-regexp 1 ',face-name))))
  (mapc #'(lambda (x)
            (put (cdr x)
                 'scheme-indent-function
                 (car x)))
        keyword-rules))

(add-hook 'scheme-mode-hook 'fsk-scheme-mode-hook)

(defun fsk-scheme-mode-hook ()
  "Custom Scheme mode hook."
  (interactive)
  (make-local-variable 'scheme-indent-function)
  ;; (put 'parameterize 'scheme-indent-function 1)
  (scheme-add-keywords 'font-lock-keyword-face
                       '((1 . when)
                         (1 . unless)
                         (2 . let1)
                         (1 . error)
                         (1 . parameterize)
                         (1 . require)
                         (1 . let*)
                         )))

;;;;; Trying to move to straight.el now
;;
;; (require 'package)
;; ;(add-to-list 'package-archives
;; ;             '("marmalade" .
;; ;               "http://marmalade-repo.org/packages/"))
;; (add-to-list 'package-archives
;;              '("melpa" . "http://melpa.milkbox.net/packages/") t)
;; (package-initialize)
;;
;; (when (not package-archive-contents)
;;   (package-refresh-contents))
;;
;; ;; (unless (package-installed-p 'scala-mode2)
;; ;;  (package-refresh-contents) (package-install 'scala-mode2))
;; ;; (unless (package-installed-p 'sbt-mode)
;; ;;   (package-refresh-contents) (package-install 'sbt-mode))

(defun say-hello ()
  "Sends a hello message to the Mac OS X message center"
  (interactive)
  (terminal-notify "Hello" "Emacs"))

(defun terminal-notify (msg &optional title subtitle group)
  "Sends a message to the Mac OS X message center"
  (cond
   ((or (executable-find "terminal-notify")
        (executable-find "terminal-notifier"))
    (let ((infile nil)
          (buffer "*terminal-notifier*")
          (display t))
      (apply 'call-process
             "terminal-notifier" infile buffer display
             "-message" msg
             (append (if title    (list "-title"    title)    nil)
                     (if subtitle (list "-subtitle" subtitle) nil)
                     (if group    (list "-group"    group)    nil))
             )))

   ;; Dispble growl because it is deadware.
   (nil (growl-page-me (if title title "")
                     (concat
                      (if subtitle (concat subtitle " ") "")
                      msg
                      (if group (concat " " group) ""))))
   ))

(defun say-when-compilation-finished (buffer string)
  "Sends a compile-done message to Mac OS X message center."
  ; (terminal-notify string "Compile finished" buffer)
  (terminal-notify string "Compile finished" (buffer-name buffer)))

(when (memq window-system '(mac ns))
  (add-to-list 'compilation-finish-functions 'say-when-compilation-finished))

(defun yank-removing-newlines ()
  "Yanks the last stretch of killed text, removing newlines.
See also `yank' (\\[yank])."
  (interactive)
  (insert-for-yank (replace-regexp-in-string "\n" "" (current-kill 0))))

;(eval-after-load 'rcirc '(require 'rcirc-notify))
;(eval-after-load 'rcirc '(require 'rcirc-color))

;; This lists hidden buffers with activity in the modeline
(eval-after-load 'rcirc '(rcirc-track-minor-mode 1))
;; C-c C-<SPC> switches to (non low-priority) buffers shown in modeline
;; C-c C-l   toggles priority of current buffer's channel (i.e. to low-priority)
;; C-c <TAG> toggles tracking the chanenl (i.e. no more in modeline)
;;
;; /keyword <word> highlights the word, and *may* also make its usage
;; be treated as activity even in a low-priority channel.
;;
;; In addition to /ignore <nickname>
;; there is also
;; /bright <nickname>
;; /dim <nickname>
;;
;; importantly: dimming a nick stops tracking its activity

;; I may want to look at 

;; FIXME: shouldn't this be unnecessary here?
;; (straight-use-package 'rcirc-notify)

;; (require 'growl)

;; Helper I made to help port header files to rust after discovering
;; that Rust numeric literals do not have an octal variant.
(defun region-octal-to-hex ()
  (interactive)
  (insert (format "0x%x" (string-to-number (current-kill 0) 8))))

(defun list-ref (lst n)
  "Returns nth element of lst."
  (nth n lst))

;; Rust Issue #6887
;; Quoting to see if I was wrong and it is now unnecessary
'(let ((re (concat "^\\([^ \n]+\\):\\([0-9]+\\):\\([0-9]+\\): "
                  "\\([0-9]+\\):\\([0-9]+\\) "
                  "\\(?:[Ee]rror\\|\\([Ww]arning\\)\\):")))
  (add-to-list 'compilation-error-regexp-alist-alist
               `(rustc ,re 1 (2 . 4) (3 . 5) (6))))

;; http://alexott.net/en/writings/emacs-devenv/EmacsCedet.html
;(add-to-list 'semantic-default-submodes 'semantic-idle-completions-mode)
;(add-to-list 'semantic-default-submodes 'global-semantic-idle-local-symbol-highlight-mode)
;;(add-to-list 'semantic-default-submodes 'global-semantic-decoration-mode)
(cond (fsk-use-cedet
       (semantic-mode 1)))

;; http://cedet.sourceforge.net/setup.shtml
;(global-ede-mode 1)                      ; Enable the Project management system
;(semantic-load-enable-code-helpers)      ; Enable prototype help and smart completion 
;; Felix: Disable because of some issue where srecode-m3-items is not properly
;; autoloading srecode/m3.el, AFAICT.
;(global-srecode-minor-mode 1)            ; Enable template insertion menu


;; (add-to-list 'load-path "~/ConfigFiles/Elisp/auto-complete")
; (add-to-list 'load-path "~/ConfigFiles/Elisp/auto-complete/lib/ert")
; (add-to-list 'load-path "~/ConfigFiles/Elisp/auto-complete/lib/fuzzy")
; (add-to-list 'load-path "~/ConfigFiles/Elisp/auto-complete/lib/popup")

;; (cond (fsk-use-cedet
;;        (require 'auto-complete-config)
;;        (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
;;        (ac-config-default)))

(setq time-1102 (- (float-time) start-time))

;; Keep this at the end of the initialization file.
(defvar worklog-directory "~/Documents/WorkLog")
(cond (emacs-is-felixs-worklog
       (setq inhibit-splash-screen t)

       ;(setq-default default-directory worklog-directory)
       (let ((default-directory worklog-directory))
         ;; (call-interactively 'find-file)
         ;; (call-interactively 'dired)
         (dired worklog-directory))

       (cond ((boundp 'split-window-below) (split-window-below))
             ((boundp 'split-window-vertically) (split-window-vertically)))

       (let ((s (shell "*worklog*")))
         (comint-send-string s "make update\n")
         s)
       ))

(cond ((not emacs-is-felixs-irc)
       ;; Apparently this is incompatible with rcirc face coloring
       (global-whitespace-mode 1)
       ))

(defun erc-compute-nick () "pnkfelix")
(defun erc-compute-full-name () "Felix S. Klock II")

(defun ignore-buffers (buffer-names)
  (mapcar
   (lambda (buffer-name) (with-current-buffer buffer-name
                           (rcirc-toggle-ignore-buffer-activity)))
   buffer-names))
   

(defun ignore-usual-rcirc-buffers ()
  "Ignores the usual set of rcirc buffers."
  (interactive)
  (ignore-buffers
   '("#developers@irc.mozilla.org"
     "#devtools@irc.mozilla.org"
     "#ionmonkey@irc.mozilla.org"
     "#pjs@irc.mozilla.org"
     "#jsapi@irc.mozilla.org"
     "#js@irc.mozilla.org"
     "#jslang@irc.mozilla.org"
     )))
       
(cond (emacs-is-felixs-irc
       (setq inhibit-splash-screen t)
       (rcirc nil)
       ;; (require 'circe)

       ;; (erc :server "irc.mozilla.org" :port 6697 :nick "pnkfelix" :full-name "Felix S. Klock II")
       ;; (erc :server "irc.freenode.net" :nick "pnkfelix" :full-name "Felix S. Klock II")

       t))

;; Maybe interesting but causing startup errors, so no.
(cond
 (nil
  (straight-use-package 'flycheck)
  (flycheck-define-checker
   servo-rust
   "A Rust syntax checker using the Rust compiler in Servo."
   :command ("rustc"
             "-L/Users/fklock/Dev/Rust/rust-sdl/objdir-opt"
             "-L/Users/fklock/opt/sdl-release-1.2.15-dbg-nopt/lib"
             "-C" "link-args=\" -I/Users/fklock/opt/sdl-release-1.2.15-dbg-nopt/include/SDL -framework CoreFoundation -framework CoreGraphics -framework AppKit /Users/fklock/Dev/Rust/rust-sdl/SDL-mirror/src/main/macosx/SDLMain.m  \""
             "--parse-only"
             source)
   :error-patterns
   ((error line-start (file-name) ":" line ":" column ": "
           (one-or-more digit) ":" (one-or-more digit) " error: "
           (message) line-end))
   :modes rust-mode)

  (add-hook 'rust-mode-hook (lambda () (flycheck-select-checker 'servo-rust)))
  ))

;; ;; From watching "Emacs Chat: Magnar Sveen (@emacsrocks)
;; ;; http://www.youtube.com/watch?v=87tjF_mYvpE
;; (require 'ido)
;; (ido-mode t)

;; (require 'flx-ido)
;; (flx-ido-mode 1)
;; ;; disable ido faces to see flx highlights
;; (setq ido-use-faces nil)

;; (require 'ido-vertical-mode)
;; (ido-vertical-mode)

;;;;; During shift to straight.el, removing anything I don't remember adding.
;;
;; (require 'guide-key)
;; (setq guide-key/guide-key-sequence '("C-x r" "C-x 4"))

(straight-use-package 'mmm-mode)

(mmm-add-classes
 '((markdown-python
    :submode python-mode
    :face mmm-declaration-submode-face
    :front "^```python[\n\r]+"
    :back "^```$")
   (markdown-rust
    :submode rust-mode
    :face mmm-declaration-submode-face
    :front "^```rust[\n\r]+"
    :back "^```$")
   (markdown-rust-code
    :submode rust-mode
    :face mmm-declaration-submode-face
    :front "^```rust,code[\n\r]+"
    :back "^```$")
   ))

;(setq mmm-global-mode 't)
(mmm-add-mode-ext-class 'markdown-mode nil 'markdown-python)
(mmm-add-mode-ext-class 'markdown-mode nil 'markdown-rust)
(mmm-add-mode-ext-class 'markdown-mode nil 'markdown-rust-code)

(straight-use-package 'yasnippet)

;; (straight-use-package 'lsp-mode)

;;(straight-use-package 'lsp-mode
;;                      :config (add-hook 'rust-mode-hook #'lsp))

;; Using version from git to get fixes
;; https://github.com/emacs-lsp/lsp-mode/pull/1258 and
;; https://github.com/emacs-lsp/lsp-mode/pull/1274
(straight-use-package
 '(lsp-mode :type git :host github :repo "emacs-lsp/lsp-mode"))

(straight-use-package 'company)
(straight-use-package 'company-lsp)

(defun average (&rest l) (/ (apply '+ l) (length l)))

(defun ediff-moz-master ()
  (interactive)
  (let ((file (ediff-get-default-file-name))
        (rev1 "moz-master")
        (rev2 ""))
    (ediff-vc-internal rev1 rev2 nil)))

(defvar read-buffer-visible-ok nil
  "Control whether the switch-to-buffer prompt will prefer non-visible windows.")

;; Hacked version of code from window.el.gz
(defun read-buffer-to-switch (prompt)
  "Read the name of a buffer to switch to, prompting with PROMPT.
Return the name of the buffer as a string.

This function is intended for the `switch-to-buffer' family of
commands since these need to omit the name of the current buffer
from the list of completions and default values."
  (let ((rbts-completion-table (internal-complete-buffer-except)))
    (minibuffer-with-setup-hook
        (lambda ()
          (setq minibuffer-completion-table rbts-completion-table)
          ;; Since rbts-completion-table is built dynamically, we
          ;; can't just add it to the default value of
          ;; icomplete-with-completion-tables, so we add it
          ;; here manually.
          (if (and (boundp 'icomplete-with-completion-tables)
                   (listp icomplete-with-completion-tables))
              (set (make-local-variable 'icomplete-with-completion-tables)
                   (cons rbts-completion-table
                         icomplete-with-completion-tables))))
      (read-buffer prompt (other-buffer (current-buffer) read-buffer-visible-ok)
                   (confirm-nonexistent-file-or-buffer)))))

(setq total-time (- (float-time) start-time))

(message "Done loading .emacs %s"
         `((time-103 ,time-103)
           (time-206 ,time-206)
           (time-300 ,time-300)
           (time-412 ,time-412)
           (time-501 ,time-501)
           (time-588 ,time-588)
           (time-689 ,time-689)
           (time-800 ,time-800)
           (time-902 ,time-902)
           (time-1102 ,time-1102)))
         
(message "Done loading .emacs %s" total-time)
(put 'company-complete 'disabled nil)
