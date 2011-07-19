;; -*- mode: lisp; indent-tabs-mode: nil -*-

;; Coding system stuff is discussed in Info node
;; Interational .. Coding Systems

(setq load-path (cons "~/ConfigFiles/Elisp" load-path))
(require 'whitespace)
(require 'uniquify)
(require 'comint) ; so that I can override some its fcns below.

;; too slow
;(require 'js2-mode)
(require 'javascript-mode)
;(require 'actionscript-mode)

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

(require 'server)
(when (not (server-running-p))
  (server-start)
  (setenv "EDITOR" "~/bin/emacsclient"))

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
  (read (car (process-lines "sysctl" "-n" "hw.ncpu"))))

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

(defun xcodebuild-list ()
  "Returns Xcode target/configuration lines for project in current directory."
  (interactive)
  (process-lines "xcodebuild" "-list"))

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

(defun xcodebuild (archs cfg tgt cmds)
  "Compile via xcodebuild prompting for ARCHS, config, target and commands."
  ; (interactive "sARCHS: \nsconfiguration: \nstarget: ")
  (interactive (progn
                 ;; Can fall back on this if necessary; it prints output to window
                 ;; (xcodebuild-list-shell)
                 (let ((xcode-lines (xcodebuild-list)))
                   (let ((archs '("x86_64" "i386"))
                         (configs (no-empties (extract-xcode-configs xcode-lines)))
                         (targets (no-empties (extract-xcode-targets xcode-lines))))
                     (list
                      (completing-read (format "ARCHS %s: " archs)
                                       (no-empties (string-combinations archs)))
                      (completing-read (format "configuration %s: " configs)
                                       (no-empties configs))
                      (completing-read "target: " (no-empties targets))
                      (completing-read "commands (build): "
                                       (list "build" "clean" "clean build"))
                      )))))
  (let ((archs-arg (concat " ONLY_ACTIVE_ARCH=NO ARCHS=\"" archs "\""))
        (cfg-arg (concat " -configuration " cfg))
        (tgt-arg (concat " -target " tgt))
        (tmp-file (make-temp-file "xcodebuild" nil ".log")))
    (let ((cmd (concat "time ( xcodebuild" archs-arg cfg-arg tgt-arg
                       " " cmds
                       " | tee " tmp-file
                       " | grep --before-context=5 ':' "
                       "&& tail -5 " tmp-file " )")))
      (compile cmd))))

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
