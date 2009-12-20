;;; load extra modes
(add-to-list 'load-path "~/elisp")
(add-to-list 'load-path "~/elisp/haskell-mode/")
(add-to-list 'load-path "~/elisp/ecb")
(add-to-list 'load-path "~/elisp/eieio-0.17")
(add-to-list 'load-path "~/elisp/semantic-1.4.4")
(add-to-list 'load-path "~/elisp/_local")
(add-to-list 'load-path "~/elisp/slime/")
(add-to-list 'load-path "~/elisp/slime/contrib")
(add-to-list 'load-path "~/elisp/mmm-mode/")
(add-to-list 'load-path "~/elisp/ocaml")
(add-to-list 'load-path "~/elisp/magit")
(add-to-list 'load-path "~/projects/cpan_modules/Stylish/emacs")
(add-to-list 'load-path "~/projects/eproject")
(add-to-list 'load-path "~/projects/eslide")
(add-to-list 'load-path "~/elisp/scala-mode")

(ignore-errors (require 'stylish-repl-iedit))
(require 'auto-inserts)
(require 'cperl-extras)
(require 'cperl-mode)
(require 'css-mode)
(require 'editing-extras)
(require 'elisp-extras)
(require 'eproject)
(require 'eproject-extras)
(require 'eshell-extras)
(require 'eslide)
(require 'espresso)
(require 'git)
(require 'gnus)
(require 'haskell-mode)
(require 'haskell-extras)
(require 'inf-haskell)
(require 'iswitchb-extras)
(require 'lisp-extras)
(require 'magit)
(require 'message)
(require 'rcirc)
(require 'rcirc-extras)
(require 'rcirc-xmonad-notify)
(require 'scala-mode-auto)
(require 'sql-extras)
(require 'term-extras)
(require 'uniquify)
(require 'w3m-extras)
(require 'w3m-load)
(require 'window-number)
(require 'windowing-extras)
(require 'xmms)
(require 'yaml-mode)

;;; slime
(eval-after-load "slime"
  '(progn
     (setq slime-lisp-implementations
           '((sbcl ("/usr/bin/sbcl"))
             (ecl ("/usr/bin/ecl"))
             (clisp ("/usr/bin/clisp"))))
     (slime-setup '(
                    slime-asdf
                    slime-autodoc
                    slime-editing-commands
                    slime-fancy-inspector
                    slime-fontifying-fu
                    slime-fuzzy
                    slime-indentation
                    slime-mdot-fu
                    slime-package-fu
                    slime-references
                    slime-repl
                    slime-sbcl-exts
                    slime-scratch
                    slime-xref-browser
                    ))
     (slime-autodoc-mode)
     (setq slime-complete-symbol*-fancy t)
     (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)))
(require 'slime)

;;; modes i want on by default
(iswitchb-mode 1)
;(desktop-save-mode 1)
(winner-mode 1)
(window-number-mode 1)
(windmove-default-keybindings)
(defalias 'perl-mode 'cperl-mode)

;;; hooks
(defun text-hooks ()
  "Turn on modes that I need when editing English text."
  (turn-on-auto-fill)
  (flyspell-mode 1)
  (local-set-key "\C-cu" 'insert-same-number-of-chars-as-line-above)
  (local-set-key "\C-ccw" 'ispell-complete-word))

(defun cperl-hooks ()
  (when (featurep 'stylish-repl)
    (local-set-key (kbd "C-M-x") 'stylish-repl-send-file)))

(defun maybe-flymake-mode ()
  (interactive)
  (if (not (or
            ;; if we've disabled flymake, don't turn it on (XXX: perl-specific)
            (and (boundp 'cperl-no-flymake) cperl-no-flymake)
            ;; if the buffer is read-only, don't turn it on
               buffer-read-only))
      (flymake-mode)))

(defun delete-trailing-whitespace-nothere ()
  "Delete trailing whitespace, except on the current line if it is all whitespace."
  (interactive)
  (let (current-whitespace)
    (when (save-excursion
            (beginning-of-line)
            (looking-at "\\([[:space:]]+\\)$"))
      (setq current-whitespace (match-string 0)))
    (delete-trailing-whitespace)
    (save-excursion
      (beginning-of-line)
      (when current-whitespace
        (insert current-whitespace)))
    (when current-whitespace
      (end-of-line))))

;;; hooks
(add-hook 'before-save-hook 'delete-trailing-whitespace-nothere)
(add-hook 'text-mode-hook 'text-hooks)
(add-hook 'tex-mode-hook (lambda () (setq ispell-parser 'tex)))
(add-hook 'c-mode-common-hook (lambda () (local-set-key '"\C-c\C-f" 'compile)))
(add-hook 'c-mode-common-hook (lambda () (local-set-key '"\C-c\C-l" 'goto-line)))
(add-hook 'cperl-mode-hook 'cperl-hooks)
;(add-hook 'cperl-mode-hook 'maybe-flymake-mode)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'org-mode-hook
          (lambda nil
            ;; the kbd is irritatingly made-of-fail
            (loop for i in (list (kbd "S-<left>")
                                 (kbd "S-<right>")
                                 (kbd "S-<up>")
                                 (kbd "S-<down>"))
                  do (local-unset-key i))))
(add-hook 'w3m-mode-hook
          (lambda nil
            (loop for old in '("S-<left>" "S-<right>")
                  do (local-unset-key (macroexpand `(kbd ,old))))
            ;; yes, the reverse order is intentional
            (local-set-key (kbd "C-<right>") #'w3m-shift-left)
            (local-set-key (kbd "C-<left>") #'w3m-shift-right)))

(add-hook 'message-setup-hook
          (lambda () (local-set-key "\C-cw" 'message-widen-reply)))
(add-hook 'rcirc-mode-hook (lambda () (flyspell-mode 1)))
(add-hook 'rcirc-mode-hook (lambda () (rcirc-omit-mode)))
(add-hook 'rcirc-mode-hook
             (lambda ()
               (when (or (= (aref (buffer-name) 0) ?#)
                         (= (aref (buffer-name) 0) ?&))
                 (setq rcirc-ignore-buffer-activity-flag t))))
(add-hook 'help-mode-hook (lambda () (local-set-key "l" #'help-go-back)))

;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

;;; auto-modes
(setq auto-mode-alist
      (append
       '(("\\.css$" . css-mode)
         ("\\.js$" . espresso-mode)
         ("\\.ya?ml$" . yaml-mode)
         ("^mutt-" . mail-mode)
         ("\\.html$" . html-mode)
         ("configure.in" . m4-mode)
         ("\\.t$" . cperl-mode)
         ("\\.tt2?$" . html-mode)
         ("\\.tmpl$" . html-mode)
         ("\\.pir$" . pir-mode)
         ("\\.[hg]s$"  . haskell-mode)
         ("\\.hi$"     . haskell-mode)
         ("\\.elt$"    . emacs-lisp-mode)
         ("\\.ml[iyl]?$" . caml-mode)
         ("\\.l[hg]s$" . literate-haskell-mode))
       auto-mode-alist))

(add-to-list 'auto-mode-alist
             (cons (concat "\\." (regexp-opt '("xml" "xsd" "sch" "rng"
                                               "xslt" "svg" "rss") t) "\\'")
                   'nxml-mode))

;;; enable/disable
(put 'downcase-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'upcase-region 'disabled nil)

;;; functions for keybindings
(defun kill-current-buffer (prefix)
  (interactive "P")
  (if prefix (call-interactively 'kill-buffer)
    (kill-buffer nil)))

;;; advice
(require 'flymake)
(defadvice flymake-perl-init (after fix-flymake-perl-path)
  (let* ((include
          (condition-case nil
              (list (concat
                     "-I" (car (perl-project-includes)))
                    (concat
                     "-I" (expand-file-name
                           (concat (look-for-Makefile.PL) "/.."))))
                    (error nil)))
         (perl "/home/jon/perl/install/bin/perl")
         (file (cadadr ad-return-value))
         (args (if include (append include (list "-c" file))
                 (list "-c" file))))
    (setq ad-return-value (list perl args))))
(ad-activate 'flymake-perl-init)

;; We need C-x C-c bound to s-b-k-t for emacsclient -t sessions, but when
;; it kills my main X session (with 9 windows or whatever), it is really
;; annoying.
(defadvice save-buffers-kill-terminal (around dont-kill-my-x-session-kthx)
  "Disable C-x C-c under X."
  (if (eq window-system 'x)
      (message "I'm afraid I can't do that, Dave.")
    ad-do-it))
(ad-activate 'save-buffers-kill-terminal)

(defadvice iswitchb-kill-buffer (after rescan-after-kill activate)
  "*Regenerate the list of matching buffer names after a kill.
    Necessary if using `uniquify' with `uniquify-after-kill-buffer-p'
    set to non-nil."
      (setq iswitchb-buflist iswitchb-matches)
      (iswitchb-rescan))

(defun iconify-or-deiconify-frame ()
  "Don't iconify, since that makes emacs freeze under xmonad"
  (interactive)
  (make-frame-visible))

(defun iswitchb-rescan ()
  "*Regenerate the list of matching buffer names."
  (interactive)
  (iswitchb-make-buflist iswitchb-default)
  (setq iswitchb-rescan t))
(ad-activate 'iswitchb-kill-buffer)

(defun log-edit-hide-buf (&optional buf where)
  (when (setq buf (get-buffer (or buf log-edit-files-buf)))
    (let ((win (get-buffer-window buf where)))
      (bury-buffer buf))))

;;; override stupid defaults
(defalias 'yes-or-no-p 'y-or-n-p) ; typing yes or no is annoying? (y or n)
(defun message-box (text) (message "%s" text))

;;; key-bindings
;; unset
(require 'flyspell)
(define-key flyspell-mode-map (kbd "C-;") nil) ; HATE.

;; set
(define-key read-expression-map (kbd "TAB") #'lisp-complete-symbol)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key "\C-xg" 'rgrep)
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-r" 'revert-buffer)
(global-set-key (kbd "C-x C-g") 'abort-recursive-edit)
(global-set-key (kbd "C-x k") 'kill-current-buffer)
(global-set-key (kbd "C-h l") (lambda nil (interactive) (info "Elisp")))
(global-set-key (kbd "C-h o") 'find-library)
(global-set-key (kbd "C-x t") (lambda nil (interactive) (ansi-term "/bin/bash")))
(global-set-key (kbd "M-?") 'hippie-expand)
(global-set-key (kbd "s-u") (lambda nil (interactive) (other-window -1)))
(global-set-key (kbd "s-U") (lambda nil (interactive) (other-window -1) (delete-window)))
(global-set-key (kbd "s-i") 'other-window)
(global-set-key (kbd "M-r") 'comment-region)
(global-set-key (kbd "C-M-r") 'uncomment-region)
(global-set-key (kbd "M-i") 'indent-region)
(global-set-key (kbd "M-;") 'replace-string)
(global-set-key (kbd "C-M-;") 'replace-regexp)
(global-set-key (kbd "C-;") 'align-regexp)
(global-set-key (kbd "M-g s") 'magit-status)

(global-set-key (kbd "<mouse-7>") 'other-window)
(global-set-key (kbd "<mouse-6>") (lambda nil (interactive) (other-window -1)))

;; eproject global bindings
(defmacro .emacs-curry (function &rest args)
  `(lambda () (interactive)
     (,function ,@args)))

(defmacro .emacs-eproject-key (key command)
  (cons 'progn
        (loop for (k . p) in (list (cons key 4) (cons (upcase key) 1))
              collect
              `(global-set-key
                (kbd ,(format "C-x p %s" k))
                (.emacs-curry ,command ,p)))))

(.emacs-eproject-key "k" eproject-kill-project-buffers)
(.emacs-eproject-key "v" eproject-revisit-project)
(.emacs-eproject-key "b" eproject-ibuffer)
(.emacs-eproject-key "o" eproject-open-all-project-files)

;; use C-h c for customize
(global-unset-key (kbd "C-h c"))
(global-set-key (kbd "C-h c a") #'customize-apropos)
(global-set-key (kbd "C-h c v") #'customize-variable)
(global-set-key (kbd "C-h c f") #'customize-face)
(global-set-key (kbd "C-h c g") #'customize-group)

;;; setqs
(setq frame-title-format "emacs")
(setq twittering-username "jrockway")
(set-language-environment "UTF-8")
(setq slime-net-coding-system 'utf-8-unix)

;;; utils

(defun tt-tags nil
  "Insert TT tags."
  (interactive)
  (insert "[%  %]")
  (backward-char 3))

(defun fix-colors nil
  "Fix colors when connecting via emacsclient."
  (interactive)
  (set-background-color "black")
  (set-foreground-color "gray90"))

(defun irc-start nil
  "Connect to all my networks."
  (interactive)
  (let ((password (password-read "IRC password: "))
        (host "localhost"))
    (loop for port from 6667 to 6671 do
          (rcirc-connect host port "jrockway" "jrockway"
                         "Jonathan Rockway" nil password))))

;;; custom-set
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(Man-notify-method (quote pushy))
 '(appt-message-warning-time 80)
 '(auto-insert-mode t)
 '(auto-insert-query nil)
 '(blink-matching-paren nil)
 '(blink-matching-paren-on-screen nil)
 '(browse-url-browser-function (quote my-w3m-browse-url))
 '(browse-url-generic-program "conkeror")
 '(browse-url-new-window-flag t)
 '(bubbles-game-theme (quote difficult))
 '(bubbles-grid-size (quote (20 . 15)))
 '(c-cleanup-list (quote (empty-defun-braces defun-close-semi list-close-comma scope-operator compact-empty-funcall)))
 '(c-default-style (quote ((java-mode . "java") (other . "gnu"))))
 '(c-electric-pound-behavior (quote (alignleft)))
 '(c-macro-cppflags "-I/usr/include -I/usr/local/include -I/usr/include/g++-3")
 '(case-fold-search t)
 '(compilation-ask-about-save nil)
 '(compilation-read-command nil)
 '(compilation-scroll-output t)
 '(compilation-window-height 10)
 '(compile-auto-highlight 10)
 '(compile-command "make")
 '(completion-ignored-extensions (quote (".o" "~" ".bin" ".lbin" ".so" ".a" ".ln" ".blg" ".bbl" ".elc" ".lof" ".glo" ".idx" ".lot" ".svn/" ".hg/" ".git/" ".bzr/" "CVS/" "_darcs/" "_MTN/" ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".fasl" ".ufsl" ".fsl" ".dxl" ".pfsl" ".dfsl" ".p64fsl" ".d64fsl" ".dx64fsl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo" "inc/" "blib/")))
 '(confirm-nonexistent-file-or-buffer nil)
 '(cperl-auto-newline nil)
 '(cperl-close-paren-offset -4)
 '(cperl-continuted-statement-offset 0)
 '(cperl-electric-backspace-untabify nil)
 '(cperl-electric-keywords nil)
 '(cperl-highlight-variables-indiscriminately nil)
 '(cperl-indent-level 4)
 '(cperl-indent-parens-as-block t)
 '(cperl-indent-subs-specially nil)
 '(cperl-invalid-face (quote default))
 '(cperl-lazy-help-time 0)
 '(cperl-merge-trailing-else nil)
 '(cperl-tab-always-indent t)
 '(cperl-under-as-char nil)
 '(current-language-environment "UTF-8")
 '(custom-buffer-done-kill t)
 '(custom-magic-show-button t)
 '(default-input-method "japanese")
 '(display-hourglass nil)
 '(display-time-mode nil)
 '(ecb-options-version "2.32")
 '(ecb-source-path (quote ("~/projects")))
 '(ecomplete-database-file-coding-system (quote utf-8-emacs))
 '(eldoc-minor-mode-string nil)
 '(emacs-lisp-mode-hook (quote (turn-on-eldoc-mode checkdoc-minor-mode (lambda nil (font-lock-add-keywords (quote emacs-lisp-mode) slime-additional-font-lock-keywords)) (lambda nil (local-set-key "" (quote macroexpand-last-sexp))))))
 '(eproject-completing-read-function (quote eproject--icompleting-read))
 '(eshell-after-prompt-hook nil)
 '(eshell-modules-list (quote (eshell-alias eshell-banner eshell-basic eshell-cmpl eshell-dirs eshell-glob eshell-hist eshell-ls eshell-pred eshell-prompt eshell-rebind eshell-script eshell-term eshell-unix)))
 '(eshell-prompt-function (lambda nil (format "
%s
%s" (eshell/pwd) (if (= (user-uid) 0) " # " " $ "))))
 '(eudc-protocol (quote ldap))
 '(eudc-server "ldap.uchicago.edu")
 '(flymake-allowed-file-name-masks (quote (("\\.c\\'" flymake-simple-make-init) ("\\.cpp\\'" flymake-simple-make-init) ("\\.xml\\'" flymake-xml-init) ("\\.html?\\'" flymake-xml-init) ("\\.cs\\'" flymake-simple-make-init) ("\\.p[lm]\\'" flymake-perl-init) ("\\.t\\'" flymake-perl-init) ("\\.h\\'" flymake-master-make-header-init flymake-master-cleanup) ("\\.java\\'" flymake-simple-make-java-init flymake-simple-java-cleanup) ("[0-9]+\\.tex\\'" flymake-master-tex-init flymake-master-cleanup) ("\\.tex\\'" flymake-simple-tex-init) ("\\.idl\\'" flymake-simple-make-init))))
 '(flyspell-issue-message-flag nil)
 '(flyspell-issue-welcome-flag nil)
 '(flyspell-mark-duplications-flag nil)
 '(flyspell-mode-line-string " Spell")
 '(font-lock-global-modes t)
 '(global-font-lock-mode t nil (font-lock))
 '(gnus-always-force-window-configuration nil)
 '(gnus-asynchronous t)
 '(gnus-fetch-old-headers nil)
 '(gnus-gcc-mark-as-read t)
 '(gnus-ignored-from-addresses "\\\\(?:jon@bar\\.jrock\\.us\\\\|jon@jrock\\.us\\\\)")
 '(gnus-local-domain "jrock.us")
 '(gnus-message-archive-group "Sent")
 '(gnus-message-replyencrypt t)
 '(gnus-message-replysign nil)
 '(gnus-novice-user nil)
 '(gnus-posting-styles (quote (((header "To" "iinteractive.com") (signature nil) (address "jonathan.rockway@iinteractive.com")))))
 '(gnus-secondary-select-methods (quote ((nnimap "localhost" (username jon)))))
 '(gnus-secondary-servers nil)
 '(gnus-select-method (quote (nnnil "")))
 '(gnus-simplify-subject-functions (quote (gnus-simplify-subject-re gnus-simplify-whitespace gnus-simplify-subject-fuzzy)))
 '(gnus-sum-thread-tree-false-root " * ")
 '(gnus-sum-thread-tree-indent " ")
 '(gnus-sum-thread-tree-leaf-with-other "├>")
 '(gnus-sum-thread-tree-single-leaf "└> ")
 '(gnus-sum-thread-tree-vertical "│")
 '(gnus-summary-line-format "%U %R %4L: %(%[ %-20,20f %]%) %B %s
")
 '(gnus-summary-mode-hook (quote (gnus-agent-mode (lambda nil (local-set-key (kbd "D") (quote gnus-summary-delete-article))))))
 '(gnus-thread-indent-level 2)
 '(gnus-update-message-archive-method t)
 '(gnus-use-full-window nil)
 '(grep-tree-command "find <D> -path '*/.svn' -prune -o <X> -type f <F> -print0 | xargs -0 -e egrep <C> -nH -e  '<R>'")
 '(gud-tooltip-echo-area t)
 '(haskell-font-lock-symbols nil)
 '(haskell-ghci-program-name "/home/jon/utils/sane-ghci")
 '(haskell-literate-default (quote latex))
 '(haskell-program-name "/home/jon/utils/sane-ghci")
 '(ibuffer-expert t)
 '(ibuffer-fontification-alist (quote ((10 buffer-read-only font-lock-constant-face) (15 (and buffer-file-name (string-match ibuffer-compressed-file-name-regexp buffer-file-name)) font-lock-doc-face) (20 (string-match "^*" (buffer-name)) font-lock-keyword-face) (25 (and (string-match "^ " (buffer-name)) (null buffer-file-name)) italic) (30 (memq major-mode ibuffer-help-buffer-modes) font-lock-comment-face) (35 (eq major-mode (quote dired-mode)) font-lock-function-name-face) (1 (eq major-mode (quote cperl-mode)) cperl-hash-face) (1 (eq major-mode (quote rcirc-mode)) rcirc-server))))
 '(ibuffer-formats (quote ((mark modified read-only " " (name 18 18 :left :elide) " " (size 9 -1 :right) " " (mode 16 16 :left :elide) " " (eproject 16 16 :left :elide) " " filename-and-process) (mark " " (name 16 -1) " " filename))))
 '(ielm-mode-hook (quote (turn-on-eldoc-mode)) t)
 '(indent-tabs-mode nil)
 '(indicate-buffer-boundaries (quote left))
 '(indicate-empty-lines nil)
 '(inferior-lisp-program "/usr/bin/sbcl")
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(iswitchb-buffer-ignore (quote ("^ ")))
 '(jde-compiler (quote ("javac" "")))
 '(jde-gen-conditional-padding-1 " ")
 '(jde-gen-conditional-padding-3 "")
 '(jde-gen-method-signature-padding-3 "")
 '(jde-help-docsets (quote (("JDK API" "/usr/local/java/docs/api" nil))))
 '(js2-auto-indent-flag nil)
 '(js2-basic-offset 4)
 '(js2-electric-keys nil)
 '(js2-enter-indents-newline nil)
 '(js2-highlight-level 3)
 '(js2-mirror-mode nil)
 '(js2-rebind-eol-bol-keys nil)
 '(js2-use-font-lock-faces t)
 '(kill-read-only-ok t)
 '(line-move-visual nil)
 '(lisp-interaction-mode-hook (quote (turn-on-eldoc-mode)))
 '(lisp-mode-hook (quote (slime-lisp-mode-hook)))
 '(mail-source-delete-incoming t)
 '(mail-sources (quote ((maildir :path "/home/jon/.nnmaildir") (file :path "/var/spool/mail/jon"))))
 '(mail-user-agent (quote gnus-user-agent))
 '(make-backup-files nil)
 '(max-lisp-eval-depth 65536)
 '(menu-bar-mode nil nil (menu-bar))
 '(message-citation-line-format "* On %a, %b %d %Y, %N wrote:")
 '(message-citation-line-function (quote message-insert-formatted-citation-line))
 '(message-dont-reply-to-names (quote ("jon@jrock.us" "jonathan.rockway@iinteractive.com")))
 '(message-kill-buffer-on-exit t)
 '(message-mail-alias-type (quote ecomplete))
 '(mew-imap-delete nil)
 '(mew-imap-ssl nil)
 '(mew-mail-domain "jrock.us")
 '(mm-verify-option (quote known))
 '(mmm-submode-decoration-level 1)
 '(mouse-avoidance-mode nil nil (avoid))
 '(mumamo-set-major-mode-delay -1)
 '(nnimap-debug nil)
 '(nxhtml-skip-welcome t)
 '(occur-mode-hook (quote (turn-on-font-lock next-error-follow-minor-mode)))
 '(pgg-default-user-id "5BF3666D")
 '(pgg-gpg-use-agent t)
 '(pop-up-windows t)
 '(rcirc-bright-nicks (quote ("schmeidi" "nothingmuch")))
 '(rcirc-buffer-maximum-lines 10000)
 '(rcirc-default-nick "jrockway")
 '(rcirc-default-server "irc.perl.org")
 '(rcirc-default-user-name "jrockway")
 '(rcirc-fill-prefix nil)
 '(rcirc-keywords (quote ("jrock" "rockway")))
 '(rcirc-prompt "[%t] ")
 '(rcirc-server-alist nil)
 '(rcirc-track-minor-mode t)
 '(safe-local-variable-values (quote ((Syntax . ANSI-Common-Lisp) (Base . 10) (flymake-mode . 0))))
 '(same-window-regexps (quote ("[*]eshell" "[*]ielm" "[*]Customize")))
 '(save-place t nil (saveplace))
 '(scheme-program-name "guile")
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(show-trailing-whitespace nil)
 '(slime-enable-evaluate-in-emacs t)
 '(slime-kill-without-query-p t)
 '(slime-startup-animation nil)
 '(special-display-function (lambda (buffer &rest list) (let ((win (split-window))) (set-window-buffer win buffer) win)))
 '(special-display-regexps (quote ("[*]Info" "[*]\\(wide \\)reply")))
 '(sql-electric-stuff (quote semicolon))
 '(sql-sqlite-program "sqlite3")
 '(term-scroll-to-bottom-on-output t)
 '(tex-default-mode (quote latex-mode))
 '(tex-dvi-view-command "xdvi")
 '(tex-shell-window-height 10)
 '(tex-show-queue-command " lpq")
 '(tool-bar-mode nil nil (tool-bar))
 '(tooltip-delay 0.1)
 '(tooltip-mode t)
 '(tooltip-use-echo-area t)
 '(transient-mark-mode nil)
 '(truncate-partial-width-windows nil)
 '(uce-mail-reader (quote gnus))
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(user-mail-address "jon@jrock.us")
 '(vc-handled-backends (quote (SVK RCS CVS SVN SCCS Arch MCVS GIT)))
 '(w3-use-unicode-table-characters t)
 '(w3-user-colors-take-precedence t)
 '(w3-user-fonts-take-precedence t)
 '(w3m-accept-languages (quote ("en")))
 '(w3m-coding-system (quote utf-8))
 '(w3m-default-coding-system (quote utf-8))
 '(w3m-default-display-inline-images t)
 '(w3m-file-coding-system (quote utf-8))
 '(w3m-file-name-coding-system (quote utf-8))
 '(w3m-key-binding (quote info))
 '(w3m-make-new-session t)
 '(w3m-new-session-in-background t)
 '(w3m-new-session-url "about:")
 '(w3m-pop-up-windows nil)
 '(w3m-terminal-coding-system (quote utf-8))
 '(w3m-track-mouse nil)
 '(w3m-use-cookies t)
 '(w3m-view-this-url-new-session-in-background t)
 '(wget-download-directory "~/tmp")
 '(woman-cache-filename "/home/jon/.wmncach.el")
 '(woman-use-own-frame nil))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:background "black" :foreground "gray90" :height 95 :family "DejaVu Sans Mono"))))
 '(cperl-array ((((class color) (background dark)) (:background "navy" :foreground "yellow"))))
 '(cperl-hash ((((class color) (background dark)) (:background "navy" :foreground "Red"))))
 '(cperl-hash-face ((((class color) (background dark)) (:background "navy" :foreground "Red" :slant normal :weight bold))))
 '(cursor ((t (:background "turquoise" :inverse-video t))))
 '(diff-added ((t (:inherit diff-changed :foreground "green"))))
 '(diff-hunk-header ((t (:inherit diff-header :foreground "turquoise"))))
 '(diff-removed ((t (:inherit diff-changed :foreground "red"))))
 '(eshell-prompt ((t (:foreground "green" :weight bold))))
 '(eslide-slideshow-normal-text ((t (:height 1000 :family "Computer Modern"))))
 '(flymake-errline ((t (:underline "red" :weight normal))))
 '(flymake-warnline ((((class color)) (:box (:line-width 2 :color "yellow" :style released-button)))))
 '(flyspell-duplicate ((t (:inherit default :underline "Gold3" :weight normal))))
 '(flyspell-incorrect ((t (:inherit default :underline "OrangeRed" :weight normal))))
 '(font-lock-doc-face ((t (:inherit font-lock-string-face :foreground "#ffbbff"))))
 '(font-lock-regexp-grouping-backslash ((t (:inherit bold :background "grey10"))))
 '(font-lock-regexp-grouping-construct ((t (:inherit font-lock-regexp-grouping-backslash))))
 '(font-lock-warning-face ((((class color) (min-colors 88) (background dark)) (:foreground "Pink"))))
 '(gnus-group-mail-3 ((t (:foreground "aquamarine1" :weight bold))))
 '(js2-error-face ((((class color) (background dark)) (:underline "red"))))
 '(js2-function-param-face ((t (:inherit font-lock-type-face))))
 '(magit-diff-add ((((class color) (background dark)) (:foreground "Green"))))
 '(magit-item-highlight ((((class color) (background dark)) (:background "gray15"))))
 '(message-header-subject ((t (:foreground "#5555ff" :weight bold))))
 '(message-header-to ((t (:foreground "#3333ff" :weight bold))))
 '(message-separator ((t (:background "black" :foreground "LightSkyBlue1" :inverse-video t :weight bold))))
 '(mmm-default-submode-face ((t (:background "black"))))
 '(mode-line ((t (:background "grey20" :foreground "white" :box (:line-width 1 :color "grey30")))))
 '(mode-line-buffer-id ((t (:foreground "green"))))
 '(mode-line-highlight ((((class color) (min-colors 88)) (:box (:line-width 1 :color "grey40")))))
 '(mode-line-inactive ((default (:inherit mode-line :background "black" :foreground "grey80" :box (:line-width 1 :color "grey20"))) (nil nil)))
 '(pod-mode-code-face ((t (:box (:line-width 1 :color "turquoise")))) t)
 '(rcirc-keyword ((t (:inherit nil :foreground "green" :weight bold))))
 '(rcirc-my-nick ((((class color) (min-colors 88) (background dark)) (:foreground "red"))))
 '(rcirc-other-nick ((((class color) (min-colors 88) (background dark)) (:foreground "grey" :weight bold))))
 '(rcirc-prompt ((((min-colors 88) (background dark)) (:foreground "#99ff99" :weight bold))))
 '(rcirc-server ((((class color) (min-colors 88) (background dark)) (:foreground "slateblue" :weight bold))))
 '(rcirc-track-keyword ((t (:foreground "green" :weight bold))))
 '(rcirc-track-nick ((t (:inherit font-lock-keyword-face :inverse-video nil :weight bold))))
 '(shadow ((((class color grayscale) (min-colors 88) (background dark)) (:foreground "grey40"))))
 '(sldb-restartable-frame-line-face ((t (:foreground "turquoise"))))
 '(slime-note-face ((((class color) (background dark)) (:underline "green"))))
 '(slime-repl-input-face ((t (:inherit default :underline t :weight normal))))
 '(slime-repl-inputed-output-face ((((class color) (background dark)) (:foreground "green"))))
 '(slime-repl-output-face ((t (:inherit nil))))
 '(slime-repl-result-face ((t (:inherit font-lock-type-face))))
 '(stylish-repl-error-face ((t (:inherit font-lock-warning-face :weight normal))))
 '(tooltip ((((class color)) (:inherit default :background "lightyellow" :foreground "black"))))
 '(window-number-face ((nil (:foreground "red")))))

(put 'narrow-to-region 'disabled nil)

(put 'save-buffers-kill-terminal 'disabled nil)

(put 'dired-find-alternate-file 'disabled nil)
