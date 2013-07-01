(require 'cl)

;;; load extra modes
(add-to-list 'load-path "~/elisp")
(add-to-list 'load-path "~/elisp/_local")
(add-to-list 'load-path "~/elisp/cperl-mode")
(add-to-list 'load-path "~/elisp/eproject")
(add-to-list 'load-path "~/elisp/eproject/contrib")
(add-to-list 'load-path "~/elisp/eproject/lang")
(add-to-list 'load-path "~/elisp/eslide")
(add-to-list 'load-path "~/elisp/ibuffer-git")
(add-to-list 'load-path "~/elisp/haskell-mode/")
(add-to-list 'load-path "~/elisp/magit")
(add-to-list 'load-path "~/elisp/ocaml")
(add-to-list 'load-path "~/elisp/scala-mode")
(add-to-list 'load-path "~/elisp/slime/")
(add-to-list 'load-path "~/elisp/slime/contrib")

(require 'auto-inserts)
(require 'cperl-mode)
(require 'cperl-extras)
(require 'cperl-hippie)
(require 'css-mode)
(require 'editing-extras)
(require 'elisp-extras)
(require 'eproject)
(require 'eproject-extras)
(require 'eproject-compile)
(require 'eproject-tags)
(require 'eshell)
(require 'eshell-extras)
(require 'eslide)
(require 'espresso)
(require 'git)
(require 'gnus)
(require 'help-mode)
(require 'haskell-extras)
(require 'haskell-mode)
(require 'ibuffer-git)
(require 'inf-haskell)
(require 'iswitchb-extras)
(require 'lisp-extras)
(require 'magit)
(require 'message)
(require 'rcirc)
(require 'rcirc-extras)
(require 'rcirc-xmonad-notify)
(require 'slime-load)
(require 'term-extras)
(require 'text-extras)
(require 'uniquify)
(require 'window-number)
(require 'windowing-extras)

;;; google
(when (file-exists-p "/home/build/public/eng/elisp/google.el")
  (load-file "/home/build/public/eng/elisp/google.el")
  (require 'google3)
  (require 'google3-build)
  (require 'csearch))

;;; modes i want on by default
(iswitchb-mode 1)
(winner-mode 1)
(window-number-mode 1)
(defalias 'perl-mode 'cperl-mode)

;;; auto-mode-alist
(setq auto-mode-alist (append auto-mode-alist
                              '(("\\.t$" . cperl-mode)
                                ("\\.hs$" . haskell-mode))))

;;; hooks
(add-hook 'before-save-hook 'delete-trailing-whitespace-nothere)

(add-hook 'ibuffer-hook (lambda ()
                          (ibuffer-filter-by-predicate
                           '(< 0 (length (buffer-file-name))))
                          (ibuffer-filter-by-predicate
                           '(not buffer-read-only))))

(defun setup-java-style ()
  (c-set-offset 'arglist-intro '+)
  (c-set-offset 'arglist-cont-nonempty '+))

(when (not (featurep 'google))
  (add-hook 'java-mode-hook #'setup-java-style))

;;; this was lurking in auto-inserts.  what?
(defadvice after-find-file (before ad-mkdir-after-find-file activate)
  "Make the directory containing the visited file."
  (make-directory (file-name-directory (buffer-file-name)) t))

;;; enable/disable
(put 'downcase-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'save-buffers-kill-terminal 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;; We need C-x C-c bound to s-b-k-t for emacsclient -t sessions, but when
;; it kills my main X session (with 9 windows or whatever), it is really
;; annoying.
(defadvice save-buffers-kill-terminal (around dont-kill-my-x-session-kthx)
  "Disable C-x C-c under X."
  (if (or (eq window-system 'x) (eq window-system 'w32))
      (message "I'm afraid I can't do that, Dave.")
    ad-do-it))
(ad-activate 'save-buffers-kill-terminal)

(defun iconify-or-deiconify-frame ()
  "Don't iconify, since that makes emacs freeze under xmonad."
  (interactive)
  (make-frame-visible))

(defun fix-font (size)
  "Interactively set the default font to DejaVu Sans Mono- SIZE.

My default seems to be ignored a good percentage of the time,
which means I type set-default-font DejaVu C-q SPC Sans C-q SPC
Mono- every time I create a new frame.  This annoys me, so we
have this now."
  (interactive
   (list (read-number "Size: " 10)))
  (set-default-font (format "DejaVu Sans Mono-%f" size)))

(defun log-edit-hide-buf (&optional buf where)
  (when (setq buf (get-buffer (or buf log-edit-files-buf)))
    (let ((win (get-buffer-window buf where)))
      (bury-buffer buf))))

(defun yank-primary ()
  "Do what middle-mouse would."
  (interactive)
  (when (eq window-system 'x)
    (let ((selection (or (x-get-selection 'PRIMARY)
                         (x-get-selection-value))))
      (when selection (insert selection)))))

;;; override stupid defaults
(defalias 'yes-or-no-p 'y-or-n-p)

;;; key-bindings
;; functions
(defun kill-current-buffer (prefix)
  (interactive "P")
  (if prefix (call-interactively 'kill-buffer)
    (kill-buffer nil)))

;; unset
(require 'flyspell)
(define-key flyspell-mode-map (kbd "C-;") nil) ; HATE.

;; set
(setq-default cc-electric-flag nil)

(define-key text-mode-map "\C-cu" 'insert-same-number-of-chars-as-line-above)
(define-key text-mode-map "\C-ccw" 'ispell-complete-word)
(define-key message-mode-map "\C-cw" 'message-widen-reply)
(define-key help-mode-map "l" 'help-go-back)
(define-key read-expression-map (kbd "TAB") #'lisp-complete-symbol)
(global-set-key (kbd "S-<insert>") #'yank-primary)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key "\C-xg" 'rgrep)
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-r" 'revert-buffer)
(global-set-key (kbd "C-x C-g") 'abort-recursive-edit)
(global-set-key (kbd "C-x k") 'kill-current-buffer)
(global-set-key (kbd "C-h l") (lambda () (interactive) (info "Elisp")))
(global-set-key (kbd "C-h o") 'find-library)
(global-set-key (kbd "C-x t") (lambda () (interactive) (ansi-term "/bin/bash")))
(global-set-key (kbd "s-(") (lambda () (interactive) (other-window -1)))
(global-set-key (kbd "s-)") (lambda () (interactive) (other-window 1)))
(global-set-key (kbd "C-(") (lambda () (interactive) (other-window -1)))
(global-set-key (kbd "C-)") (lambda () (interactive) (other-window 1)))
(global-set-key (kbd "s-U") (lambda () (interactive) (other-window -1) (delete-window)))
(global-set-key (kbd "s-i") 'other-window)
(global-set-key (kbd "M-r") 'comment-region)
(global-set-key (kbd "C-M-r") 'uncomment-region)
(global-set-key (kbd "M-i") 'indent-region)
(global-set-key (kbd "M-;") 'replace-string)
(global-set-key (kbd "C-M-;") 'replace-regexp)
(global-set-key (kbd "C-;") 'align-regexp)
(global-set-key (kbd "M-g s") 'magit-status)
(global-set-key (kbd "C-c C-k") 'compile)
(define-key c-mode-map (kbd "C-c C-l") 'compile)

(define-key eproject-mode-map (kbd "C-c x") 'eproject-eshell-cd-here)
(define-key gnus-summary-mode-map (kbd "D") 'gnus-summary-delete-article)
(define-key gnus-summary-mode-map (kbd "S") 'gnus-summary-mark-as-spam)

;; use hippie instead of dabbrev
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "M-?") 'dabbrev-expand)

;; use C-h c for customize
(global-unset-key (kbd "C-h c"))
(global-set-key (kbd "C-h c a") #'customize-apropos)
(global-set-key (kbd "C-h c v") #'customize-variable)
(global-set-key (kbd "C-h c f") #'customize-face)
(global-set-key (kbd "C-h c g") #'customize-group)

;;; random functions

(defun xml-unescape ()
  (interactive)
  (ignore-errors (replace-string "\\x0a" "\n" nil (point-min) (point-max)))
  (ignore-errors (replace-string "\\\"" "\"" nil (point-min) (point-max)))
  (ignore-errors (replace-string "&#xa;" "\n" nil (point-min) (point-max)))
  (ignore-errors (replace-string "&lt;" "<" nil (point-min) (point-max)))
  (ignore-errors (replace-string "&gt;" ">" nil (point-min) (point-max)))
  (ignore-errors (replace-string "&quot;" "\"" nil (point-min) (point-max)))
  (ignore-errors (replace-string "&apos;" "'" nil (point-min) (point-max)))
  (ignore-errors (replace-string "&amp;" "&" nil (point-min) (point-max))))

;;; custom-set
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Man-notify-method (quote pushy))
 '(auto-insert-mode t)
 '(auto-insert-query nil)
 '(browse-url-browser-function (quote browse-url-generic))
 '(browse-url-generic-program "google-chrome")
 '(c-electric-pound-behavior (quote (alignleft)))
 '(case-fold-search t)
 '(column-number-mode t)
 '(compilation-ask-about-save nil)
 '(compilation-disable-input t)
 '(compilation-message-face (quote bold))
 '(compilation-read-command nil)
 '(compilation-scroll-output t)
 '(compile-auto-highlight 10)
 '(completion-ignored-extensions (quote (".o" "~" ".bin" ".lbin" ".so" ".a" ".ln" ".blg" ".bbl" ".elc" ".lof" ".glo" ".idx" ".lot" ".svn/" ".hg/" ".git/" ".bzr/" "CVS/" "_darcs/" "_MTN/" ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".fasl" ".ufsl" ".fsl" ".dxl" ".pfsl" ".dfsl" ".p64fsl" ".d64fsl" ".dx64fsl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo" "inc/" "blib/" ".hi")))
 '(confirm-nonexistent-file-or-buffer nil)
 '(cperl-auto-newline nil)
 '(cperl-close-paren-offset -4)
 '(cperl-continued-statement-offset 4)
 '(cperl-continuted-statement-offset 0)
 '(cperl-electric-backspace-untabify nil)
 '(cperl-electric-keywords nil)
 '(cperl-highlight-variables-indiscriminately nil)
 '(cperl-indent-level 4)
 '(cperl-indent-parens-as-block t)
 '(cperl-indent-region-fix-constructs nil)
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
 '(eldoc-echo-area-use-multiline-p nil)
 '(eldoc-minor-mode-string nil)
 '(emacs-lisp-mode-hook (quote (turn-on-eldoc-mode)))
 '(eproject-completing-read-function (quote eproject--icompleting-read))
 '(eshell-after-prompt-hook nil)
 '(eshell-prompt-function (lambda nil (format "
%s
%s" (eshell/pwd) (if (= (user-uid) 0) " # " " $ "))))
 '(espresso-auto-indent-flag nil)
 '(fill-column 80)
 '(flowtimer-start-hook (quote (flowtimer-disable-rcirc-tracking)))
 '(flymake-start-syntax-check-on-newline nil)
 '(flyspell-issue-message-flag nil)
 '(flyspell-issue-welcome-flag nil)
 '(flyspell-mark-duplications-flag nil)
 '(flyspell-mode-line-string " Spell")
 '(font-lock-global-modes t)
 '(gnus-always-force-window-configuration nil)
 '(gnus-asynchronous t)
 '(gnus-dribble-directory "~/.gnus.dribble")
 '(gnus-fetch-old-headers nil)
 '(gnus-gcc-mark-as-read t)
 '(gnus-ignored-from-addresses "\\\\(?:jon@snowball2\\.jrock\\.us\\\\|jon@jrock\\.us\\\\)")
 '(gnus-local-domain "jrock.us")
 '(gnus-message-archive-group "Sent")
 '(gnus-message-replyencrypt t)
 '(gnus-message-replysign nil)
 '(gnus-novice-user nil)
 '(gnus-secondary-select-methods (quote ((nnimap "localhost" (username jon)))))
 '(gnus-secondary-servers nil)
 '(gnus-select-method (quote (nnnil "")))
 '(gnus-simplify-subject-functions (quote (gnus-simplify-subject-re gnus-simplify-whitespace gnus-simplify-subject-fuzzy)))
 '(gnus-use-full-window nil)
 '(haskell-font-lock-symbols t)
 '(haskell-indentation-cycle-warn nil)
 '(haskell-literate-default (quote latex))
 '(haskell-mode-hook (quote (turn-on-haskell-indentation turn-on-haskell-doc-mode imenu-add-menubar-index)))
 '(hippie-expand-try-functions-list (quote (try-complete-moose-method try-expand-all-abbrevs try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-list try-expand-line try-expand-dabbrev-from-kill try-complete-lisp-symbol-partially try-complete-lisp-symbol try-complete-file-name-partially try-complete-file-name)))
 '(ibuffer-expert t)
 '(ibuffer-fontification-alist (quote ((10 buffer-read-only font-lock-constant-face) (15 (and buffer-file-name (string-match ibuffer-compressed-file-name-regexp buffer-file-name)) font-lock-doc-face) (20 (string-match "^*" (buffer-name)) font-lock-keyword-face) (25 (and (string-match "^ " (buffer-name)) (null buffer-file-name)) italic) (30 (memq major-mode ibuffer-help-buffer-modes) font-lock-comment-face) (35 (eq major-mode (quote dired-mode)) font-lock-function-name-face) (1 (eq major-mode (quote cperl-mode)) cperl-hash-face) (1 (eq major-mode (quote rcirc-mode)) rcirc-server))))
 '(ibuffer-formats (quote ((mark modified read-only git-status-mini " " (name 18 18 :left :elide) " " (size 9 -1 :right) " " (mode 16 16 :left :elide) " " (eproject 16 16 :left :elide) " " (git-status 8 8 :left) " " filename-and-process) (mark " " (name 16 -1) " " filename))))
 '(ibuffer-git-column-length 8)
 '(ielm-mode-hook (quote (turn-on-eldoc-mode)))
 '(indent-tabs-mode nil)
 '(indicate-buffer-boundaries (quote left))
 '(indicate-empty-lines nil)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(kill-read-only-ok t)
 '(line-move-visual nil)
 '(lisp-interaction-mode-hook (quote (turn-on-eldoc-mode)))
 '(mail-user-agent (quote gnus-user-agent))
 '(make-backup-files nil)
 '(max-lisp-eval-depth 65536)
 '(menu-bar-mode nil nil (menu-bar))
 '(message-citation-line-format "* On %a, %b %d %Y, %N wrote:")
 '(message-citation-line-function (quote message-insert-formatted-citation-line))
 '(message-dont-reply-to-names (quote ("jon@jrock.us")))
 '(message-kill-buffer-on-exit t)
 '(message-mail-alias-type (quote ecomplete))
 '(minibuffer-prompt-properties (quote (read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)))
 '(mouse-avoidance-mode nil nil (avoid))
 '(occur-mode-hook (quote (turn-on-font-lock next-error-follow-minor-mode)))
 '(p4-use-p4config-exclusively t)
 '(pgg-default-user-id "5BF3666D")
 '(pgg-gpg-use-agent t)
 '(rcirc-bright-nicks (quote ("schmeidi" "nothingmuch" "rafl")))
 '(rcirc-buffer-maximum-lines 3000)
 '(rcirc-default-nick "jrockway")
 '(rcirc-default-server "irc.perl.org")
 '(rcirc-default-user-name "jrockway")
 '(rcirc-fill-prefix nil)
 '(rcirc-keywords (quote ("jrock" "rockway")))
 '(rcirc-prompt "[%t] ")
 '(rcirc-server-alist nil)
 '(rcirc-track-minor-mode t)
 '(read-buffer-completion-ignore-case t)
 '(read-file-name-completion-ignore-case t)
 '(save-place t nil (saveplace))
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(sql-sqlite-program "sqlite3")
 '(term-scroll-to-bottom-on-output t)
 '(tex-default-mode (quote latex-mode))
 '(text-mode-hook (quote (turn-on-flyspell turn-on-auto-fill text-mode-hook-identify)))
 '(tool-bar-mode nil nil (tool-bar))
 '(tooltip-use-echo-area t)
 '(transient-mark-mode nil)
 '(truncate-partial-width-windows nil)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(user-mail-address "jon@jrock.us")
 '(vc-follow-symlinks t)
 '(vc-handled-backends (quote (git)))
 '(woman-use-own-frame nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background "black" :foreground "gray90" :height 110 :family "DejaVu Sans Mono"))))
 '(c-annotation-face ((t (:foreground "DodgerBlue"))))
 '(compilation-error ((t (:inherit font-lock-warning-face))))
 '(compilation-info ((((class color) (min-colors 88) (background dark)) (:foreground "Green1"))))
 '(compilation-warning ((((class color) (min-colors 16)) (:foreground "Orange"))))
 '(completions-common-part ((t (:inherit default :foreground "grey40"))))
 '(completions-first-difference ((t (:inherit bold :underline "green"))))
 '(cperl-array ((((class color) (background dark)) (:background "navy" :foreground "yellow"))))
 '(cperl-hash ((((class color) (background dark)) (:background "navy" :foreground "Red"))))
 '(cperl-hash-face ((((class color) (background dark)) (:background "navy" :foreground "Red" :slant normal :weight bold))))
 '(cursor ((t (:background "turquoise" :inverse-video t))))
 '(diff-added ((t (:inherit diff-changed :foreground "green"))))
 '(diff-hunk-header ((t (:inherit diff-header :foreground "turquoise"))))
 '(diff-removed ((t (:inherit diff-changed :foreground "red"))))
 '(eshell-prompt ((t (:foreground "green" :weight bold))))
 '(eslide-slideshow-normal-text ((t (:height 1000 :family "Computer Modern"))))
 '(flymake-errline ((t (:background "red4"))))
 '(flymake-warnline ((t (:background "#404000"))))
 '(flyspell-duplicate ((t (:underline "Gold3" :weight normal))))
 '(flyspell-incorrect ((t (:underline "OrangeRed" :weight normal))))
 '(font-lock-doc-face ((t (:inherit font-lock-string-face :foreground "#ffbbff"))))
 '(font-lock-regexp-grouping-backslash ((t (:inherit bold :background "grey10"))))
 '(font-lock-regexp-grouping-construct ((t (:inherit font-lock-regexp-grouping-backslash))))
 '(font-lock-warning-face ((((class color) (min-colors 88) (background dark)) (:foreground "Pink"))))
 '(gnus-group-mail-3 ((t (:foreground "aquamarine1" :weight bold))))
 '(magit-diff-add ((((class color) (background dark)) (:foreground "Green"))))
 '(magit-item-highlight ((((class color) (background dark)) (:background "gray15"))))
 '(message-header-subject ((t (:foreground "#5555ff" :weight bold))))
 '(message-header-to ((t (:foreground "#3333ff" :weight bold))))
 '(message-separator ((t (:background "black" :foreground "LightSkyBlue1" :inverse-video t :weight bold))))
 '(mode-line ((t (:background "grey20" :foreground "white" :box (:line-width 1 :color "grey30")))))
 '(mode-line-buffer-id ((t (:foreground "green"))))
 '(mode-line-highlight ((((class color) (min-colors 88)) (:box (:line-width 1 :color "grey40")))))
 '(mode-line-inactive ((default (:inherit mode-line :background "black" :foreground "grey80" :box (:line-width 1 :color "grey20"))) (nil nil)))
 '(next-error ((t (:inherit region :underline "blue"))))
 '(rcirc-server ((((class color) (min-colors 88) (background dark)) (:foreground "purple"))))
 '(region ((((class color) (min-colors 88) (background dark)) (:background "#033"))))
 '(shadow ((((class color grayscale) (min-colors 88) (background dark)) (:foreground "grey40"))))
 '(sldb-restartable-frame-line-face ((t (:foreground "turquoise"))))
 '(slime-note-face ((((class color) (background dark)) (:underline "green"))))
 '(slime-repl-input-face ((t (:inherit default :underline t :weight normal))))
 '(slime-repl-inputed-output-face ((((class color) (background dark)) (:foreground "green"))))
 '(slime-repl-output-face ((t (:inherit nil))))
 '(slime-repl-result-face ((t (:inherit font-lock-type-face))))
 '(spam ((((class color) (background dark)) (:foreground "red"))))
 '(stylish-repl-error-face ((t (:inherit font-lock-warning-face :weight normal))))
 '(tooltip ((((class color)) (:inherit default :background "lightyellow" :foreground "black"))))
 '(window-number-face ((nil (:foreground "red")))))

(set-face-foreground 'default "grey90")
(set-face-background 'default "black")
