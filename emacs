(require 'cl)

(when (file-exists-p "~/elisp" )
  ;; load extra modes from ~/elisp
  (add-to-list 'load-path "~/elisp")
  (add-to-list 'load-path "~/elisp/_local")
  (add-to-list 'load-path "~/elisp/eproject")
  (add-to-list 'load-path "~/elisp/eproject/contrib")
  (add-to-list 'load-path "~/elisp/eproject/lang")
  (add-to-list 'load-path "~/elisp/eslide")
  (add-to-list 'load-path "~/elisp/ibuffer-git")
  (require 'eproject)
  (require 'eproject-compile)
  (require 'eproject-extras)
  (require 'eshell)
  (require 'eslide)
  (require 'ibuffer-git)
  (require 'editing-extras)
  (require 'text-extras))

;;; modes i want on by default
(ido-mode 1)
(winner-mode 1)

;;; override stupid defaults
(defalias 'yes-or-no-p 'y-or-n-p)

;;; auto-mode-alist
(setq auto-mode-alist (append
                       '(("Dockerfile" . dockerfile-mode)
                         ("\\.t$" . cperl-mode)
                         ("\\.hs$" . haskell-mode)
                         ("\\.html$" . web-mode)
                         ("\\.ino$" . c++-mode))
                       auto-mode-alist))

;;; hooks
(add-hook 'before-save-hook 'delete-trailing-whitespace-nothere)

(add-hook 'ibuffer-hook (lambda ()
                          (ibuffer-filter-by-predicate
                           '(< 0 (length (buffer-file-name))))
                          (ibuffer-filter-by-predicate
                           '(not buffer-read-only))))

(defun setup-c-mode ()
  (add-hook 'before-save-hook #'clang-format-buffer nil t))
(add-hook 'c-mode-common-hook #'setup-c-mode)

(defadvice after-find-file (before ad-mkdir-after-find-file activate)
  "Make the directory containing the visited file."
  (make-directory (file-name-directory (buffer-file-name)) t))

;;; load packages
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; comment/uncomment these two lines to enable/disable melpa and melpa stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; for important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

(use-package lsp
  :config
  (define-key lsp-mode-map (kbd "M-DEL") #'lsp-describe-thing-at-point))
(use-package lsp-ui
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))

(defface my-window-number-face '((t :foreground "red")) "")

(use-package window-number
  :config
  (defun window-number-set-inactive-color () (force-mode-line-update))
  (defun window-number-set-active-color () (force-mode-line-update))
  (defun window-number-string ()
    (propertize (number-to-string (window-number))
                'face 'my-window-number-face))
  (window-number-meta-mode 1))


(defun setup-golang-style ()
  (set-fill-column 100)
  (add-hook 'before-save-hook #'gofmt-before-save nil t))
(use-package go-mode
  :config
  (add-hook 'go-mode-hook #'setup-golang-style)
  (add-hook 'go-mode-hook #'lsp-deferred))

(defun setup-tide-mode ()
  (tide-setup)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (flycheck-mode t)
  (company-mode t))
(use-package tide
  :config
  (add-hook 'typescript-mode-hook #'setup-tide-mode))

(defun setup-vue-mode ()
  (turn-off-flyspell))
(use-package vue-mode
  :config
  (add-hook 'vue-mode-hook #'setup-vue-mode))

(use-package web-mode)
(use-package yaml-mode)

(use-package highlight-indentation
  :config
  (add-hook 'yaml-mode-hook #'highlight-indentation-mode))

(use-package prettier-js
  :config
  (add-hook 'typescript-mode-hook #'prettier-js-mode)
  (add-hook 'web-mode-hook #'prettier-js-mode)
  (add-hook 'yaml-mode-hook #'prettier-js-mode)
  (add-hook 'vue-mode-hook #'prettier-js-mode)
  (add-hook 'markdown-mode-hook #'prettier-js-mode))

(use-package clang-format)

(defun setup-protobuf-mode ()
  (c-add-style "my-style" '((c-basic-offset . 4) (indent-tabs-mode . nil)) t)
  (add-hook 'before-save-hook #'clang-format-buffer nil t))
(use-package protobuf-mode
  :config
  (add-hook 'protobuf-mode-hook #'setup-protobuf-mode))

(use-package yasnippet
  :config
  (setq yas-snippet-dirs '("~/elisp/snippets/"))
  (yas-global-mode t))

;;; enable/disable
(put 'downcase-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'save-buffers-kill-terminal 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;;; per-platform setup
(cond
 ((eq window-system 'w32)
  (add-to-list 'exec-path "C:/Program Files/Git/usr/bin")
  (add-to-list 'exec-path "C:/Users/jon/go/bin")
  (add-to-list 'exec-path "C:/Program Files/nodejs")))

;; ;; We need C-x C-c bound to s-b-k-t for emacsclient -t sessions, but when
;; ;; it kills my main X session (with 9 windows or whatever), it is really
;; ;; annoying.
;; (defadvice save-buffers-kill-terminal (around dont-kill-my-x-session-kthx)
;;   "Disable C-x C-c under X."
;;   (if (or (eq window-system 'x) (eq window-system 'w32))
;;       (message "I'm afraid I can't do that, Dave.")
;;     ad-do-it))
;; (ad-activate 'save-buffers-kill-terminal)


;;; key-bindings
;; functions
(defun kill-current-buffer (prefix)
  (interactive "P")
  (if prefix (call-interactively 'kill-buffer)
    (kill-buffer nil)))

;; unset
(require 'flyspell)
(define-key flyspell-mode-map (kbd "C-;") nil)

;; set
(setq-default cc-electric-flag nil)
(setq-default c-electric-flag nil)

(define-key text-mode-map "\C-cu" 'insert-same-number-of-chars-as-line-above)
(define-key text-mode-map "\C-ccw" 'ispell-complete-word)
(define-key help-mode-map "l" 'help-go-back)
(define-key read-expression-map (kbd "TAB") #'completion-at-point)
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
(global-set-key (kbd "C-c C-k") 'compile)
(global-set-key (kbd "M-=") 'company-complete)
(global-set-key (kbd "<mouse-4>") 'scroll-down-line)
(global-set-key (kbd "<mouse-5>") 'scroll-up-line)

(define-key eproject-mode-map (kbd "C-c x") 'eproject-eshell-cd-here)

;; use hippie instead of dabbrev
(global-set-key (kbd "M-/") 'hippie-expand)

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
 '(browse-url-browser-function (quote browse-url-generic))
 '(browse-url-generic-program "google-chrome")
 '(c-default-style
   (quote
    ((c-mode . "python")
     (java-mode . "java")
     (awk-mode . "awk")
     (other . "gnu"))))
 '(c-electric-pound-behavior nil)
 '(case-fold-search t)
 '(column-number-mode t)
 '(company-backends
   (quote
    (company-lsp company-elisp company-dabbrev-code company-dabbrev)))
 '(company-frontends
   (quote
    (company-pseudo-tooltip-frontend company-echo-metadata-frontend company-preview-if-just-one-frontend)))
 '(company-go-show-annotation t)
 '(company-idle-delay nil)
 '(company-show-numbers t)
 '(compilation-ask-about-save nil)
 '(compilation-disable-input t)
 '(compilation-message-face (quote bold))
 '(compilation-read-command nil)
 '(compilation-scroll-output t)
 '(compile-auto-highlight 10)
 '(completion-ignored-extensions
   (quote
    (".o" "~" ".bin" ".lbin" ".so" ".a" ".ln" ".blg" ".bbl" ".elc" ".lof" ".glo" ".idx" ".lot" ".svn/" ".hg/" ".git/" ".bzr/" "CVS/" "_darcs/" "_MTN/" ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".fasl" ".ufsl" ".fsl" ".dxl" ".pfsl" ".dfsl" ".p64fsl" ".d64fsl" ".dx64fsl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo" "inc/" "blib/" ".hi")))
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
 '(create-lockfiles nil)
 '(css-tab-mode (quote indent))
 '(current-language-environment "UTF-8")
 '(custom-buffer-done-kill t)
 '(custom-magic-show-button t)
 '(dabbrev-case-fold-search nil)
 '(default-input-method "japanese")
 '(display-hourglass nil)
 '(eldoc-echo-area-use-multiline-p nil)
 '(eldoc-minor-mode-string nil)
 '(electric-indent-mode nil)
 '(electric-pair-mode t)
 '(emacs-lisp-mode-hook (quote (turn-on-eldoc-mode)))
 '(eproject-completing-read-function (quote eproject--ido-completing-read))
 '(eshell-after-prompt-hook nil)
 '(eshell-prompt-function
   (lambda nil
     (format "
%s
%s"
             (eshell/pwd)
             (if
                 (=
                  (user-uid)
                  0)
                 " # " " $ "))))
 '(espresso-auto-indent-flag nil)
 '(fill-column 100)
 '(flowtimer-start-hook (quote (flowtimer-disable-rcirc-tracking)))
 '(flycheck-display-errors-delay 0)
 '(flyspell-issue-message-flag nil)
 '(flyspell-issue-welcome-flag nil)
 '(flyspell-mark-duplications-flag nil)
 '(flyspell-mode-line-string " Spell")
 '(font-lock-global-modes t)
 '(global-company-mode t)
 '(global-prettify-symbols-mode t)
 '(godoc-at-point-function (quote godoc-gogetdoc))
 '(gofmt-command "goimports")
 '(haskell-font-lock-symbols t)
 '(haskell-indentation-cycle-warn nil)
 '(haskell-literate-default (quote latex))
 '(haskell-mode-hook
   (quote
    (turn-on-haskell-indentation turn-on-haskell-doc-mode imenu-add-menubar-index)))
 '(help-window-select t)
 '(hippie-expand-try-functions-list
   (quote
    (try-complete-file-name-partially try-complete-file-name try-expand-all-abbrevs try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill try-complete-lisp-symbol-partially try-complete-lisp-symbol try-expand-line)))
 '(ibuffer-expert t)
 '(ibuffer-fontification-alist
   (quote
    ((10 buffer-read-only font-lock-constant-face)
     (15
      (and buffer-file-name
           (string-match ibuffer-compressed-file-name-regexp buffer-file-name))
      font-lock-doc-face)
     (20
      (string-match "^*"
                    (buffer-name))
      font-lock-keyword-face)
     (25
      (and
       (string-match "^ "
                     (buffer-name))
       (null buffer-file-name))
      italic)
     (30
      (memq major-mode ibuffer-help-buffer-modes)
      font-lock-comment-face)
     (35
      (eq major-mode
          (quote dired-mode))
      font-lock-function-name-face)
     (1
      (eq major-mode
          (quote cperl-mode))
      cperl-hash-face)
     (1
      (eq major-mode
          (quote rcirc-mode))
      rcirc-server))))
 '(ibuffer-formats
   (quote
    ((mark modified read-only " "
           (name 18 18 :left :elide)
           " "
           (size 9 -1 :right)
           " "
           (mode 16 16 :left :elide)
           " "
           (git-status 8 8 :left)
           " " filename-and-process)
     (mark " "
           (name 16 -1)
           " " filename))))
 '(ibuffer-git-column-length 8)
 '(ido-cannot-complete-command (quote ido-next-match))
 '(ido-completion-buffer "nil")
 '(ido-enable-regexp t)
 '(ido-everywhere nil)
 '(ido-mode (quote buffer) nil (ido))
 '(ido-show-dot-for-dired t)
 '(ielm-mode-hook (quote (turn-on-eldoc-mode)))
 '(indent-tabs-mode nil)
 '(indicate-buffer-boundaries (quote left))
 '(indicate-empty-lines nil)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(kill-read-only-ok t)
 '(line-move-visual nil)
 '(lisp-interaction-mode-hook (quote (turn-on-eldoc-mode)))
 '(lsp-auto-guess-root nil)
 '(lsp-debounce-full-sync-notifications nil)
 '(lsp-eldoc-enable-hover nil)
 '(lsp-eldoc-enable-signature-help nil)
 '(lsp-enable-links nil)
 '(lsp-enable-symbol-highlighting nil)
 '(lsp-idle-delay 0.05)
 '(lsp-keep-workspace-alive nil)
 '(lsp-prefer-flymake nil)
 '(lsp-restart (quote auto-restart))
 '(lsp-signature-render-documentation nil)
 '(lsp-ui-doc-enable nil)
 '(lsp-ui-doc-max-width 30)
 '(lsp-ui-flycheck-enable t)
 '(lsp-ui-imenu-enable nil)
 '(lsp-ui-peek-enable t)
 '(lsp-ui-sideline-enable nil)
 '(lsp-ui-sideline-ignore-duplicate t)
 '(lsp-ui-sideline-show-code-actions nil)
 '(lsp-ui-sideline-show-symbol nil)
 '(lsp-yaml-schemas
   (quote #s(hash-table size 65 test eql rehash-size 1.5 rehash-threshold 0.8125 data
                        ())))
 '(make-backup-files nil)
 '(max-lisp-eval-depth 65536)
 '(menu-bar-mode nil nil (menu-bar))
 '(message-citation-line-format "* On %a, %b %d %Y, %N wrote:")
 '(message-citation-line-function (quote message-insert-formatted-citation-line))
 '(message-dont-reply-to-names (quote ("jon@jrock.us")))
 '(message-kill-buffer-on-exit t)
 '(message-mail-alias-type (quote ecomplete))
 '(minibuffer-prompt-properties
   (quote
    (read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)))
 '(mode-line-format
   (quote
    ("%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote " "
     (:eval
      (window-number-string))
     " " mode-line-buffer-identification " " mode-line-position
     (vc-mode vc-mode)
     "  " mode-line-modes mode-line-misc-info)))
 '(mouse-avoidance-mode nil nil (avoid))
 '(mouse-yank-at-point t)
 '(occur-mode-hook (quote (turn-on-font-lock next-error-follow-minor-mode)))
 '(p4-use-p4config-exclusively t t)
 '(package-selected-packages
   (quote
    (powershell use-package window-number fill-column-indicator bazel-mode go-mode jsonnet-mode lsp-ui company-lsp lsp-mode clang-format groovy-mode dockerfile-mode highlight-indentation scss-mode yaml-mode markdown-mode prettier-js protobuf-mode web-mode tide with-editor yasnippet vue-mode php-mode company)))
 '(pgg-default-user-id "5BF3666D")
 '(pgg-gpg-use-agent t)
 '(read-buffer-completion-ignore-case t)
 '(read-file-name-completion-ignore-case t)
 '(save-place-mode t nil (saveplace))
 '(savehist-mode t)
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(sql-product (quote mysql))
 '(sql-sqlite-program "sqlite3")
 '(term-scroll-to-bottom-on-output t)
 '(tex-default-mode (quote latex-mode))
 '(tool-bar-mode nil nil (tool-bar))
 '(tooltip-use-echo-area t)
 '(transient-mark-mode nil)
 '(truncate-partial-width-windows nil)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(user-mail-address "jon@jrock.us")
 '(vc-follow-symlinks t)
 '(vc-handled-backends (quote (git)))
 '(view-inhibit-help-message t)
 '(woman-use-own-frame nil)
 '(xterm-mouse-mode t)
 '(yaml-indent-offset 4))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background "black" :foreground "grey90" :weight normal :height 140 :family "Iosevka"))))
 '(cursor ((t (:background "turquoise" :inverse-video t))))
 '(eslide-slideshow-normal-text ((t (:height 1000 :family "Computer Modern"))))
 '(highlight-indentation-current-column-face ((t (:background "#338833"))))
 '(ido-first-match ((t (:foreground "green"))))
 '(ido-only-match ((t (:background "grey30" :foreground "green"))))
 '(lsp-ui-sideline-global ((t (:background "midnight blue"))))
 '(markdown-code-face ((t (:foreground "dodgerblue"))))
 '(mmm-default-submode-face ((t nil)))
 '(mode-line ((t (:background "grey20" :foreground "white" :box (:line-width 1 :color "grey30")))))
 '(mode-line-buffer-id ((t (:foreground "green"))))
 '(mode-line-highlight ((((class color) (min-colors 88)) (:box (:line-width 1 :color "grey40")))))
 '(mode-line-inactive ((default (:inherit mode-line :background "black" :foreground "grey80" :box (:line-width 1 :color "grey20"))) (nil nil)))
 '(window-number-face ((t (:foreground "red"))) t))

(set-face-foreground 'default "grey90")
(set-face-background 'default "black")
(server-start)
