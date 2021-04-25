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

(setq read-process-output-max (* 4 1024 1024))

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
                         ("\\.svelte$" . web-mode)
                         ("\\.tsx$" . web-mode)
                         ("\\.js$" . typescript-mode)
                         ("\\.graphqls" . graphql-mode)
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
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)


(use-package lsp
  :config
  (define-key lsp-mode-map (kbd "M-DEL") #'lsp-describe-thing-at-point))

(use-package lsp-ui
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))

(use-package company
  :config
  (define-key company-mode-map [remap indent-for-tab-command] #'company-indent-or-complete-common))

(defface my-window-number-face '((t :foreground "red")) "")

(use-package window-number
  :config
  (defun window-number-set-inactive-color () (force-mode-line-update))
  (defun window-number-set-active-color () (force-mode-line-update))
  (defun window-number-string ()
    (propertize (number-to-string (window-number))
                'face 'my-window-number-face))
  (window-number-meta-mode 1))

(use-package go-mode
  :config
  (progn
    (add-hook 'go-mode-hook (lambda ()
                              (set-fill-column 100)
                              (add-hook 'before-save-hook #'lsp-format-buffer t t)
                              (add-hook 'before-save-hook #'lsp-organize-imports t t)))
    (add-hook 'go-mode-hook #'lsp-deferred)))

(defun setup-vue-mode ()
  (turn-off-flyspell))
(use-package vue-mode
  :config
  (add-hook 'vue-mode-hook #'setup-vue-mode))

(use-package web-mode
  :config
  (add-hook 'web-mode-hook #'lsp-deferred))

(use-package yaml-mode)

(use-package highlight-indentation
  :config
  (add-hook 'yaml-mode-hook #'highlight-indentation-mode))

(use-package typescript-mode
  :config
  (add-hook 'typescript-mode-hook #'lsp-deferred))

(use-package prettier-js
  :config
  (add-hook 'typescript-mode-hook #'prettier-js-mode)
  (add-hook 'yaml-mode-hook #'prettier-js-mode)
  (add-hook 'vue-mode-hook #'prettier-js-mode)
  (add-hook 'markdown-mode-hook #'prettier-js-mode)
  (add-hook 'js-mode-hook #'prettier-js-mode)
  (add-hook 'web-mode-hook #'prettier-js-mode))

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

(use-package god-mode
  :config
  (global-set-key (kbd "<insertchar>") #'god-local-mode)
  (define-key god-local-mode-map (kbd "z") #'repeat)
  (define-key god-local-mode-map (kbd "i") #'god-local-mode))

;;; enable/disable
(put 'downcase-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'set-goal-column 'disabled t)
(global-unset-key (kbd "C-x C-n"))
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
(define-key search-map "d" 'deadgrep)
(global-set-key "\C-xm" 'execute-extended-command)
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-r" 'revert-buffer)
(global-set-key (kbd "C-x C-g") 'abort-recursive-edit)
(global-set-key (kbd "C-x k") 'kill-current-buffer)
(global-set-key (kbd "C-h l") (lambda () (interactive) (info "Elisp")))
(global-set-key (kbd "C-h o") 'find-library)
(global-set-key (kbd "C-x t") #'tmux-here)
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
(global-set-key (kbd "C-j") 'newline)

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

(defun tmux-here ()
  (interactive)
  (if (not (eq (getenv "TMUX") ""))
      (shell-command (format "tmux new-window -c %s" default-directory))
    (error "Not inside a tmux session.")))

;;; custom-set
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Man-notify-method 'pushy)
 '(auto-save-default nil)
 '(browse-url-browser-function 'browse-url-generic)
 '(browse-url-generic-program "google-chrome")
 '(c-default-style
   '((c-mode . "python")
     (java-mode . "java")
     (awk-mode . "awk")
     (other . "gnu")))
 '(c-electric-pound-behavior nil)
 '(case-fold-search t)
 '(column-number-mode t)
 '(company-backends '(company-capf))
 '(company-frontends '(company-pseudo-tooltip-frontend))
 '(company-idle-delay nil)
 '(company-selection-wrap-around t)
 '(company-show-numbers ''t)
 '(company-tooltip-limit 20)
 '(compilation-ask-about-save nil)
 '(compilation-disable-input t)
 '(compilation-message-face 'bold)
 '(compilation-read-command nil)
 '(compilation-scroll-output t)
 '(compile-auto-highlight 10)
 '(completion-ignored-extensions
   '(".o" "~" ".bin" ".lbin" ".so" ".a" ".ln" ".blg" ".bbl" ".elc" ".lof" ".glo" ".idx" ".lot" ".svn/" ".hg/" ".git/" ".bzr/" "CVS/" "_darcs/" "_MTN/" ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".fasl" ".ufsl" ".fsl" ".dxl" ".pfsl" ".dfsl" ".p64fsl" ".d64fsl" ".dx64fsl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo" "inc/" "blib/" ".hi"))
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
 '(cperl-invalid-face 'default)
 '(cperl-lazy-help-time 0)
 '(cperl-merge-trailing-else nil)
 '(cperl-tab-always-indent t)
 '(cperl-under-as-char nil)
 '(create-lockfiles nil)
 '(css-tab-mode 'indent)
 '(current-language-environment "UTF-8")
 '(custom-buffer-done-kill t)
 '(custom-magic-show-button t)
 '(dabbrev-case-fold-search nil)
 '(default-input-method "japanese")
 '(display-hourglass nil)
 '(eldoc-echo-area-use-multiline-p nil)
 '(eldoc-minor-mode-string nil)
 '(electric-indent-mode t)
 '(electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
 '(electric-pair-mode t)
 '(electric-pair-preserve-balance nil)
 '(electric-pair-skip-self nil)
 '(emacs-lisp-mode-hook '(turn-on-eldoc-mode))
 '(eproject-completing-read-function 'eproject--ido-completing-read)
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
 '(flowtimer-start-hook '(flowtimer-disable-rcirc-tracking))
 '(flycheck-display-errors-delay 0)
 '(flycheck-keymap-prefix "f")
 '(flycheck-mode-line-prefix "Fl")
 '(flyspell-issue-message-flag nil)
 '(flyspell-issue-welcome-flag nil)
 '(flyspell-mark-duplications-flag nil)
 '(flyspell-mode-line-string " Spell")
 '(font-lock-global-modes t)
 '(gc-cons-threshold 200000000)
 '(global-company-mode t)
 '(global-prettify-symbols-mode t)
 '(godoc-at-point-function 'godoc-gogetdoc)
 '(haskell-font-lock-symbols t)
 '(haskell-indentation-cycle-warn nil)
 '(haskell-literate-default 'latex)
 '(haskell-mode-hook
   '(turn-on-haskell-indentation turn-on-haskell-doc-mode imenu-add-menubar-index))
 '(help-window-select t)
 '(hippie-expand-try-functions-list
   '(try-complete-file-name-partially try-complete-file-name try-expand-all-abbrevs try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill try-complete-lisp-symbol-partially try-complete-lisp-symbol try-expand-line))
 '(ibuffer-expert t)
 '(ibuffer-fontification-alist
   '((10 buffer-read-only font-lock-constant-face)
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
      (eq major-mode 'dired-mode)
      font-lock-function-name-face)
     (1
      (eq major-mode 'cperl-mode)
      cperl-hash-face)
     (1
      (eq major-mode 'rcirc-mode)
      rcirc-server)))
 '(ibuffer-formats
   '((mark modified read-only " "
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
           " " filename)))
 '(ibuffer-git-column-length 8)
 '(ido-cannot-complete-command 'ido-next-match)
 '(ido-completion-buffer "nil")
 '(ido-enable-regexp t)
 '(ido-everywhere nil)
 '(ido-mode 'buffer nil (ido))
 '(ido-show-dot-for-dired t)
 '(ielm-mode-hook '(turn-on-eldoc-mode))
 '(indent-tabs-mode nil)
 '(indicate-buffer-boundaries 'left)
 '(indicate-empty-lines nil)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(kill-read-only-ok t)
 '(line-move-visual nil)
 '(lisp-interaction-mode-hook '(turn-on-eldoc-mode))
 '(lsp-auto-guess-root nil)
 '(lsp-completion-show-kind nil)
 '(lsp-debounce-full-sync-notifications t)
 '(lsp-diagnostics-flycheck-default-level 'info)
 '(lsp-diagnostics-provider :flycheck)
 '(lsp-eldoc-enable-hover nil)
 '(lsp-eldoc-enable-signature-help nil)
 '(lsp-eldoc-render-all nil)
 '(lsp-enable-links nil)
 '(lsp-enable-snippet t)
 '(lsp-enable-symbol-highlighting nil)
 '(lsp-file-watch-threshold 15000)
 '(lsp-go-codelens nil)
 '(lsp-go-codelenses nil)
 '(lsp-go-env
   #s(hash-table size 65 test eql rehash-size 1.5 rehash-threshold 0.8125 data ("GOFLAGS" "-tags=glfw")))
 '(lsp-go-hover-kind "FullDocumentation")
 '(lsp-go-link-target "pkg.go.dev")
 '(lsp-go-links-in-hover nil)
 '(lsp-go-use-placeholders t)
 '(lsp-gopls-use-placeholders t)
 '(lsp-headerline-breadcrumb-enable nil)
 '(lsp-idle-delay 0.25)
 '(lsp-keep-workspace-alive nil)
 '(lsp-keymap-prefix "M-p")
 '(lsp-log-io nil)
 '(lsp-modeline-code-actions-enable nil)
 '(lsp-modeline-diagnostics-scope :file)
 '(lsp-prefer-flymake nil)
 '(lsp-restart 'auto-restart)
 '(lsp-semantic-tokens-enable t)
 '(lsp-signature-render-documentation t)
 '(lsp-ui-doc-alignment 'window)
 '(lsp-ui-doc-delay 0.01)
 '(lsp-ui-doc-enable nil)
 '(lsp-ui-doc-header t)
 '(lsp-ui-doc-include-signature t)
 '(lsp-ui-doc-max-width 30)
 '(lsp-ui-doc-position 'bottom)
 '(lsp-ui-doc-use-childframe nil)
 '(lsp-ui-flycheck-enable t)
 '(lsp-ui-imenu-enable nil)
 '(lsp-ui-peek-enable t)
 '(lsp-ui-sideline-actions-kind-regex ".*")
 '(lsp-ui-sideline-delay 0.01)
 '(lsp-ui-sideline-enable nil)
 '(lsp-ui-sideline-ignore-duplicate t)
 '(lsp-ui-sideline-show-code-actions t)
 '(lsp-ui-sideline-show-hover t)
 '(lsp-ui-sideline-show-symbol nil)
 '(lsp-ui-sideline-wait-for-all-symbols nil)
 '(lsp-yaml-schemas
   '#s(hash-table size 65 test eql rehash-size 1.5 rehash-threshold 0.8125 data ()))
 '(make-backup-files nil)
 '(max-lisp-eval-depth 65536)
 '(menu-bar-mode nil nil (menu-bar))
 '(message-citation-line-format "* On %a, %b %d %Y, %N wrote:")
 '(message-citation-line-function 'message-insert-formatted-citation-line)
 '(message-dont-reply-to-names '("jon@jrock.us"))
 '(message-kill-buffer-on-exit t)
 '(message-mail-alias-type 'ecomplete)
 '(minibuffer-prompt-properties
   '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))
 '(mode-line-format
   '("%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote " "
     (:eval
      (window-number-string))
     " " mode-line-buffer-identification " " mode-line-position
     (vc-mode vc-mode)
     "  " mode-line-modes mode-line-misc-info))
 '(mouse-avoidance-mode nil nil (avoid))
 '(mouse-yank-at-point t)
 '(occur-mode-hook '(turn-on-font-lock next-error-follow-minor-mode))
 '(p4-use-p4config-exclusively t t)
 '(package-selected-packages
   '(god-mode flycheck typescript-mode graphql-mode magit lsp-ui deadgrep powershell use-package window-number fill-column-indicator bazel-mode go-mode jsonnet-mode lsp-mode clang-format groovy-mode dockerfile-mode highlight-indentation scss-mode yaml-mode markdown-mode prettier-js protobuf-mode web-mode with-editor yasnippet vue-mode php-mode company))
 '(pgg-default-user-id "5BF3666D")
 '(pgg-gpg-use-agent t)
 '(prettier-js-args '("prettier"))
 '(prettier-js-command "npx")
 '(read-buffer-completion-ignore-case t)
 '(read-file-name-completion-ignore-case t)
 '(save-place-mode t nil (saveplace))
 '(savehist-mode t)
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(sql-product 'postgres)
 '(sql-sqlite-program "sqlite3")
 '(term-scroll-to-bottom-on-output t)
 '(tex-default-mode 'latex-mode)
 '(tool-bar-mode nil nil (tool-bar))
 '(tooltip-use-echo-area t)
 '(transient-mark-mode nil)
 '(truncate-partial-width-windows nil)
 '(typescript-auto-indent-flag nil)
 '(typescript-indent-level 2)
 '(uniquify-buffer-name-style 'forward nil (uniquify))
 '(user-mail-address "jon@jrock.us")
 '(vc-follow-symlinks t)
 '(vc-handled-backends '(git))
 '(view-inhibit-help-message t)
 '(web-mode-code-indent-offset 4)
 '(web-mode-part-padding 4)
 '(web-mode-script-padding 4)
 '(web-mode-style-padding 4)
 '(woman-use-own-frame nil)
 '(xterm-mouse-mode t)
 '(yaml-indent-offset 4))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background "black" :foreground "grey90" :slant normal :weight normal :height 135 :family "Iosevka Term"))))
 '(cursor ((t (:background "turquoise" :inverse-video t))))
 '(eslide-slideshow-normal-text ((t (:height 1000 :family "Computer Modern"))))
 '(flycheck-error ((t (:foreground "color-225" :underline t))))
 '(highlight-indentation-current-column-face ((t (:background "#338833"))))
 '(ido-first-match ((t (:foreground "green"))))
 '(ido-only-match ((t (:background "grey30" :foreground "green"))))
 '(lsp-ui-sideline-global ((t nil)))
 '(markdown-code-face ((t (:foreground "dodgerblue"))))
 '(mmm-default-submode-face ((t nil)))
 '(mode-line ((t (:background "grey30" :foreground "white" :box (:line-width 1 :color "grey30")))))
 '(mode-line-buffer-id ((t (:foreground "green"))))
 '(mode-line-highlight ((((class color) (min-colors 88)) (:box (:line-width 1 :color "grey40")))))
 '(mode-line-inactive ((t (:inherit mode-line :background "grey10" :foreground "grey80" :box (:line-width 1 :color "grey20")))))
 '(window-number-face ((t (:foreground "red"))) t))

(set-face-foreground 'default "grey90")
(set-face-background 'default "black")
(server-start)
