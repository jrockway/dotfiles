(require 'cl)

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
                         ("\\.ino$" . c++-mode)
                         ("\\.json$" . json-ts-mode)
                         ("\\.yaml$" . yaml-ts-mode)
                         ("\\.yml$" . yaml-ts-mode))
                       auto-mode-alist))

(when (file-exists-p "~/elisp" )
  ;; load extra modes from ~/elisp
  (add-to-list 'load-path "~/elisp")
  (add-to-list 'load-path "~/elisp/_local")
  (add-to-list 'load-path "~/elisp/eproject")
  (add-to-list 'load-path "~/elisp/eproject/contrib")
  (add-to-list 'load-path "~/elisp/eproject/lang")
  (add-to-list 'load-path "~/elisp/eslide")
  (add-to-list 'load-path "~/elisp/ibuffer-git")
  (add-to-list 'load-path "~/elisp/devil")
  (require 'devil)
  (require 'eproject)
  (require 'eproject-compile)
  (require 'eproject-extras)
  (require 'eshell)
  (require 'eslide)
  (require 'ibuffer-git)
  (require 'editing-extras)
  (require 'text-extras)
  (require 'treesit)
  (setq treesit-extra-load-path '("~/elisp/tree-sitter-modules/")))

(require 'project)

(setq read-process-output-max (* 4 1024 1024))

;;; modes i want on by default
(ido-mode 1)
(winner-mode 1)
(global-devil-mode)

;;; override stupid defaults
(defalias 'yes-or-no-p 'y-or-n-p)

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

(with-eval-after-load 'xt-mouse
  (defun xterm-mouse--tracking-sequence (suffix)
    "Return a control sequence to enable or disable mouse tracking.
SUFFIX is the last character of each escape sequence (?h to
enable, ?l to disable)."
    (mapcar
     (lambda (code) (format "\e[?%d%c" code suffix))
     ;; This removes 1003, which disables mouse movement events.
     `(1000 ,@(when xterm-mouse-utf-8 '(1005)) 1006)))

  (defun xterm-mouse-tracking-enable-sequence () (apply #'concat (xterm-mouse--tracking-sequence ?h))))

;;; load packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(use-package quelpa)
(use-package quelpa-use-package)

(defface my-window-number-face '((t :foreground "red")) "")

(use-package corfu :init (global-corfu-mode))
(use-package corfu-terminal
  :bind
  (:map corfu-map
        ("M-1" . digit-argument)
        ("M-2" . digit-argument)
        ("M-3" . digit-argument)
        ("M-4" . digit-argument)
        ("M-5" . digit-argument)
        ("M-6" . digit-argument)
        ("M-7" . digit-argument)
        ("M-8" . digit-argument)
        ("M-9" . digit-argument)
        ("M-0" . digit-argument))
  :config
  (corfu-terminal-mode +1))

(use-package rainbow-delimiters :config (add-hook 'prog-mode-hook #'rainbow-delimiters-mode-enable))

(use-package window-number
  :config
  (defun window-number-set-inactive-color () (force-mode-line-update))
  (defun window-number-set-active-color () (force-mode-line-update))
  (defun window-number-string ()
    (propertize (number-to-string (window-number))
                'face 'my-window-number-face))
  (window-number-meta-mode 1))

(defun project-find-go-module (dir)
  (when-let ((root (locate-dominating-file dir "go.mod")))
    (cons 'go-module root)))

(cl-defmethod project-root ((project (head go-module)))
  (cdr project))

(add-hook 'project-find-functions #'project-find-go-module)

(setq-default eglot-workspace-configuration
              '(
                :gopls (
                        :staticcheck t
                        :analyses (:QF1008 :json-false)
                        :usePlaceholders t
                        :buildFlags ["-tags=k8s,unit_test"]
                        :hints (:constantValues t)
                        :diagnosticsDelay "250ms"
                        :linksInHover :json-false
                        :directoryFilters ["-bazel-out" "-bazel-bin" "-bazel-testlogs" "-bazel-pachyderm" "-bazel-gazelle" "-**/node_modules"]
                        )
                :json (
                       :schemas [
                                 (:fileMatch ["deno.json"]
                                             :url "https://raw.githubusercontent.com/denoland/deno/main/cli/schemas/config-file.v1.json")
                                 (:fileMatch ["*.pipeline.json"]
                                             :url "https://raw.githubusercontent.com/pachyderm/pachyderm/master/src/internal/jsonschema/pps_v2/CreatePipelineRequest.schema.json")
                                 ])))

(defun my-eglot-organize-imports () (interactive)
       (eglot-code-actions nil nil "source.organizeImports" t))

(defun setup-go-mode (hook)
  (add-hook hook (lambda ()
                   (modify-syntax-entry ?_ "w" go-mode-syntax-table)
                   (set-fill-column 100)
                   (turn-on-eldoc-mode)
                   (eglot-ensure)
                   (make-variable-buffer-local 'eldoc-documentation-functions)
                   (setq eldoc-documentation-functions '(flymake-eldoc-function eglot-signature-eldoc-function eglot-hover-eldoc-function))
                   (add-hook 'before-save-hook 'my-eglot-organize-imports nil t)
                   (add-hook 'before-save-hook #'eglot-format nil t))))
(use-package go-mode :config (setup-go-mode 'go-mode-hook))
(setup-go-mode 'go-ts-mode-hook)

(use-package bazel :config
  (add-to-list 'auto-mode-alist '("\\.star$" . bazel-starlark-mode)))

(use-package web-mode)

(use-package yaml-mode)

(require 'yaml-ts-mode)

(define-key yaml-ts-mode-map (kbd "TAB") #'yaml-indent-line)

(use-package highlight-indentation
  :config
  (add-hook 'yaml-mode-hook #'highlight-indentation-mode)
  (add-hook 'yaml-ts-mode-hook #'highlight-indentation-mode))

(use-package typescript-mode)

(use-package prettier-js
  :config
  (add-hook 'web-mode-hook #'prettier-js-mode))

(use-package jsonnet-mode)

(use-package format-all
  :config
  (setq-default format-all-formatters
                '(("JSON" . (deno))
                  ("Markdown" . (deno))
                  ("TypeScript" . (deno))
                  ("JavaScript" . (deno))
                  ("YAML" . (prettier))
                  ("Jsonnet" . (jsonnetfmt))
                  ("Emacs Lisp" . (emacs-lisp))
                  ("Bazel" . (buildifier))))
  (add-hook 'prog-mode-hook #'format-all-mode)
  (add-hook 'text-mode-hook #'format-all-mode))

(use-package clang-format)

(defun setup-protobuf-mode ()
  (c-add-style "my-style" '((c-basic-offset . 2) (indent-tabs-mode . nil)) t)
  (add-hook 'before-save-hook #'clang-format-buffer nil t))
(use-package protobuf-mode
  :config
  (add-hook 'protobuf-mode-hook #'setup-protobuf-mode))

(defun my-yas-clear-snippet ()
  (interactive)
  (let ((snippet (car (yas-active-snippets))))
    (when snippet
      (let ((start (yas--field-start (yas-current-field)))
            (last-field (car (last (yas--snippet-fields (car (yas-active-snippets)))))))
        (when last-field
          (goto-char start)
          (delete-region (point) (yas--field-end last-field))
          (yas-abort-snippet snippet))))))

(use-package yasnippet
  :config
  (setq yas-snippet-dirs '("~/elisp/snippets/"))
  (yas-global-mode t)
  (define-key yas-keymap (kbd "C-k") #'my-yas-clear-snippet))


(use-package copilot
  :quelpa (copilot :fetcher github
                   :repo "zerolfx/copilot.el"
                   :branch "main"
                   :files ("dist" "*.el")))

(use-package markdown-mode)

(use-package flymake :defer t :config
  (define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error)
  (define-key flymake-mode-map (kbd "C-c p") 'flymake-show-project-diagnostics)
  (define-key flymake-mode-map (kbd "C-c f") 'flymake-show-buffer-diagnostics)
  (add-hook 'flymake-diagnostics-buffer-mode-hook (lambda (setq truncate-lines nil))))

(add-hook 'js-json-mode-hook #'eglot-ensure)
(add-hook 'js-json-mode-hook #'rainbow-delimiters-mode-enable)
(add-hook 'json-ts-mode-hook #'eglot-ensure)
(add-hook 'json-ts-mode-hook #'rainbow-delimiters-mode-enable)

;;; enable/disable
(put 'downcase-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'set-goal-column 'disabled t)
(global-unset-key (kbd "C-x C-n"))
(keymap-unset help-map "g")
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
(setq-default completion-ignore-case t)

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
(global-set-key (kbd "C-x t") #'tmux-here)
(global-set-key (kbd "s-(") (lambda () (interactive) (other-window -1)))
(global-set-key (kbd "s-)") (lambda () (interactive) (other-window 1)))
(global-set-key (kbd "C-(") (lambda () (interactive) (other-window -1)))
(global-set-key (kbd "C-)") (lambda () (interactive) (other-window 1)))
(global-set-key (kbd "s-U") (lambda () (interactive) (other-window -1) (delete-window)))
(global-set-key (kbd "M-r") 'comment-region)
(global-set-key (kbd "C-M-r") 'uncomment-region)
(global-set-key (kbd "M-i") 'indent-region)
(global-set-key (kbd "M-;") 'replace-regexp)
(global-set-key (kbd "C-c C-k") 'compile)
(global-set-key (kbd "M-=") 'xref-find-references)
(global-set-key (kbd "<mouse-4>") 'scroll-down-line)
(global-set-key (kbd "<mouse-5>") 'scroll-up-line)
(global-set-key (kbd "C-j") 'newline)

;; use hippie instead of dabbrev
(global-set-key (kbd "M-/") 'hippie-expand)

;; use C-h c for customize
(global-unset-key (kbd "C-h c"))
(global-unset-key (kbd "C-h g"))
(global-set-key (kbd "C-h c a") #'customize-apropos)
(global-set-key (kbd "C-h c v") #'customize-variable)
(global-set-key (kbd "C-h c f") #'customize-face)
(global-set-key (kbd "C-h c g") #'customize-group)

;;; unbind most help
(global-set-key (kbd "C-h l") (lambda () (interactive) (info "Elisp")))

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
 '(bazel-buildifier-before-save nil)
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
 '(compilation-ask-about-save nil)
 '(compilation-disable-input t)
 '(compilation-message-face 'bold)
 '(compilation-read-command nil)
 '(compilation-scroll-output t)
 '(compile-auto-highlight 10)
 '(completion-ignored-extensions
   '(".o" "~" ".bin" ".lbin" ".so" ".a" ".ln" ".blg" ".bbl" ".elc" ".lof" ".glo" ".idx" ".lot" ".svn/" ".hg/" ".git/" ".bzr/" "CVS/" "_darcs/" "_MTN/" ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".fasl" ".ufsl" ".fsl" ".dxl" ".pfsl" ".dfsl" ".p64fsl" ".d64fsl" ".dx64fsl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo" "inc/" "blib/" ".hi"))
 '(completion-styles '(basic partial-completion flex emacs22))
 '(confirm-nonexistent-file-or-buffer nil)
 '(corfu-echo-delay '(0 . 0))
 '(corfu-indexed-mode t)
 '(corfu-indexed-start 1)
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
 '(display-buffer-base-action '(nil))
 '(display-hourglass nil)
 '(eglot-autoshutdown t)
 '(eglot-confirm-server-initiated-edits nil)
 '(eglot-events-buffer-size 200000)
 '(eglot-extend-to-xref t)
 '(eglot-ignored-server-capabilities '(:documentHighlightProvider))
 '(eglot-menu-string "eg")
 '(eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
 '(eldoc-echo-area-use-multiline-p 10)
 '(eldoc-idle-delay 0.05)
 '(eldoc-minor-mode-string nil)
 '(electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
 '(electric-pair-mode t)
 '(electric-pair-pairs nil)
 '(electric-pair-skip-whitespace-chars '(32))
 '(electric-pair-text-pairs nil)
 '(emacs-lisp-mode-hook '(eldoc-mode format-all-mode))
 '(eproject-completing-read-function 'eproject--ido-completing-read)
 '(eshell-after-prompt-hook nil)
 '(eshell-prompt-function
   (lambda nil
     (format "\12%s\12%s"
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
 '(flycheck-keymap-prefix "\3f")
 '(flycheck-mode-line-prefix "Fl")
 '(flymake-mode-line-counter-format
   '(":" flymake-mode-line-error-counter flymake-mode-line-warning-counter flymake-mode-line-note-counter))
 '(flymake-mode-line-lighter "F")
 '(flyspell-issue-message-flag nil)
 '(flyspell-issue-welcome-flag nil)
 '(flyspell-mark-duplications-flag nil)
 '(flyspell-mode-line-string " Spell")
 '(font-lock-global-modes t)
 '(format-all-mode-lighter " fmt")
 '(gc-cons-threshold 200000000)
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
 '(js-enabled-frameworks '(javascript))
 '(js-indent-first-init t)
 '(js-indent-level 2)
 '(kill-read-only-ok t)
 '(line-move-visual nil)
 '(lisp-interaction-mode-hook '(turn-on-eldoc-mode))
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
   '(corfu-terminal rainbow-delimiters format-all copilot editorconfig quelpa-use-package quelpa consult-dir consult bazel minizinc-mode typescript-mode graphql-mode magit deadgrep powershell use-package window-number fill-column-indicator go-mode jsonnet-mode clang-format dockerfile-mode highlight-indentation scss-mode yaml-mode markdown-mode prettier-js protobuf-mode web-mode with-editor yasnippet))
 '(pgg-default-user-id "5BF3666D")
 '(pgg-gpg-use-agent t)
 '(prettier-js-args '("prettier"))
 '(prettier-js-command "npx")
 '(rainbow-delimiters-max-face-count 5)
 '(rainbow-delimiters-outermost-only-face-count 1)
 '(read-buffer-completion-ignore-case t)
 '(read-file-name-completion-ignore-case t)
 '(save-place-mode t nil (saveplace))
 '(savehist-mode t)
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(smartparens-global-strict-mode t)
 '(sp-hybrid-kill-excessive-whitespace 'kill)
 '(sql-product 'postgres)
 '(sql-sqlite-program "sqlite3")
 '(tab-always-indent 'complete)
 '(term-scroll-to-bottom-on-output t)
 '(tex-default-mode 'latex-mode)
 '(tool-bar-mode nil nil (tool-bar))
 '(tooltip-use-echo-area t)
 '(transient-mark-mode nil)
 '(truncate-partial-width-windows nil)
 '(typescript-auto-indent-flag nil)
 '(typescript-indent-level 4)
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
 '(xref-after-jump-hook '(recenter))
 '(xref-after-return-hook nil)
 '(xref-auto-jump-to-first-definition nil)
 '(xref-auto-jump-to-first-xref nil)
 '(xterm-mouse-mode t)
 '(xterm-set-window-title t)
 '(yaml-backspace-function 'backward-delete-char)
 '(yaml-block-literal-search-lines 1000)
 '(yaml-indent-offset 2)
 '(yas-verbosity 1 t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t nil)))
 '(eglot-inlay-hint-face ((t (:foreground "grey20" :slant italic))))
 '(eglot-mode-line ((t (:inherit font-lock-constant-face))))
 '(eslide-slideshow-normal-text ((t (:height 1000 :family "Computer Modern"))))
 '(flycheck-error ((t (:foreground "pink" :underline (:color foreground-color :style wave)))))
 '(flymake-error ((t (:foreground "pink" :underline (:color foreground-color :style wave :position nil)))))
 '(flymake-warning ((t (:weight normal :inherit warning))))
 '(font-lock-comment-face ((t (:foreground "chocolate1" :slant italic))))
 '(font-lock-keyword-face ((t (:foreground "Cyan1" :weight bold))))
 '(highlight-indentation-current-column-face ((t (:background "#338833"))))
 '(ido-first-match ((t (:foreground "green"))))
 '(ido-only-match ((t (:background "grey30" :foreground "green"))))
 '(markdown-code-face ((t (:foreground "dodgerblue"))))
 '(mmm-default-submode-face ((t nil)))
 '(mode-line ((t (:box (:line-width (1 . 1) :color "grey30") :foreground "white" :background "blue4"))))
 '(mode-line-buffer-id ((t (:foreground "green"))))
 '(mode-line-highlight ((((class color) (min-colors 88)) (:box (:line-width 1 :color "grey40")))))
 '(mode-line-inactive ((t (:box (:line-width (1 . 1) :color "grey20") :foreground "grey80" :background "grey20" :inherit mode-line))))
 '(rainbow-delimiters-base-error-face ((t (:inherit rainbow-delimiters-base-face :foreground "red"))))
 '(rainbow-delimiters-base-face ((t (:inherit nil))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "grey90" :inherit rainbow-delimiters-base-face))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "grey80" :inherit rainbow-delimiters-base-face))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "grey70" :inherit rainbow-delimiters-base-face))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "grey60" :inherit rainbow-delimiters-base-face))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "grey50" :inherit rainbow-delimiters-base-face))))
 '(rainbow-delimiters-depth-6-face ((t (:inherit rainbow-delimiters-base-face :foreground "violetred"))))
 '(rainbow-delimiters-depth-7-face ((t (:inherit rainbow-delimiters-base-face :foreground "gold"))))
 '(rainbow-delimiters-depth-8-face ((t (:inherit rainbow-delimiters-base-face :foreground "lawngreen"))))
 '(rainbow-delimiters-depth-9-face ((t (:inherit rainbow-delimiters-base-face :foreground "mediumaquamarine"))))
 '(window-number-face ((t (:foreground "red"))) t)
 '(xref-file-header ((t (:foreground "green")))))

(set-face-foreground 'default "grey90")
(set-face-background 'default "black")
(server-start)
