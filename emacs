(require 'cl)

;;; load extra modes
(add-to-list 'load-path "~/elisp")
(add-to-list 'load-path "~/elisp/go-mode")
(add-to-list 'load-path "~/elisp/_local")
(add-to-list 'load-path "~/elisp/cperl-mode")
(add-to-list 'load-path "~/elisp/eproject")
(add-to-list 'load-path "~/elisp/eproject/contrib")
(add-to-list 'load-path "~/elisp/eproject/lang")
(add-to-list 'load-path "~/elisp/eslide")
(add-to-list 'load-path "~/elisp/ibuffer-git")

(require 'css-mode)
(require 'eproject)
(require 'eproject-compile)
(require 'eproject-extras)
(require 'eshell)
(require 'eslide)
(require 'go-mode)
(require 'help-mode)
(require 'ibuffer-git)
(require 'uniquify)
(require 'window-number)

(require 'editing-extras)
(require 'text-extras)

;;; modes i want on by default
(ido-mode 1)
(winner-mode 1)
(window-number-mode 1)
(defalias 'perl-mode 'cperl-mode)

;;; auto-mode-alist
(setq auto-mode-alist (append auto-mode-alist
                              '(("\\.t$" . cperl-mode)
                                ("\\.hs$" . haskell-mode)
                                ("\\.js$" . js2-mode))))

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

(defun setup-golang-style ()
  (set-fill-column 100)
  (add-hook 'before-save-hook #'gofmt-before-save))

(add-hook 'go-mode-hook #'setup-golang-style)

(defun setup-tide-mode ()
  (tide-setup)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (flycheck-mode t)
  (eldoc-mode t)
  (tide-hl-identifier-mode t)
  (company-mode t))

(add-hook 'typescript-mode-hook #'setup-tide-mode)

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

;;; package
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

;;; things that depend on packages
(when (file-exists-p "~/go/src/github.com/mdempsky/gocode/emacs-company")
  (add-to-list 'load-path "~/go/src/github.com/mdempsky/gocode/emacs-company")
  (require 'company-go))

(setq yas-snippet-dirs '("~/elisp/snippets/"))

;;; per-platform setup
(cond
 ((eq window-system 'w32)
  (setq ispell-program-name "C:/Program Files (x86)/Aspell/bin/aspell.exe")
  (setq exec-path
        (quote
         ("c:/WINDOWS/system32" "C:/WINDOWS" "C:/WINDOWS/System32/Wbem" "C:/WINDOWS/System32/WindowsPowerShell/v1.0/" "C:/Program Files (x86)/NVIDIA Corporation/PhysX/Common" "c:/Users/jon/go/bin" "C:/Go/bin" "C:/Program Files/Git/cmd" "C:/Users/jon/AppData/Local/Microsoft/WindowsApps" "c:/Users/jon/Programs/emacs-25.3_1-x86_64/libexec/emacs/25.3/x86_64-w64-mingw32" "C:\\msys64\\usr\\bin" "c:/Users/jon/Programs/emacs-25.3_1-x86_64/bin" "c:/Program Files/nodejs"))))
 ((eq window-system 'ns)
  (setq exec-path
        (quote
         ("/Users/jonathanrockway/go/bin" "/usr/local/go/bin" "/Users/jonathanrockway/.depot_tools/depot_tools" "/usr/local/jrockway/android-sdk-linux/tools" "/usr/local/jrockway/android-sdk-linux/platform-tools" "/Users/jonathanrockway/.cabal/bin" "/Users/jonathanrockway/.local/bin" "/usr/local/scripts" "/usr/local/buildtools/java/jdk/bin" "/usr/local/sbin" "/usr/local/bin" "/usr/sbin" "/usr/bin" "/sbin" "/bin")))))

;; ;; We need C-x C-c bound to s-b-k-t for emacsclient -t sessions, but when
;; ;; it kills my main X session (with 9 windows or whatever), it is really
;; ;; annoying.
;; (defadvice save-buffers-kill-terminal (around dont-kill-my-x-session-kthx)
;;   "Disable C-x C-c under X."
;;   (if (or (eq window-system 'x) (eq window-system 'w32))
;;       (message "I'm afraid I can't do that, Dave.")
;;     ad-do-it))
;; (ad-activate 'save-buffers-kill-terminal)

(defun iconify-or-deiconify-frame ()
  "Don't iconify, since that makes emacs freeze under xmonad."
  (interactive)
  (make-frame-visible))

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
(define-key flyspell-mode-map (kbd "C-;") nil)

;; set
(setq-default cc-electric-flag nil)
(setq-default c-electric-flag nil)

(define-key text-mode-map "\C-cu" 'insert-same-number-of-chars-as-line-above)
(define-key text-mode-map "\C-ccw" 'ispell-complete-word)
(define-key help-mode-map "l" 'help-go-back)
(define-key read-expression-map (kbd "TAB") #'completion-at-point)
(define-key go-mode-map "\M-." 'godef-jump)
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
(global-set-key (kbd "C-<backspace>") 'backward-char)
(global-set-key (kbd "M-=") 'company-complete)

(define-key eproject-mode-map (kbd "C-c x") 'eproject-eshell-cd-here)

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
 '(browse-url-browser-function (quote browse-url-generic))
 '(browse-url-generic-program "google-chrome")
 '(c-electric-pound-behavior (quote (alignleft)))
 '(case-fold-search t)
 '(column-number-mode t)
 '(company-backends
   (quote
    (company-elisp
     (company-go company-dabbrev-code company-dabbrev)
     company-dabbrev-code company-capf)))
 '(company-go-show-annotation t)
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
 '(current-language-environment "UTF-8")
 '(custom-buffer-done-kill t)
 '(custom-magic-show-button t)
 '(dabbrev-case-fold-search nil)
 '(default-input-method "japanese")
 '(display-hourglass nil)
 '(eldoc-echo-area-use-multiline-p nil)
 '(eldoc-minor-mode-string nil)
 '(electric-indent-mode t)
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
 '(fill-column 80)
 '(flowtimer-start-hook (quote (flowtimer-disable-rcirc-tracking)))
 '(flyspell-issue-message-flag nil)
 '(flyspell-issue-welcome-flag nil)
 '(flyspell-mark-duplications-flag nil)
 '(flyspell-mode-line-string " Spell")
 '(font-lock-global-modes t)
 '(global-company-mode t)
 '(godoc-and-godef-command "/usr/local/go/bin/go doc")
 '(godoc-at-point-function (quote godoc-gogetdoc))
 '(godoc-command "/usr/local/go/bin/go doc")
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
 '(mouse-avoidance-mode nil nil (avoid))
 '(mouse-yank-at-point t)
 '(occur-mode-hook (quote (turn-on-font-lock next-error-follow-minor-mode)))
 '(p4-use-p4config-exclusively t t)
 '(package-selected-packages
   (quote
    (go-eldoc tide with-editor magit yasnippet vue-mode php-mode js2-mode company)))
 '(pgg-default-user-id "5BF3666D")
 '(pgg-gpg-use-agent t)
 '(read-buffer-completion-ignore-case t)
 '(read-file-name-completion-ignore-case t)
 '(save-place-mode t nil (saveplace))
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(sql-product (quote mysql))
 '(sql-sqlite-program "sqlite3")
 '(term-scroll-to-bottom-on-output t)
 '(tex-default-mode (quote latex-mode))
 '(text-mode-hook
   (quote
    (turn-on-flyspell turn-on-auto-fill text-mode-hook-identify)))
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
 '(xterm-mouse-mode t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "black" :foreground "grey90" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "outline" :family "Droid Sans Mono Slashed"))))
 '(cursor ((t (:background "turquoise" :inverse-video t))))
 '(eslide-slideshow-normal-text ((t (:height 1000 :family "Computer Modern"))))
 '(ido-first-match ((t (:foreground "green"))))
 '(ido-only-match ((t (:background "grey30" :foreground "green"))))
 '(mode-line ((t (:background "grey20" :foreground "white" :box (:line-width 1 :color "grey30")))))
 '(mode-line-buffer-id ((t (:foreground "green"))))
 '(mode-line-highlight ((((class color) (min-colors 88)) (:box (:line-width 1 :color "grey40")))))
 '(mode-line-inactive ((default (:inherit mode-line :background "black" :foreground "grey80" :box (:line-width 1 :color "grey20"))) (nil nil)))
 '(window-number-face ((nil (:foreground "red")))))

(set-face-foreground 'default "grey90")
(set-face-background 'default "black")
