(x-focus-frame nil)

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

;; let emacs go crazy
(setq gc-cons-threshold 50000000)
(setq read-process-output-max (* 1024 1024))
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(defvar my-packages
  '(paredit
    ;; clojure-mode
    ;; clojure-mode-extra-font-locking
    which-key))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))


(if (eq system-type 'darwin)
    (add-to-list 'my-packages 'exec-path-from-shell))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))
(require 'which-key)
(which-key-mode)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(use-package magit
  :ensure t)

(use-package material-theme
  :ensure t)

(load-theme 'material t)

;; jschaf/esup: ESUP - Emacs Start Up Profiler - GitHub
(use-package esup
  :defer t
  :ensure t)

(use-package nginx-mode
  :defer t
  :ensure t
  :config
  (setq nginx-indent-level 2))

;; TODO explore!!!
;; (use-package multi-term
;;   :defer t
;;   :ensure t
;;   )

(use-package multiple-cursors
  :ensure t
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)))

(use-package highlight-symbol
  :ensure t
  :init (highlight-symbol-mode)
  :bind (("C-." . highlight-symbol-next)
         ("C-," . highlight-symbol-prev))
  :config
  (add-hook 'prog-mode-hook 'highlight-symbol-mode)
  (setq highlight-symbol-idle-delay 0))

;;;;;;;;;;
;; Functions
;;;;;;;;;;
 
(defun toggle-comment-on-line ()
  "Comment or uncomment current line."
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

(global-set-key (kbd "C-;") 'toggle-comment-on-line)

(defun paredit-kill-region-or-backward-delete ()
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (paredit-backward-delete)))

;; Make buffername unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers
    
(require 'saveplace)

(electric-pair-mode 1)

;; TODO make diff-hl only in go, js, elisp project!
(use-package diff-hl
  :ensure t
  :init (diff-hl-mode))

;; (use-package hl-todo
;;   :ensure t
;;   :init (global-hl-todo-mode))

(blink-cursor-mode 0)
(show-paren-mode 1)
(toggle-indicate-empty-lines)
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(delete-selection-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)
(tool-bar-mode -1)

;; (use-package keyfreq
;;   :defer t
;;   :ensure t
;;   :init (keyfreq-mode 1)
;;   :config
;;   (progn
;;     (keyfreq-autosave-mode 1)))

(use-package recentf
  :defer t
  :ensure t
  :init (require 'recentf)
  :config
  (progn 
    (setq 
     recentf-save-file (concat user-emacs-directory ".recentf")
     recentf-max-menu-items 128)))

;; TODO Explore undo-tree. SOme behavior is unexpected
(use-package undo-tree
  :defer t
  :ensure t
  ;; :init (global-undo-tree-mode)
  :bind (("C-z" . undo-tree-undo)
         ("M-z" . undo-tree-undo)
         ("C-S-z" . undo-tree-redo)
         ("M-Z" . undo-tree-redo)))

;; TODO explore more configuration for this powerful package
(use-package expand-region
  :defer t
  :ensure t
  :config 
  (setq expand-region-subword-enabled t)
  :bind (("<M-up>" . er/expand-region)
         ("<M-down>" . er/contract-region)))

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs
   '("PATH")))

;; TODO tidy up this setq
(setq-default
 frame-title-format "%b (%f)"
 indent-tabs-mode nil
 indicate-empty-lines t
 save-place t)
(setq
 select-enable-clipboard t
 select-enable-primary t
 save-interprogram-paste-before-kill t
 
 history-length t
 
 auto-save-default nil
 ring-bell-function 'ignore
 electric-indent-mode nil
 create-lockfiles nil
 inhibit-startup-message t
 
 scroll-conservatively 10000
 scroll-preserve-screen-position t
 truncate-lines t
 )
(setq message-log-max 16)

;; TODO explore!!
(use-package smex
  :ensure t
  :defer t
  :init (smex-initialize)
  :bind (("C-1" . smex))
  :config 
  (progn
    (setq
     smex-save-file (concat user-emacs-directory ".smex-items")
     save-place-file (concat user-emacs-directory "places"))))

;; TODO explore!!
(use-package ido
  :ensure t
  :defer t
  :init (ido-mode)
  :bind (("C-o" . ido-find-file)
         ("C-b" . ido-switch-buffer))
    :config
  (progn
    (setq 
     ido-enable-flex-matching t
     ido-use-filename-at-point nil
     ido-auto-merge-work-directories-length -1
     ido-use-virtual-buffers t)
    (add-to-list 'ido-ignore-files "\\.DS_Store")))

;; TODO explore!!
(use-package flx-ido
  :ensure t
  :defer t)

;; TODO explore!!
(use-package ido-completing-read+
  :ensure t
  :defer t
  :init (ido-ubiquitous-mode 1))

(use-package company
  :init (global-company-mode)
  :ensure t
  :defer t
  :config 
  (progn
    (setq company-idle-delay 0.5
          company-minimum-prefix-length 1)
    (setq company-tooltip-align-annotations t
          ;; Easy navigation to candidates with M-<n>
          company-show-numbers t)
    ;; (setq company-dabbrev-downcase nil)
    )
  :bind (("M-TAB" . company-complete)
         ("TAB" . company-indent-or-complete-common)
         ))

;; TODO make flycheck only in go, js, elisp project!
(use-package flycheck
  ;; :init (global-flycheck-mode) 
  :ensure t
  :defer t
  :config
  (progn
    (setq flycheck-highlighting-mode 'lines)))

;; Crazy fast grep alternative
(use-package rg
 :ensure t
 :defer t
 :init (require 'rg)
 :bind (("C-S-f" . rg-project)))

;;;;;;;;;;
;; Global Kbds
;;;;;;;;;;
;;How do i manage this :( ??????
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "C-;") 'toggle-comment-on-line)
(global-set-key (kbd "C-x g") 'magit-status)

(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "M-x") 'kill-region)
(global-set-key (kbd "M-c") 'copy-region-as-kill)
(global-set-key (kbd "M-v") 'yank)
(global-set-key (kbd "C-w") 'intellij-kill-current-buffer)
;; (global-set-key (kbd "M-1") 'other-window)
(global-set-key (kbd "<f12>") 'other-window)
(global-set-key (kbd "C-f") 'isearch-forward)
;; (global-set-key (kbd "C-F") 'isearch-forward-regexp)
(global-set-key (kbd "C-v") 'yank)
(global-set-key (kbd "<home>") 'beginning-of-buffer)
(global-set-key (kbd "<end>") 'end-of-buffer)
(global-set-key (kbd "C-c C-l") 'isearch-forward)

;; TODO move to clojure/cider mode 
;; (global-set-key (kbd "C-S-l") 'cider-load-buffer)
;; (global-set-key (kbd "C-S-n") 'cider-repl-set-ns)
;; (global-set-key (kbd "C-S-p") 'intellij-send-top-form-to-repl)
;; (global-set-key (kbd "<f5>") 'cider-dev>reset)
;; (global-set-key (kbd "<f6>") 'cider-dev>c.t.n.repl/refresh)

;;;;;;;;;;
;; Elisp
;;;;;;;;;;

;; Automatically load paredit when editing a lisp file
;; More at http://www.emacswiki.org/emacs/ParEdit
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)

;;;;;;;;;;
;; Web Mode
;;;;;;;;;;

(use-package web-mode
  :ensure t
  :defer t
  :config
  (progn
    ;;Indentation
    (subword-mode)
    (setq web-mode-markup-indent-offset 4) 
    (setq web-mode-code-indent-offset 4)
    (setq web-mode-css-indent-offset 4)
    (setq web-mode-comment-style 4)
    
    (add-hook 'html-mode-hook 'subword-mode)
    
    ;;Use only that i know.
    (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)) 
    ))

;;;;;;;;;;
;; Go Mode
;;;;;;;;;;

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "GOPATH"))

(defun my-go-mode-hook ()
  (setq tab-width 1)
  (setq standard-indent ) 
  (setq indent-tabs-mode nil)
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  (add-hook 'go-mode-hook 'subword-mode))

(use-package go-mode
  :ensure t
  :defer t)

(use-package company-quickhelp
  :ensure t
  :defer t
  :config
  (company-quickhelp-mode))

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook (go-mode . lsp-deferred))
(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))
(setq lsp-keymap-prefix "M-p")
(setq lsp-keep-workspace-alive t)

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;; Optional - provides fancier overlays.
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

;; Company mode is a standard completion package that works well with lsp-mode.
(use-package company
  :ensure t
  :config
  ;; Optionally enable completion-as-you-type behavior.
  (setq company-idle-delay 1)
  (setq company-minimum-prefix-length 1))

;; company-lsp integrates company mode completion with lsp-mode.
;; completion-at-point also works out of the box but doesn't support snippets.
(use-package company-lsp
  :ensure t
  :commands company-lsp)

(use-package rainbow-delimiters
  :ensure t
  :defer t)

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook 'hl-line-mode)
(add-hook 'prog-mode-hook 'diff-hl-mode)

;; (use-package go-eldoc
;;   :ensure t
;;   :config 
;;   (add-hook 'go-mode-hook 'go-eldoc-setup)
;;   (set-face-attribute 'eldoc-highlight-function-argument nil
;;                     :underline t :foreground "green"
;;                     :weight 'bold)
;;   )

;;;;;;;;;;
;; Paredit
;;;;;;;;;;


;; TODO Fix this $hit. it's so clutered :(
(use-package paredit
  :ensure t
  :defer t
  :config
  (progn
    (define-key paredit-mode-map (kbd "<backspace>") 'paredit-kill-region-or-backward-delete)
    (define-key paredit-mode-map (kbd "<M-right>") 'paredit-forward)
    (define-key paredit-mode-map (kbd "<M-left>")  'paredit-backward)
    (define-key paredit-mode-map (kbd "<M-up>") nil)
    (define-key paredit-mode-map (kbd "<M-down>")  nil)
    (define-key paredit-mode-map (kbd "<C-right>")  'move-end-of-line)
    (define-key paredit-mode-map (kbd "<C-left>")  'move-beginning-of-line)
    ;; (define-key paredit-mode-map (kbd "M-k") 'paredit-kill)
    (define-key paredit-mode-map (kbd "M-s") nil)
    (define-key paredit-mode-map (kbd "M-s M-d") 'paredit-forward-slurp-sexp)
    (define-key paredit-mode-map (kbd "M-s M-a") 'paredit-backward-slurp-sexp)
    (define-key paredit-mode-map (kbd "M-b M-n") 'paredit-forward-barf-sexp)
    (define-key paredit-mode-map (kbd "M-b M-v") 'paredit-backward-barf-sexp)
    (define-key paredit-mode-map (kbd "M-w M-w") 'paredit-splice-sexp)
    (define-key paredit-mode-map (kbd "M-w M-q") 'paredit-splice-sexp-killing-backward)
    (define-key paredit-mode-map (kbd "M-w M-e") 'paredit-splice-sexp-killing-forward)
    (define-key paredit-mode-map (kbd "C-{") nil)
    (define-key paredit-mode-map (kbd "C-}") nil)
    ))

;;;;;;;;;;
;; js-mode
;;;;;;;;;;

;; prettier-js-command is the prettier command
;; prettier-js-args are the args passed to the prettier command
;; prettier-js-show-errors customizes where to display the error output (buffer, echo or nil)
;; prettier-js-width-mode customizes the width when formatting buffer contents (window, fill or nil)
(use-package prettier-js
  :ensure t
  :defer t
  :config
  (progn
    (setq prettier-js-args '("--tab-width" "2"
                             "--use-tabs" "false"
                             ;; "--trailing-comma" "all"
                             "--single-quote" "true"
                             "--print-width" "100"
                             "--semi" "true"))
    (add-hook 'js2-mode-hook 'prettier-js-mode)))

;;TODO Find out what is this for
(use-package add-node-modules-path
  :ensure t
  :defer t
  )

(use-package nyan-mode
  :if window-system
  :ensure t
  :defer t
  :init (nyan-mode)
  :config
  (progn
   (nyan-start-animation)
   (nyan-toggle-wavy-trail)))

;;TODO Slow as f*ck when opening new file
(use-package rjsx-mode
  :ensure t
  :defer t
  :mode "\\.js$"
  :bind (("<C-right>" . move-end-of-line)
         ("<C-left>" . move-beginning-of-line))
  :config
  (progn
    (subword-mode)
    (setq indent-tabs-mode nil)
    (setq js-indent-level 4)
    (setq js2-strict-missing-semi-warning nil)
    (defadvice js-jsx-indent-line (after js-jsx-indent-line-after-hack activate)
      "Workaround sgml-mode and follow airbnb component style."
      (save-excursion
        (beginning-of-line)
        (if (looking-at-p "^ +\/?> *$")
            (delete-char sgml-basic-offset))))
    ;;bad boy bad boy
    (js2-refactor-mode)
  ))


(use-package js2-refactor
  :ensure t
  
  :config
  (progn
    (add-hook 'js2-mode-hook #'js2-refactor-mode)
    (add-hook 'rjsx-mode #'js2-refactor-mode)
    (js2r-add-keybindings-with-prefix "C-j")
    ))

(use-package yaml-mode
  :ensure t)

;; (use-package yasnippet
;;   :ensure t
;;   :bind (("M-TAB" . yas-expand)))

;; (use-package yasnippet-snippets
;;   :ensure t)
;; (add-hook 'prog-mode-hook #'yas-minor-mode)


;; (defun check-expansion ()
;;     (save-excursion
;;       (if (looking-at "\\_>") t
;;         (backward-char 1)
;;         (if (looking-at "\\.") t
;;           (backward-char 1)
;;           (if (looking-at "->") t nil)))))

;;   (defun do-yas-expand ()
;;     (let ((yas/fallback-behavior 'return-nil))
;;       (yas/expand)))

;;   (defun tab-indent-or-complete ()
;;     (interactive)
;;     (if (minibufferp)
;;         (minibuffer-complete)
;;       (if (or (not yas/minor-mode)
;;               (null (do-yas-expand)))
;;           (if (check-expansion)
;;               (company-complete-common)
;;             (indent-for-tab-command)))))

;;   (global-set-key [tab] 'tab-indent-or-complete)
;; ;; Add yasnippet support for all company backends
;; ;; https://github.com/syl20bnr/spacemacs/pull/179
;; (defvar company-mode/enable-yas t
;;   "Enable yasnippet for all backends.")

;; (defun company-mode/backend-with-yas (backend)
;;   (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
;;       backend
;;     (append (if (consp backend) backend (list backend))
;;             '(:with company-yasnippet))))

;; (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

;; (use-package discover-js2-refactor
;;   :ensure t)

(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
  backup-by-copying t    ; Don't delink hardlinks
  version-control t      ; Use version numbers on backups
  delete-old-versions t  ; Automatically delete excess backups
  kept-new-versions 20   ; how many of the newest versions to keep
  kept-old-versions 5    ; and how many of the old
  )

(use-package mode-icons
  :defer t
  :ensure t
  :init 
  (mode-icons-mode))

(use-package ace-jump-mode
  :defer t
  :ensure t)

(global-set-key (kbd "M-j") 'ace-jump-mode)
(use-package ansi-color
  :defer t
  :ensure t)

(setq js-indent-level 2)

;; (use-package all-the-icons)
;; (use-package all-the-icons-dired)

;; (use-package elpy
;;   :defer t
;;   :ensure t
;;   :init
;;   (elpy-enable))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(go-add-tags protobuf-mode php-mode hcl-mode git-link go-mode mode-icons yaml-mode js2-refactor rjsx-mode nyan-mode add-node-modules-path prettier-js rainbow-delimiters company-lsp lsp-ui lsp-mode company-quickhelp web-mode rg flycheck company ido-completing-read+ flx-ido smex expand-region undo-tree hl-todo diff-hl highlight-symbol multiple-cursors nginx-mode esup material-theme magit use-package which-key clojure-mode-extra-font-locking clojure-mode paredit exec-path-from-shell)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
