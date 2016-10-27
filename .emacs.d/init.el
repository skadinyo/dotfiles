(setq make-backup-files nil)
(setq auto-save-default nil)

(require 'package)

(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(add-to-list 'package-archives
             '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)

(package-initialize)

(add-to-list 'default-frame-alist '(font . "Monospace-14:weight=normal"))
(set-face-attribute 'default t :font "Monospace-14:weight=normal")

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages
  '(paredit
    clojure-mode
    expand-region
    clojure-mode-extra-font-locking
    cider
    ido-ubiquitous
    smex
    projectile
    rainbow-delimiters
    tagedit
    magit
    company
    ;;company-flx
    web-mode
    undo-tree))

;; On OS X, an Emacs instance started from the graphical user
;; interface will have a different environment than a shell in a
;; terminal window, because OS X does not run a shell during the
;; login. Obviously this will lead to unexpected results when
;; calling external utilities like make from Emacs.
;; This library works around this problem by copying important
;; environment variables from the user's shell.
;; https://github.com/purcell/exec-path-from-shell
(if (eq system-type 'darwin)
    (add-to-list 'my-packages 'exec-path-from-shell))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))
;;;;
;; Customization
;;;;
(add-to-list 'load-path "~/.emacs.d/customizations")

;; Sets up exec-path-from-shell so that Emacs will use the correct
;; environment variables
(load "shell-integration.el")

(load "navigation.el")
(load "ui.el")
(load "editing.el")
(load "misc.el")
;;(load "firacode.el")
(load "elisp-editing.el")
;; Language-specific
(load "setup-clojure.el")
(load "setup-js.el")

(load "kbd.el")
(load "intellij.el")
;; load tabbar
(add-to-list 'load-path "~/dotskadinyo/module/tabbar")
(require 'tabbar)
(tabbar-mode t)

(global-company-mode)

(setq company-idle-delay nil) ; never start completions automatically

(with-eval-after-load 'company
  (company-flx-mode +1))

(global-set-key (kbd "M-TAB") #'company-complete) ; use M-TAB, a.k.a. C-M-i, as manual trigger
(global-set-key (kbd "TAB") #'company-indent-or-complete-common)


(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2) 
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-comment-style 2)
  )
(add-hook 'web-mode-hook  'my-web-mode-hook)

(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))


(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
