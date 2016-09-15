;;; Make Emacs UTF-8 compatible for both display and editing:
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-default buffer-file-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)
(set-default default-buffer-file-coding-system 'utf-8-unix)
(setq system-time-locale "C") ; use english dates in org-mode
 
;;; Settings only for Mac OS X - enable square brackets and curly brackets
(if (eq system-type 'darwin)
    (setq default-input-method "MacOSX"
          mac-command-modifier 'meta
          mac-option-modifier nil
          mac-allow-anti-aliasing t
          mac-command-key-is-meta t))
 
(tool-bar-mode -1)
(menu-bar-mode -1)
(column-number-mode 1)
(setq-default indent-tabs-mode nil)
 
(require 'package)
(add-to-list 'package-archives
             '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(when (not (package-installed-p 'better-defaults))
    (package-refresh-contents))

(defvar my-packages '(ido-ubiquitous
                      smex
                      better-defaults
                      magit
                      paredit
                      clojure-mode
                      clj-refactor
                      haml-mode
                      haskell-mode
                      monokai-theme
                      sass-mode
                      auto-complete
                      yasnippet
                      typescript))
 
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))
 
;;; Auto Complete
(require 'auto-complete-config)
(ac-config-default)
(setq ac-disable-faces nil) ; AC within strings
 
;;; Clojure and ClojureScript
(setq cljr-warn-on-eval nil)
(require 'clj-refactor)
(add-hook 'clojure-mode-hook (lambda ()
                               (clj-refactor-mode 1)
                               (yas-minor-mode)
                               (paredit-mode)
                               (cljr-add-keybindings-with-prefix "C-c C-m")))
(add-hook 'cider-repl-mode-hook (lambda ()
                               (eldoc-mode 1)))

;;; SASS
(add-to-list 'auto-mode-alist '("\\.scss\\'" . sass-mode))
 
;;; ERC
(setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))
 
;;; JavaScript
(setq js-indent-level 2)
 
;; TypeScript
(setq typescript-indent-level 2
      typescript-expr-indent-offset 2)
 
;;; org mode
(setq org-directory "~/org"
      org-mobile-files '("~/org/borland.org" "~/org/borland-timesheet.org")
      org-mobile-inbox-for-pull "~/org/from-mobile.org"
      org-default-notes-file "~/org/notes.org"
      org-log-done 'time
      org-deadline-warning-days 42
      org-time-clocksum-format (quote (:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))
      org-agenda-files (quote ("~/org/borland.org")))
(define-key global-map "\C-cc" 'org-capture)
(defun my-org-clocktable-indent-string (level)
  (if (= level 1)
      ""
    (let ((str ""))
      (while (> level 2)
        (setq level (1- level)
              str (concat str ".")))
      (concat str " "))))
(advice-add 'org-clocktable-indent-string :override #'my-org-clocktable-indent-string)

;;; HTML mode
(add-hook 'html-mode-hook (lambda () (auto-fill-mode -1)))

;; Haskell (enable haskell-mode for Frege files)
(add-to-list 'auto-mode-alist '("\\.fr$" . haskell-mode))

;;; IDO
(setq ido-ubiquitous-mode t)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;;; Smex
(autoload 'smex "smex"
  "Smex is a M-x enhancement for Emacs, it provides a convenient interface to
your recently and most frequently used commands.")
(global-set-key (kbd "M-x") 'smex)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (monokai)))
 '(custom-safe-themes
   (quote
    ("a800120841da457aa2f86b98fb9fd8df8ba682cebde033d7dbf8077c1b7d677a" "1e3b2c9e7e84bb886739604eae91a9afbdfb2e269936ec5dd4a9d3b7a943af7f" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
