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
(setq blink-cursor-mode -1)
(column-number-mode 1)
(setq-default indent-tabs-mode nil)
(setq inhibit-startup-screen t)
(setq initial-scratch-message ";; Emacs")

;;; Backup files in ~/.saves
(setq
 backup-by-copying t      ; don't clobber symlinks
 backup-directory-alist
 '(("." . "~/.saves/"))    ; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)       ; use versioned backups

(require 'package)
(add-to-list 'package-archives
             '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(when (not (package-installed-p 'better-defaults))
    (package-refresh-contents))

(defvar my-packages '(;ido-ubiquitous
                      smex
                      better-defaults
                      ;magit
                      ;paredit
                      ;clojure-mode
                      ;clj-refactor
                      elixir-mode
                      erlang
                      haml-mode
                      haskell-mode
                      monokai-theme
                      sass-mode
                      auto-complete
                      yasnippet
                      typescript-mode
                      ;rainbow-mode
                      purescript-mode
                      ;parinfer
                      use-package))
 
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))
 
;;; Window switching
(windmove-default-keybindings)

;;; Auto Complete
(require 'auto-complete-config)
(ac-config-default)
(setq ac-disable-faces nil) ; AC within strings
 
;;; Clojure and ClojureScript
;(setq cljr-warn-on-eval nil)
;(require 'clj-refactor)
;(add-hook 'clojure-mode-hook (lambda ()
;                               (clj-refactor-mode 1)
;                               (yas-minor-mode)
;                               (paredit-mode)
;                               (cljr-add-keybindings-with-prefix "C-c C-m")))
;(add-hook 'cider-repl-mode-hook (lambda ())
;                               (eldoc-mode 1))
;;; set host to localhost instead of :: to fix IPv6 issues with OpenJDK
;(setq cider-lein-parameters "repl :headless :host localhost")

;;; SASS
(add-to-list 'auto-mode-alist '("\\.scss\\'" . sass-mode))
(add-hook 'sass-mode-hook (lambda () (rainbow-mode 1)))


;;; ERC
(setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))
 
;;; JavaScript
(setq js-indent-level 2)
 
;; TypeScript
(setq typescript-indent-level 2
      typescript-expr-indent-offset 2)

;; PureScript mode
(require 'purescript-mode)
;(require 'psc-ide)
(add-hook 'purescript-mode-hook
  (lambda ()
;    (psc-ide-mode)
;    (company-mode)
                                        ;    (flycheck-mode)
    (turn-on-purescript-indent)))
    

;(use-package parinfer
;  :ensure t
;  :bind
;  (("C-," . parinfer-toggle-mode))
;  :init
;  (progn
;    (setq parinfer-extensions
;          '(defaults       ; should be included.
;            pretty-parens  ; different paren styles for different modes.
;            evil           ; If you use Evil.
;            ;;lispy          ; If you use Lispy. With this extension, you should install Lispy and do not enable lispy-mode directly.
;            paredit        ; Introduce some paredit commands.
;            smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
;            smart-yank))   ; Yank behavior depend on mode.
;    (add-hook 'clojure-mode-hook #'parinfer-mode)
;    (add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
;    (add-hook 'common-lisp-mode-hook #'parinfer-mode)
;    (add-hook 'scheme-mode-hook #'parinfer-mode)
;    (add-hook 'lisp-mode-hook #'parinfer-mode)))

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
 '(custom-enabled-themes (quote (monokai))))
