(setq custom-safe-themes t)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (and custom-file
           (file-exists-p custom-file))
  (load custom-file nil :nomessage))


(load "C:/projects/crafted-emacs/modules/crafted-init-config")


(require 'crafted-completion-packages)
(require 'crafted-lisp-packages)
(require 'crafted-ide-packages)


(add-to-list 'package-selected-packages 'ef-themes)
(add-to-list 'package-selected-packages 'doom-themes)
(load-theme 'doom-henna)

(add-to-list 'package-selected-packages 'racket-mode)
;;(setq exec-path (append exec-path '("/usr/racket/bin/")))

(add-to-list 'package-selected-packages 'which-key)
(add-to-list 'package-selected-packages 'magit)

(package-install-selected-packages :noconfirm)

(which-key-mode)

(require 'crafted-defaults-config)
(require 'crafted-completion-config)
(require 'crafted-lisp-config)
(require 'crafted-ide-config)


;; Supercollider
(add-to-list 'load-path "C:/Users/nflynn/AppData/Local/SuperCollider/downloaded-quarks/scel/el")
(add-to-list 'package-selected-packages 'w3m)
(require 'sclang)

(if (eq system-type 'windows-nt)
    (setq exec-path (append exec-path
                            '("C:/Program Files/Git/usr/bin/"
                              "C:/Program Files/SuperCollider-3.13.0/"))))


(package-install-selected-packages :noconfirm)


;; Disable the compose-mail keybind
(global-unset-key (kbd "C-x m"))

;; Other window
(global-set-key (kbd "M-o") 'other-window)
;; Next/previous word
(global-set-key (kbd "M-n") 'forward-word)
(global-set-key (kbd "M-p") 'backward-word)


(setq inhibit-startup-message t)
(setq visible-bell t)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(set-fringe-mode 10)

(hl-line-mode 1)

(global-display-line-numbers-mode 1)

(dolist (mode '(org-mode-hook
		term-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


;; suppress indent messages
;;(setq aggressive-indent-region-function #'(lambda (x y) (let ((inhibit-message t)) (indent-region x y))))

(fset #'jsonrpc--log-event #'ignore)
;;(eglot-events-buffer-size 0)

(setq inferior-lisp-program "\"c:/Program Files/Steel Bank Common Lisp/sbcl.exe\"")






(provide 'init)
