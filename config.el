;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Hong Jiang"
      user-mail-address "j@1byte.io")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))
(cond
 ((find-font (font-spec :family "FuraCode Nerd Font"))
  (setq doom-font
        (font-spec :family "FuraCode Nerd Font" :size 14 :weight 'light)))
 ((find-font (font-spec :family "Fira Code"))
  (setq doom-font
        (font-spec :family "Fira Code" :size 14 :weight 'medium))))

(dolist (charset '(kana han cjk-misc hangul kanbun bopomofo))
  (set-fontset-font (frame-parameter nil 'font) charset
                    (font-spec :family "Yuanti SC" :size 14) nil 'prepend))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-vibrant)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.


(custom-set-faces!
  '(doom-modeline-buffer-modified :foreground "orange"))

(defun doom-modeline-conditional-buffer-encoding ()
  "We expect the encoding to be LF UTF-8, so only show the modeline when this \
is not the case"
  (setq-local doom-modeline-buffer-encoding
              (unless (or (eq buffer-file-coding-system 'utf-8-unix)
                          (eq buffer-file-coding-system 'utf-8)))))

(add-hook 'after-change-major-mode-hook
          #'doom-modeline-conditional-buffer-encoding)

(when (featurep 'ns)
  (defun ns-raise-emacs ()
    "Raise Emacs."
    (ns-do-applescript "tell application \"Emacs\" to activate"))

  (defun ns-raise-emacs-with-frame (frame)
    "Raise Emacs and select the provided frame."
    (with-selected-frame frame
      (when (display-graphic-p)
        (ns-raise-emacs))))

  (add-hook 'after-make-frame-functions 'ns-raise-emacs-with-frame))


(keychain-refresh-environment)

(setq inhibit-compacting-font-caches t)

;; Programming

(use-package! paredit
  :hook (emacs-lisp-mode . paredit-mode))

;; Org mode

(defun orgfile (name)
  (concat "~/org/" name))

(defun gtdfile (name)
  (orgfile (concat "gtd/" name)))

(defconst gtd-inbox (gtdfile "inbox.org"))
(defconst gtd-mobile-inbox (gtdfile "mobile-inbox.org"))
(defconst gtd-projects (gtdfile "projects.org"))
(defconst gtd-tickler (gtdfile "tickler.org"))
(defconst gtd-someday (gtdfile "someday.org"))

(use-package! org
  :config
  (setq org-agenda-files (list gtd-inbox
                               gtd-mobile-inbox
                               gtd-projects
                               gtd-tickler))
  (setq org-capture-templates `(("t" "Todo [inbox]" entry
                                 (file+headline ,gtd-inbox "Tasks")
                                 "* TODO %i%?")
                                ("T" "Tickler" entry
                                 (file+headline ,gtd-tickler "Tickler")
                                 "* %i%? \n %U")))
  (setq org-refile-targets `((,gtd-projects :maxlevel . 3)
                             (,gtd-someday :level . 1)
                             (,gtd-tickler :maxlevel . 2)))
  (setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)"
                                      "CANCELLED(c)")))
  (setq org-html-doctype "html5")
  (setq org-html-html5-fancy t)
  (defun org-archive-finished-tasks ()
    "Archive all tasks in any finished state."
    (interactive)
    (org-map-entries
     (lambda ()
       (org-archive-subtree)
       (setq org-map-continue-from (org-element-property :begin (org-element-at-point))))
     "/DONE|CANCELLED" 'agenda))
  (setq org-ditaa-jar-path "/usr/local/Cellar/ditaa/0.11.0_1/libexec/ditaa-0.11.0-standalone.jar"))

(setq-default TeX-master nil)

(use-package! auctex-latexmk
  :config
  (auctex-latexmk-setup))

(use-package! js2-mode
  :config
  (setq js2-basic-offset 2))

(global-wakatime-mode)

;; Useful functions

(defun smart-split ()
  "Split the window into 100-column sub-windows."
  (interactive)
  (cl-labels ((smart-split-helper (w)
                                  (if (> (window-width w) 180)
                                      (let ((w2 (split-window w 100 t)))
                                        (smart-split-helper w2)))))
    (smart-split-helper nil)))

(defun move-file (new-location)
  "Write this file to NEW-LOCATION, and delete the old one."
  (interactive (list (expand-file-name
                      (if buffer-file-name
                          (read-file-name "Move file to: ")
                        (read-file-name "Move file to: "
                                        default-directory
                                        (expand-file-name (file-name-nondirectory (buffer-name))
                                                          default-directory))))))
  (when (file-exists-p new-location)
    (delete-file new-location))
  (let ((old-location (expand-file-name (buffer-file-name))))
    (message "old file is %s and new file is %s"
             old-location
             new-location)
    (write-file new-location t)
    (when (and old-location
               (file-exists-p new-location)
               (not (string-equal old-location new-location)))
      (delete-file old-location))))
