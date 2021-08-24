;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Mark Olson"
      user-mail-address "hendryolson@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
(setq doom-font (font-spec :family "Source Code Pro" :size 16))

;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-one)
(setq doom-theme 'doom-nord)
;;(setq doom-theme 'doom-nord-light)
;;
;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/")
(setq org-roam-directory "~/Github/roam/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; Nicer Buffer names
(setq doom-fallback-buffer-name "► Doom"
      +doom-dashboard-name "► Doom")

;; Change the Window Title
(setq frame-title-format
      '(""
        (:eval
         (if (s-contains-p org-roam-directory (or buffer-file-name ""))
             (replace-regexp-in-string
              ".*/[0-9]*-?" "☰ "
              (subst-char-in-string ?_ ?  buffer-file-name))
           "%b"))
        (:eval
         (let ((project-name (projectile-project-name)))
           (unless (string= "-" project-name)
             (format (if (buffer-modified-p)  " ◉ %s" "  ●  %s") project-name))))))
;; General Settings
(setq undo-limit 80000000                         ; Raise undo-limit to 80Mb
      evil-want-fine-undo t                       ; By default while in insert all changes are one big blob. Be more granular
      auto-save-default t                         ; Nobody likes to loose work, I certainly don't
      truncate-string-ellipsis "…"                ; Unicode ellispis are nicer than "...", and also save /precious/ space
      password-cache-expiry nil                   ; I can trust my computers ... can't I?
      scroll-margin 4)                            ; It's nice to maintain a little margin
                                        ;
(display-time-mode 1)                             ; Enable time in the mode-line

(unless (string-match-p "^Power N/A" (battery))   ; On laptops...
  (display-battery-mode 1))                       ; it's nice to know how much power you have
;; Load whichkey a little faster
(setq which-key-idle-delay 0.5)
;; Setting up mac keyboard bindings
;; https://nickdrozd.github.io/2019/12/28/emacs-mac-mods.html
;;(setq ns-option-modifier nil
;;      ns-command-modifier 'meta
;;      ns-right-command-modifier 'control
;;      ns-control-modifier 'super
;;      ns-function-modifier 'hyper)
(setq mac-option-modifier nil
      mac-command-modifier 'meta
      mac-right-command-modifier 'control
      mac-control-modifier 'super
      mac-function-modifier 'hyper)
;; =-=
;; KEYBINDINGS
;; =-=
;;(global-set-key (kbd "M-g") 'hippie-expand)
(global-set-key (kbd "M-å") 'sp-wrap-curly)
(global-set-key (kbd "M-ö") 'sp-up-sexp)
(global-set-key (kbd "M-w") 'save-buffer)
;;(setq-default evil-escape-key-sequence "öö")
;; =-=
;; PYTHON
;; =-=
(setq python-shell-interpreter "/usr/local/bin/python3")
(setq py-python-command "/usr/local/bin/python3")
; LaTeX
(setq TeX-save-query nil
      ;;TeX-show-compilation t
      TeX-command-extra-options "-shell-escape")
(after! latex
  (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t)))
(setq +latex-viewers '(skim preview))
; TeX > using xeLaTeX
;; (eval-after-load "tex"
;;   '(add-to-list 'TeX-command-list
;;                 '("XeLaTeX" "xelatex -interaction=nonstopmode %s"
;;                   TeX-run-command t t :help "Run xelatex") t))
(after! tex
  (map!
   :map LaTeX-mode-map
   :ei [C-return] #'LaTeX-insert-item)
  (setq TeX-electric-math '("\\(" . "")))
;; Making \( \) less visible
(defface unimportant-latex-face
  '((t :inherit font-lock-comment-face :weight extra-light))
  "Face used to make \\(\\), \\[\\] less visible."
  :group 'LaTeX-math)

(map! :map LaTeX-mode-map
      :localleader                  ; Use local leader
      :desc "View" "v" #'TeX-view ; Add which-key description
      :desc "Preview pane" "p" #'latex-preview-pane-mode
      :desc "Update preview" "u" #'latex-preview-pane-update
      :desc "Compile" "c" #'TeX-command-master
      :desc "Run all" "r" #'TeX-command-run-all
      :desc "Environment" "e" #'LaTeX-environment
      )
  ;; Viewers
(setq TeX-view-program-list
        '(("Preview" "/usr/bin/open -a Preview.app %o")
          ("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline -r -b %n %o %b")))
(setq TeX-view-program-selection
        '((output-dvi "Skim") (output-pdf "Skim") (output-html "open")));;
(setq TeX-source-correlate-mode t)
(setq TeX-source-correlate-start-server t)
(setq TeX-source-correlate-method 'synctex)
  ;; ;; Packages
;;
;; Auto Activating Snippets
;;
(use-package aas
  :hook (LaTeX-mode . aas-activate-for-major-mode)
  :hook (org-mode . aas-activate-for-major-mode)
  :config
  (aas-set-snippets 'text-mode
                    ;; expand unconditionally
                    "o-" "ō"
                    "i-" "ī"
                    "a-" "ā"
                    "u-" "ū"
                    "e-" "ē")

  ;; disable snippets by redefining them with a nil expansion
  (aas-set-snippets 'latex-mode
                    "supp" nil))
(use-package laas
  :hook (LaTeX-mode . laas-mode)
  :config ; do whatever here
  (aas-set-snippets 'laas-mode
                    ;; set condition!
                    :cond #'texmathp ; expand only while in math
                    "On" "O(n)"
                    "O1" "O(1)"
                    "Olog" "O(\\log n)"
                    "Olon" "O(n \\log n)"
                    ;; bind to functions!
                    "Sum" (lambda () (interactive)
                            (yas-expand-snippet "\\sum_{$1}^{$2} $0"))
                    "jf" (lambda () (interactive)
                            (yas-expand-snippet "\\\( $0 \\\)"))
                    ;; add accent snippets
                    :cond #'laas-object-on-left-condition
                    "qq" (lambda () (interactive) (laas-wrap-previous-object "sqrt"))))
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
