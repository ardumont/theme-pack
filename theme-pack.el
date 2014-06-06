;;; theme-pack.el --- Theme setup

;;; Commentary:

;;; Code:

(install-packs '(cyberpunk-theme
                 solarized-theme
                 gandalf-theme
                 color-theme
                 dash))

(require 'hl-line)

;; highlight the current line everywhere
(global-hl-line-mode 1)

;; death to scroll bar
(set-scroll-bar-mode nil)

;; some text/font/color tweaks

(set-frame-parameter nil 'font "DejaVu Sans Mono-12")
;; (x-list-fonts "*")

(setq-default fill-column 120)
;; (set-face-background 'default "black")

(set-language-environment "UTF-8")
(blink-cursor-mode 1)

(require 'dash)

(defun buffer-pack/hostname! ()
  "Return the hostname of the current computer."
  (-> "hostname" shell-command-to-string s-trim))

;; Depending on the hostname, will set a font or another
(let* ((hostname  (buffer-pack/hostname!))
       (font-size (if (string= hostname "dagobah") 120 100)))
  (set-face-attribute 'default nil :height font-size))

;;; dark theme
;; (load-theme 'solarized-dark 'no-confirm)
;; (load-theme 'cyberpunk 'no-confirm)
;; (load-theme 'misterioso 'no-confirm)

;;; grey theme
;; (load-theme 'zenburn 'no-confirm)

;;; light themes
;; (load-theme 'gandalf 'no-confirm)
;; (load-theme 'adwaita 'no-confirm)
;; (load-theme 'solarized-light 'no-confirm)

(require 'color-theme)

;;;###autoload
(defun theme-pack/light! ()
  "For outside."
  (interactive)
  (load-theme 'solarized-light 'no-confirm))

;;;###autoload
(defun theme-pack/dark! ()
  "Default theme for the inside."
  (interactive)
  (load-theme 'cyberpunk 'no-confirm))

(theme-pack/dark!)

(provide 'theme-pack)
;;; theme-pack.el ends here
