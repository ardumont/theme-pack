;;; theme-pack.el --- Theme setup

;;; Commentary:

;;; Code:

(install-packs '(cyberpunk-theme
                 solarized-theme
                 gandalf-theme
                 color-theme
                 dash))
(require 'dash)

(require 'hl-line)
(require 'frame)

;; highlight the current line everywhere
(global-hl-line-mode 1)

;; death to scroll bar
(set-scroll-bar-mode nil)

;; some text/font/color tweaks

(setq-default fill-column 120)
;; (set-face-background 'default "black")

(set-language-environment "UTF-8")
(blink-cursor-mode 1)

(defun theme-pack/hostname! ()
  "Return the hostname of the current computer."
  (-> "hostname" shell-command-to-string s-trim))

;; Depending on the hostname, will set a font or another
(let* ((hostname  (theme-pack/hostname!))
       (font-size (if (string= hostname "dagobah") 120 80)))
  ;; (set-frame-parameter nil 'font "DejaVu Sans Mono-15")
  (set-default-font "-unknown-DejaVu Sans Mono-normal-normal-normal-*-16-*-*-*-m-0-iso10646-1")
  ;; (x-list-fonts "*")
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

(defun theme-pack/--disable-themes! ()
  "Disable current enabled themes."
  (mapc 'disable-theme custom-enabled-themes))

(defun theme-pack/--load-theme (theme)
  "Disable currently enabled themes then load THEME."
  (theme-pack/--disable-themes!)
  (load-theme theme 'no-confirm))

;;;###autoload
(defun theme-pack/light! ()
  "For outside."
  (interactive)
  (theme-pack/--load-theme 'solarized-light))

;;;###autoload
(defun theme-pack/dark! ()
  "Default theme for the inside."
  (interactive)
  (theme-pack/--load-theme 'cyberpunk))

(theme-pack/dark!)

(provide 'theme-pack)
;;; theme-pack.el ends here
