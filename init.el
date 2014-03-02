;;; theme-pack.el --- Theme setup

;;; Commentary:

;;; Code:

(install-packs '(cyberpunk-theme
                 solarized-theme
                 gandalf-theme
                 dash))

;; some text/font/color tweaks

(setq-default fill-column 120)
;; (set-face-background 'default "black")

(set-language-environment "UTF-8")
(blink-cursor-mode 1)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(require 'dash)

(defun buffer-pack/hostname! () "Return the hostname of the current computer." (-> "hostname" shell-command-to-string s-trim))

;; Depending on the hostname, will set a font or another
(let* ((hostname  (buffer-pack/hostname!))
       (font-size (if (string= hostname "dagobah") 140 100)))
  (set-face-attribute 'default nil :height font-size))

;; (disable-them 'zenburn)
;; (load-theme 'zenburn t)
;; (load-theme 'solarized-dark t)
(load-theme 'cyberpunk t)
;; (load-theme 'gandalf t)


;;; theme-pack.el ends here
