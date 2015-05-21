;;; theme-pack.el --- Theme setup

;;; Commentary:

;;; Code:

(install-packages-pack/install-packs '(cyberpunk-theme
                                       solarized-theme
                                       gandalf-theme
                                       color-theme
                                       deferred
                                       dash
                                       smart-mode-line
                                       s))

(require 's)
(require 'smart-mode-line)
(setq sml/no-confirm-load-theme t)

(require 'deferred)
(require 'dash)

(require 'hl-line)
(require 'frame)

;; highlight the current line everywhere
(global-hl-line-mode 1)

;; death to scroll bar
(require 'scroll-bar)
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
       (font-size (if (string= hostname "dagobah") 105 80)))
  ;; (set-frame-parameter nil 'font "DejaVu Sans Mono-15")
  (set-frame-font "-unknown-DejaVu Sans Mono-normal-normal-normal-*-16-*-*-*-m-0-iso10646-1")
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

(defun theme-pack/apply (fn log)
  "Execute the function FN.
Display the LOG when done."
  (lexical-let ((msg log))
    (deferred:$
      (deferred:next
        'theme-pack/--disable-themes!)
      (deferred:nextc it
        fn)
      (deferred:nextc it
        (lambda ()
          (message (format "theme-pack - %s" msg)))))))

(defun theme-pack/--disable-themes! ()
  "Disable current enabled themes."
  (mapc 'disable-theme custom-enabled-themes))

(defun theme-pack/--load-theme (theme)
  "Disable currently enabled themes then load THEME."
  (load-theme theme 'no-confirm))

;;;###autoload
(defun theme-pack/light! ()
  "For outside."
  (interactive)
  (theme-pack/apply (lambda ()
                      (theme-pack/--load-theme 'solarized-light)
                      (sml/apply-theme 'light))
                    "Light theme installed!"))

;;;###autoload
(defun theme-pack/dark! ()
  "Default theme for the inside."
  (interactive)
  (theme-pack/apply (lambda ()
                      (theme-pack/--load-theme 'cyberpunk)
                      (sml/apply-theme 'dark))
                    "Dark theme installed!"))

;;;###autoload
(defun theme-pack/no-theme! ()
  "Revert to no theme."
  (interactive)
  (theme-pack/apply (lambda ()) "Reset theme done!"))

(theme-pack/dark!)

;; ######### define mode

(defvar theme-pack-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c t d") 'theme-pack/dark!)
    (define-key map (kbd "C-c t l") 'theme-pack/light!)
    (define-key map (kbd "C-c t r") 'theme-pack/no-theme!)
    map)
  "Keymap for theme-pack mode.")

(define-minor-mode theme-pack-mode
  "Minor mode to consolidate them-pack extensions.

\\{theme-pack-mode-map}"
  :lighter " TP"
  :keymap theme-pack-mode-map)

(define-globalized-minor-mode global-theme-pack-mode theme-pack-mode theme-pack-on)

(defun theme-pack-on ()
  "Turn on `theme-pack-mode'."
  (theme-pack-mode +1))

(global-theme-pack-mode)

(provide 'theme-pack)
;;; theme-pack.el ends here
