;;; theme-pack.el --- Theme setup

;;; Commentary:

;;; Code:

(require 's)
(require 'deferred)
(require 'dash)
(require 'powerline)
(require 'hl-line)
(global-hl-line-mode 1)

(require 'scroll-bar)
;; death to scroll bar
(set-scroll-bar-mode nil)

(set-language-environment "UTF-8")
(require 'frame)
(blink-cursor-mode 1)
(require 'whitespace)
(custom-set-variables '(blink-cursor-blinks 0) ;; <= 0 blinks forever, otherwise stops after `'10`'
                      '(cursor-type 'bar)           ;; box, hollow, hbar, bar
		      '(whitespace-line-column 79) ;; limit line length
                      '(whitespace-style '(face tabs empty trailing lines-tail)))

;; disable menu bar
(menu-bar-mode -1)
;; disable scrollbar
(toggle-scroll-bar -1)
;; disable toolbar
(tool-bar-mode -1)

;;;###autoload
(defun theme-pack-set-size (&optional font-size-input)
  "Depending on the hostname, will set the optional FONT-SIZE-INPUT.
ARGS With universal argument, can force the font-size to the input value."
  (interactive "P")
  (let ((font-size (if font-size-input font-size-input 100)))
    (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono"))
    ;; (set-face-attribute 'default nil :font "DejaVu Sans Mono-10")
    ;; (set-face-attribute 'mode-line nil :font "DejaVu Sans Mono-8")
    ;; (set-frame-parameter nil 'font "DejaVu Sans Mono-16")
    ;; (set-frame-font "xft:-PfEd-DejaVu Sans Mono-normal-normal-normal-*-16-*-*-*-m-0-iso10646-1")
    (set-face-attribute 'default nil :height font-size)
    (set-face-attribute 'mode-line nil :height (- font-size 20))))

(defvar theme-pack-default-make-frame-function after-make-frame-functions
  "Keep the original functions from after-make-frame-functions variables")

(defun theme-pack--install (fn log)
  "Execute the function FN (install theme function).
Display the LOG when done."
  (lexical-let ((msg log)
		(theme-fn fn))
    (deferred:$
      (deferred:next 'theme-pack--disable-themes)  ;; deactivate previous theme
      (deferred:nextc it theme-fn)                 ;; apply theme
      (deferred:nextc it
	(lambda ()
	  ;; remove any prior installed theme functions (frame creation routine)
	  (setq after-make-frame-functions theme-pack-default-make-frame-function)
	  ;; install new one to draw with the right theme each frame (x or no)
	  (add-hook 'after-make-frame-functions (lambda (&optional frame) (funcall theme-fn)))))
      (deferred:nextc it
	(lambda ()
	  (message (format "theme-pack - %s" msg)))))))

(defun theme-pack--disable-themes ()
  "Disable current enabled themes."
  (mapc 'disable-theme custom-enabled-themes))

(defun theme-pack--load-theme (theme)
  "Disable currently enabled themes then load THEME."
  (load-theme theme 'no-confirm))

;;;###autoload
(defun theme-pack-light ()
  "For outside."
  (interactive)
  (theme-pack--install (lambda ()
			 (theme-pack--load-theme 'spacemacs-light))
		       "Light theme installed!"))

;;;###autoload
(defun theme-pack-dark ()
  "Default theme for the inside."
  (interactive)
  (theme-pack--install (lambda ()
			 (theme-pack--load-theme 'spacemacs-dark))
		       "Dark theme installed!"))

;;;###autoload
(defun theme-pack-no-theme ()
  "Revert to no theme."
  (interactive)
  (theme-pack--install (lambda ()) "Reset theme done!"))

;; ######### define mode

(defvar theme-pack-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c t d") 'theme-pack-dark)
    (define-key map (kbd "C-c t l") 'theme-pack-light)
    (define-key map (kbd "C-c t r") 'theme-pack-no-theme)
    (define-key map (kbd "C-c t s") 'theme-pack-set-size)
    map)
  "Keymap for theme-pack mode.")

(define-minor-mode theme-pack-mode
  "Minor mode to consolidate them-pack extensions.

\\{theme-pack-mode-map}"
  :lighter " TP"
  :keymap theme-pack-mode-map
  :global t)

;;;###autoload
(define-globalized-minor-mode global-theme-pack-mode theme-pack-mode theme-pack-on)

(defun theme-pack-on ()
  "Turn on `theme-pack-mode'."
  (theme-pack-mode +1))

(provide 'theme-pack)
;;; theme-pack.el ends here
