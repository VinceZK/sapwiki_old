;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-enabled-themes (quote (misterioso)))
 '(org-export-show-temporary-export-buffer nil)
 '(org-export-with-sub-superscripts (quote {}))
 '(org-html-table-align-individual-fields nil)
 '(org-html-text-markup-alist
   (quote
    ((bold . "<b>%s</b>")
     (code . "<code>%s</code>")
     (italic . "<i>%s</i>")
     (strike-through . "<s>%s</s>")
     (underline . "<u>%s</u>")
     (verbatim . "<em>%s</em>"))))
 '(org-startup-indented t)
 '(org-use-sub-superscripts (quote {}))
 '(package-enable-at-startup nil)
 '(send-mail-function (quote mailclient-send-it))
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; MELPA setup
;; (require 'package)
;; (add-to-list 'package-archives
;;             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;; on to the visual settings
(setq inhibit-splash-screen t)          ; no splash screen, thanks
(line-number-mode 1)                    ; have line numbers and
(column-number-mode 1)                  ; column numbers in the mode line
(global-hl-line-mode)                   ; highlight current line
(global-linum-mode 1)                   ; add line numbers on the left

;; slime setup
;(setq inferior-lisp-program "/usr/local/bin/sbcl")
;(add-to-list 'load-path "~/.emacs.d/slime-2.15/")
;(require 'slime)
;(slime-setup)

; color theme setup 
(add-to-list 'custom-theme-load-path "~/.emacs.d/emacs-color-theme-solarized/")
(load-theme 'solarized t)

; set the font
(set-face-attribute 'default nil :font "Monaco")
(set-face-attribute 'default nil :height 160)
(set-fontset-font "fontset-default" 'han '("STHeiti"))

;; el-get setup
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))
;; set local recipes
(setq
el-get-sources
'((:name buffer-move               ; have to add your own keys
       :after (progn
             (global-set-key (kbd "<C-S-up>")     'buf-move-up)
             (global-set-key (kbd "<C-S-down>")   'buf-move-down)
             (global-set-key (kbd "<C-S-left>")   'buf-move-left)
             (global-set-key (kbd "<C-S-right>")  'buf-move-right)))

   (:name smex                    ; a better (ido like) M-x
       :after (progn
             (setq smex-save-file "~/.emacs.d/.smex-items")
             (global-set-key (kbd "M-x") 'smex)
             (global-set-key (kbd "M-X") 'smex-major-mode-commands)))

   (:name magit                    ; git meet emacs, and a binding
       :after (progn
		(global-set-key (kbd "C-x C-z") 'magit-status)))
   
   (:name goto-last-change		; move pointer back to last change
	  :after (progn
		   ;; when using AZERTY keyboard, consider C-x C-_
		   (global-set-key (kbd "C-x C-/") 'goto-last-change)))))
;; now set our own packages
(setq
my:el-get-packages
'(el-get                    ; el-get is self-hosting
  escreen                   ; screen for emacs, C-\ C-h
  switch-window             ; takes over C-x o
  org-ac                    ; support auto complete in org mode
  auto-complete             ; complete as you type with overlays
  yasnippet                 ; powerful snippet mode
  ))
;; now initialize packages
(setq my:el-get-packages
      (append
       my:el-get-packages
       (loop for src in el-get-sources collect (el-get-source-name src))))
;; install new packages and init already installed packages
(el-get 'sync my:el-get-packages)

;; copy/paste with C-c and C-v and C-x, check out C-RET too
;; (cua-mode)

;; Use the clipboard, pretty please, so that copy/paste "works"
(setq select-enable-clipboard t)

;; Navigate windows with M-<arrows>
(windmove-default-keybindings 'meta)
(setq windmove-wrap-around t)

; winner-mode provides C-<left> to get back to previous window layout
(winner-mode 1)

;; whenever an external process changes a file underneath emacs, and there
;; was no unsaved changes in the corresponding buffer, just revert its
;; content to reflect what's on-disk.
(global-auto-revert-mode 1)

;; M-x shell is a nice shell interface to use, let's make it colorful.  If
;; you need a terminal emulator rather than just a shell, consider M-x term
;; instead.
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; org mode setup
;(package-initialize)
(org-ac/config-default) ;auto-complete
(add-hook 'org-mode-hook 'toggle-truncate-lines)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; elisp setup
(global-set-key (kbd "C-<") 'comment-region)
(global-set-key (kbd "C->") 'uncomment-region)
(global-set-key "\C-cc" 'emacs-lisp-byte-compile-and-load)

;; dk&sapwiki html backend
(add-to-list 'load-path "~/.emacs.d/sapwiki/")
(require 'ox-dk-html)
(require 'sapwiki)
(setq org-publish-project-alist
       '(("blog"
          :base-directory "~/workspace/javascript/darkhouse/app/orgNote/"
          :base-extension "org"
          :publishing-directory "~/workspace/javascript/darkhouse/app/blogs/"
          :recursive t
          :publishing-function dk-html-publish-to-html
          :headline-levels 4
          :section-numbers nil
          :auto-preamble nil
          :auto-sitemap t            ; Generate sitemap.org automagically...
          :sitemap-filename "sitemap.org"  ; ... call it sitemap.org
          :sitemap-title "Sitemap"         ; ... with title 'Sitemap'.
   	  :html-doctype "html5"
  	  :html-html5-fancy t
 	  :html-indent t
 	  :html-preamble nil
 	  :html-postamble nil
 	  :with-author nil
 	  :with-date nil
 	  :with-title nil
 	  :with-footnotes nil
 	  :with-toc nil)
	 ("sapwiki"
	  :base-directory "~/.emacs.d/sapwiki/work/"
	  :base-extension "org"
	  :publishing-directory "~/.emacs.d/sapwiki/pub/"
	  :recursive t
	  :publishing-function dk-html-publish-to-sapwiki
	  :headline-levels 4
	  :section-number t
	  :auto-preamble nil
	  :auto-sitemap nil
	  :html-doctype "xhtml5"
	  :html-html5-fancy t
 	  :html-indent t
 	  :html-preamble nil
 	  :html-postamble nil
	  :html-table-header-tags ("<th><p>" . "</p></th>")
	  :html-table-data-tags ("<td><p>" . "</p></td>")
 	  :with-author nil
 	  :with-date nil
 	  :with-title nil
 	  :with-footnotes nil	  
	  :with-toc nil)))

;; Etags to get *.gz files
(require 'jka-compr) 
