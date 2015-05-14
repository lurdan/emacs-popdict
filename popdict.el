;;; popdict.el --- Consult various dictionaries and show result using tooltip

;; Copyright (C) 2015 KURASHIKI Satoru

;; Filename: popdict.el
;; Description: Consult various dictionaries and show result using tooltip
;; Author: KURASHIKI Satoru
;; Created: 2015-04-23
;; Version: 0.0.1
;; Package-Requires: ((pos-tip "0.4.6"))
;; URL: https://github.com/lurdan/emacs-popdict
;; Keywords: dictionary

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.

;; It is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:

;;; Code:

(defconst popdict-version "0.0.1")

(require 'pos-tip)
(require 'url-http)

;; TODO: README.org 作る
;; TODO: DOCSTRING ちゃんと書く

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup popdict nil
  "Popdict group"
  :group 'applications)

(defcustom popdict-mode-delay 0.80
  ""
  :type 'int
  :group 'popdict)

;; lookup が動かないのでひとまず weblio をデフォルトに
(defcustom popdict-search-backend 'popdict-backend-weblio
  "dictionary backend function to use for searching."
  :type 'string
  :group 'popdict)

(defcustom popdict-capture-auto nil
  ""
  :type 'boolean
  :group 'popdict)

;; for pos-tip
(defcustom popdict-timeout-auto 5
  "Timeout of tooltip for automatic popup (in seconds).
See `pos-tip-show' for details."
  :type 'int
  :group 'popdict)

(defcustom popdict-timeout-man 0
  "Timeout of tooltip for manual popup (in seconds).
See `pos-tip-show' for details."
  :type 'int
  :group 'popdict)

(defcustom popdict-max-width 80
  "Maximum width of tooltip.  nil means use display width."
  :type 'int
  :group 'popdict)

(defface popdict
  '((t
     :foreground "white"
     :background "RoyalBlue4"))
  "Face for description in tooltip."
  :group 'popdict)

(defface popdict-entry
  '((t
     :foreground "cyan"
     :bold t
     :inherit popdict))
  "Face for entry in tooltip."
  :group 'popdict)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; popdict minor mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar popdict-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "C-c @ C-c") 'popdict)
    keymap))

(define-minor-mode popdict-mode
  "popdict-mode. Display the meaning of word under the point."
  :keymap popdict-map
  :lighter " Popdict"
  (if popdict-mode
      (popdict-start-timer)
    (popdict-stop-timer)))

(defvar popdict-timer nil)
(defun popdict-start-timer ()
  "start timer."
  (when popdict-timer
    (cancel-timer popdict-timer))
  (setq popdict-timer
        (run-with-idle-timer popdict-mode-delay t 'popdict-hook)))

(defun popdict-stop-timer ()
  "stop timer"
  (when popdict-timer
    (cancel-timer popdict-timer)
    (setq popdict-timer nil)))

(defun popdict-hook ()
  "running or not running `popdict'."
  (cond
   ((not (minibufferp))
    (condition-case err
        (progn
          (unless popdict-mode
            (setq popdict-mode t))
          (popdict))
      (error
       (unwind-protect
           (message "Error: %S; popdict-mode now disabled." err)
         (setq popdict-mode nil)))))
   (t
    (setq popdict-mode nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Online Backends
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Weblio backend
(defvar popdict-backend-weblio-url "http://api.weblio.jp/act/quote/v_1_0/e/%s")
(defun popdict-backend-weblio-parse (buf)
  (let* ((xml (with-current-buffer buf
                (set-buffer-multibyte t)
                (goto-char url-http-end-of-headers)
                (libxml-parse-html-region (point) (point-max))))
         (defs (cddr (nth 11 (nth 5 (cadr (cddr (nth 4 (assq 'body xml))))))))
         (result (mapcar (lambda (x) (if (funcall 'stringp x) x "\n")) defs))
         )
    (kill-buffer buf)
    (replace-regexp-in-string (rx (or (: bos (* (any "\r")))
                                      ;;(: (* (any " \t\n")) eos)
                                      )) "" (mapconcat 'identity result ""))
    ))

(defun popdict-backend-weblio (word)
  "Search weblio.jp"
  (let* ((buf (let ((url-mime-charset-string "utf-8")
                    (url (format popdict-backend-weblio-url (url-hexify-string word))))
                (url-retrieve-synchronously url t)))
         (result (popdict-backend-weblio-parse buf)))
    result))

;; voca-builder (dictionary.com) backend
(when (require 'voca-builder nil t)
  (defun popdict-backend-voca-builder (word)
    "Search dictionary.com using voca-builder."
    (cdr (voca-builder/fetch-meaning word))
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Offline Backends
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; lookup backend
;; TODO: lookup2 がない場合はスルーできるように？
(when (require 'lookup-autoloads nil t)
  ;; WISH: モジュールでなく辞書を選択できるように
  (defvar popdict-backend-lookup-module "default")
  (defun popdict-backend-lookup (word)
    ""
    (let ((current-prefix-arg nil)
          (entry (car (lookup-dictionary-search
                       (car (lookup-module-dictionaries (lookup-get-module popdict-backend-lookup-module)))
                       (lookup-parse-pattern word)))))
      (with-current-buffer (lookup-get-buffer (lookup-content-buffer))
        (let ((inhibit-read-only t))
          (erase-buffer)
          (let ((content (lookup-get-property entry 'content)))
            (if (and content lookup-enable-format (not lookup-force-update))
                (insert content)
              (lookup-with-message (format "Inserting `%s'" (lookup-entry-heading entry))
                                   (insert (lookup-entry-content entry))
                                   (when lookup-enable-format
                                     (lookup-arrange-content entry)
                                     (lookup-put-property entry 'content (buffer-string)))))
            (if lookup-enable-format (lookup-adjust-content entry))
            )
          ;; arrange functions might change the buffer mode
          (lookup-content-mode)
          (set-buffer-modified-p nil))
                                        ;      (setq lookup-content-entry entry)
                                        ;      (setq lookup-content-line-heading (lookup-entry-heading entry))
        (cons word (let ((beg (progn (forward-line 1) (point)))
                         (end (progn (end-of-buffer) (previous-line 3) (point))))
                     (buffer-substring beg end)))
        )))
  ;; (let ((popdict-backend-lookup-module "english")) (popdict-backend-lookup "test"))
  )

;; rdictcc (dict.cc) backend
(when (require 'rdictcc nil t)
  (defun popdict-backend-rdictcc (word)
    "Search `word' using rdictcc.el"
    (rdictcc-translate-word-to-string word)
    )
  )

;; sdic backend
;; (when (require 'sdic nil t)
;;   (defun popdict-backend-sdic ()
;;     ""
;;     ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto org-capture
;; TODO: ちゃんと実装
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'org-capture)
;; 検索結果を文字列展開した上でテンプレートを動的に設定する感じ？
(defvar popdict-capture-template "apropriate default template")
(defun popdict-capture ()
  ""
  (interactive)
  (unless popdict-mode
    (let ((org-capture-templates popdict-capture-template))
      (org-capture nil "")
      )
    )
  )

;; WISH: 検索結果を自動でクリップボードに入れられると嬉しい？
;;(defvar popdict-auto-kill-ring nil)

;;文字列を受け取ってバックエンド関数に投げ、検索結果とのコンスセルを返す
;;自動キャプチャが有効ならついでに実行
(defvar popdict-last-word "")
(defvar popdict-last-desc "")
(defun popdict-search (item)
  ""
  (if (string= popdict-last-word item)
      (cons popdict-last-word popdict-last-desc)
    (let ((result (funcall popdict-search-backend item)))
      (if popdict-capture-auto
          (popdict-capture))
      (setq popdict-last-word item
            popdict-last-desc result)
      (cons item result)
      )
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun popdict-minibuffer (entry)
  ""
  )

;; WISH: うまく動かないので対応
(defun popdict-popup (entry)
  ""
  (interactive)
  (let* (lst
        (targets (split-string entry "\n"))
        (len (length targets)))
    (dolist (item targets)
      (let ((result (popdict-search item)))
        (setq lst (cons (list (car result)
                              (cdr result))
                        lst))))
    (cond
     ((> len 1)
      (message "Select item to show detail.")
      (popup-tip (popup-cascade-menu (nreverse lst))))
     ;;        (popup-cascade-menu lst))
     ((= len 1)
      (popup-tip (nth 1 (car lst))))
     (t
      nil))))
;;(popdict-popup "searcher\nsearch")

(defun popdict-popup-pos-tip (entry)
  ""
  ;; Use the same font as selected frame in tooltip
  (set-face-font 'popdict (frame-parameter nil 'font))
  ;; Main part
  (let ((width 0)
        (height 0)
        ;; "\n" should be propertized by the same face as the text
        ;; because their height also affect tooltip height.
        (nl-head (propertize "\n" 'face 'popdict-entry))
        (nl-desc (propertize "\n" 'face 'popdict)))
    (pos-tip-show-no-propertize
     ;; Arrange string
     (mapconcat
      (lambda (item)
        (let* ((result (popdict-search item))
               (head (car result))
               ;; Split and justify the description if longer than max-width
               (desc (pos-tip-fill-string (cdr result)
                                          popdict-max-width
                                          1 'full))
               (w-h (pos-tip-string-width-height desc)))
          ;; Calculate tooltip width and height
          (setq width (max width (string-width head) (car w-h))
                height (+ height 1 (cdr w-h)))
          ;; Propertize entry string by appropriate faces
          (concat (propertize head 'face 'popdict-entry)
                  nl-head
                  (propertize desc 'face 'popdict))))
      (split-string entry "\n") nl-desc)
     ;; Face which specifies tooltip's background color
     'popdict
     ;; Display current point, then omit POS and WINDOW
     nil nil
     ;; Timeout
     (if (called-interactively-p 'any)
         popdict-timeout-man
       popdict-timeout-auto)
     ;; Calculate tooltip's pixel size
     (pos-tip-tooltip-width width (frame-char-width))
     (pos-tip-tooltip-height height (frame-char-height))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interactive commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun popdict-switch-backend ()
  (interactive)
  )

;; stem-english 的な処理を自前でやる？ バックエンドに任せる？
;; sdic-inline-pos-tip からの複数エントリ処理をどうする？

;;;###autoload
(defun popdict (&optional entry)
  "カーソル位置の単語を拾って辞書を検索し、結果を適切な機能で表示する"
  (interactive)
;;  (if (called-interactively-p 'any)
  (setq entry (thing-at-point 'word))
;;  )
  (when entry
    (cond
     (window-system
      (popdict-popup-pos-tip entry))
     ((fboundp 'popup-cascade-menu)
      (popdict-popup entry))
     (t
      (popdict-minibuffer entry))
     )))

(provide 'popdict)

;;; popdict.el ends here
