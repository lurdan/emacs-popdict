;;; popdict.el -- Consult various dictionaries and show result using tooltip

;; Copyright (C) 2015 KURASHIKI Satoru

;; Filename: popdict.el
;; Description: Consult various dictionaries and show result using tooltip
;; Author: KURASHIKI Satoru
;; Created:
;; Version:
;; Package-Requires:
;; Keywords: dictionary
;; URL:

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

;; TODO: README.org 作る
;; TODO: DOCSTRING ちゃんと書く

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: とりあえず defvar ですませているものを defgroup と defcustom に整理

(defvar popdict-timeout-auto 5
  "Timeout of tooltip for automatic popup (in seconds).
See `pos-tip-show' for details.")

(defvar popdict-timeout-man 0
  "Timeout of tooltip for manual popup (in seconds).
See `pos-tip-show' for details.")

(defvar popdict-max-width 80
  "Maximum width of tooltip.  nil means use display width.")

(defface popdict
  '((t
     :foreground "white"
     :background "RoyalBlue4"))
  "Face for description in tooltip.")

(defface popdict-entry
  '((t
     :foreground "cyan"
     :bold t
     :inherit popdict))
  "Face for entry in tooltip.")

(defvar popdict-search-backend 'popdict-search-backend-lookup)

;; TODO: バックエンドを追加する

;; (defun popdict-search-backend-rdictcc (word)
;;   ""
;;   (require 'rdictcc)
;;   (rdictcc-translate-word-to-string word)
;;   )

;; (defun popdict-search-backend-sdic ()
;;   ""
;;   (require 'sdic)
;;   )

;; (defun popdict-search-backend-voca-builder (word)
;;   ""
;;   (require 'voca-builder)
;;   (voca-builder/fetch-meaning word)
;;   )

;; TODO: lookup2 がない場合はスルーできるように？
(eval-when-compile (require  'lookup-autoloads))

;; WISH: モジュールでなく辞書を選択できるように
(defvar popdict-search-backend-lookup-module "default")
(defun popdict-search-backend-lookup (word)
  ""
  (let ((current-prefix-arg nil)
        (entry (car (lookup-dictionary-search
                     (car (lookup-module-dictionaries (lookup-get-module popdict-search-backend-lookup-module)))
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
;; (let ((popdict-search-backend-lookup-module "english")) (popdict-search-backend-lookup "test"))


;; TODO: ちゃんと実装
;; org-capture を使って検索結果を記録
;; 検索結果を文字列展開した上でテンプレートを動的に設定する感じ？
(defvar popdict-auto-capture nil)
(defvar popdict-capture-template "apropriate default template")
(defun popdict-capture ()
  ""
  (interactive)
  (let ((org-capture-templates popdict-capture-template))
    (org-capture nil "")
    )
  )




;; TODO: autolookup できるマイナーモードを追加する

;; WISH: 検索結果を自動でクリップボードに入れられると嬉しい？
;;(defvar popdict-auto-kill-ring nil)

;;文字列を受け取ってバックエンド関数に投げ、検索結果とのコンスセルを返す
;;自動キャプチャが有効ならついでに実行
(defun popdict-search (item)
  ""
  (let ((result (funcall popdict-search-backend item)))
    (if popdict-auto-capture
        (popdict-capture))
    result
    ))

;; WISH: 実装
;; autolookup 相当の挙動
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

;; カーソル位置の単語を拾って辞書を検索し、結果を適切な機能で表示する
;; stem-english 的な処理を自前でやる？ バックエンドに任せる？
;; sdic-inline-pos-tip からの複数エントリ処理をどうする？
(defun popdict (&optional entry)
  ""
  (interactive)
  (if (called-interactively-p 'any)
      (setq entry (thing-at-point 'word)))
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
