;;; eglot-semantic-token.el -- eglot semantic tokens support -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2023 Free Software Foundation, Inc.

;; Version: 0
;; Author: João Távora <joaotavora@gmail.com>
;; Maintainer: EOWNERDEAD <eownerdead@disroot.org>
;; URL: https://codeberg.org/eownerdead/eglot-semantic-tokens
;; Keywords: convenience, languages
;; Package-Requires: ((emacs "26.3") (eglot "1.9"))

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with This program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Most of this was taken from the pull request
;; https://github.com/joaotavora/eglot/pull/839 by Akib Azmain Turja.
;; However, it won't be merged for over a year because of problems related to
;; gnushit copyright assignments ;-(

;;; Code:

(require 'eglot)
(require 'text-property-search)

(cl-defmethod eglot-client-capabilities :around (_s)
  "Extend for semantic tokens support."
  (if (not eglot--semantic-tokens-mode)
      (cl-call-next-method)
    (let ((cap (copy-tree (cl-call-next-method))))
      (let* ((ws (plist-get cap :workspace))
             (new-ws (plist-put ws :semanticTokens
                                `(:refreshSupport :json-false))))
        (setq cap (plist-put cap :workspace new-ws)))
      (let* ((td (plist-get cap :textDocument))
             (new-td (plist-put td :semanticTokens
                                (list :dynamicRegistration :json-false
                                      :requests `(:full t :range t)
                                      :tokenTypes
                                      (eglot--semantic-supported-token-types)
                                      :tokenModifiers
                                      (eglot--semantic-supported-token-modifiers)
                                      :formats ["relative"]
                                      :overlappingTokenSupport :json-false
                                      :multilineTokenSupport t))))
        (setq cap (plist-put cap :textDocument new-td))))))

;;; Semantic Tokens
;;;
(defcustom eglot-enable-semantic-tokens nil
  "If non-nil, enable semantic token highlighting.
Reconnect to server for changes to take effect."
  :type 'boolean
  :safe #'booleanp)

(defcustom eglot-semantic-tokens-use-delta t
  "If non-nil, request delta of tokens from server instead of full token list.
Setting this to t will speed up token processing and highlighting, but
may cause highlighting to be inaccurat:e.  The value is ignored when
the server doesn't support it."
  :type 'boolean
  :safe #'booleanp)

(defcustom eglot-semantic-token-faces
  '(("comment" . font-lock-comment-face)
    ("keyword" . font-lock-keyword-face)
    ("string" . font-lock-string-face)
    ("number" . font-lock-constant-face)
    ("regexp" . font-lock-string-face)
    ("operator" . font-lock-function-name-face)
    ("namespace" . font-lock-keyword-face)
    ("type" . font-lock-type-face)
    ("struct" . font-lock-type-face)
    ("class" . font-lock-type-face)
    ("interface" . font-lock-type-face)
    ("enum" . font-lock-type-face)
    ("typeParameter" . font-lock-type-face)
    ("function" . font-lock-function-name-face)
    ("method" . font-lock-function-name-face)
    ("member" . font-lock-variable-name-face)
    ("field" . font-lock-variable-name-face)
    ("property" . font-lock-variable-name-face)
    ("event" . font-lock-variable-name-face)
    ("macro" . font-lock-preprocessor-face)
    ("variable" . font-lock-variable-name-face)
    ("parameter" . font-lock-variable-name-face)
    ("label" . font-lock-comment-face)
    ("enumConstant" . font-lock-constant-face)
    ("enumMember" . font-lock-constant-face)
    ("dependent" . font-lock-type-face)
    ("concept" . font-lock-type-face))
  "Alist of faces to use to highlight semantic tokens.
Each element is a cons cell whose car is a token type name and cdr is
the face to use."
  :type `(alist :key-type (string :tag "Token name")
                :value-type (choice (face :tag "Face")
                                    (plist :tag "Face Attributes"
                                           :key-type
                                           (choice
                                            ,@(mapcar
                                               (lambda (cell)
                                                 `(const :tag ,(capitalize
                                                                (cdr cell))
                                                         ,(car cell)))
                                               face-attribute-name-alist))))))

(defcustom eglot-semantic-token-modifier-faces
  '(("declaration" font-lock-type-face)
    ("definition" font-lock-function-name-face)
    ("implementation" font-lock-function-name-face)
    ("readonly" font-lock-constant-face)
    ("static" font-lock-keyword-face)
    ("abstract" font-lock-keyword-face)
    ("async" font-lock-preprocessor-face)
    ("modification" font-lock-function-name-face)
    ("deprecated" eglot-diagnostic-tag-deprecated-face)
    ("documentation" font-lock-doc-face)
    ("defaultLibrary" font-lock-builtin-face))
  "List of face to use to highlight tokens with modifiers.
Each element is a list of the following form: (MODIFIER FACE
[PRIORITY]).  MODIFIER is a token modifiers name.  FACE is the face to
use to highlight.  Option PRIORITY is a number whose value should
be between -100 to 100 (inclusive).  It's default value is 0.
Set to nil to disable special treatment of modifiers."
  :type `(alist :key-type (string :tag "Token name")
                :value-type (list (choice (face :tag "Face")
                                          (plist :tag "Face Attributes"
                                                 :key-type
                                                 (choice
                                                  ,@(mapcar
                                                     (lambda (cell)
                                                       `(const :tag ,(capitalize
                                                                      (cdr cell))
                                                               ,(car cell)))
                                                     face-attribute-name-alist))))
                                  (radio (const :inline t :tag "Use default priority" nil)
                                         (number :tag "Priority")))))

(defvar-local eglot--semantic-tokens nil
  "Semantic tokens of current buffer.")

(defvar-local eglot--semantic-tokens-last-result-id nil
  "Last value of `:resultId'.")

(defvar-local eglot--semantic-tokens-update-timer nil
  "Idle timer to update tokens.")

(defun eglot--semantic-supported-token-types ()
  "Return the list of supported token types."
  (apply #'vector (mapcar #'car eglot-semantic-token-faces)))

(defun eglot--semantic-supported-token-modifiers ()
  "Return the list of supported token types."
  (apply #'vector (mapcar #'car eglot-semantic-token-modifier-faces)))

(cl-defun eglot--semantic-tokens-relative-to-internal
    (token
     legend
     &optional
     (previous (list :begin nil
                     :end nil
                     :type 0
                     :modifiers 0)))
  "Make a semantic token in internal formats.
The token is made from TOKEN, LEGEND and its PREVIOUS token."
  (cl-destructuring-bind (delta-line delta-col length type modifiers &rest _) token
    (let* ((prev-pos (if (plist-get previous :begin)
                         (eglot--pos-to-lsp-position
                          (marker-position (plist-get previous :begin)))
                       '(:line 0 :character 0)))
           (prev-line (plist-get prev-pos :line))
           (prev-col (plist-get prev-pos :character))
           (line (+ prev-line delta-line))
           (col (if (eql prev-line line)
                    (+ prev-col delta-col)
                  delta-col)))
      (list :begin (let ((marker (make-marker)))
                     (set-marker marker (eglot--lsp-position-to-point
                                         (list :line line :character col)))
                     marker)
            :end (let ((delta-line 0)
                       (col (+ col length))
                       (marker (make-marker)))
                   (cl-block nil
                     (while t
                       (let ((last-col
                              (save-excursion
                                (goto-char (eglot--lsp-position-to-point
                                            (list :line (+ line delta-line)
                                                  :character 0)))
                                (goto-char (line-end-position))
                                (funcall eglot-current-column-function))))
                         (if (<= col last-col)
                             (cl-return)
                           (setq col (- col last-col 1)
                                 delta-line (1+ delta-line))))))
                   (set-marker marker
                               (eglot--lsp-position-to-point
                                (list :line (+ line delta-line) :character col)))
                   marker)
            :type (aref (plist-get legend :tokenTypes) type)
            :modifiers (let ((x modifiers)
                             (l nil)
                             (i 0))
                         (while (not (zerop x))
                           (unless (zerop (% x 2))
                             (push (aref (plist-get legend :tokenModifiers) i) l))
                           (setq x (/ x 2)
                                 i (1+ i)))
                         l)))))

(defun eglot--semantic-tokens-get-face (type modifiers)
  "Get face for a token of TYPE with MODIFIERS."
  (let ((face (cdr (assoc-string type eglot-semantic-token-faces)))
        (modifier-faces nil))
    (dolist (modifier modifiers)
      (let ((spec (assoc-string modifier eglot-semantic-token-modifier-faces)))
        (when spec
          (cl-destructuring-bind (face &optional (priority 0)) (cdr spec)
            (push (cons priority face) modifier-faces)))))
    (when modifier-faces
      (setq face (list face)))
    (dolist (f (sort modifier-faces #'car-less-than-car))
      (push (cdr f) face))
    face))

(defun eglot--semantic-tokens-highlight-token (token)
  "Highlight token TOKEN."
  (cl-destructuring-bind (&key begin end type modifiers) token
    (let ((face (eglot--semantic-tokens-get-face type modifiers)))
      (when face
        (with-silent-modifications
          (dolist (i (number-sequence begin (1- end)))
            (put-text-property i (1+ i) 'eglot--original-props
                               (cons (get-text-property i 'face)
                                      (get-text-property i 'font-lock-face))))
          (put-text-property begin end 'eglot--semantic-token-p face)
          (put-text-property begin end 'face face)
          (put-text-property begin end 'font-lock-face face))))))

(defun eglot--semantic-tokens-unhighlight-region (begin end)
  "Unhighlight the text between BEGIN and END."
  (save-excursion
    (goto-char begin)
    (let (match)
      (while (and (< (point) end)
                  (setq match (text-property-search-forward
                               'eglot--semantic-token-p)))
        (let ((b (prop-match-beginning match))
              (e (prop-match-end match)))
          (with-silent-modifications
            (put-text-property b e 'eglot--semantic-token-p nil)
            (dolist (i (number-sequence b (1- e)))
              (let ((orig (get-text-property i 'eglot--original-props)))
                (put-text-property i (1+ i) 'face (car orig))
                (put-text-property i (1+ i) 'font-lock-face (cdr orig))))))))))

(defun eglot--semantic-tokens-process-full-or-range (response range)
  "Process RESPONSE returned by `textDocument/semanticTokens/{full,range}'.
RANGE is should be a cons cell of the form (BEGIN END), or nil, where
BEGIN and END is the beginning and the end of range or region
\(exclusive).
When RANGE is non-nil, RESPONSE is treated as the response of
`textDocument/semanticTokens/full', otherwise as the response of
`textDocument/semanticTokens/range'."
  (when response
    (unless range
      (setq eglot--semantic-tokens nil)
      (setq eglot--semantic-tokens-last-result-id (plist-get response :resultId)))
    (let ((tokens nil))
      (let ((data (append (plist-get response :data) nil))
            (legend (eglot--server-capable :semanticTokensProvider :legend))
            (prev-token nil))
        (dotimes (i (/ (length data) 5))
          (let ((token (if (not prev-token)
                           (eglot--semantic-tokens-relative-to-internal
                            (nthcdr (* i 5) data) legend)
                         (eglot--semantic-tokens-relative-to-internal
                          (nthcdr (* i 5) data) legend prev-token))))
            (setq prev-token token)
            (if range
                (push token tokens)
              (setq eglot--semantic-tokens
                    (nconc eglot--semantic-tokens `(,token)))))))
      (with-silent-modifications
        (save-restriction
          (widen)
          (eglot--semantic-tokens-unhighlight-region (or (car range) (point-min))
                                                     (or (cdr range) (point-max)))
          (dolist (token (if range tokens eglot--semantic-tokens))
            (eglot--semantic-tokens-highlight-token token))))
      (font-lock-ensure))))

(defun eglot--semantic-tokens-process-full/delta-1 (response)
  "Process RESPONSE returned by `textDocument/semanticTokens/full/delta'."
  (setq eglot--semantic-tokens-last-result-id (plist-get response :resultId))
  (let ((additions nil)
        (deletions nil))
    (dolist (edit (append (plist-get response :edits) nil))
      (let* ((index (/ (plist-get edit :start) 5))
             (delete (/ (plist-get edit :deleteCount) 5))
             (data (append (plist-get edit :data) nil))
             (legend (eglot--server-capable :semanticTokensProvider :legend))
             (prev-token (unless (zerop index)
                           (nth (1- index) eglot--semantic-tokens)))
             (new-tokens nil))
        (dolist (i (number-sequence index (1- (+ index delete))))
          (push (nth i eglot--semantic-tokens) deletions))
        (if (zerop index)
            (setq eglot--semantic-tokens
                  (nthcdr (+ index delete) eglot--semantic-tokens))
          (setcdr (nthcdr (1- index) eglot--semantic-tokens)
                  (nthcdr (+ index delete) eglot--semantic-tokens)))
        (dotimes (i (/ (length data) 5))
          (let ((token (if (not prev-token)
                           (eglot--semantic-tokens-relative-to-internal
                            (nthcdr (* i 5) data) legend)
                         (eglot--semantic-tokens-relative-to-internal
                          (nthcdr (* i 5) data) legend prev-token))))
            (push token additions)
            (setq prev-token token
                  new-tokens (nconc new-tokens `(,token)))))
        (if (zerop index)
            (setq eglot--semantic-tokens (nconc new-tokens eglot--semantic-tokens))
          (setcdr (nthcdr (1- index) eglot--semantic-tokens)
                  (nconc new-tokens (nthcdr index eglot--semantic-tokens))))))
    (with-silent-modifications
      (save-restriction
        (widen)
        (dolist (token deletions)
          (eglot--semantic-tokens-unhighlight-region (plist-get token :begin)
                                                     (plist-get token :end))
          (set-marker (plist-get token :begin) nil)
          (set-marker (plist-get token :end) nil))
        (dolist (token additions)
          (eglot--semantic-tokens-highlight-token token)))))
  (font-lock-ensure))

(defun eglot--semantic-tokens-process-full/delta (response)
  "Process RESPONSE returned by `textDocument/semanticTokens/full/delta'."
  (when response
    (if (plist-member response :edits)
        ;; If something goes wrong in delta processing, our tokens will mess up
        (let ((success-flag nil))
          (unwind-protect
              (progn
                (eglot--semantic-tokens-process-full/delta-1 response)
                (setq success-flag t))
            (unless success-flag
              (setq eglot--semantic-tokens nil
                    eglot--semantic-tokens-last-result-id nil))))
      (eglot--semantic-tokens-process-full-or-range response nil))))

(defun eglot--semantic-tokens-highlight-full ()
  "Highlight whole buffer."
  (let ((buffer (current-buffer)))
    (jsonrpc-async-request
     (eglot--current-server-or-lose)
     :textDocument/semanticTokens/full
     `(:textDocument (:uri ,(eglot--path-to-uri (buffer-file-name))))
     :success-fn
     (lambda (result)
       (eglot--when-live-buffer buffer
         (eglot--semantic-tokens-process-full-or-range result nil)))
     :deferred :textDocument/semanticTokens/full)))

(defun eglot--semantic-tokens-highlight-full/delta ()
  "Highlight whole buffer."
  (let ((buffer (current-buffer)))
    (jsonrpc-async-request
     (eglot--current-server-or-lose)
     :textDocument/semanticTokens/full/delta
     `(:textDocument (:uri ,(eglot--path-to-uri (buffer-file-name)))
                     :previousResultId ,eglot--semantic-tokens-last-result-id)
     :success-fn
     (lambda (result)
       (eglot--when-live-buffer buffer
         (eglot--semantic-tokens-process-full/delta result)))
     :deferred :textDocument/semanticTokens/full)))

(defun eglot--semantic-tokens-highlight-range (begin end)
  "Highlight the text between BEGIN and END."
  (let ((buffer (current-buffer)))
    (jsonrpc-async-request
     (eglot--current-server-or-lose)
     :textDocument/semanticTokens/range
     `(:textDocument (:uri ,(eglot--path-to-uri (buffer-file-name)))
                     :range (:start ,(eglot--pos-to-lsp-position begin)
                                    :end ,(eglot--pos-to-lsp-position end)))
     :success-fn
     (lambda (result)
       (when (and result (not (zerop (length (plist-get result :data)))))
         (eglot--when-live-buffer buffer
           (eglot--semantic-tokens-process-full-or-range result (cons begin end)))))
     :deferred :textDocument/semanticTokens/range)))

(defun eglot--semantic-tokens-highlight-region (begin end)
  "Ensure the text between BEGIN and END is highlighted."
  (cond
   ((and eglot-semantic-tokens-use-delta
         (eglot--server-capable :semanticTokensProvider :full :delta)
         eglot--semantic-tokens-last-result-id)
    (eglot--semantic-tokens-highlight-full/delta))
   ((and (eglot--server-capable :semanticTokensProvider :range)
         (save-restriction
           (widen)
           (not (and (eql begin (point-min))
                     (eql end (point-max))))))
    (eglot--semantic-tokens-highlight-range begin end))
   ((eglot--server-capable :semanticTokensProvider :full)
    (eglot--semantic-tokens-highlight-full))
   (t
    (error "Server doesn't provide semantic tokens"))))

(defun eglot--semantic-tokens-mode-disable-maybe ()
  "Disable Eglot--Semantic-Tokens-Mode if buffer is not managed."
  (unless (eglot-managed-p)
    (eglot--semantic-tokens-mode -1)))

(define-minor-mode eglot--semantic-tokens-mode
  "Toggle semantic token support."
  :lighter nil
  (if (and eglot--semantic-tokens-mode
           (not (and (eglot-managed-p)
                     (eglot-current-server)
                     (or (eglot--server-capable :semanticTokensProvider
                                                :full)
                         (eglot--server-capable :semanticTokensProvider
                                                :range)))))
      (setq eglot--semantic-tokens-mode nil)
    (with-silent-modifications
      (if eglot--semantic-tokens-mode
          (progn
            (add-hook 'eglot-managed-mode-hook
                      #'eglot--semantic-tokens-mode-disable-maybe nil t)
            (add-hook 'eglot--document-changed-hook
                      #'eglot--semantic-tokens-queue-update nil t)
            (eglot--semantic-tokens-highlight-region (point-min) (point-max)))
        (remove-hook 'eglot-managed-mode-hook
                     #'eglot--semantic-tokens-mode-disable-maybe t)
        (setq eglot--semantic-tokens nil
              eglot--semantic-tokens-last-result-id nil)
        (save-restriction
          (widen)
          (eglot--semantic-tokens-unhighlight-region (point-min) (point-max))))
      (font-lock-ensure))))

(defun eglot--semantic-tokens-update-maybe ()
  "Update semantic tokens if Eglot--Semantic-Tokens-Mode is enabled."
  (when eglot--semantic-tokens-mode
    (save-restriction
      (widen)
      (eglot--semantic-tokens-highlight-region (point-min) (point-max)))))

(defun eglot--semantic-tokens-queue-update ()
  (when eglot-enable-semantic-tokens
        (run-with-idle-timer eglot-send-changes-idle-time nil
                #'eglot--semantic-tokens-mode +1)))

(provide 'eglot-semantic-tokens)

;;; eglot-semantic-tokens.el ends here
