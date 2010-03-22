; -*- Mode: Emacs-Lisp -*-

;;; zml-mode.el -- Major mode for ZML files

;; Software License Agreement (BSD License)
;;
;; Copyright (c) 2010 Sergei Matusevich <sergei.matusevich@gmail.com>
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 3. The name of the author may not be used to endorse or promote products
;;    derived from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
;; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
;; IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;; Put this file into your Emacs lisp path (eg. site-lisp)
;; and add the following line to your ~/.emacs file:
;;
;;   (require 'zml-mode)

(require 'font-lock)

(defvar zml-mode-hook nil)

(defvar zml-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "RET") 'newline-and-indent)
    keymap)
  "Keymap for ZML major mode")

(add-to-list 'auto-mode-alist '("\\.zml\\'" . zml-mode))

(defconst zml-font-lock-defaults
  '(("\\(?:^\\|\\s \\)\\(\\*\\w+\\)\\(\\(?:[.#]\\w+\\)*\\)"
     (1 font-lock-keyword-face)
     (2 font-lock-variable-name-face))
    ("\\(?:^\\|\\s \\)\\(:\\w+\\)\\(\\(?:[.#]\\w+\\)*\\)"
     (1 font-lock-builtin-face)
     (2 font-lock-variable-name-face))
    ("\\(?:^\\|\\s \\)\\(\\(?:[.#]\\w+\\)+\\)"
     (1 font-lock-variable-name-face))
    ("\\(\\w+:\\)"
     (1 font-lock-type-face))
    ("\\(\\<-?\\(?:[0-9]+\\.?\\|\\.[0-9]+\\)[0-9]*\\(?:[eE][-+]?[0-9]+\\)?\\>\\)"
     (1 font-lock-constant-face))
    ("\\(|\"[^\d]*?\"|\\)"
     (1 font-lock-string-face)))
  "Regexps to highlight in zml mode")

(defconst zml-font-lock-syntactic-keywords
  '(("\\(|\\)\\(\"\\)" (1 "\"") (2 "\""))
    ("\\(\"\\)\\(|\\)" (1 "\"") (2 "\"")))
  "Syntactic keywords for zml mode")

(defvar zml-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?-  "w"     st)
    (modify-syntax-entry ?|  ". 124" st)
    (modify-syntax-entry ?#  ". 23b" st)
    (modify-syntax-entry ?\n ">"     st)
    (modify-syntax-entry ?\" "."     st)
    st)
  "Syntax table for zml mode")

(define-derived-mode zml-mode fundamental-mode "zml"
  "Major mode for editing ZML files"
  :syntax-table zml-mode-syntax-table
  (set (make-local-variable 'font-lock-syntactic-keywords)
       zml-font-lock-syntactic-keywords)
  (set (make-local-variable 'font-lock-defaults)
       '(zml-font-lock-defaults nil t)) )

(provide 'zml-mode)

;;; end of zml-mode.el
