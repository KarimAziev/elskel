;;; elskel.el --- Context aware Elisp completions -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/elskel
;; Version: 0.1.0
;; Keywords: lisp
;; Package-Requires: ((emacs "29.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Context aware Elisp completions

;;; Code:

(require 'transient)


(require 'subr-x)
(require 'find-func)

(declare-function info--manual-names "info")
(declare-function info--filter-manual-names "info")

(defcustom elskel-extra-var-types (list
                                   (cons 'defvar 3)
                                   (cons 'defconst 3)
                                   (cons 'defvar-keymap 3))
  "List of variable definition forms and documentation positions."
  :group 'elskel
  :type '(alist
          :key-type symbol
          :value-type natnum))


(defcustom elskel-func-types (list
                              (cons 'defun 3)
                              (cons 'defmacro 2)
                              (cons 'defsubst 3)
                              (cons 'defhydra 3)
                              (cons 'transient-define-prefix 3)
                              (cons 'transient-define-suffix 3)
                              (cons 'transient-define-argument 3)
                              (cons 'transient-define-infix 3)
                              (cons 'cl-defun 3)
                              (cons 'cl-defsubst 3)
                              (cons 'cl-defmacro 3)
                              (cons 'cl-defgeneric 3)
                              (cons 'cl-defmethod 3))
  "List of function definition forms and documentation positions."
  :group 'elskel
  :type '(alist
          :key-type symbol
          :value-type natnum))


(defcustom elskel-def-type-doc-poses (append
                                      (list (cons 'define-skeleton 2)
                                            (cons 'ert-deftest 3)
                                            (cons 'define-widget 3)
                                            (cons
                                             'easy-mmode-define-minor-mode 2)
                                            (cons 'defclass 4)
                                            (cons 'cl-defstruct 3))
                                      elskel-extra-var-types
                                      (list
                                       (cons 'defcustom 3))
                                      elskel-func-types
                                      (list
                                       (cons 'define-minor-mode 2)
                                       (cons 'define-derived-mode 4)
                                       (cons 'define-generic-mode 8)
                                       (cons 'define-compilation-mode 3)
                                       (cons 'easy-mmode-define-minor-mode 2)))
  "Alist mapping definition macros to their position arguments."
  :group 'elskel
  :type '(alist
          :key-type symbol
          :value-type natnum))

(defcustom elskel-use-package-symbol-names '("use-package"
                                             "use-package!"
                                             "straight-use-package")
  "List of symbol names for extra `use-package' declarations.

A list of symbol names that represent `use-package' macros or functions.

Each element in the list should be a string that corresponds to the name of a
`use-package' macro or function. These names are used to identify `use-package'
declarations when performing operations such as searching for package
declarations or copying their configurations.

The default value includes common variations of `use-package' declarations.
Users can add or remove entries to customize the behavior of functions that rely
on this list.

To modify this list, use `M-x `customize-option'` and search for the
corresponding customization option, then add or remove strings as needed. Ensure
that each entry is an exact match for the `use-package' macro or function name
used in the Emacs configuration."
  :group 'elskel--extra
  :type '(repeat string))

(defvar use-package-keywords)

(defun elskel--extra-use-package-at-point-p ()
  "Check if point is on a `use-package' form."
  (when-let ((sexp (elskel--extra-sexp-at-point)))
    (and
     (car-safe sexp)
     (symbolp (car-safe sexp))
     (member (symbol-name (car sexp)) elskel-use-package-symbol-names)
     (listp (cdr sexp))
     (symbolp (cadr sexp))
     (cadr sexp))))

(defun elskel--extra-inside-use-package-p ()
  "Check if point is inside a `use-package' form."
  (with-syntax-table emacs-lisp-mode-syntax-table
    (save-excursion
      (catch 'found
        (while (elskel--extra-move-with 'backward-up-list)
          (when (elskel--extra-use-package-at-point-p)
            (throw 'found (point))))))))

(defun elskel--extra-read-string (str)
  "Read and return a list of sexps from a string STR.

Argument STR is a string from which to read Lisp expressions."
  (with-temp-buffer
    (erase-buffer)
    (save-excursion
      (insert str))
    (let ((sexps)
          (sexp))
      (while
          (setq sexp
                (ignore-errors (read
                                (current-buffer))))
        (push sexp sexps))
      (reverse sexps))))

(defun elskel--extra-get-keyword-value (&optional keyword)
  "Extract value for KEYWORD in `use-package' declaration.

Optional argument KEYWORD is the keyword to search for within the `use-package'
declaration."
  (pcase-let ((`(,beg . ,_end)
               (elskel--extra-get-use-package-bounds)))
    (when beg
      (save-excursion
        (when (elskel--extra-jump-to-keyword keyword)
          (let* ((beg (point))
                 (end (save-excursion
                        (elskel--extra-move-to--next-keyword))))
            (buffer-substring-no-properties beg end)))))))

(defun elskel--extra-get-current-package-name ()
  "Extract the current package name at point."
  (when-let ((beg (elskel--extra-inside-use-package-p)))
    (save-excursion
      (goto-char beg)
      (down-list 1)
      (forward-sexp 2)
      (symbol-at-point))))

(defun elskel--extra-move-with (fn &optional n)
  "Move point using FN, optionally N times, within syntax table scope.

Argument FN is a function that moves the point and returns the new position.

Optional argument N is an integer specifying the number of times to move; it
defaults to 1."
  (with-syntax-table emacs-lisp-mode-syntax-table
    (unless n (setq n 1))
    (when-let ((str-start (nth 8 (syntax-ppss (point)))))
      (goto-char str-start))
    (let ((init-pos (point))
          (pos)
          (count n))
      (while (and (not (= count 0))
                  (when-let ((end (ignore-errors
                                    (funcall fn)
                                    (point))))
                    (unless (= end (or pos init-pos))
                      (setq pos end))))
        (setq count (1- count)))
      (if (= count 0)
          pos
        (goto-char init-pos)
        nil))))

(defun elskel--extra-sexp-at-point ()
  "Extract the S-expression at the current point."
  (with-syntax-table emacs-lisp-mode-syntax-table
    (sexp-at-point)))

(defun elskel--extra-re-search-forward-inner (regexp &optional bound count)
  "Search forward, skipping strings and comments.

Argument REGEXP is a regular expression string to search for.

Optional argument BOUND is the buffer position to limit the search; nil means
search to the end of the accessible portion of the buffer.

Optional argument COUNT is the number of successful matches to find; nil means
search until the end of the buffer."
  (let ((parse))
    (while (> count 0)
      (with-syntax-table emacs-lisp-mode-syntax-table
        (re-search-forward regexp bound)
        (setq parse (syntax-ppss))
        (cond ((and (nth 3 parse)
                    (nth 8 parse))
               (goto-char (nth 8 parse))
               (forward-sexp))
              ((and (nth 4 parse)
                    (nth 8 parse))
               (goto-char (nth 8 parse))
               (forward-line))
              (t
               (setq count (1- count)))))))
  (point))

(defun elskel--extra-re-search-backward-inner (regexp &optional bound count)
  "Search backward for REGEXP, skipping over strings and comments.

Argument REGEXP is a regular expression string to search backward for.

Optional argument BOUND is a buffer position that bounds the search; it must not
be smaller than (point-min).

Optional argument COUNT is the number of successful matches to find; it defaults
to 1."
  (let ((parse))
    (while (> count 0)
      (with-syntax-table emacs-lisp-mode-syntax-table
        (re-search-backward regexp bound)
        (setq parse (syntax-ppss))
        (cond ((and (or (nth 3 parse))
                    (nth 8 parse))
               (goto-char (nth 8 parse)))
              ((and (nth 4 parse)
                    (nth 8 parse))
               (goto-char (nth 8 parse)))
              (t
               (setq count (1- count)))))))
  (point))

(defun elskel--extra-move-to--next-keyword ()
  "Navigate to the next non-keyword sexp."
  (ignore-errors (while (keywordp (elskel--extra-sexp-at-point))
                   (forward-sexp)))
  (let ((pos))
    (while (while (progn
                    (and (not (keywordp (elskel--extra-sexp-at-point)))
                         (setq pos (elskel--extra-move-with 'forward-sexp))))))
    (if pos
        (progn (goto-char pos)
               (elskel--extra-move-with 'backward-sexp 1)
               (point))
      (point))))

(defun elskel--extra-get-use-package-bounds ()
  "Find bounds of the current `use-package' form."
  (when-let ((start (elskel--extra-inside-use-package-p)))
    (save-excursion
      (goto-char start)
      (elskel--extra-move-with 'forward-sexp)
      (cons start (point)))))

(defun elskel--extra-jump-or-insert-to-use-package-keyword (&optional keyword)
  "Jump to or insert a `use-package' keyword.

Optional argument KEYWORD is a symbol or string representing the `use-package'
KEYWORD to jump to or insert. If not provided, the user is prompted to choose
one."
  (interactive)
  (pcase-let ((`(,beg . ,_end)
               (elskel--extra-get-use-package-bounds)))
    (when beg
      (when-let ((keyword-end
                  (save-excursion
                    (let* ((existing
                            (elskel--extra-get-use-package-keywords))
                           (annotf
                            (lambda (s)
                              (if
                                  (not (memq (intern s) existing))
                                  " (insert)"
                                "")))
                           (keyword-name
                            (or
                             (when keyword
                               (if (symbolp keyword)
                                   (symbol-name keyword)
                                 keyword))
                             (completing-read "Keyword"
                                              (lambda
                                                (str pred action)
                                                (if
                                                    (eq action
                                                        'metadata)
                                                    `(metadata
                                                      (annotation-function
                                                       . ,annotf))
                                                  (complete-with-action
                                                   action
                                                   (seq-uniq
                                                    (append
                                                     existing
                                                     use-package-keywords))
                                                   str
                                                   pred))))))
                           (keyword (intern keyword-name)))
                      (if (memq keyword existing)
                          (progn (elskel--extra-jump-to-keyword keyword)
                                 (elskel--extra-move-to--next-keyword)
                                 (skip-chars-backward "\s\t\n")
                                 (point))
                        (goto-char beg)
                        (down-list)
                        (when existing
                          (elskel--extra-jump-to-keyword (car (last existing))))
                        (elskel--extra-move-to--next-keyword)
                        (newline-and-indent)
                        (save-excursion
                          (insert (concat keyword-name
                                          "\s")))
                        (indent-for-tab-command)
                        (re-search-forward "\s" nil t 1))))))
        (goto-char keyword-end)))))

(defun elskel--extra-re-search-forward (regexp &optional bound noerror count)
  "Search forward using REGEXP, optionally up to BOUND, COUNT times.

Argument REGEXP is a regular expression string to search for.

Optional argument BOUND is a buffer position that bounds the search; it must be
a number or a marker, or nil.

Optional argument NOERROR, if non-nil, means do not signal an error if the
search fails, and return nil instead.

Optional argument COUNT is the number of times to search; it defaults to 1."
  (unless count (setq count 1))
  (let ((init-point (point))
        (search-fun
         (cond ((< count 0)
                (setq count (- count))
                #'elskel--extra-re-search-backward-inner)
               ((> count 0) #'elskel--extra-re-search-forward-inner)
               (t #'ignore))))
    (condition-case err
        (funcall search-fun regexp bound count)
      (search-failed
       (goto-char init-point)
       (unless noerror
         (signal (car err)
                 (cdr err)))))))

(defun elskel--extra-jump-to-keyword (keyword)
  "Jump to the specified KEYWORD in a `use-package' declaration.

Argument KEYWORD is the symbol to search for within the `use-package'
declaration."
  (pcase-let ((`(,beg . ,_end)
               (elskel--extra-get-use-package-bounds)))
    (when beg
      (goto-char beg)
      (down-list)
      (let ((pos))
        (while (progn
                 (and (not (eq (elskel--extra-sexp-at-point) keyword))
                      (setq pos (elskel--extra-move-with 'forward-sexp)))))
        (when pos
          (goto-char pos)
          pos)))))

(defun elskel--extra-get-customs ()
  "Extract custom variables from the current package."
  (when-let* ((package-name (elskel--extra-get-current-package-name))
              (items (elskel--extra-get-library-items package-name))
              (customs (cdr (assq :custom items))))
    (mapcar (lambda (it)
              (list (car it)
                    (plist-get (cdr it) :value)))
            customs)))

(defun elskel--extra-get-keyword-value-sexps (&optional keyword)
  "Extract KEYWORD value sexps from a string.

Optional argument KEYWORD is a keyword symbol for which to get the value sexps."
  (when-let ((str (elskel--extra-get-keyword-value keyword)))
    (pcase keyword
      ((or :bind :bind*)
       (let ((val (car (elskel--extra-read-string str))))
         (if (or (keywordp (car-safe val))
                 (stringp (car-safe val)))
             (progn
               (elskel--extra-jump-to-keyword keyword)
               (down-list)
               (forward-char -1)
               (pcase-let* ((`(,beg . ,end)
                             (bounds-of-thing-at-point 'sexp))
                            (rep
                             (when beg
                               (buffer-substring-no-properties beg end))))
                 (replace-region-contents beg end
                                          (lambda ()
                                            (concat "(" rep ")"))))
               (list val))
           val)))
      (_ (elskel--extra-read-string str)))))

(defun elskel--extra-get-use-package-keywords ()
  "Extract keywords from a `use-package' declaration."
  (pcase-let ((`(,beg . ,_end)
               (elskel--extra-get-use-package-bounds)))
    (save-excursion
      (when beg
        (goto-char beg)
        (ignore-errors (seq-filter #'keywordp (elskel--extra-sexp-at-point)))))))

(declare-function info-initialize "info")
(declare-function Man-parse-man-k "man")
(declare-function Man-default-directory "man")

(require 'elskel-skeletons)

(defvar elskel--extra--interactive-types
  (list
   (cons 'defun 3)
   (cons 'defsubst 3)
   (cons 'cl-defun 3)
   (cons 'cl-defsubst 3))
  "List of interactive Lisp function types and their arity.")

(defvar elskel--extra-keywords-inserters-alist
  '((:bind . elskel--extra-insert-bind)
    (:bind* . elskel--extra-insert-bind*)
    (:config . elskel--extra-insert-config-keyword)
    (:custom . elskel--extra-insert-customs)
    (:straight . elskel--extra-insert-straight-keyword))
  "Alist mapping keywords to functions for inserting package configurations.")

(defun elskel--extra-insert-config-keyword ()
  "Insert or update `:config' keyword values in `use-package' form."
  (interactive)
  (let ((keywords (elskel--extra-get-use-package-keywords))
        (sexps (elskel--extra-get-keyword-value-sexps :config)))
    (let ((result (mapcar (lambda (it)
                            (append (list 'setq-default) it))
                          (elskel--extra-get-customs))))
      (setq result (if (listp result)
                       (mapconcat #'prin1-to-string result "\n")
                     result))
      (cond ((and (memq :config keywords)
                  sexps)
             (elskel--extra-jump-to-keyword :config)
             (newline-and-indent)
             (insert result))
            ((and (memq :config keywords)
                  (not sexps))
             (elskel--extra-jump-to-keyword :config)
             (newline-and-indent)
             (insert result))
            (t
             (elskel--extra-jump-or-insert-to-use-package-keyword :config)
             (newline-and-indent)
             (insert result))))))

(defun elskel--extra--sexp-declare-p (sexp)
  "Check if SEXP is a declaration and return its type and name.

Argument SEXP is an s-expression to be checked for declaration forms."
  (pcase sexp
    (`(defvar ,name)
     (list 'defvar name))
    (`(declare-function ,name)
     (list 'declare-function name))
    (`(declare-function ,name
       ,_file)
     (list 'declare-function name))
    (`(declare-function ,name
       ,_file
       ,_args)
     (list 'declare-function name))
    (`(declare-function ,name ,_file ,_args ,_fileonly)
     (list 'declare-function name))))

(defun elskel--extra--get-doc-from-sexp (sexp)
  "Extract documentation string from S-expression.

Argument SEXP is a proper list representing an Emacs Lisp expression."
  (when (proper-list-p sexp)
    (let* ((type (car-safe sexp))
           (pos (and type
                     (cdr
                      (assq type elskel-def-type-doc-poses))))
           (doc-str
            (pcase type
              ('defvar-keymap (plist-get sexp :doc))
              ((guard (and pos
                           (eq type 'cl-defmethod)
                           (memq (nth 2 sexp) '(:around :after
                                                :before))))
               (nth (1+ pos) sexp))
              ((guard (and pos))
               (nth pos sexp)))))
      doc-str)))

(defun elskel--extra--find-lib-in-dir (sym dir)
  "Find and return the file path of library SYM in directory DIR.

Argument SYM is a symbol representing the library to find.

Argument DIR is a string specifying the directory to search for the library."
  (require 'find-func)
  (when-let ((file (ignore-errors
                     (file-truename (find-library-name (symbol-name sym))))))
    (when (file-in-directory-p file dir)
      file)))

(defun elskel--extra--unquote (exp)
  "Remove \\='function quote from EXP if present.

Argument EXP is an expression to be unquoted."
  (declare (pure t)
           (side-effect-free t))
  (while (memq (car-safe exp) '(quote function))
    (setq exp (cadr exp)))
  exp)

(defun elskel--extra--parse-sexp (item &optional extra-props)
  "Parse and transform ITEM into a plist with EXTRA-PROPS.

Argument ITEM is the sexp to parse.

Optional argument EXTRA-PROPS is a plist of additional properties to include in
the result."
  (when (proper-list-p item)
    (let ((type (car-safe item)))
      (when (and type
                 (symbolp type))
        (pcase type
          ('quote nil)
          ((or 'with-eval-after-load 'eval-when-compile
               'eval-after-load
               'if 'progn
               'and
               'let 'if-let 'when-let 'with-no-warnings
               'when 'unless 'eval-and-compile)
           (mapcan (lambda (it)
                     (elskel--extra-read-sexp it extra-props))
                   (cdr item)))
          ('define-key
           (pcase item
             (`(define-key ,map ,key
                ,(and cmd
                  (guard
                   (symbolp (elskel--extra--unquote
                             cmd)))))
              (list
               (cons (elskel--extra--unquote cmd)
                     (append
                      (list
                       :type 'define-key
                       :map map
                       :value
                       (cond ((stringp key)
                              key)
                             ((vectorp key)
                              (key-description key))
                             ((and (listp key)
                                   (stringp (cadr key)))
                              (cadr key))
                             (t key)))
                      extra-props))))))
          ('require
           (when-let ((sym
                       (pcase item
                         (`(require ,(and name
                                      (guard (listp name))
                                      (guard (eq (car-safe name) 'quote))))
                          (elskel--extra--unquote name))
                         (`(require ,(and name
                                      (guard (listp name))
                                      (guard (eq (car-safe name) 'quote)))
                            ,_)
                          (elskel--extra--unquote name))
                         (`(require ,(and name
                                      (guard (listp name))
                                      (guard (eq (car-safe name) 'quote)))
                            ,_
                            ,(and optional
                              (guard (not (eq optional nil)))))
                          (elskel--extra--unquote name)))))
             (if-let ((file (elskel--extra--find-lib-in-dir
                             sym
                             default-directory)))
                 (append (list (cons sym (append extra-props
                                                 (list :type type))))
                         (elskel--extra--read-file file))
               (elskel--extra-plist-remove-nils
                (list (cons sym (append extra-props
                                        (list :type type))))))))
          ((or 'use-package 'use-package!)
           (when-let ((sym (and
                            (cadr item)
                            (symbolp (cadr item))
                            (cadr item))))
             (let* ((data
                     (mapcan (lambda (it)
                               (elskel--extra-read-sexp it extra-props))
                             (cdr item)))
                    (v (cons sym
                             (append
                              extra-props
                              (list :type
                                    type)))))
               (append
                data
                (list v)))))
          (_
           (let* ((doc (elskel--extra--get-doc-from-sexp item))
                  (sym
                   (cond ((not (cadr item))
                          nil)
                         ((and (symbolp (cadr item)))
                          (cadr item))
                         (t (elskel--extra--unquote (cadr item)))))
                  (declaration (elskel--extra--sexp-declare-p item))
                  (props (append (list
                                  :type type
                                  :doc doc)
                                 extra-props)))
             (when (and sym (symbolp sym))
               (pcase type
                 ((guard declaration)
                  (setq props (plist-put props :declared t)))
                 ((guard
                   (assq type elskel-func-types))
                  (let ((args (seq-find
                               #'proper-list-p
                               item)))
                    (setq props (plist-put props :args args))
                    (when (and (assq type
                                     elskel--extra--interactive-types)
                               (ignore-errors (eq 'interactive
                                                  (if doc
                                                      (caadr
                                                       (member
                                                        doc
                                                        item))
                                                    (car-safe
                                                     (nth
                                                      (cdr
                                                       (assq
                                                        type
                                                        elskel--extra--interactive-types))
                                                      item))))))
                      (setq props (plist-put props :interactive t)))))
                 ('defcustom (setq props (plist-put props :value (nth 2 item))))
                 ((guard
                   (assq type (append
                               elskel-extra-var-types)))
                  (setq props (plist-put props :keymap
                                         (or (eq type 'defvar-keymap)
                                             (when-let*
                                                 ((value (nth 2 item))
                                                  (vals
                                                   (and (listp value)
                                                        (symbolp
                                                         (car value))
                                                        (memq (car
                                                               value)
                                                              '(let let*))
                                                        (car (seq-drop
                                                              value 1)))))
                                               (when (and (listp vals)
                                                          (listp (car vals)))
                                                 (seq-find
                                                  (lambda (it)
                                                    (when-let ((val (and
                                                                     (listp
                                                                      (cdr
                                                                       it))
                                                                     (listp
                                                                      (cadr
                                                                       it))
                                                                     (cadr
                                                                      it))))
                                                      (and
                                                       (= 1 (length val))
                                                       (symbolp (car val))
                                                       (memq (car val)
                                                             '(make-sparse-keymap
                                                               make-keymap)))))
                                                  vals)))))))
                 ('provide
                  (setq sym (elskel--extra--unquote sym)))
                 ((or 'put 'add-hook
                      'advice-add)
                  (when-let ((hook
                              (elskel--extra--unquote
                               (nth 1
                                    item))))
                    (setq sym hook)
                    (setq props (plist-put
                                 props
                                 :value
                                 (elskel--extra--unquote (nth 2
                                                              item))))))
                 (_
                  (unless (special-form-p sym)
                    (setq sym nil))))
               (when sym
                 (cons sym
                       (elskel--extra-plist-remove-nils
                        props)))))))))))

(defun elskel--extra-read-sexp (item &optional extra-props)
  "Parse ITEM into a sexp, optionally with EXTRA-PROPS.

Argument ITEM is the expression to be read and parsed.

Optional argument EXTRA-PROPS is a plist of additional properties that may
modify the parsing behavior."
  (let ((result (elskel--extra--parse-sexp item extra-props)))
    (if (and (car-safe result)
             (symbolp (car result)))
        (list result)
      result)))

(defun elskel--extra-parse-items-in-file (file)
  "Parse and return items from FILE as sexp lists.

Argument FILE is a string specifying the path to the file to be parsed."
  (with-temp-buffer
    (erase-buffer)
    (insert-file-contents file)
    (let ((sexps)
          (emacs-lisp-mode-hook nil)
          (sexp))
      (emacs-lisp-mode)
      (goto-char (point-min))
      (while (setq sexp (ignore-errors (read (current-buffer))))
        (let ((end (point))
              (beg)
              (autoload-item))
          (save-excursion
            (forward-sexp -1)
            (setq beg (point))
            (forward-line -1)
            (when (looking-at ";;;###")
              (setq autoload-item
                    (buffer-substring-no-properties
                     (line-beginning-position)
                     (line-end-position)))))
          (let* ((default-directory (file-name-directory
                                     file))
                 (parsed (delq nil
                               (elskel--extra-read-sexp sexp
                                                        (list
                                                         :autoload
                                                         autoload-item
                                                         :start beg
                                                         :end end
                                                         :file file)))))
            (setq sexps (append sexps parsed)))))
      sexps)))

(defun elskel--extra-plist-remove-nils (plist)
  "Remove nil values from PLIST, returning a clean property list.

Argument PLIST is a property list where each even element is a key and each odd
element is a value."
  (let* ((result (list 'head))
         (last result))
    (while plist
      (let* ((key (pop plist))
             (val (pop plist))
             (new (and val (list key val))))
        (when new
          (setcdr last new)
          (setq last (cdr new)))))
    (cdr result)))

(defvar elskel--extra--readed-files)

(defun elskel--extra--read-file (file)
  "Read and parse items from FILE if not already read.

Argument FILE is the name of the file to be read."
  (if (not (boundp 'elskel--extra--readed-files))
      (let ((elskel--extra--readed-files))
        (unless (member file elskel--extra--readed-files)
          (push file elskel--extra--readed-files)
          (elskel--extra-parse-items-in-file file)))
    (unless (member file elskel--extra--readed-files)
      (push file elskel--extra--readed-files)
      (elskel--extra-parse-items-in-file file))))

(defun elskel--extra-group-with (fn items &optional transform-fn)
  "Group ITEMS using FN and optionally transform with TRANSFORM-FN.

Argument FN is a function that takes an item from ITEMS and returns a key for
grouping.

Argument ITEMS is a list of elements to be grouped according to the keys
generated by FN.

Optional argument TRANSFORM-FN is a function applied to each item before
grouping; if nil, ITEMS are grouped as is."
  (seq-reduce (lambda (acc it)
                (let* ((key (funcall fn it))
                       (val (if transform-fn (funcall transform-fn it) it))
                       (cell (assoc key acc))
                       (group (if cell
                                  (append (cdr cell)
                                          (list val))
                                (list val))))
                  (if cell
                      (setcdr cell group)
                    (push (cons key group) acc))
                  acc))
              (seq-copy items) '()))

(defun elskel--extra-get-library-items (lib)
  "Group library items by type after loading LIB.

Argument LIB is a symbol or string representing the library to get items from."
  (let* ((sym (if (stringp lib)
                  (intern lib)
                lib))
         (name (if (stringp lib)
                   lib
                 (symbol-name lib)))
         (found (progn
                  (require sym nil t)
                  (file-truename (find-library-name name)))))
    (elskel--extra-group-with
     (lambda (it)
       (let* ((props (cdr it))
              (type (plist-get props :type)))
         (cond ((or (eq type 'transient-define-prefix)
                    (plist-get props :interactive))
                :commands)
               ((memq type
                      (list 'define-minor-mode 'define-derived-mode
                            'define-global-minor-mode
                            'define-globalized-minor-mode))
                :commands)
               ((plist-get props :autoload)
                :autoload)
               ((eq type 'defcustom) :custom)
               ((eq type 'defvar-keymap) :bind)
               ((plist-get props :keymap) :bind)
               ((and (car-safe it)
                     (symbolp (car-safe it))
                     (boundp (car-safe it))
                     (keymapp (symbol-value (car it))))
                :bind)
               ((and (car-safe it)
                     (symbolp (car-safe it))
                     (boundp (car-safe it))
                     (keymapp (symbol-value (car it))))
                :bind)
               (t type))))
     (elskel--extra--read-file found)
     (lambda (it)
       (cons (car it)
             (elskel--extra-plist-remove-nils
              (cdr
               it)))))))

(defvar manual-program)
(defvar elskel--use-package-keywords-completions nil
  "List of completion candidates for `use-package' keywords.")

(defcustom elskel-use-package-keyword-completions '((":demand" nil t)
                                                    (":defer" nil t)
                                                    (":straight" . elskel--straight-keyword-complete))
  "Completions for `use-package' keywords, with optional custom functions.

A list of cons cells where each cell contains a `use-package' keyword and its
completion function or a list of symbols.

Each cons cell in the list has a `use-package' keyword as the car (a string
starting with a colon) and either a function or a list of symbols as the cdr.

The function should return string to insert while the list of symbols represents
possible values for the keyword."
  :group 'elskel
  :type '(alist
          :key-type string
          :value-type
          (radio
           (function :tag "Function")
           (repeat
            (symbol :tag "Symbol")))))

(defcustom elskel-simple-data-type-descriptions '(("sexp" . "The value may be any Lisp object that can be printed and read back. You can use ‘sexp’ as a fall-back for any option, if you don’t want to take the time to work out a more specific type to use.")
                                                  ("integer" . "The value must be an integer.")
                                                  ("natnum" . "The value must be a nonnegative integer.")
                                                  ("number" . "The value must be a number (floating point or integer).")
                                                  ("float" . "The value must be floating point.")
                                                  ("string" . "The value must be a string. The customization buffer shows the string without delimiting ‘\"’ characters or ‘\\’ quotes.")
                                                  ("regexp" . "Like ‘string’ except that the string must be a valid regular expression.")
                                                  ("character" . "The value must be a character code. A character code is actually an integer, but this type shows the value by inserting the character in the buffer, rather than by showing the number.")
                                                  ("file" . "The value must be a file name. The widget provides completion.")
                                                  ("file :must-match t" . "The value must be a file name for an existing file. The widget provides completion.")
                                                  ("directory" . "The value must be a directory. The widget provides completion.")
                                                  ("hook" . "The value must be a list of functions. This customization type is used for hook variables. You can use the ‘:options’ keyword in a hook variable’s ‘defcustom’ to specify a list of functions recommended for use in the hook; *Note Variable Definitions::.")
                                                  ("symbol" . "The value must be a symbol. It appears in the customization buffer as the symbol name. The widget provides completion.")
                                                  ("function" . "The value must be either a lambda expression or a function name. The widget provides completion for function names.")
                                                  ("variable" . "The value must be a variable name. The widget provides completion.")
                                                  ("face" . "The value must be a symbol which is a face name. The widget provides completion.")
                                                  ("boolean" . "The value is boolean—either ‘nil’ or ‘t’. Note that by using ‘choice’ and ‘const’ together (see the next section), you can specify that the value must be ‘nil’ or ‘t’, but also specify the text to describe each value in a way that fits the specific meaning of the alternative.")
                                                  ("key" . "The value is a valid key according to ‘key-valid-p’, and suitable for use with, for example ‘keymap-set’.")
                                                  ("key-sequence" . "The value is a key sequence. The customization buffer shows the key sequence using the same syntax as the ‘kbd’ function. *Note Key Sequences::. This is a legacy type; use ‘key’ instead.")
                                                  ("coding-system" . "The value must be a coding-system name, and you can do completion with ‘M-<TAB>’.")
                                                  ("color" . "The value must be a valid color name. The widget provides completion for color names, as well as a sample and a button for selecting a color name from a list of color names shown in a ‘*Colors*’ buffer.")
                                                  ("fringe-bitmap" . "The value must be a valid fringe bitmap name. The widget provides completion."))
  "Alist of simple data types for custom variables with descriptions.

A list of simple customization types, each associated with a description of the
expected value.

Each element in the list is a cons cell, where the car is a string representing
the type, and the cdr is a string describing the expected value for that type."
  :group 'elskel
  :type '(alist
          :key-type string
          :value-type string))

(defcustom elskel-compound-data-type-descriptions '(("cons" . "(cons CAR-TYPE CDR-TYPE) The value must be a cons cell, its CAR must fit CAR-TYPE, and its CDR must fit CDR-TYPE. For example, ‘(cons string symbol)’ is a customization type which matches values such as ‘(\"foo\" . foo)’. In the customization buffer, the CAR and CDR are displayed and edited separately, each according to their specified type.")
                                                    ("list" . "(list ELEMENT-TYPES...) The value must be a list with exactly as many elements as the ELEMENT-TYPES given; and each element must fit the corresponding ELEMENT-TYPE. For example, ‘(list integer string function)’ describes a list of three elements; the first element must be an integer, the second a string, and the third a function. In the customization buffer, each element is displayed and edited separately, according to the type specified for it.")
                                                    ("group" . "(group ELEMENT-TYPES...) This works like ‘list’ except for the formatting of text in the Custom buffer. ‘list’ labels each element value with its tag; ‘group’ does not.")
                                                    ("vector" . "(vector ELEMENT-TYPES...) Like ‘list’ except that the value must be a vector instead of a list. The elements work the same as in ‘list’.")
                                                    ("alist" . "(alist :key-type KEY-TYPE :value-type VALUE-TYPE) The value must be a list of cons-cells, the CAR of each cell representing a key of customization type KEY-TYPE, and the CDR of the same cell representing a value of customization type VALUE-TYPE. The user can add and delete key/value pairs, and edit both the key and the value of each pair. If omitted, KEY-TYPE and VALUE-TYPE default to ‘sexp’. The user can add any key matching the specified key type, but you can give some keys a preferential treatment by specifying them with the ‘:options’ (*note Variable Definitions::). The specified keys will always be shown in the customize buffer (together with a suitable value), with a checkbox to include or exclude or disable the key/value pair from the alist. The user will not be able to edit the keys specified by the ‘:options’ keyword argument. The argument to the ‘:options’ keywords should be a list of specifications for reasonable keys in the alist. Ordinarily, they are simply atoms, which stand for themselves. For example: :options '(\"foo\" \"bar\" \"baz\") specifies that there are three known keys, namely ‘\"foo\"’, ‘\"bar\"’ and ‘\"baz\"’, which will always be shown first. You may want to restrict the value type for specific keys, for example, the value associated with the ‘\"bar\"’ key can only be an integer. You can specify this by using a list instead of an atom in the list. The first element will specify the key, like before, while the second element will specify the value type. For example: :options '(\"foo\" (\"bar\" integer) \"baz\") Finally, you may want to change how the key is presented. By default, the key is simply shown as a ‘const’, since the user cannot change the special keys specified with the ‘:options’ keyword. However, you may want to use a more specialized type for presenting the key, like ‘function-item’ if you know it is a symbol with a function binding. This is done by using a customization type specification instead of a symbol for the key. :options '(\"foo\" ((function-item some-function) integer) \"baz\") Many alists use lists with two elements, instead of cons cells. For example, (defcustom list-alist '((\"foo\" 1) (\"bar\" 2) (\"baz\" 3)) \"Each element is a list of the form (KEY VALUE).\") instead of (defcustom cons-alist '((\"foo\" . 1) (\"bar\" . 2) (\"baz\" . 3)) \"Each element is a cons-cell (KEY . VALUE).\") Because of the way lists are implemented on top of cons cells, you can treat ‘list-alist’ in the example above as a cons cell alist, where the value type is a list with a single element containing the real value. (defcustom list-alist '((\"foo\" 1) (\"bar\" 2) (\"baz\" 3)) \"Each element is a list of the form (KEY VALUE).\" :type '(alist :value-type (group integer))) The ‘group’ widget is used here instead of ‘list’ only because the formatting is better suited for the purpose. Similarly, you can have alists with more values associated with each key, using variations of this trick: (defcustom person-data '((\"brian\" 50 t) (\"dorith\" 55 nil) (\"ken\" 52 t)) \"Alist of basic info about people. Each element has the form (NAME AGE MALE-FLAG).\" :type '(alist :value-type (group integer boolean)))")
                                                    ("alist :key-type string :value-type string" . "Alist")
                                                    ("plist :key-type string :value-type string" . "Plist")
                                                    ("plist" . "(plist :key-type KEY-TYPE :value-type VALUE-TYPE) This customization type is similar to ‘alist’ (see above), except that (i) the information is stored as a property list, (*note Property Lists::), and (ii) KEY-TYPE, if omitted, defaults to ‘symbol’ rather than ‘sexp’.")
                                                    ("choice" . "(choice ALTERNATIVE-TYPES...) The value must fit one of ALTERNATIVE-TYPES. For example, ‘(choice integer string)’ allows either an integer or a string. In the customization buffer, the user selects an alternative using a menu, and can then edit the value in the usual way for that alternative. Normally the strings in this menu are determined automatically from the choices; however, you can specify different strings for the menu by including the ‘:tag’ keyword in the alternatives. For example, if an integer stands for a number of spaces, while a string is text to use verbatim, you might write the customization type this way, (choice (integer :tag \"Number of spaces\") (string :tag \"Literal text\")) so that the menu offers ‘Number of spaces’ and ‘Literal text’. In any alternative for which ‘nil’ is not a valid value, other than a ‘const’, you should specify a valid default for that alternative using the ‘:value’ keyword. *Note Type Keywords::. If some values are covered by more than one of the alternatives, customize will choose the first alternative that the value fits. This means you should always list the most specific types first, and the most general last. Here’s an example of proper usage: (choice (const :tag \"Off\" nil) symbol (sexp :tag \"Other\")) This way, the special value ‘nil’ is not treated like other symbols, and symbols are not treated like other Lisp expressions.")
                                                    ("radio" . "(radio ELEMENT-TYPES...) This is similar to ‘choice’, except that the choices are displayed using radio buttons rather than a menu. This has the advantage of displaying documentation for the choices when applicable and so is often a good choice for a choice between constant functions (‘function-item’ customization types).")
                                                    ("const" . "(const VALUE) The value must be VALUE—nothing else is allowed. The main use of ‘const’ is inside of ‘choice’. For example, ‘(choice integer (const nil))’ allows either an integer or ‘nil’. ‘:tag’ is often used with ‘const’, inside of ‘choice’. For example, (choice (const :tag \"Yes\" t) (const :tag \"No\" nil) (const :tag \"Ask\" foo)) describes a variable for which ‘t’ means yes, ‘nil’ means no, and ‘foo’ means “ask”.")
                                                    ("other" . "(other VALUE) This alternative can match any Lisp value, but if the user chooses this alternative, that selects the value VALUE. The main use of ‘other’ is as the last element of ‘choice’. For example, (choice (const :tag \"Yes\" t) (const :tag \"No\" nil) (other :tag \"Ask\" foo)) describes a variable for which ‘t’ means yes, ‘nil’ means no, and anything else means “ask”. If the user chooses ‘Ask’ from the menu of alternatives, that specifies the value ‘foo’; but any other value (not ‘t’, ‘nil’ or ‘foo’) displays as ‘Ask’, just like ‘foo’.")
                                                    ("function-item" . "(function-item FUNCTION) Like ‘const’, but used for values which are functions. This displays the documentation string as well as the function name. The documentation string is either the one you specify with ‘:doc’, or FUNCTION’s own documentation string.")
                                                    ("variable-item" . "(variable-item VARIABLE) Like ‘const’, but used for values which are variable names. This displays the documentation string as well as the variable name. The documentation string is either the one you specify with ‘:doc’, or VARIABLE’s own documentation string.")
                                                    ("set" . "(set TYPES...) The value must be a list, and each element of the list must match one of the TYPES specified. This appears in the customization buffer as a checklist, so that each of TYPES may have either one corresponding element or none. It is not possible to specify two different elements that match the same one of TYPES. For example, ‘(set integer symbol)’ allows one integer and/or one symbol in the list; it does not allow multiple integers or multiple symbols. As a result, it is rare to use nonspecific types such as ‘integer’ in a ‘set’. Most often, the TYPES in a ‘set’ are ‘const’ types, as shown here: (set (const :bold) (const :italic)) Sometimes they describe possible elements in an alist: (set (cons :tag \"Height\" (const height) integer) (cons :tag \"Width\" (const width) integer)) That lets the user specify a height value optionally and a width value optionally.")
                                                    ("repeat" . "(repeat ELEMENT-TYPE) The value must be a list and each element of the list must fit the type ELEMENT-TYPE. This appears in the customization buffer as a list of elements, with ‘[INS]’ and ‘[DEL]’ buttons for adding more elements or removing elements.")
                                                    ("restricted-sexp :match-alternatives (symbolp keymapp)" . "(restricted-sexp :match-alternatives CRITERIA) This is the most general composite type construct. The value may be any Lisp object that satisfies one of CRITERIA. CRITERIA should be a list, and each element should be one of these possibilities:"))
  "Alist of composite types for custom variables with descriptions.

Each entry in the list is a cons cell where the car is a string representing the
composite type, and the cdr is a string describing how to use that type."
  :group 'elskel
  :type '(alist
          :key-type string
          :value-type string))

(defcustom elskel-custom-option-keywords '((":value" . "Provide a default value. If ‘nil’ is not a valid value for the alternative, then it is essential to specify a valid default with ‘:value’. If you use this for a type that appears as an alternative inside of ‘choice’; it specifies the default value to use, at first, if and when the user selects this alternative with the menu in the customization buffer. Of course, if the actual value of the option fits this alternative, it will appear showing the actual value, not DEFAULT.")
                                           (":format \"%v\"" . "This string will be inserted in the buffer to represent the value corresponding to the type. The following ‘%’ escapes are available for use in FORMAT-STRING.")
                                           (":tag \"TAG\"" . "Use TAG (a string) as the tag for the value (or part of the value) that corresponds to this type.")
                                           (":doc \"DOC\"" . "Use DOC as the documentation string for this value (or part of the value) that corresponds to this type. In order for this to work, you must specify a value for ‘:format’, and use ‘%d’ or ‘%h’ in that value. The usual reason to specify a documentation string for a type is to provide more information about the meanings of alternatives inside a ‘choice’ type or the parts of some other composite type.")
                                           (":help-echo \"MOTION-DOC\"" . "When you move to this item with ‘widget-forward’ or ‘widget-backward’, it will display the string MOTION-DOC in the echo area. In addition, MOTION-DOC is used as the mouse ‘help-echo’ string and may actually be a function or form evaluated to yield a help string. If it is a function, it is called with one argument, the widget.")
                                           (":match (lambda (widget value) )" . "Specify how to decide whether a value matches the type. The corresponding value, FUNCTION, should be a function that accepts two arguments, a widget and a value; it should return non-‘nil’ if the value is acceptable.")
                                           (":match-alternatives (symbolp keymapp)")
                                           (":match-inline (lambda (widget inline-value))" . "Specify how to decide whether an inline value matches the type. The corresponding value, FUNCTION, should be a function that accepts two arguments, a widget and an inline value; it should return non-‘nil’ if the value is acceptable. See *note Splicing into Lists:: for more information about inline values.")
                                           (":action ACTION" . "Perform ACTION if the user clicks on a button.")
                                           (":button-face FACE" . "Use the face FACE (a face name or a list of face names) for button text displayed with ‘%[...%]’.")
                                           (":button-prefix \"PREFIX\"" . "")
                                           (":button-suffix \"SUFFIX\"" . "These specify the text to display before and after a button. Each can be: ‘nil’ No text is inserted. a string The string is inserted literally. a symbol The symbol’s value is used.")
                                           (":validate (lambda (widget) (unless (widget-value widget) (widget-put widget :error \"Invalid value\") widget))" . "Specify a validation function for input. FUNCTION takes a widget as an argument, and should return ‘nil’ if the widget’s current value is valid for the widget. Otherwise, it should return the widget containing the invalid data, and set that widget’s ‘:error’ property to a string explaining the error.")
                                           (":type-error \"STRING\"" . "STRING should be a string that describes why a value doesn’t match the type, as determined by the ‘:match’ function. When the ‘:match’ function returns ‘nil’, the widget’s ‘:error’ property will be set to STRING."))
  "Alist of keyword-value pairs for customizing elements to complete.

A list of keyword-value pairs for customizing the behavior of a widget or
option. Each keyword is a string that starts with a colon, followed by a value
that specifies how the widget should behave or be displayed."
  :group 'elskel
  :type '(alist
          :key-type string
          :value-type (choice string symbol)))

(defcustom elskel-format-specification-alist '(("%[BUTTON%]" . "Display the text BUTTON marked as a button")
                                               ("%{SAMPLE%}" . "This string will be inserted in the buffer to represent the value corresponding to the type. The following ‘%’ escapes are available for use in FORMAT-STRING.")
                                               ("%d" . "Substitute the item’s documentation string")
                                               ("%h" . "Substitute the first line of item’s documentation string")
                                               ("%v" . "Substitute the item’s value")
                                               ("%t" . "Substitute the tag here")
                                               ("%%" . "Display a literal ‘%’"))
  "An alist mapping format specification keywords to complete.

An alist mapping format specification keywords to their descriptions, used for
customizing the display of certain elements in a buffer. Each element of the
alist is a cons cell where the car is a string representing the format
specification keyword and the cdr is a string describing what the keyword will
be substituted with when formatting text."
  :group 'elskel
  :type '(alist
          :key-type string
          :value-type string))

(defun elskel--straight-keyword-complete ()
  "Offer completion for repository type and return selection."
  (let* ((choices
          '("git"
            "built-in"
            "nil"))
         (result (completing-read ":type\s" choices)))
    (pcase result
      ("built-in"
       (prin1-to-string
        '(:type
          built-in)))
      ("git"
       (require 'gh-repo
                nil
                t)
       (let ((user
              (ignore-errors
                (car
                 (process-lines
                  "git"
                  "config"
                  "user.email")))))
         (prin1-to-string
          (if
              (and
               user
               (yes-or-no-p
                (format
                 "Repo of %s?"
                 user)))
              `(:repo
                ,(substring-no-properties
                  (if
                      (fboundp
                       'gh-repo-read-user-repo)
                      (gh-repo-read-user-repo
                       "Repo:"
                       'identity)
                    (read-string
                     "Repo: ")))
                :type
                git
                :flavor
                nil
                :host
                github)
            (let ((repo
                   (if
                       (fboundp
                        'gh-repo-search-repos)
                       (gh-repo-search-repos "+language:elisp")
                     (read-string
                      "Repo: "))))
              `(:repo ,repo
                :type
                git
                :flavor
                nil
                :host
                github))))))
      (_
       result))))

(defun elskel--candidate-man-pages ()
  "Generate candidates for man page completion."
  (require 'man)
  (with-temp-buffer
    (setq default-directory (Man-default-directory))
    (let ((process-environment (copy-sequence process-environment)))
      (setenv "COLUMNS" "999")
      (when (eq 0
                (ignore-errors
                  (process-file
                   manual-program nil '(t nil) nil
                   "-k" (concat
                         "^"
                         ""))))
        (mapcar (lambda (it)
                  (prin1-to-string
                   (substring-no-properties
                    (replace-regexp-in-string "[(][0-9]+[)]" "" it))))
                (Man-parse-man-k))))))

(defun elskel--select-info-manual ()
  "Display a prompt to choose an Info manual name."
  (require 'info)
  (progn
    (info-initialize)
    (completing-read "Manual name: "
                     (info--filter-manual-names
                      (info--manual-names nil))
                     nil t)))

(defun elskel--get-sexps-backward ()
  "Collect sexps before point, ignoring comments."
  (save-excursion
    (let ((sexps)
          (parse-sexp-ignore-comments t))
      (condition-case _
          (progn
            ;; First account for the case the point is directly over a
            ;; beginning of a nested sexp.
            (condition-case _
                (progn
                  (forward-sexp -1)
                  (forward-sexp 1))
              (error))
            (while
                (let ((p (point)))
                  (forward-sexp -1)
                  (when (< (point) p)
                    (unless (looking-at comment-start)
                      (push (sexp-at-point) sexps))))))
        (error))
      sexps)))

(defun elskel--default-transient-prefix-slots ()
  "Generate alist of transient prefix slot defaults."
  (let* ((alist (list
                 (cons :transient-suffix
                       '("t" "nil"))
                 (cons :transient-non-suffix
                       '("t" "nil"))
                 (cons :value '("'()"))
                 (cons :incompatible '("'()"))
                 (cons :refresh-suffixes
                       '("t" "nil"))
                 (cons :man-page #'elskel--candidate-man-pages)
                 (cons :info-manual #'elskel--select-info-manual)
                 (cons :suffix-description
                       (list
                        "#'transient-command-summary-or-name")))))
    (seq-reduce (lambda (acc curr)
                  (unless (assq curr acc)
                    (push (cons curr nil) acc))
                  acc)
                (mapcar #'car (elskel--get-class-initialization-arguments
                               'transient-prefix))
                alist)))

(defun elskel--autocomplete-transient-prefix-slots ()
  "Fill in transient prefix slots with completion."
  (pcase (elskel--get-sexps-backward)
    (`(transient-define-prefix ,(pred (symbolp)) ,(pred (listp)) . ,body)
     (let* ((form (car (last body)))
            (tslots (elskel--default-transient-prefix-slots))
            (slots (assq form tslots)))
       (cond ((and (consp slots)
                   (functionp (cdr slots)))
              (elskel--insert " ")
              (elskel--insert
               (elskel--completing-read-with-preview "Value:"
                                                     (funcall
                                                      (cdr
                                                       slots)))))
             ((and (consp slots))
              (elskel--insert " ")
              (elskel--insert
               (elskel--completing-read-with-preview "Slot: "
                                                     (cdr
                                                      slots))))
             ((vectorp form)
              (unless (or (eq (car-safe (sexp-at-point)) 'interactive)
                          (save-excursion
                            (elskel--complete-forward-with 'forward-sexp 1)
                            (eq (car-safe (sexp-at-point)) 'interactive)))
                (elskel--insert "(interactive)")))
             ((and (keywordp form))
              (let ((prefix
                     (elskel--completing-read-with-preview
                      "Choice"
                      (all-completions
                       (format "%s"
                               form)
                       (mapcar (pcase-lambda
                                 (`(,k
                                    .
                                    ,_v))
                                 (format
                                  "%s"
                                  k))
                               tslots)))))
                (elskel--insert prefix)))
             ((not (seq-find #'vectorp body))
              (when-let ((cand (elskel--completing-read-with-preview "Slot: "
                                                                     tslots)))
                (elskel--insert
                 (concat cand (if (looking-at " ")  "" " "))))))))))

(defun elskel--get-class-initialization-arguments (sym)
  "Extract class slot initialization arguments.

Argument SYM is a symbol representing the class for which to retrieve
initialization arguments."
  (require 'eieio-core)
  (require 'eieio)
  (let* ((obj
          (when (fboundp 'eieio--class-object)
            (eieio--class-object sym)))
         (slots
          (when (fboundp 'eieio-class-slots)
            (eieio-class-slots obj))))
    (remove nil
            (mapcar (lambda (it)
                      (let* ((name
                              (when (fboundp 'eieio-slot-descriptor-name)
                                (eieio-slot-descriptor-name it)))
                             (props (cl--slot-descriptor-props it))
                             (initarg
                              (when (fboundp 'eieio--class-slot-initarg)
                                (eieio--class-slot-initarg obj name))))
                        (when initarg
                          (append (list
                                   initarg
                                   (when
                                       (when (fboundp 'slot-boundp)
                                         (slot-boundp sym name))
                                     (when (fboundp 'eieio-oref-default)
                                       (eieio-oref-default obj name))))
                                  props))))
                    slots))))

(defun elskel-minibuffer-completion-metadata ()
  "Retrieve metadata for minibuffer completion."
  (completion-metadata
   (buffer-substring-no-properties
    (minibuffer-prompt-end)
    (max (minibuffer-prompt-end)
         (point)))
   minibuffer-completion-table
   minibuffer-completion-predicate))

(defun elskel-minibuffer-ivy-selected-candidate ()
  "Get selected candidate or text from Ivy minibuffer."
  (when (and (memq 'ivy--queue-exhibit post-command-hook)
             (boundp 'ivy-text)
             (boundp 'ivy--length)
             (boundp 'ivy-last)
             (fboundp 'ivy--expand-file-name)
             (fboundp 'ivy-state-current))
    (cons
     (completion-metadata-get (ignore-errors
                                (elskel-minibuffer-completion-metadata))
                              'category)
     (ivy--expand-file-name
      (if (and (> ivy--length 0)
               (stringp (ivy-state-current ivy-last)))
          (ivy-state-current ivy-last)
        ivy-text)))))

(defun elskel-minibuffer-default-candidates ()
  "Retrieve default completion candidates from the minibuffer."
  (when (minibufferp)
    (let* ((all (completion-all-completions
                 (minibuffer-contents)
                 minibuffer-completion-table
                 minibuffer-completion-predicate
                 (max 0 (- (point)
                           (minibuffer-prompt-end)))))
           (last (last all)))
      (when last (setcdr last nil))
      (cons
       (completion-metadata-get (elskel-minibuffer-completion-metadata)
                                'category)
       all))))

(defun elskel-minibuffer-default-completion ()
  "Retrieve default completion from minibuffer."
  (when (and (minibufferp) minibuffer-completion-table)
    (pcase-let* ((`(,category . ,candidates)
                  (elskel-minibuffer-default-candidates))
                 (contents (minibuffer-contents))
                 (top (if (test-completion contents
                                           minibuffer-completion-table
                                           minibuffer-completion-predicate)
                          contents
                        (let ((completions (completion-all-sorted-completions)))
                          (if (null completions)
                              contents
                            (concat
                             (substring contents
                                        0 (or (cdr (last completions)) 0))
                             (car completions)))))))
      (cons category (or (car (member top candidates)) top)))))

(defvar elskel-minibuffer-candidate-finders
  '(elskel-minibuffer-ivy-selected-candidate
    elskel-minibuffer-default-completion)
  "List of functions to find minibuffer completion targets.")

(defun elskel-minibuffer-current-candidate ()
  "Retrieve the current candidate from the minibuffer."
  (let (target)
    (run-hook-wrapped
     'elskel-minibuffer-candidate-finders
     (lambda (fun)
       (when-let ((result (funcall fun)))
         (when (and (cdr-safe result)
                    (stringp (cdr-safe result))
                    (not (string-empty-p (cdr-safe result))))
           (setq target result)))
       (and target (minibufferp))))
    target))

(defun elskel--completing-read-with-preview-action (prompt collection &optional
                                                           preview-action keymap
                                                           predicate
                                                           require-match
                                                           initial-input hist
                                                           def
                                                           inherit-input-method)
  "Display preview while reading input with completion.

Argument PROMPT is a string to display as the prompt in the minibuffer.

Argument COLLECTION is a list or array of strings, or an alist where keys are
strings, or a function that generates such a list.

Optional argument PREVIEW-ACTION is a function that takes a single string
argument and is called with the current selection.

Optional argument KEYMAP is a keymap to use while in the minibuffer.

Optional argument PREDICATE is a function that takes one argument and returns
non-nil if the argument should be included in the completion list.

Optional argument REQUIRE-MATCH is a boolean; if non-nil, the user is not
allowed to exit unless the input matches one of the completions.

Optional argument INITIAL-INPUT is a string to prefill the minibuffer with.

Optional argument HIST is a symbol representing a history list to use for
completion.

Optional argument DEF is a string or list of strings that are the default
values.

Optional argument INHERIT-INPUT-METHOD is a boolean; if non-nil, the minibuffer
inherits the current input method."
  (let ((collection (if (stringp (car-safe collection))
                        (copy-tree collection)
                      collection))
        (prev))
    (minibuffer-with-setup-hook
        (lambda ()
          (when (minibufferp)
            (when keymap
              (let ((map (make-composed-keymap keymap
                                               (current-local-map))))
                (use-local-map map)))
            (when preview-action
              (add-hook 'after-change-functions
                        (lambda (&rest _)
                          (pcase-let
                              ((`(,_category .
                                  ,current)
                                (elskel-minibuffer-current-candidate)))
                            (with-minibuffer-selected-window
                              (cond ((or (not prev)
                                         (not (string=
                                               prev
                                               current)))
                                     (setq prev current)
                                     (funcall
                                      preview-action
                                      current))))))
                        nil t))))
      (completing-read prompt
                       collection
                       predicate
                       require-match initial-input hist
                       def inherit-input-method))))

(defun elskel--symbols-make-annotate-fn (alist)
  "Make annotation function from ALIST."
  (let* ((names (remove nil
                        (mapcar (lambda (it)
                                  (if (symbolp (car-safe it))
                                      (length (symbol-name (car-safe it)))
                                    (if (stringp (car-safe it))
                                        (length (car-safe it))
                                      nil)))
                                alist)))
         (max-len (if names (1+ (apply #'max names))
                    1)))
    (lambda (str)
      (let* ((sym (if (stringp str)
                      (intern str)
                    str))
             (pl (or (cdr (assq
                           sym
                           alist))
                     (cdr (assoc-string
                           str
                           alist))))
             (type (propertize
                    (capitalize
                     (format "%s"
                             (plist-get pl
                                        :type)))
                    'face
                    'font-lock-keyword-face))
             (args (propertize
                    (let ((arglist (plist-get pl :args)))
                      (if (stringp arglist)
                          (substring-no-properties arglist)
                        (if (not arglist)
                            "()"
                          (format "%S" (mapcar (lambda (arg)
                                                 (cond ;; Parameter name.
                                                  ((symbolp arg)
                                                   (let ((name (symbol-name
                                                                arg)))
                                                     (cond ((string-match
                                                             "\\`&"
                                                             name)
                                                            (bare-symbol
                                                             arg))
                                                           ((string-match
                                                             "\\`_."
                                                             name)
                                                            (intern
                                                             (upcase
                                                              (substring
                                                               name
                                                               1))))
                                                           (t
                                                            (intern
                                                             (upcase
                                                              name))))))
                                                  ;; Parameter with a default value (from
                                                  ;; cl-defgeneric etc).
                                                  ((and (consp arg)
                                                        (symbolp (car
                                                                  arg)))
                                                   (cons
                                                    (intern
                                                     (upcase
                                                      (symbol-name
                                                       (car
                                                        arg))))
                                                    (cdr arg)))
                                                  ;; Something else.
                                                  (t arg)))
                                               arglist)))))
                    'face
                    'font-lock-operator-face))
             (lib (if (plist-get pl :lib)
                      (format "%s"  (plist-get pl :lib))
                    ""))
             (interactivep
              (and (plist-get pl :interactive)
                   (propertize "Command" 'face
                               'diff-refine-added)))
             (autloaded (and (plist-get pl :autoload)
                             (propertize
                              "AUTOLOAD"
                              'face 'font-lock-warning-face)))
             (final
              (remove nil
                      (seq-filter #'stringp (list
                                             (if interactivep
                                                 (concat interactivep " " type)
                                               type)
                                             args
                                             autloaded
                                             lib
                                             (and (if (stringp
                                                       (plist-get pl :doc))
                                                      (car (split-string
                                                            (plist-get
                                                             pl :doc)
                                                            "\n" t))
                                                    nil)))))))
        (concat
         (propertize " " 'display `(space :align-to ,max-len))
         (string-join
          final
          " "))))))

(defun elskel--completing-read-hook (prompt)
  "PROMPT for input with completion and annotations, updating preview dynamically.

Argument PROMPT is a string to display as the prompt in the minibuffer."
  (let* ((ob-hooks
          (let ((hooks))
            (mapatoms (lambda (sym)
                        (when-let*
                            ((name
                              (when (and (symbolp sym)
                                         (not (keywordp sym)))
                                (symbol-name sym)))
                             (res
                              (cond ((not name)
                                     nil)
                                    ((and
                                      (string-suffix-p "-mode" name))
                                     (list
                                      (format "%s-hook" sym)
                                      :type 'hook
                                      :doc
                                      (ignore-errors
                                        (substring-no-properties (documentation
                                                                  sym)))))
                                    ((or (string-suffix-p "-hook" name)
                                         (string-suffix-p "-functions"
                                                          name))
                                     (list
                                      sym
                                      :type 'defvar
                                      :doc
                                      (ignore-errors
                                        (substring-no-properties
                                         (documentation-property sym
                                                                 'variable-documentation
                                                                 t))))))))
                          (push res hooks)))
                      obarray)
            hooks))
         (alist
          (append (elskel--extra--read-file
                   buffer-file-name)
                  ob-hooks))
         (annotfn (elskel--symbols-make-annotate-fn
                   alist))
         (table (lambda (str pred action)
                  (if (eq action
                          'metadata)
                      `(metadata
                        (annotation-function .
                         ,annotfn))
                    (complete-with-action
                     action alist str
                     pred))))
         (pred (lambda (it)
                 (when-let* ((sym (car-safe it))
                             (name (if (symbolp sym)
                                       (symbol-name sym)
                                     sym)))
                   (or (string-suffix-p "-hook" name)
                       (string-suffix-p "-functions" name)
                       (string-suffix-p "-mode" name))))))
    (elskel--completing-read-with-preview
     prompt
     table
     nil
     nil
     pred)))

(defun elskel--completing-read-definition-annotated (prompt &optional pred)
  "PROMPT for input with completion and annotations from a file.

Argument PROMPT is a string to display as the prompt in the minibuffer.

Optional argument PRED is a function that takes one argument and returns non-nil
if the argument should be included in the completion list."
  (let* ((alist
          (elskel--extra--read-file
           buffer-file-name))
         (annotfn (elskel--symbols-make-annotate-fn
                   alist))
         (table (lambda (str pred action)
                  (if (eq action
                          'metadata)
                      `(metadata
                        (annotation-function .
                         ,annotfn))
                    (complete-with-action
                     action alist str
                     pred)))))
    (elskel--completing-read-with-preview
     prompt
     table
     nil
     nil
     pred)))



(defun elskel--completing-read-variable-sym (&optional prompt)
  "Select variable symbol with completion.

Optional argument PROMPT is a string to display as the prompt in the minibuffer.
It defaults to \"Variable \"."
  (elskel--completing-read-definition-annotated
   (or prompt "Variable ")
   (lambda (it)
     (when-let* ((pl (cdr-safe it))
                 (type (plist-get pl :type)))
       (pcase type
         ((or 'defvar 'defcustom 'defconst
              'defvar-local)
          t))))))

(defun elskel--completing-read-commmand-or-autoload (&optional prompt)
  "PROMPT for a command or autoload with completion.

Optional argument PROMPT is a string to display as the prompt in the minibuffer."
  (elskel--completing-read-definition-annotated
   (or prompt "Command: ")
   (lambda (it)
     (when-let ((pl (cdr-safe it)))
       (or (plist-get pl :interactive)
           (plist-get pl :autoload))))))

(defun elskel--completing-read-with-preview (prompt alist &optional
                                                    display-to-real keymap pred
                                                    require-match initial-input
                                                    hist &rest args)
  "Preview completion candidates while prompting for input.

Argument PROMPT is a string to display as the prompt in the minibuffer.

Argument ALIST is a list or array of strings, or an alist where keys are
strings, or a function that generates such a list.

Optional argument DISPLAY-TO-REAL is a function that takes a single string
argument and returns the real value associated with the display value.

Optional argument KEYMAP is a keymap to use while in the minibuffer.

Optional argument PRED is a function that takes one argument and returns
non-nil if the argument should be included in the completion list.

Optional argument REQUIRE-MATCH is a boolean; if non-nil, the user is not
allowed to exit unless the input matches one of the completions.

Optional argument INITIAL-INPUT is a string to prefill the minibuffer with.

Optional argument HIST is a symbol representing a history list to use for
completion.

Remaining arguments ARGS are additional arguments passed to the internal
function `elskel--completing-read-with-preview-action'."
  (let ((pos (point))
        (buff (current-buffer))
        (display-to-real-fn (if (functionp display-to-real)
                                display-to-real
                              #'elskel--get-completion-prefix))
        (ov))
    (unwind-protect
        (apply #'elskel--completing-read-with-preview-action
               prompt
               alist
               (lambda (it)
                 (when (overlayp ov)
                   (delete-overlay ov))
                 (when (buffer-live-p buff)
                   (setq ov (elskel--make-overlay pos
                                                  pos nil nil nil 'after-string
                                                  (or (funcall display-to-real-fn
                                                               it)
                                                      it)))))
               keymap
               pred
               require-match
               initial-input
               hist
               args)
      (when (overlayp ov)
        (delete-overlay ov)))))

(defun elskel--make-overlay (start end &optional buffer front-advance
                                   rear-advance &rest props)
  "Create an overlay in BUFFER between START and END, applying PROPS.

Argument START is the position at which the overlay begins.

Argument END is the position at which the overlay ends.

Optional argument BUFFER is the buffer in which to create the overlay; defaults
to the current buffer.

Optional argument FRONT-ADVANCE when non-nil, makes the START of the overlay
advance when text is inserted at the overlay's beginning.

Optional argument REAR-ADVANCE when non-nil, makes the END of the overlay
advance when text is inserted at the overlay's end.

Remaining arguments PROPS are properties to set on the overlay, provided as a
property list."
  (let ((overlay (make-overlay start end buffer front-advance
                               rear-advance)))
    (dotimes (idx (length props))
      (when (eq (logand idx 1) 0)
        (let* ((prop-name (nth idx props))
               (val (plist-get props prop-name)))
          (overlay-put overlay prop-name val))))
    overlay))

(defun elskel--autocomplete-from-alist (base-prompt alist &rest args)
  "Merge user choices into a result list from ALIST.

Argument BASE-PROMPT is a string used as the base for the interactive prompt.

Argument ALIST can be a list, vector, or function that provides the completion
candidates.

Remaining arguments ARGS are additional arguments passed to `completing-read'."
  (let ((current)
        (result)
        (count 0))
    (if (functionp alist)
        (funcall alist)
      (while (and alist
                  (not (keywordp alist))
                  (or (listp alist)
                      (vectorp alist))
                  (not (functionp alist)))
        (when (vectorp alist)
          (setq alist
                (seq-map-indexed (lambda (it i)
                                   (cons (format "%s" i) it))
                                 (append alist nil))))
        (setq count (1+ count))
        (let ((prompt (format "%s %s\s "
                              base-prompt
                              (if (= 1 (% count 2))
                                  (string-join
                                   (reverse result) "\s")
                                (string-join
                                 (or result "") "\s")))))
          (setq current (apply #'completing-read prompt alist args))
          (push current result)
          (setq alist (or (cdr-safe (assoc current alist))
                          (cdr (seq-find
                                (lambda (it) (when-let ((val (car-safe it)))
                                               (pcase val
                                                 ((pred symbolp)
                                                  (string=
                                                   current
                                                   (symbol-name val))))))
                                alist))))
          (when (functionp alist)
            (let ((value (funcall alist)))
              (if (and (listp value)
                       (not (listp (car value))))
                  (setq alist value)
                (push value result)
                (setq alist nil))))
          alist))
      (setq result
            (reverse result)))))

(defun elskel--common-prefix (s1 s2)
  "Find common prefix of strings S1 and S2.

Argument S1 is a string to compare with S2.

Argument S2 is a string to compare with S1."
  (declare (pure t)
           (side-effect-free t))
  (let ((search-length (min (length s1)
                            (length s2)))
        (i 0))
    (while (and (< i search-length)
                (= (aref s1 i)
                   (aref s2 i)))
      (setq i (1+ i)))
    (substring s1 0 i)))

(defun elskel--provided-name ()
  "Extract provided feature name from Emacs Lisp code."
  (let ((parse-sexp-ignore-comments t)
        (found))
    (save-excursion
      (goto-char (point-max))
      (with-syntax-table emacs-lisp-mode-syntax-table
        (while
            (when (and (not found)
                       (condition-case nil
                           (progn (backward-list) t)
                         (error nil)))
              (when-let ((item (ignore-errors (sexp-at-point))))
                (pcase item
                  (`(provide (quote ,(and (pred (symbolp)) sym)) . ,_)
                   (setq found (symbol-name sym))))))))
      found)))

(defun elskel--rpartial (fun &rest args)
  "Return a partial application of a function FUN to right-hand ARGS.

ARGS is a list of the last N arguments to pass to FN. The result is a new
function which does the same as FUN, except that the last N arguments are fixed
at the values with which this function was called."
  (declare (side-effect-free error-free))
  (lambda (&rest pre-args)
    (apply fun (append pre-args args))))



(defun elskel--symbol-p (thing)
  "Check if THING is a non-t symbol.

Argument THING is the object being checked if it's a symbol and not t."
  (and thing
       (symbolp thing)
       (not (eq thing t))))

(defun elskel--symbol-name-p (thing)
  "Check if THING is a non-t, non-keyword symbol.

Argument THING is the object being checked if it's a symbol and not a keyword."
  (and (elskel--symbol-p thing)
       (not (keywordp thing))))


(defun elskel--get-def-names ()
  "Extract defined symbol names from buffer's Lisp code."
  (let ((syms))
    (elskel--read-with (lambda (sexp)
                         (pcase sexp
                           (`(,(or 'defun 'cl-defun 'defmacro 'cl-defmacro)
                              ,(and (pred (symbolp))
                                sym
                                (guard (not (memq sym '(t nil)))))
                              ,(pred (listp))
                              . ,_)
                            (push (symbol-name sym) syms))
                           (`(,(or 'defvar 'defvar-local 'defvar-keymap
                                'defcustom)
                              ,(and (pred (symbolp))
                                sym
                                (guard (not (memq sym '(t nil)))))
                              . ,_)
                            (push (symbol-name sym) syms)))))
    syms))

(defun elskel--infer-definition-prefix ()
  "Guess and return the common prefix for definition names."
  (or (elskel--provided-name)
      (let ((prefix (or
                     (let ((strings (elskel--get-def-names)))
                       (seq-reduce (lambda (acc it)
                                     (let ((prefix
                                            (elskel--common-prefix acc it)))
                                       (if (string-empty-p prefix)
                                           acc
                                         prefix)))
                                   strings
                                   (pop strings)))
                     (replace-regexp-in-string
                      "[][\s\t\n\r\f*'\")(]+"
                      (lambda (s)
                        (if (string-empty-p (string-trim s)) "-" ""))
                      (file-name-base (buffer-name))))))
        (replace-regexp-in-string "[-]+$" ""
                                  prefix))))

(defun elskel--quoted-symbol-or-function (sexp)
  "Extract symbol from quoted or sharp-quoted expression.

Argument SEXP is an s-expression to be analyzed for a quoted symbol or function."
  (pcase sexp
    (`(function ,(and (pred (symbolp)) sym))
     sym)
    (`(quote ,(and (pred (symbolp)) sym))
     sym)))

(defun elskel--command-symbol-from-define-key (sexp)
  "Extract command symbol from `define-key' form.

Argument SEXP is an s-expression representing a call to `define-key'."
  (pcase sexp
    (`(define-key ,(pred symbolp)
       ,(or
         `(pred (stringp))
         `(pred (vectorp))
         `(kbd ,(pred (stringp))))
       ,(or `(function ,(and (pred (symbolp)) sym))
         `(quote ,(and (pred (symbolp)) sym))))
     sym)))

(defun elskel--is-keymap-definition (sexp)
  "Check if SEXP is a keymap definition.

Argument SEXP is an S-expression to analyze for keymap patterns."
  (pcase sexp
    (`(defvar ,(and (pred symbolp) name)
        (let ((,(and (pred symbolp) sym)
               (make-sparse-keymap)))
         . ,(and body (guard (eq sym (car (last body))))
             (guard (seq-every-p (lambda (it)
                                   (memq (car-safe it)
                                    '(set-keymap-parent
                                      define-key lookup-key
                                      make-composed-keymap
                                      when if
                                      unless)))
                     (butlast body)))))
        . ,_rest)
     (cons name
           (delq nil (mapcar #'elskel--command-symbol-from-define-key body))))
    (`(defvar-keymap ,(and (pred symbolp) name)
       . ,rest)
     (let ((body (reverse rest))
           (cmds))
       (while
           (let* ((fn
                   (pop body))
                  (key (pop body)))
             (when (and
                    key
                    fn
                    (stringp key))
               (when (memq (car-safe fn) '(quote function))
                 (setq fn (elskel--quoted-symbol-or-function fn))
                 (push fn cmds))
               t)))
       (cons name cmds)))))

(defun elskel--read-with (fn &optional transform-fn)
  "Collect sexps from buffer if they match FN, optionally transforming them.

Argument FN is a function that takes one argument and returns non-nil if the
sexp should be included.

Optional argument TRANSFORM-FN is a function that takes one argument and returns
the transformed sexp."
  (let ((sexps)
        (sexp))
    (save-excursion
      (goto-char (point-min))
      (while (setq sexp (ignore-errors (read (current-buffer))))
        (when (funcall fn sexp)
          (push (if transform-fn
                    (funcall transform-fn sexp)
                  sexp)
                sexps))))
    sexps))

(defun elskel--complete-forward-with (fn &optional n)
  "Move forward in buffer using FN, optionally by N step.

Argument FN is a function to be called with N as its argument.

Optional argument N is an integer specifying the number of times to call FN,
defaulting to 1."
  (unless n (setq n 1))
  (let ((parse-sexp-ignore-comments t)
        (pos (point)))
    (when-let ((str-start (nth 8 (syntax-ppss (point)))))
      (goto-char str-start))
    (condition-case nil
        (progn (funcall fn n)
               (point))
      (error (goto-char pos)
             nil))))

(defun elskel--use-package-keywords-completions-alist ()
  "Generate completions for `use-package' keywords."
  (seq-uniq
   (append
    elskel-use-package-keyword-completions
    (mapcar
     (lambda (it)
       (list (symbol-name it)))
     (when (boundp 'use-package-keywords)
       use-package-keywords)))
   (lambda (a b)
     (string= (car a)
              (car b)))))

(defun elskel--skeleton-insert ()
  "Insert a predefined code snippet into the buffer.

Optional argument CHOICE is a string representing the user's choice of snippet
to insert."
  (let* ((alist (mapcar (lambda (it)
                          (ignore-errors
                            (cons (get it 'elskel-skeleton)
                                  it)))
                        elskel-skeletons-skels))
         (skeleton (elskel--completing-read-with-preview
                    "Insert\s"
                    (mapcar #'car alist)
                    nil
                    nil
                    nil
                    t)))
    (call-interactively (cdr (assoc-string skeleton alist)))))

(defun elskel--get-completion-prefix (item)
  "Extract prefix for completion from ITEM at point.

Argument ITEM is a string representing the completion item."
  (let* ((pos (point))
         (item-chars (reverse (append item nil)))
         (char (char-before pos)))
    (catch 'found
      (while
          (when-let ((chars (member char item-chars)))
            (setq item-chars (cdr chars))
            (let* ((str (mapconcat #'char-to-string (reverse chars) ""))
                   (beg (- pos
                           (length str)))
                   (prefix (and (>= beg (point-min))
                                (buffer-substring-no-properties beg pos))))
              (if (and prefix
                       (string-prefix-p prefix str))
                  (throw 'found (substring-no-properties item (length prefix)))
                t)))))))

(defun elskel--insert (item)
  "Insert ITEM or its completion prefix into the buffer.

Argument ITEM is a string that will be inserted into the buffer."
  (when item
    (insert (or (elskel--get-completion-prefix item) item))))


(defun elskel--interactive-place-p ()
  "Check if point is at an interactive place in code."
  (and (save-excursion
         (let ((prefix (elskel--get-completion-prefix "interactive"))
               (ppss))
           (when prefix
             (forward-char (- (length prefix)
                              (length "interactive")))
             (setq ppss (syntax-ppss (point)))
             (when (and (cadr ppss)
                        (= (1+ (cadr ppss))
                           (point)))
               (elskel--complete-forward-with #'backward-up-list 1))))
         (when (elskel--complete-forward-with #'backward-sexp 1)
           (and (elskel--complete-forward-with #'backward-sexp (if
                                                                   (save-excursion
                                                                     (nth 3
                                                                          (syntax-ppss
                                                                           (1+
                                                                            (point)))))
                                                                   3
                                                                 2))
                (and
                 (not (elskel--complete-forward-with #'backward-sexp 1))
                 (elskel--complete-forward-with #'backward-up-list 1)
                 (pcase (sexp-at-point)
                   (`(,(or 'defun 'cl-defun)
                      ,(and (pred (symbolp))
                        sym
                        (guard (not (memq sym '(t nil)))))
                      ,(pred (listp))
                      . ,body-list)
                    (let ((body (if (stringp (car body-list))
                                    (cadr body-list)
                                  (car body-list))))
                      (not (eq (car-safe body) 'interactive)))))))))))

(defun elskel--autocomplete-use-package-keywords ()
  "Autocomplete `use-package' keywords and insert them."
  (setq elskel--use-package-keywords-completions
        (elskel--use-package-keywords-completions-alist))
  (let ((words (mapcar #'car (elskel--use-package-keywords-completions-alist)))
        (prefix
         (when-let ((s (symbol-at-point)))
           (symbol-name s))))
    (cond ((and prefix (member prefix words))
           (insert "\s")
           (elskel--autocomplete-use-package-keywords))
          ((and prefix
                (all-completions prefix words))
           (elskel--insert (string-join
                            (flatten-list
                             (elskel--autocomplete-from-alist
                              prefix
                              (seq-filter (pcase-lambda (`(,k . ,_v))
                                            (string-prefix-p prefix k))
                                          elskel--use-package-keywords-completions)))
                            "\s")))
          ((setq prefix (save-excursion
                          (skip-chars-backward "\s\t\n")
                          (car (member (when-let ((s (symbol-at-point)))
                                         (symbol-name s))
                                       words))))
           (let* ((sublist
                   (cdr (assoc prefix
                               elskel--use-package-keywords-completions)))
                  (choice
                   (cond ((stringp sublist)
                          sublist)
                         ((functionp sublist)
                          (funcall sublist))
                         ((and (listp sublist)
                               (> (length sublist) 0))
                          (string-join
                           (flatten-list
                            (elskel--autocomplete-from-alist "Sublist" sublist))
                           "\s")))))
             (when (stringp choice)
               (insert choice))))
          (t (insert
              (string-join
               (flatten-list
                (elskel--autocomplete-from-alist
                 "Complete: "
                 elskel--use-package-keywords-completions))
               "\s"))))))

(defun elskel--insert-function-argument-with-completion ()
  "Insert a Lisp argument with completion."
  (let ((prefix
         (when-let ((sym (symbol-at-point)))
           (format "%s" sym)))
        (args (save-excursion
                (elskel--goto-outer-list)
                (sexp-at-point)))
        (collection '(&optional &rest &key
                      item
                      it
                      file
                      start
                      end
                      beg
                      end)))
    (setq collection (mapcar (apply-partially #'format "%s") (seq-difference collection args)))
    (let ((choice (completing-read "Argument: "
                                   (if prefix
                                       (all-completions prefix collection)
                                     collection))))
      (insert
       (if prefix
           (concat
            (if (string-prefix-p prefix choice) "" " ")
            (if (string-prefix-p prefix choice)
                (substring choice (length prefix))
              choice)
            (if (string-prefix-p "&" choice) " " ""))
         (concat
          choice
          (if (string-prefix-p "&" choice) " " "")))))))

(defun elskel--is-within-function-arguments ()
  "Check if point is within function arguments."
  (save-excursion
    (and (elskel--complete-forward-with #'backward-up-list 1)
         (elskel--complete-forward-with #'backward-sexp 1)
         (when-let ((sym (symbol-at-point)))
           (or (and (eq sym 'lambda)
                    (not (elskel--complete-forward-with #'backward-sexp 1)))
               (and (elskel--complete-forward-with #'backward-sexp 1)
                    (memq (symbol-at-point)
                          '(cl-defun defun transient-define-prefix
                            defclass cl-defmethod))
                    (not (elskel--complete-forward-with #'backward-sexp 1))))))))

(defun elskel--use-package-bind-cons-p (sexp)
  "Check if SEXP is a valid `use-package' key binding form.

Argument SEXP is a sexp (s-expression) that is checked to match specific
patterns for key binding declarations."
  (pcase sexp
    (`(,(or (pred (vectorp))
         (pred (stringp)))
       nil)
     t)
    (`(,(or (pred (vectorp))
         (pred (stringp)))
       .
       ,(pred (symbolp)))
     t)
    (`(,(or (pred (vectorp))
         (pred (stringp)))
       nil
       ,(pred (symbolp)))
     t)))

(defun elskel--use-package-inside-bind-consp ()
  "Check if point is inside a `use-package' key binding form."
  (if-let ((sexps (elskel--get-sexps-backward)))
      (elskel--use-package-bind-cons-p
       sexps)
    (save-excursion
      (and (elskel--goto-outer-list)
           (elskel--use-package-bind-cons-p (sexp-at-point))))))

(defun elskel--after-use-package-bind-p ()
  "Check if `:bind' or `:bind*' is used after `use-package'."
  (when-let* ((items (elskel--get-sexps-backward))
              (sym (and
                    items
                    (memq (car items)
                          '(use-package use-package!))
                    (symbolp (cadr items))
                    (car (last items)))))
    (and (memq sym '(:bind :bind*))
         (cons (cadr items) sym))))

(defun elskel--check-use-package-bind-list-p (items)
  "Check if ITEMS is a valid `use-package' bind list.

Argument ITEMS is a list of elements to check for valid `use-package' key
binding forms."
  (and (listp items)
       (let ((valid t)
             (keywords))
         (while (and valid items)
           (let ((curr (pop items)))
             (setq valid (or (elskel--use-package-bind-cons-p curr)
                             (pcase curr
                               ((pred (keywordp))
                                (push curr keywords)
                                (if (not items)
                                    curr
                                  (and (symbolp (car items))
                                       (push (car items) keywords)
                                       (pop items))))
                               ((pred (stringp))
                                (key-valid-p curr)))))))
         (cond (valid
                (or (reverse keywords)
                    valid))
               (t valid)))))

(defun elskel--complete-inside-use-package-bind ()
  "Complete command or keymap names within `use-package' bindings."
  (let* ((full-context (elskel--inside-use-package-bind-p))
         (context (car (last full-context)))
         (found (car full-context))
         (items (elskel--extra-get-library-items (car found)))
         (commands (cdr (assq :commands
                              items)))
         (maps (cdr (assq :bind items)))
         (alist
          (pcase context
            ('command commands)
            (:map maps)
            (_ (if (elskel--use-package-inside-bind-consp)
                   (or commands (mapcan 'cdr items))
                 (or (append commands maps)
                     (mapcan 'cdr items))))))
         (display-to-real
          (pcase context
            ('command nil)
            ;; ((guard (and (not (elskel--use-package-inside-bind-consp))
            ;;              (not (save-excursion
            ;;                     (elskel--goto-outer-list)
            ;;                     (looking-at "()"))))))
            (_ (lambda (str)
                 (let* ((sym (intern str))
                        (val (cdr (assq sym alist)))
                        (type (plist-get val :type))
                        (varp
                         (memq type
                               '(defvar defvar-keymap defvar-map)))
                        (citems (elskel--get-sexps-backward)))
                   (pcase citems
                     (`(,(or (pred (vectorp))
                          (pred (stringp))))
                      (concat " . " str))
                     (`(,(or (pred (vectorp))
                          (pred (stringp)))
                        nil)
                      str)
                     (`(,(or (pred (vectorp))
                          (pred (stringp)))
                        .
                        ,(pred (symbolp)))
                      str)
                     (`(,(or (pred (vectorp))
                          (pred (stringp)))
                        nil
                        ,(pred (symbolp)))
                      str)
                     (`(:map)
                      (if varp
                          str
                        (format "(\"\" . %s)" str)))
                     (_
                      (let ((res (if (and (not varp))
                                     (format "\"\" . %s" str)
                                   (format ":map %s" str))))
                        (if (not (looking-back "(" 0))
                            (concat "(" res ")")
                          res)))))))))
         (annotf (lambda (str)
                   (concat " "
                           (let* ((sym (intern str))
                                  (val (cdr (assq sym alist)))
                                  (type (plist-get val :type))
                                  (value (and (eq type :defcustom)
                                              (ignore-errors (symbol-value
                                                              sym))))
                                  (doc (plist-get val :doc)))
                             (string-join
                              (remove nil
                                      (list
                                       (when type
                                         (propertize
                                          (prin1-to-string type)
                                          'face
                                          'font-lock-keyword-face))
                                       (when value
                                         (propertize
                                          (replace-regexp-in-string
                                           "[\n\r\f]+"
                                           " "
                                           (prin1-to-string
                                            value))
                                          'face
                                          'font-lock-keyword-face))
                                       (when doc
                                         (replace-regexp-in-string
                                          "[\r\f\n]+"
                                          " "
                                          doc))))
                              " ")))))
         (table (lambda (str pred action)
                  (if (eq action 'metadata)
                      `(metadata
                        (annotation-function . ,annotf))
                    (complete-with-action action alist str pred)))))
    (elskel--insert (funcall (or display-to-real 'identity)
                             (elskel--completing-read-with-preview
                              "Command: " table display-to-real)))))

(defun elskel--inside-use-package-bind-p ()
  "Check if point is inside a `use-package' bind form."
  (let ((context)
        (stack)
        (stop)
        (found))
    (setq found (catch
                    'found (save-excursion
                             (while
                                 (and (not stop)
                                      (setq context
                                            (or
                                             (when-let ((bindform (elskel--after-use-package-bind-p)))
                                               (throw 'found bindform))
                                             (and (elskel--use-package-inside-bind-consp)
                                                  'command)
                                             (let ((items (elskel--get-sexps-backward)))
                                               (or
                                                (elskel--check-use-package-bind-list-p items)
                                                (when (proper-list-p items)
                                                  (seq-every-p
                                                   #'elskel--check-use-package-bind-list-p
                                                   (seq-drop-while
                                                    #'elskel--use-package-bind-cons-p
                                                    items)))))
                                             (and (elskel--goto-outer-list)
                                                  (when-let ((bindform (elskel--after-use-package-bind-p)))
                                                    (throw 'found bindform))))))
                               (push context stack)
                               (setq stop (not (elskel--goto-outer-list)))))))
    (when found
      (cons found context))))

(defun elskel--goto-outer-list (&optional arg)
  "Move backward out of one level of parentheses.

Optional argument ARG is the number of levels to go up in the list structure; it
defaults to 1."
  (elskel--complete-forward-with 'backward-up-list arg))

(defun elskel--insert-keymap-with-completion ()
  "Insert keymap strings with completion into the buffer."
  (interactive)
  (let* ((alist (mapcan
                 (pcase-lambda (`(,k . ,cmds))
                   (let* ((prefix (format "\\\\<%s>" k))
                          (self-suffix (format "\\\\{%s}" k))
                          (full-map (if (length> (concat prefix self-suffix) 79)
                                        (concat prefix self-suffix)
                                      (concat prefix self-suffix))))
                     (cons full-map
                           (mapcar (lambda (c)
                                     (concat prefix (format "\\\\[%s] `%s'"
                                                            c c)))
                                   cmds))))
                 (elskel--read-with #'elskel--is-keymap-definition
                                    #'elskel--is-keymap-definition))))
    (insert (or (elskel--completing-read-with-preview
                 "Keymap: "
                 alist)
                ""))))

(defun elskel--insert-initial-defcustom-type ()
  "Insert custom type with completion for `defcustom'."
  (let* ((prefix
          (let* ((ppss (syntax-ppss (point)))
                 (depth (car ppss)))
            (cond ((and (= depth 1)
                        (not (or (eq (char-before) ?\')
                                 (eq (char-before) ?\`))))
                   (condition-case nil
                       (save-excursion
                         (backward-sexp 1)
                         (and (eq (symbol-at-point) :type)
                              (progn
                                (backward-up-list 1)
                                (pcase (sexp-at-point)
                                  (`(defcustom ,(pred (symbolp))
                                      ,_val
                                      ,(pred (stringp))
                                      . ,_body)
                                   "'")))))
                     (error nil))))))
         (alist
          (let ((vals)
                (processed))
            (mapatoms
             (lambda (s)
               (when-let* ((ctype (and (symbolp s)
                                       (ignore-errors (get
                                                       s
                                                       'custom-type)))))
                 (unless (member ctype processed)
                   (push ctype processed)
                   (push (cons
                          (concat (or prefix "")
                                  (string-join (split-string
                                                (prin1-to-string ctype) nil t)
                                               " "))
                          ctype)
                         vals)))))
            vals))
         (col (current-column))
         (margin (make-string col ?\s))
         (choice (cdr (assoc-string
                       (elskel--completing-read-with-preview
                        "Keymap: "
                        alist
                        (lambda (it)
                          (let* ((parts (split-string
                                         (with-temp-buffer
                                           (let ((emacs-lisp-mode-hook nil)
                                                 (indent-tabs-mode nil))
                                             (emacs-lisp-mode)
                                             (save-excursion
                                               (insert
                                                (concat (or prefix "")
                                                        (pp-to-string
                                                         (cdr
                                                          (assoc-string it
                                                                        alist))))))
                                             (font-lock-ensure))
                                           (buffer-string))
                                         "[\n]" t))
                                 (first-line (pop parts))
                                 (str
                                  (if parts
                                      (concat first-line "\n"
                                              (mapconcat (lambda (l)
                                                           (concat margin l))
                                                         parts "\n"))
                                    first-line)))
                            str)))
                       alist))))
    (when choice
      (setq choice (pp-to-string choice))
      (save-excursion
        (insert (if prefix
                    (concat prefix choice)
                  choice)))
      (indent-sexp))))

(defun elskel--insert-nested-defcustom-type ()
  "Complete nested custom type with annotations."
  (pcase-let* ((`(,type  ,idx)
                (elisp--fnsym-in-current-sexp))
               (str-type (car (split-string (format "%s" type) nil)))
               (is-odd (and idx (eq (logand idx 1) 1)))
               (stx (syntax-ppss (point)))
               (inside-str (nth 3 stx))
               (is-in-empty-list (and idx
                                      (zerop idx)
                                      (not type)))
               (keyword (save-excursion
                          (when-let ((sym (ignore-errors
                                            (when inside-str
                                              (goto-char (nth 8 stx)))
                                            (forward-sexp -1)
                                            (symbol-at-point))))
                            (and (eq (logand (cadr
                                              (elisp--fnsym-in-current-sexp))
                                             1)
                                     1)
                                 sym))))
               (alist
                (cond ((eq keyword :format)
                       elskel-format-specification-alist)
                      ((memq keyword '(:tag :help-echo
                                       :button-prefix
                                       :button-suffix
                                       :type-error))
                       (list (cons (format "%s" keyword)
                                   "")))
                      ((and is-odd
                            (eq type 'const))
                       elskel-custom-option-keywords)
                      ((and (not type)
                            (not idx)))
                      ((and idx
                            (zerop idx))
                       (append elskel-simple-data-type-descriptions
                               elskel-compound-data-type-descriptions))
                      ((and
                        (assoc-string str-type
                                      elskel-compound-data-type-descriptions))
                       (if is-odd
                           (append elskel-simple-data-type-descriptions
                                   elskel-compound-data-type-descriptions
                                   elskel-custom-option-keywords)
                         (append elskel-simple-data-type-descriptions
                                 elskel-compound-data-type-descriptions)))
                      ((assoc-string str-type
                                     elskel-simple-data-type-descriptions)
                       (if is-odd
                           elskel-custom-option-keywords
                         (append elskel-simple-data-type-descriptions
                                 elskel-compound-data-type-descriptions)))))
               (annotf (lambda (str)
                         (concat " " (or (cdr (assoc-string str alist)) ""))))
               (table (lambda (str pred action)
                        (if (eq action 'metadata)
                            `(metadata
                              (annotation-function . ,annotf))
                          (complete-with-action action alist str pred))))
               (display-to-real (lambda (it)
                                  (substring-no-properties
                                   (cond ((or is-in-empty-list
                                              (assoc-string it
                                                            elskel-custom-option-keywords))
                                          it)
                                         ((or
                                           (assoc-string it
                                                         elskel-format-specification-alist)
                                           (memq keyword '(:tag :help-echo
                                                           :button-prefix
                                                           :button-suffix
                                                           :type-error)))
                                          (if inside-str it
                                            (prin1-to-string it)))
                                         (t (format "(%s)" it))))))
               (item (substring-no-properties
                      (elskel--completing-read-with-preview "Complete "
                                                            table
                                                            display-to-real))))
    (when item
      (elskel--insert (substring-no-properties (funcall display-to-real item)))
      (when (looking-back "[)]" nil)
        (forward-char -1)))
    item))

(defun elskel-complete-custom-type ()
  "Insert initial custom type with completion for `defcustom'."
  (if (> (car (syntax-ppss (point))) 1)
      (elskel--insert-nested-defcustom-type)
    (elskel--insert-initial-defcustom-type)))

(defun elskel--is-within-defcustom-type ()
  "Check if point is inside `defcustom' type specification."
  (let* ((ppss (syntax-ppss (point)))
         (depth (car ppss)))
    (cond ((= depth 1)
           (condition-case nil
               (let ((quoted (char-before)))
                 (progn (backward-sexp 1)
                        (and (eq (symbol-at-point) :type)
                             (progn
                               (backward-up-list 1)
                               (let ((sexp (or (sexp-at-point))))
                                 (pcase sexp
                                   (`(defcustom ,(pred (symbolp))
                                       ,_val
                                       ,(pred (stringp))
                                       . ,_body)
                                    t)
                                   ((pred not)
                                    (progn (down-list 1)
                                           (forward-sexp)
                                           (when (and
                                                  (eq 'defcustom
                                                      (symbol-at-point)))
                                             (forward-sexp 1)
                                             (and (symbol-at-point)
                                                  (memq quoted '(?\`?\'))))))))))))
             (error nil)))
          ((> depth 1)
           (when-let ((start (cadr (nth 9 ppss))))
             (goto-char (cadr (nth 9 ppss)))
             (elskel--is-within-defcustom-type))))))

(defun elskel--is-within-defcustom-type-definition ()
  "Check if point is within `defcustom' type specification."
  (save-excursion
    (elskel--is-within-defcustom-type)))

(defun elskel--inside-string ()
  "Determine if point is inside a string."
  (nth 3 (syntax-ppss (point))))

(defun elskel--rename-definitions (alist)
  "Rename symbols in buffer based on ALIST.

Argument ALIST is a list of cons cells where each cell contains a symbol and its
new name to be renamed."
  (pcase-dolist (`(,sym . ,new-sym) alist)
    (let ((re (regexp-opt (list (if (symbolp sym)
                                    (symbol-name sym)
                                  sym))
                          'symbols))
          (rep (if (symbolp new-sym)
                   (symbol-name new-sym)
                 new-sym)))
      (save-excursion
        (goto-char (point-max))
        (while (re-search-backward re nil t 1)
          (replace-match rep))))))

(defun elskel--alist-p (list)
  "Non-nil if and only if LIST is an alist with simple keys."
  (declare (pure t)
           (side-effect-free error-free))
  (while (and (consp (car-safe list))
              (atom (caar list))
              (setq list (cdr list))))
  (null list))

(defun elskel--list-of (fn list)
  "Check if all elements in LIST satisfy FN.

Argument FN is a function used to test each element in LIST.

Argument LIST is a proper list to be checked with FN."
  (declare (pure t)
           (side-effect-free error-free))
  (and (proper-list-p list)
       (seq-every-p fn list)))

(defun elskel--infer-custom-type-from-value (value)
  "Convert VALUE to its corresponding custom type specifier.

Argument VALUE is the value to be converted to a custom type specifier."
  (pcase value
    ('t 'boolean)
    (`(,(and (pred (atom)) key) .
       ,(and (pred (atom)) val))
     `(cons
       ,(elskel--infer-custom-type-from-value key)
       ,(elskel--infer-custom-type-from-value val)))
    ((pred (stringp)) 'string)
    ((pred (symbolp)) 'symbol)
    ((pred (natnump)) 'natnum)
    ((pred (floatp)) 'float)
    ((pred (integerp)) 'integer)
    ((pred (numberp)) 'number)
    ((pred (vectorp))
     `(vector
       ,@(mapcar #'elskel--infer-custom-type-from-value
          (append
           value
           nil))))
    ((pred (elskel--list-of #'stringp))
     `(repeat string))
    ((pred (elskel--list-of #'functionp))
     `(repeat function))
    ((pred (elskel--list-of #'symbolp))
     `(set :greedy t
       ,@(mapcar #'symbol-name value)
       (repeat
        :tag ,(prin1-to-string "Other")
        :inline t (symbol :tag ,(prin1-to-string "Symbol")))))
    (`(quote ,it)
     (elskel--infer-custom-type-from-value it))
    ((pred (elskel--alist-p))
     (let* ((key-types (seq-uniq
                        (mapcar #'elskel--infer-custom-type-from-value
                                (mapcar #'car value))))
            (key-type (if (length> key-types 1)
                          `(choice ,@key-types)
                        (car key-types)))
            (value-types (seq-uniq
                          (mapcar #'elskel--infer-custom-type-from-value
                                  (mapcar #'cdr value))))
            (value-type (if (length> value-types 1)
                            `(choice ,@value-types)
                          (car value-types))))
       `(alist
         :key-type ,key-type
         :value-type ,value-type)))
    ((pred (atom)) 'sexp)))

;;;###autoload
(defun elskel-transform ()
  "Transform a `defvar' form into a `defcustom' form at point."
  (interactive)
  (let ((sexp (sexp-at-point)))
    (pcase sexp
      (`(defvar ,(and (pred (symbolp))
                  sym
                  (guard (not (memq sym '(t nil)))))
          ,val
          . ,rest)
       (pcase-let* ((`(,beg . ,end)
                     (bounds-of-thing-at-point 'sexp))
                    (sexp-str (buffer-substring-no-properties beg end))
                    (suggested-custom-group
                     (elskel--infer-definition-prefix))
                    (suggested-custom-type
                     (elskel--infer-custom-type-from-value
                      (condition-case nil (eval val t)
                        (error val))))
                    (doc (car rest))
                    (rep (with-temp-buffer
                           (let ((emacs-lisp-mode-hook nil))
                             (insert sexp-str)
                             (emacs-lisp-mode)
                             (font-lock-ensure)
                             (forward-char -1)
                             (newline-and-indent)
                             (unless doc
                               (insert (prin1-to-string "Doc."))
                               (newline-and-indent))
                             (insert
                              (format ":group '%s" suggested-custom-group))
                             (newline-and-indent)
                             (insert (format ":type '%s" suggested-custom-type))
                             (goto-char (point-min))
                             (down-list)
                             (let ((sym-start (point)))
                               (forward-sexp)
                               (delete-region sym-start (point))
                               (insert "defcustom"))
                             (goto-char (point-min))
                             (indent-sexp)
                             (buffer-string)))))
         (delete-region beg end)
         (insert rep)
         (when (re-search-backward (regexp-opt '(":type") 'symbols) nil t 1)
           (forward-char 1)
           (when (and
                  (require 'prettier-elisp nil t)
                  (fboundp 'prettier-elisp))
             (prettier-elisp))))))))

;;;###autoload
(defun elskel-complete ()
  "Insert context-aware completions or snippets at point."
  (interactive)
  (let ((parent (save-excursion
                  (when (elskel--goto-outer-list)
                    (sexp-at-point)))))
    (with-undo-amalgamate
      (cond ((elskel--is-within-defcustom-type-definition)
             (indent-according-to-mode)
             (elskel-complete-custom-type))
            ((elskel--inside-string)
             (elskel--insert-keymap-with-completion))
            ((elskel--interactive-place-p)
             (elskel--insert "interactive")
             (unless (save-excursion
                       (elskel--complete-forward-with
                        #'backward-up-list
                        1)
                       (eq (car-safe (sexp-at-point)) 'interactive))
               (pcase-let ((`(,beg . ,end)
                            (bounds-of-thing-at-point 'symbol)))
                 (delete-region beg end)
                 (indent-according-to-mode)
                 (insert "(interactive)"))))
            ((elskel--is-within-function-arguments)
             (elskel--insert-function-argument-with-completion))
            ((pcase parent
               (`(defvar ,(and (pred (symbolp))
                           sym
                           (guard (not (memq sym '(t nil)))))
                   ,_val
                   . ,_rest)
                t))
             (elskel--goto-outer-list)
             (elskel-transform))
            ((elskel--inside-use-package-bind-p)
             (elskel--complete-inside-use-package-bind))
            ((pcase parent
               (`(use-package ,(pred (symbolp))
                   . ,_rest)
                t))
             (indent-according-to-mode)
             (if-let* ((sym (or (symbol-at-point)
                                (save-excursion
                                  (ignore-errors (backward-sexp))
                                  (symbol-at-point))))
                       (fn (cdr (assq sym
                                      elskel--extra-keywords-inserters-alist))))
                 (funcall fn)
               (elskel--autocomplete-use-package-keywords)))
            ((pcase parent
               (`(transient-define-prefix ,(pred (symbolp)) . ,_)
                t))
             (indent-according-to-mode)
             (elskel--autocomplete-transient-prefix-slots))
            (t (indent-according-to-mode)
               (elskel--skeleton-insert))))))


(provide 'elskel)
;;; elskel.el ends here
