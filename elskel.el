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

(defvar manual-program)

(declare-function info--manual-names "info")
(declare-function info--filter-manual-names "info")
(declare-function info-initialize "info")
(declare-function Man-parse-man-k "man")
(declare-function Man-default-directory "man")


(defvar elskel--snippets '(("lambda" . "")
                           ("defun" . "")
                           ("defmacro" . "")
                           ("cl-defun" . "")
                           ("defvar" . "")
                           ("defvar-local" . "")
                           ("defcustom" . "")
                           ("use-package" . "")
                           ("transient-define-argument" . "")
                           ("transient-define-suffix" . "")
                           ("transient-define-prefix" . "")
                           ("advice-add" . ""))
  "Alist of snippet templates for common Lisp expressions.")

(defvar elskel--use-package-keywords-completions nil
  "List of completion candidates for `use-package' keywords.")

(defvar elskel--use-package--keywords-completions
  `((":disabled" . (t nil))
    (":load-path")
    (":straight" . (lambda ()
                     (let* ((choices '("git" "built-in" "nil"))
                            (result (completing-read ":type\s" choices)))
                      (pcase result
                       ("built-in" (prin1-to-string '(:type built-in)))
                       ("git"
                        (require 'gh-repo nil t)
                        (require 'git-util nil t)
                        (let ((user (git-util-config "user.email")))
                         (prin1-to-string
                          (if
                              (and user
                               (yes-or-no-p (format
                                             "Repo of %s?" user)))
                              `(:repo
                                ,(substring-no-properties
                                  (if (fboundp 'gh-repo-read-user-repo)
                                      (gh-repo-read-user-repo "Repo:" 'identity)
                                    (read-string "Repo: ")))
                                :type git
                                :flavor nil
                                :host github)
                            (let* ((url
                                    (if (fboundp 'git-util-url-get-candidates)
                                        (completing-read
                                         "Repo: "
                                         (git-util-url-get-candidates))
                                      (read-string "Repo: ")))
                                   (pl (or (and
                                            (fboundp 'git-util-url-to-recipe)
                                            (git-util-url-to-recipe url))
                                        `(:repo ,(read-string "Repo:"
                                                  url)
                                          :type git
                                          :flavor nil
                                          :host github))))
                             pl)))))
                       (_ result)))))
    (":init" . elskel--snippet-insert)
    (":config" . elskel--snippet-insert))
  "Completions for `use-package' keywords.")

(defun elskel--manpage-cands ()
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

(defun elskel--info-manual ()
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

(defun elskel--transient-prefix-slots ()
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
                 (cons :man-page #'elskel--manpage-cands)
                 (cons :info-manual #'elskel--info-manual)
                 (cons :suffix-description
                       (list
                        "#'transient-command-summary-or-name")))))
    (seq-reduce (lambda (acc curr)
                  (unless (assq curr acc)
                    (push (cons curr nil) acc))
                  acc)
                (mapcar #'car (elskel--complete-get-class-initargs
                               'transient-prefix))
                alist)))

(defun elskel--complete-transient-slots ()
  "Fill in transient prefix slots with completion."
  (pcase (elskel--get-sexps-backward)
    (`(transient-define-prefix ,(pred (symbolp)) ,(pred (listp)) . ,body)
     (let* ((form (car (last body)))
            (tslots (elskel--transient-prefix-slots))
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

(defun elskel--complete-get-class-initargs (sym)
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

(defun elskel-minibuffer-get-metadata ()
  "Retrieve metadata for minibuffer completion."
  (completion-metadata
   (buffer-substring-no-properties
    (minibuffer-prompt-end)
    (max (minibuffer-prompt-end)
         (point)))
   minibuffer-completion-table
   minibuffer-completion-predicate))

(defun elskel-minibuffer-ivy-selected-cand ()
  "Get selected candidate or text from Ivy minibuffer."
  (when (and (memq 'ivy--queue-exhibit post-command-hook)
             (boundp 'ivy-text)
             (boundp 'ivy--length)
             (boundp 'ivy-last)
             (fboundp 'ivy--expand-file-name)
             (fboundp 'ivy-state-current))
    (cons
     (completion-metadata-get (ignore-errors (elskel-minibuffer-get-metadata))
                              'category)
     (ivy--expand-file-name
      (if (and (> ivy--length 0)
               (stringp (ivy-state-current ivy-last)))
          (ivy-state-current ivy-last)
        ivy-text)))))

(defun elskel-minibuffer-get-default-candidates ()
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
       (completion-metadata-get (elskel-minibuffer-get-metadata) 'category)
       all))))

(defun elskel-get-minibuffer-get-default-completion ()
  "Retrieve default completion from minibuffer."
  (when (and (minibufferp) minibuffer-completion-table)
    (pcase-let* ((`(,category . ,candidates)
                  (elskel-minibuffer-get-default-candidates))
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

(defvar elskel-minibuffer-targets-finders
  '(elskel-minibuffer-ivy-selected-cand
    elskel-get-minibuffer-get-default-completion)
  "List of functions to find minibuffer completion targets.")

(defun elskel-minibuffer-get-current-candidate ()
  "Retrieve the current candidate from the minibuffer."
  (let (target)
    (run-hook-wrapped
     'elskel-minibuffer-targets-finders
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
                                (elskel-minibuffer-get-current-candidate)))
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

(defun elskel--completing-read-with-preview (prompt alist &rest args)
  "Preview completion candidates while prompting for input.

Argument PROMPT is a string to display as the prompt in the minibuffer.

Argument ALIST is an association list where each element is a cons cell (key .
value) used for completion.

Remaining arguments ARGS are additional arguments passed to the internal
function `elskel--completing-read-with-preview-action'."
  (let ((pos (point))
        (ov))
    (unwind-protect
        (apply #'elskel--completing-read-with-preview-action
               prompt
               alist
               (lambda (it)
                 (when ov
                   (delete-overlay ov))
                 (setq ov (elskel--make-overlay pos
                                                pos nil nil nil 'after-string
                                                (or
                                                 (elskel--get-completion-prefix it)
                                                 it))))
               args)
      (when ov (delete-overlay ov)))))

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

(defun elskel--complete-alist (base-prompt alist &rest args)
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

(defun elskel--shared-start (s1 s2)
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

(defun elskel--get-def-names ()
  "Extract defined symbol names from buffer's Lisp code."
  (save-excursion
    (let ((sexp)
          (syms))
      (goto-char (point-min))
      (while (setq sexp (ignore-errors (read (current-buffer))))
        (when-let ((symb
                    (pcase sexp
                      (`(,(or 'defun 'cl-defun 'defmacro 'cl-defmacro)
                         ,(and (pred (symbolp))
                           sym
                           (guard (not (memq sym '(t nil)))))
                         ,(pred (listp))
                         . ,_)
                       sym)
                      (`(,(or 'defvar 'defvar-local 'defvar-keymap
                           'defcustom)
                         ,(and (pred (symbolp))
                           sym
                           (guard (not (memq sym '(t nil)))))
                         . ,_)
                       sym))))
          (push (symbol-name symb) syms)))
      syms)))

(defun elskel--guess-def-name ()
  "Guess and return the common prefix for definition names."
  (let ((prefix (or
                 (elskel--provided-name)
                 (let ((strings (elskel--get-def-names)))
                   (seq-reduce (lambda (acc it)
                                 (let ((prefix (elskel--shared-start acc it)))
                                   (if (string-empty-p prefix)
                                       acc
                                     prefix)))
                               strings
                               (pop strings)))
                 (replace-regexp-in-string "[^a-z]" "-"
                                           (replace-regexp-in-string
                                            "\\.[a-z]+$" (buffer-name
                                                          (current-buffer))
                                            "")))))
    (if (and
         (not (string= "-" prefix))
         (string-suffix-p "-" prefix))
        prefix
      (format "%s-" prefix))))

(defun elskel--quoted-symbol-or-function (sexp)
  "Extract symbol from quoted or sharp-quoted expression.

Argument SEXP is an s-expression to be analyzed for a quoted symbol or function."
  (pcase sexp
    (`(function ,(and (pred (symbolp)) sym))
     sym)
    (`(quote ,(and (pred (symbolp)) sym))
     sym)))

(defun elskel--command-from-define-key (sexp)
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

(defun elskel--looks-like-keymapp (sexp)
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
           (delq nil (mapcar #'elskel--command-from-define-key body))))
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
    elskel--use-package--keywords-completions
    (mapcar
     (lambda (it)
       (list (symbol-name it)))
     (when (boundp 'use-package-keywords)
       use-package-keywords)))
   (lambda (a b)
     (string= (car a)
              (car b)))))

(define-skeleton elskel-defcustom-skeleton
  "Generate a `defcustom' skeleton with group and type placeholders."
  "Defcustom:"
  "(defcustom "  (setq v1 (file-name-base (buffer-name))) "-"
  " nil" \n >
  (prin1-to-string (concat (capitalize v1) "."))
  \n >
  ":group '" v1
  \n >
  ":type " _
  ")")

(define-skeleton elskel-defvar-local-skeleton
  "Create a `defvar-local' skeleton with buffer-based variable name."
  "Defcustom:"
  "(defvar-local "  (setq v1 (concat (file-name-base (buffer-name)) "-"))
  _
  " nil" ")")

(define-skeleton elskel-defvar-skeleton
  "Create a `defvar' skeleton with a variable name prefix."
  "Defcustom:"
  "(defvar "  (setq v1 (concat (file-name-base (buffer-name)) "-"))
  _
  " nil" ")")

(defun elskel--read-new-name (&optional prompt)
  "PROMPT for a new name with an optional default.

Optional argument PROMPT is the string presented to the user when asking for
input. It defaults to a single space \" \"."
  (read-string
   (or prompt " ")
   (replace-regexp-in-string
    "--$"
    "-"
    (format
     "%s-"
     (elskel--guess-def-name)))))

(defun elskel--snippet-insert (&optional choice)
  "Insert a predefined code snippet into the buffer.

Optional argument CHOICE is a string representing the user's choice of snippet
to insert."
  (let ((func-name)
        (choice choice))
    (setq choice
          (elskel--completing-read-with-preview
           "Insert\s"
           elskel--snippets))
    (setq func-name
          (unless (member choice '("use-package"
                                   "defhydra"
                                   "pretty-hydra-define"
                                   "lambda"))
            (elskel--read-new-name (concat choice "\s"))))
    (pcase choice
      ((or "cl-defun" "defun" "defmacro")
       (insert (format "(%s %s ())" choice func-name))
       (forward-char -1)
       (newline-and-indent))
      ("defvar"
       (elskel-defvar-skeleton))
      ("defvar-local"
       (elskel-defvar-local-skeleton))
      ((or "defcustom")
       (elskel-defcustom-skeleton))
      ("use-package"
       (require 'straight-extra nil t)
       (when (fboundp 'straight-extra-insert-use-package-at-point)
         (straight-extra-insert-use-package-at-point)))
      ("advice-add"
       (let* ((alist
               (mapcar (lambda (it)
                         (let ((descr (format "%s" (cdr it))))
                           (cons (format "%s"(car it))
                                 (if-let ((pos (string-match
                                                "(lambda ("
                                                descr)))
                                     (substring descr pos
                                                (1-
                                                 (length
                                                  descr)))
                                   descr))))
                       advice--how-alist))
              (annotf
               (lambda (str)
                 (concat " " (or (cdr (assoc str alist)) ""))))
              (cycle-sort-fn (lambda (it) it))
              (display-sort-fn (lambda (it)
                                 (seq-sort-by #'length '> it)))
              (cand (completing-read
                     "Candidates: "
                     (lambda (str pred action)
                       (if (eq action 'metadata)
                           `(metadata
                             (annotation-function .
                              ,annotf)
                             (cycle-sort-function .
                              ,cycle-sort-fn)
                             (display-sort-function
                              . ,display-sort-fn))
                         (complete-with-action
                          action alist str pred)))))
              (fn (cdr (assoc cand alist))))
         (insert
          "\n"(concat (replace-regexp-in-string "^(lambda "
                                                (format
                                                 "(defun %s"
                                                 func-name)
                                                (downcase fn))
                      "\n"
                      (format "(advice-add %s #'%s)"  cand
                              func-name)))
         (re-search-backward "advice-add" nil t 1)
         (skip-chars-forward "a-z-")
         (insert "\s")))
      ("lambda" (insert (format "(lambda (%s) )" (read-string "Argument: ")))
       (forward-char -1))
      ("transient-define-prefix"
       (insert (format
                "(transient-define-prefix %s ())"
                func-name))
       (forward-char -1)
       (newline-and-indent)
       (insert "\"\"")))))

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

(defun elskel--complete-use-package ()
  "Autocomplete `use-package' keywords and insert them."
  (setq elskel--use-package-keywords-completions
        (elskel--use-package-keywords-completions-alist))
  (let ((words (mapcar #'car (elskel--use-package-keywords-completions-alist)))
        (prefix
         (when-let ((s (symbol-at-point)))
           (symbol-name s))))
    (cond ((and prefix (member prefix words))
           (insert "\s")
           (elskel--complete-use-package))
          ((and prefix
                (all-completions prefix words))
           (elskel--insert (string-join
                            (flatten-list
                             (elskel--complete-alist
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
                            (elskel--complete-alist "Sublist" sublist))
                           "\s")))))
             (when (stringp choice)
               (insert choice))))
          (t (insert
              (string-join
               (flatten-list
                (elskel--complete-alist
                 "Complete: "
                 elskel--use-package-keywords-completions))
               "\s"))))))

(defun elskel--complete-inside-args ()
  "Insert a Lisp argument with completion."
  (let ((prefix
         (when-let ((sym (symbol-at-point)))
           (format "%s" sym)))
        (args (save-excursion
                (elskel--backward-up-list)
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

(defun elskel--inside-args-p ()
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

(defun elskel--backward-up-list (&optional arg)
  "Move backward out of one level of parentheses.

Optional argument ARG is the number of levels to go up in the list structure; it
defaults to 1."
  (elskel--complete-forward-with 'backward-up-list arg))

(defun elskel--substitute-map ()
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
                 (elskel--read-with #'elskel--looks-like-keymapp
                                    #'elskel--looks-like-keymapp))))
    (insert (or (elskel--completing-read-with-preview
                 "Keymap: "
                 alist)
                ""))))

(defun elskel-complete-custom-type ()
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
                          (concat (or prefix "")
                                  (pp-to-string ctype)))
                         vals)))))
            vals))
         (pos (point))
         (ov))
    (unwind-protect
        (insert
         (cdr (assoc-string
               (elskel--completing-read-with-preview
                "Keymap: "
                alist
                (lambda (it)
                  (when-let ((val (cdr (assoc-string it alist))))
                    (when ov
                      (delete-overlay ov))
                    (setq ov (elskel--make-overlay pos
                                                   pos nil nil nil 'after-string
                                                   val)))))
               alist)))
      (when ov (delete-overlay ov)))))

(defun elskel--inside-defcustom-type-p ()
  "Check if point is inside `defcustom' type specification."
  (let* ((ppss (syntax-ppss (point)))
         (depth (car ppss)))
    (cond ((= depth 1)
           (condition-case nil
               (progn (backward-sexp 1)
                      (and (eq (symbol-at-point) :type)
                           (progn
                             (backward-up-list 1)
                             (pcase (sexp-at-point)
                               (`(defcustom ,(pred (symbolp))
                                   ,_val
                                   ,(pred (stringp))
                                   . ,_body)
                                t)))))
             (error nil)))
          ((> depth 1)
           (when-let ((start (cadr (nth 9 ppss))))
             (goto-char (cadr (nth 9 ppss)))
             (elskel--inside-defcustom-type-p))))))

(defun elskel--complete-defcustom-type-p ()
  "Check if point is within `defcustom' type specification."
  (save-excursion
    (elskel--inside-defcustom-type-p)))

(defun elskel--inside-string ()
  "Determine if point is inside a string."
  (nth 3 (syntax-ppss (point))))

;;;###autoload
(defun elskel-complete ()
  "Insert context-aware completions or snippets at point."
  (interactive)
  (let ((parent (save-excursion
                  (when (elskel--backward-up-list)
                    (sexp-at-point)))))
    (with-undo-amalgamate
      (cond ((elskel--complete-defcustom-type-p)
             (indent-according-to-mode)
             (elskel-complete-custom-type))
            ((elskel--inside-string)
             (elskel--substitute-map))
            ((elskel--interactive-place-p)
             (indent-according-to-mode)
             (elskel--insert "(interactive)"))
            ((elskel--inside-args-p)
             (elskel--complete-inside-args))
            ((pcase parent
               (`(use-package ,(pred (symbolp))
                   . ,_rest)
                t))
             (indent-according-to-mode)
             (elskel--complete-use-package))
            ((pcase parent
               (`(transient-define-prefix ,(pred (symbolp)) . ,_)
                t))
             (indent-according-to-mode)
             (elskel--complete-transient-slots))
            (t (indent-according-to-mode)
               (elskel--snippet-insert))))))

(provide 'elskel)
;;; elskel.el ends here
