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

(defun elskel--completing-read-with-preview (prompt alist &optional
                                                    display-to-real &rest args)
  "Preview completion candidates while prompting for input.

Argument PROMPT is a string to display as the prompt in the minibuffer.

Argument ALIST is a list or array of strings, or an alist where keys are
strings, or a function that generates such a list.

Optional argument DISPLAY-TO-REAL is a function that takes a single string
argument and returns the real value associated with the display value.

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
  "Insert a customizable option with documentation and type."
  "Custom Name: "
  "(defcustom " (file-name-base (buffer-file-name)) "-" str " nil"
  > \n "\"Doc.\""
  \n >
  ":group '" (file-name-base (buffer-file-name))
  \n > ":type '(repeat string))")

(define-skeleton elskel-defvar-local-skeleton
  "Create a `defvar-local' skeleton with buffer-based variable name."
  "defvar-local"
  "(defvar-local "  (concat (file-name-base (buffer-name)) "-")
  _
  " nil" ")")

(define-skeleton elskel-defvar-skeleton
  "Create a `defvar' skeleton with a variable name prefix."
  "defvar"
  "(defvar "  (concat (file-name-base (buffer-name)) "-")
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
  (let ((choice (or choice
                    (elskel--completing-read-with-preview
                     "Insert\s"
                     elskel--snippets))))
    (pcase choice
      ((or "cl-defun" "defun" "defmacro")
       (insert
        (format "(%s %s ())" choice (elskel--read-new-name (concat choice
                                                                   "\s"))))
       (forward-char -1)
       (newline-and-indent))
      ("defvar"
       (call-interactively 'elskel-defvar-skeleton))
      ("defvar-local"
       (call-interactively 'elskel-defvar-local-skeleton))
      ("defcustom"
       (call-interactively 'elskel-defcustom-skeleton))
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
                                                 (elskel--read-new-name (concat
                                                                         choice
                                                                         "\s")))
                                                (downcase fn))
                      "\n"
                      (format "(advice-add %s #'%s)"  cand
                              (elskel--read-new-name (concat choice "\s")))))
         (re-search-backward "advice-add" nil t 1)
         (skip-chars-forward "a-z-")
         (insert "\s")))
      ("lambda" (insert (format "(lambda (%s) )" (read-string "Argument: ")))
       (forward-char -1))
      ("transient-define-prefix"
       (insert (format
                "(transient-define-prefix %s ())"
                (elskel--read-new-name (concat choice "\s"))))
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

(defun elskel--complete-custom-type ()
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

(defvar elskel--simple-custom-types
  '(("sexp" . "The value may be any Lisp object that can be printed and read back. You can use ‘sexp’ as a fall-back for any option, if you don’t want to take the time to work out a more specific type to use.")
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
    ("fringe-bitmap" . "The value must be a valid fringe bitmap name. The widget provides completion.")))

(defvar elskel--custom-composite-types
  '(("cons" . "(cons CAR-TYPE CDR-TYPE) The value must be a cons cell, its CAR must fit CAR-TYPE, and its CDR must fit CDR-TYPE. For example, ‘(cons string symbol)’ is a customization type which matches values such as ‘(\"foo\" . foo)’. In the customization buffer, the CAR and CDR are displayed and edited separately, each according to their specified type.")
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
    ("restricted-sexp :match-alternatives (symbolp keymapp)" . "(restricted-sexp :match-alternatives CRITERIA) This is the most general composite type construct. The value may be any Lisp object that satisfies one of CRITERIA. CRITERIA should be a list, and each element should be one of these possibilities:")))

(defvar elskel-custom-keywords
  '((":value" . "Provide a default value. If ‘nil’ is not a valid value for the alternative, then it is essential to specify a valid default with ‘:value’. If you use this for a type that appears as an alternative inside of ‘choice’; it specifies the default value to use, at first, if and when the user selects this alternative with the menu in the customization buffer. Of course, if the actual value of the option fits this alternative, it will appear showing the actual value, not DEFAULT.")
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
    (":type-error \"STRING\"" . "STRING should be a string that describes why a value doesn’t match the type, as determined by the ‘:match’ function. When the ‘:match’ function returns ‘nil’, the widget’s ‘:error’ property will be set to STRING.")))

(defvar elskel--custom-format-keyword-alist
  '(("%[BUTTON%]" . "Display the text BUTTON marked as a button")
    ("%{SAMPLE%}" . "This string will be inserted in the buffer to represent the value corresponding to the type. The following ‘%’ escapes are available for use in FORMAT-STRING.")
    ("%d" . "Substitute the item’s documentation string")
    ("%h" . "Substitute the first line of item’s documentation string")
    ("%v" . "Substitute the item’s value")
    ("%t" . "Substitute the tag here")
    ("%%" . "Display a literal ‘%’")))

(defun elskel--complete-custom-nested-type ()
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
                       elskel--custom-format-keyword-alist)
                      ((memq keyword '(:tag :help-echo
                                       :button-prefix
                                       :button-suffix
                                       :type-error))
                       (list (cons (format "%s" keyword)
                                   "")))
                      ((and is-odd
                            (eq type 'const))
                       elskel-custom-keywords)
                      ((and (not type)
                            (not idx)))
                      ((and idx
                            (zerop idx))
                       (append elskel--simple-custom-types
                               elskel--custom-composite-types))
                      ((and (assoc-string str-type
                                          elskel--custom-composite-types))
                       (if is-odd
                           (append elskel--simple-custom-types
                                   elskel--custom-composite-types
                                   elskel-custom-keywords)
                         (append elskel--simple-custom-types
                                 elskel--custom-composite-types)))
                      ((assoc-string str-type elskel--simple-custom-types)
                       (if is-odd
                           elskel-custom-keywords
                         (append elskel--simple-custom-types
                                 elskel--custom-composite-types)))))
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
                                                            elskel-custom-keywords))
                                          it)
                                         ((or
                                           (assoc-string it
                                                         elskel--custom-format-keyword-alist)
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
      (elskel--complete-custom-nested-type)
    (elskel--complete-custom-type)))

(defun elskel--inside-defcustom-type-p ()
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
