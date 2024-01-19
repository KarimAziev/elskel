;;; elskel-skeletons.el --- Define elisp skeletons -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>

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

;; Define elisp skeletons

;;; Code:

(defvar elskel-skeletons-skels nil)



(defmacro elskel-skeletons-define-skeleton (command documentation &rest body)
  "Define a new skeleton COMMAND and register it.

Argument COMMAND is the name of the skeleton command being defined.

Argument DOCUMENTATION is a string describing the skeleton command.

Remaining arguments BODY are the parts of the skeleton template, including
interactor strings and skeleton elements."
  (declare (doc-string 2)
           (debug (&define name stringp skeleton-edebug-spec))
           (indent defun))
  `(progn
     (prog1 (define-skeleton ,command
              ,documentation
              ,@body)
      (put ',command 'elskel-skeleton ,(car body))
      (add-to-list 'elskel-skeletons-skels ',command))))

(elskel-skeletons-define-skeleton elskel-skeleton-define-skeleton
  "Create a `define-skeleton' skeleton."
  "define-skeleton"
  "(define-skeleton "
  (elskel--infer-definition-prefix)
  "-"
  (setq v1 (skeleton-read "Definition type: "))
  "-" "skeleton"
  \n > (prin1-to-string (format "Create a `%s' skeleton." v1))
  \n > (prin1-to-string (concat v1 " "))
  \n > (prin1-to-string (concat "(" v1 " "))
  " "
  _
  "(setq v1 (elskel--infer-definition-prefix)) " (prin1-to-string "-") " _ "
  (prin1-to-string " ()")
  \n >
  "\\n >"
  \n >
  (prin1-to-string '(prin1-to-string (concat (capitalize v1) ".")))
  \n >
  "\\n > "
  (prin1-to-string ")")
  ")")


(elskel-skeletons-define-skeleton elskel-defcustom-skeleton
  "Create a `defcustom' skeleton."
  "defcustom "
  "(defcustom " (setq v1 (elskel--infer-definition-prefix)) "-" _ " nil"
  > \n (prin1-to-string (concat (capitalize v1) "."))
  > \n
  ":group '" v1
  > \n
  ":type 'boolean)")


(elskel-skeletons-define-skeleton elskel-defvar-skeleton
  "Create a `defvar' skeleton."
  "defvar "
  "(defvar " (setq v1 (elskel--infer-definition-prefix)) "-" _
  \n >
  "nil"
  ")")


(elskel-skeletons-define-skeleton elskel-defvar-local-skeleton
  "Create a `defvar-local' skeleton."
  "defvar-local "
  "(defvar-local " (setq v1 (elskel--infer-definition-prefix)) "-" _
  \n >
  "nil"
  ")")

(elskel-skeletons-define-skeleton elskel-defsubst-skeleton
  "Create a `defsubst' skeleton."
  "defsubst "
  "(defsubst " (setq v1 (elskel--infer-definition-prefix)) "-" _ " ()"
  \n >
  (prin1-to-string (concat (capitalize v1) "."))
  \n >
  ")")



(elskel-skeletons-define-skeleton elskel-defun-skeleton
  "Create a `defun' skeleton."
  "defun "
  "(defun " (setq v1 (elskel--infer-definition-prefix)) "-" _ " ()"
  \n >
  (prin1-to-string (concat (capitalize v1) "."))
  \n >
  ")")


(elskel-skeletons-define-skeleton elskel-defmacro-skeleton
  "Create a `defmacro' skeleton."
  "defmacro "
  "(defmacro " (setq v1 (elskel--infer-definition-prefix)) "-" _ " (&rest body)"
  \n >
  (prin1-to-string (concat (capitalize v1) "."))
  \n >
  "`(progn ,@body)"
  ")")



(elskel-skeletons-define-skeleton skeletons-use-package-skeleton
  "Create a `use-package' skeleton."
  "use-package "
  "(use-package " _ ")")

(elskel-skeletons-define-skeleton elskel-skeletons-lambda-skeleton
  "Create a `lambda' skeleton."
  "lambda "
  "(lambda ("  _ "))")


(elskel-skeletons-define-skeleton elskel-advice-add-skeleton
  "Create a `advice-add' skeleton."
  "advice-add "
  "(advice-add '"_ " "
  (setq v1 (mapcar (lambda (it)
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
                   advice--how-alist)
        v2 (let* ((annotf
                   (lambda (str)
                     (concat " " (or (cdr (assoc str v1)) ""))))
                  (cycle-sort-fn (lambda (it) it))
                  (display-sort-fn (lambda (it)
                                     (seq-sort-by #'length '> it))))
             (completing-read
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
                   action v1 str pred))))))
  " "
  (concat "; " (cdr (assoc-string v2 v1)))
  \n >
  (pcase v2
    (":around" "(defun f (fn &rest args) (apply fn args))")
    (":filter-args" "(defun f (args) args)")
    (":filter-return" "(defun f (args) args)")
    (_ "(defun f (&rest args) args)"))
  ")")



(elskel-skeletons-define-skeleton elskel-skeletons-transient-define-prefix-skeleton
  "Create a `transient-define-prefix' skeleton."
  "transient-define-prefix "
  "(transient-define-prefix " (setq v1 (elskel--infer-definition-prefix)) "-" _
  " ()"
  \n >
  (prin1-to-string (concat (capitalize v1) "."))
  \n > ")")

(elskel-skeletons-define-skeleton elskel-skeletons-transient-define-suffix-skeleton
  "Create a `transient-define-suffix' skeleton."
  "transient-define-suffix "
  "(transient-define-suffix " (setq v1 (elskel--infer-definition-prefix)) "-" _ " ()"
  \n >
  (prin1-to-string (concat (capitalize v1) "."))
  \n > ")")

(elskel-skeletons-define-skeleton
  elskel-skeletons-transient-define-argument-skeleton
  "Create a `transient-define-argument' skeleton."
  "transient-define-argument "
  "(transient-define-argument " (setq v1 (elskel--infer-definition-prefix)) "-"
  _ " ()"
  \n >
  (prin1-to-string (concat (capitalize v1) "."))
  \n > ")")


(provide 'elskel-skeletons)
;;; elskel-skeletons.el ends here