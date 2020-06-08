;;; agtags-xref.el --- A xref frontend to GNU Global -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Vietor Liu

;; Author: Vietor Liu <vietor.liu@gmail.com>
;; Version: 0.2.0
;; Keywords: tools, convenience
;; Created: 2018-12-14
;; URL: https://github.com/vietor/agtags
;; Package-Requires: ((emacs "25"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; A package to integrate GNU Global source code tagging system
;; (http://www.gnu.org/software/global) with Emacs.

;;; Code:
(require 'xref)
(require 'agtags)

(defun agtags-xref--make-xref (ctags-x-line)
  "Create and return an xref object pointing to a file location.
This uses the output of a based on global -x output line provided
in CTAGS-X-LINE argument.  If the line does not match the
expected format, return nil."
  (if (string-match
       "^\\([^ \t]+\\)[ \t]+\\([0-9]+\\)[ \t]+\\([^ \t\]+\\)[ \t]+\\(.*\\)"
       ctags-x-line)
      (xref-make (match-string 4 ctags-x-line)
                 (xref-make-file-location (match-string 3 ctags-x-line)
                                          (string-to-number (match-string 2 ctags-x-line))
                                          0))))

(defun agtags-xref--find-symbol (symbol &rest args)
  "Run GNU Global to find a symbol SYMBOL.
Return the results as a list of xref location objects.  ARGS are
any additional command line arguments to pass to GNU Global."
  (let* ((process-args (append
                        args
                        (list "-x" "-a" symbol)))
         (global-output (agtags--run-global-to-list process-args)))
    (remove nil (mapcar #'agtags-xref--make-xref global-output))))

;;;###autoload
(defun agtags-xref-backend ()
  "The agtags backend for Xref."
  (when (agtags--is-active)
    'agtags))

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql agtags)))
  (agtags--read-dwim))

(cl-defmethod xref-backend-definitions ((_backend (eql agtags)) symbol)
  (agtags-xref--find-symbol (agtags--quote symbol) "-d"))

(cl-defmethod xref-backend-references ((_backend (eql agtags)) symbol)
  (agtags-xref--find-symbol (agtags--quote symbol) "-r"))

(cl-defmethod xref-backend-apropos ((_backend (eql agtags)) symbol)
  (agtags-xref--find-symbol symbol "-g"))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql agtags)))
  (agtags--run-global-to-list (list "-c")))

(provide 'agtags-xref)
;;; agtags-xref.el ends here
