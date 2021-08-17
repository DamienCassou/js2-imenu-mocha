;;; js2-imenu-mocha.el --- Hide everything not in imenu  -*- lexical-binding: t; -*-
;; Copyright (C) 2021  Damien Cassou

;; Author: Damien Cassou <damien@cassou.me>
;; Url: https://github.com/DamienCassou/js2-imenu-mocha
;; Package-requires: ((emacs "27.2") (js2-mode "20201220"))
;; Version: 0.1.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Hide everything in the current buffer expect lines corresponding to
;; imenu entries.

;;; Code:
(require 'js2-mode)

(defvar-local js2-imenu-mocha--initial-imenu-create-index-function nil
  "Contains the value of `imenu-create-index-function' before activating `js2-imenu-mocha-mode'.")

(define-minor-mode js2-imenu-mocha-mode
  "Minor mode to add describe/it test blocks to imenu entries."
  :lighter "imenu-mocha "
  (if js2-imenu-mocha-mode
      (js2-imenu-mocha-enable)
    (js2-imenu-mocha-disable)))

(defun js2-imenu-mocha-enable ()
  "Add describe/it test blocks to imenu entries in the current buffer."
  (unless js2-imenu-mocha--initial-imenu-create-index-function
    (setq js2-imenu-mocha--initial-imenu-create-index-function imenu-create-index-function)
    (setq imenu-create-index-function #'js2-imenu-mocha--create-index)))

(defun js2-imenu-mocha-disable ()
  "Bring back the initial value of `imenu-create-index-function'."
  (setq imenu-create-index-function js2-imenu-mocha--initial-imenu-create-index-function))

(defun js2-imenu-mocha--create-index ()
  "Return imenu entries for the current buffer.
Contains entries from the current package and entries from the
initial `imenu-create-index-function'."
  (append
   (js2-imenu-mocha--generate-imenu-entries)
   (when js2-imenu-mocha--initial-imenu-create-index-function
     (funcall js2-imenu-mocha--initial-imenu-create-index-function))))

(defun js2-imenu-mocha--generate-imenu-entries ()
  "Generate an imenu alist for current buffer mirroring the mocha test suite structure."
  (when js2-mode-ast
    (js2-imenu-mocha--visit-node js2-mode-ast t)))

(defun js2-imenu-mocha--visit-node (node &optional ignore-functions)
  "Return the imenu entries corresponding to NODE and its children.

If IGNORE-FUNCTIONS is non-nil and if NODE is a function, return
no imenu entry."
  (let ((result (list)))
    (js2-visit-ast
     node
     (lambda (node end-p)
       (when (not end-p)
         (let ((imenu-entry (js2-imenu-mocha--imenu-entry-for-unknown-node node ignore-functions)))
           (if (not imenu-entry)
               ;; make the AST visitor search deeper:
               t
             (push imenu-entry result)
             ;; stop the AST visitor from going deeper:
             nil)))))
    (nreverse result)))

(defun js2-imenu-mocha--imenu-entry-for-unknown-node (node ignore-functions)
  "Return the imenu entries corresponding to NODE.
Return nil if NODE isn't supposed to have an imenu entry.

If IGNORE-FUNCTIONS is non-nil and if NODE is a function, return
no imenu entry."
  (cond
   ((js2-imenu-mocha--named-test-block-p node) (js2-imenu-mocha--imenu-entry-for-named-test-block node))
   ((js2-imenu-mocha--named-test-leaf-p node) (js2-imenu-mocha--imenu-entry-for-named-test-leaf node))
   ((js2-imenu-mocha--unnamed-test-leaf-p node) (js2-imenu-mocha--imenu-entry-for-unnamed-test-leaf node))
   ((and (not ignore-functions) (js2-imenu-mocha--function-definition-p node)) (js2-imenu-mocha--imenu-entry-for-function-definition node))
   (t nil)))

(defun js2-imenu-mocha--named-test-block-p (node)
  "Return non-nil if NODE is a `describe' block."
  (when-let* ((name (js2-imenu-mocha--call-target-name node)))
    (member name '("describe" "describe.only" "fdescribe"))))

(defun js2-imenu-mocha--named-test-leaf-p (node)
  "Return non-nil if NODE is a `it' block."
  (when-let* ((name (js2-imenu-mocha--call-target-name node)))
    (member name '("it" "it.only" "fit"))))

(defun js2-imenu-mocha--unnamed-test-leaf-p (node)
  "Return non-nil if NODE is a `{before,after}{Each,All}' block."
  (when-let* ((name (js2-imenu-mocha--call-target-name node)))
    (member name '("beforeEach" "afterEach" "beforeAll" "afterAll"))))

(defun js2-imenu-mocha--function-definition-p (node)
  "Return non-nil if NODE is function definition."
  (js2-function-node-p node))

(defun js2-imenu-mocha--imenu-entry-for-named-test-block (node)
  "Return an imenu entry for test leaf NODE.
NODE must satisfy `js2-imenu-mocha--named-test-block-p' (e.g., `describe')."
  `(,(concat "describe " (js2-imenu-mocha--test-name node))
    ,(cons "*declaration*" (js2-node-abs-pos node))
    ,@(js2-imenu-mocha--visit-node (js2-imenu-mocha--node-body node))))

(defun js2-imenu-mocha--imenu-entry-for-named-test-leaf (node)
  "Return imenu-entry-for imenu entry for test leaf NODE.
NODE must satisfy `js2-imenu-mocha--named-test-leaf-p' (e.g., `it')."
  (cons (concat "it " (js2-imenu-mocha--test-name node)) (js2-node-abs-pos node)))

(defun js2-imenu-mocha--imenu-entry-for-unnamed-test-leaf (node)
  "Return an imenu entry for test leaf NODE.
NODE must satisfy `js2-imenu-mocha--unnamed-test-leaf-p' (e.g., `beforeEach')."
  (cons (js2-imenu-mocha--call-target-name node) (js2-node-abs-pos node)))

(defun js2-imenu-mocha--imenu-entry-for-function-definition (node)
  "Return an imenu entry for test leaf NODE.
NODE must satisfy `js2-imenu-mocha--function-definition-p'."
  (cons (concat "function " (js2-function-name node)) (js2-node-abs-pos node)))

(defun js2-imenu-mocha--call-target-name (node)
  "Return the function name called by NODE.
If node is not a function call, return nil."
  (when (js2-call-node-p node)
    (js2-imenu-mocha--string-content (js2-call-node-target node))))

(defun js2-imenu-mocha--string-content (node)
  "Return a string representing the value of NODE."
  (if (not node)
      "~empty~"
    (if (js2-string-node-p node)
        (js2-string-node-value node)
      (let ((start (js2-node-abs-pos node)))
        (buffer-substring-no-properties
         start
         (+ start (js2-node-len node)))))))

(defun js2-imenu-mocha--test-name (node)
  "Return the name of a `describe' or `it' NODE.
NODE must be a function call."
  (let ((first-arg (car (js2-call-node-args node))))
    (js2-imenu-mocha--string-content first-arg)))

(defun js2-imenu-mocha--node-body (node)
  "Return a node corresponding to the body of a `describe' or `it' NODE."
  (when-let ((second-arg (cadr (js2-call-node-args node))))
    (when (js2-function-node-p second-arg)
      (js2-function-node-body second-arg))))

(provide 'js2-imenu-mocha)
;;; js2-imenu-mocha.el ends here

; LocalWords:  beforeEach
