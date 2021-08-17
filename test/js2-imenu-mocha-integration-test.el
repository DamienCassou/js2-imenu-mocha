;;; js2-imenu-mocha-integration-test.el --- End-to-end tests for imenu-inplace  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Damien Cassou

;; Author: Damien Cassou <damien@cassou.me>

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

;; Test the imenu-inplace package as a whole.

;;; Code:
(require 'js2-imenu-mocha)
(require 'ert)

(defmacro js2-imenu-mocha-integration--create-js2-buffer (lines &rest body)
  "Execute BODY in a `js2-mode' buffer with LINES."
  `(with-temp-buffer
     ,@(mapcar (lambda (line) `(insert ,line)) lines)
     (js2-mode)
     (js2-parse)
     ,@body))

(ert-deftest js2-imenu-mocha-integration-general ()
  (js2-imenu-mocha-integration--create-js2-buffer
   ("describe(\"top-level\", () => {\n"
    "    describe(\"first suite\", () => {\n"
    "        beforeEach(() => {});\n"
    "        it(\"test 1.1\", () => {});\n"
    "        it(\"test 1.2\", () => {});\n"
    "    });\n"
    "\n"
    "    describe(\"second suite\", () => {\n"
    "        it(\"test 2.1\", () => {});\n"
    "        function foo () {};\n"
    "    });\n"
    "});\n")
   (let ((result (js2-imenu-mocha--generate-imenu-entries)))
     (should (equal result
                    '(("describe top-level"
                       ("*declaration*" . 1)
                       ("describe first suite"
                        ("*declaration*" . 35)
                        ("beforeEach" . 75)
                        ("it test 1.1" . 105)
                        ("it test 1.2" . 139))
                       ("describe second suite"
                        ("*declaration*" . 178)
                        ("it test 2.1" . 219)
                        ("function foo" . 253)))))))))

(ert-deftest js2-imenu-mocha-integration-describe-only ()
  (js2-imenu-mocha-integration--create-js2-buffer
   ("describe.only(\"top-level\", () => {})\n")
   (let ((result (js2-imenu-mocha--generate-imenu-entries)))
     (should (equal result
                    '(("describe top-level"
                       ("*declaration*" . 1))))))))

(ert-deftest js2-imenu-mocha-integration-fdescribe ()
  (js2-imenu-mocha-integration--create-js2-buffer
   ("fdescribe(\"top-level\", () => {})\n")
   (let ((result (js2-imenu-mocha--generate-imenu-entries)))
     (should (equal result
                    '(("describe top-level"
                       ("*declaration*" . 1))))))))

(ert-deftest js2-imenu-mocha-integration-it-only ()
  (js2-imenu-mocha-integration--create-js2-buffer
   ("it.only(\"top-level\", () => {})\n")
   (let ((result (js2-imenu-mocha--generate-imenu-entries)))
     (should (equal result
                    '(("it top-level" . 1)))))))

(ert-deftest js2-imenu-mocha-integration-fit ()
  (js2-imenu-mocha-integration--create-js2-buffer
   ("fit(\"top-level\", () => {})\n")
   (let ((result (js2-imenu-mocha--generate-imenu-entries)))
     (should (equal result
                    '(("it top-level" . 1)))))))

;; We don't want this package to list functions defined outside of
;; any describe because js2-mode already lists them
(ert-deftest js2-imenu-mocha-integration-no-outer-function ()
  (js2-imenu-mocha-integration--create-js2-buffer
   ("function foo () {};\n")
   (let ((result (js2-imenu-mocha--generate-imenu-entries)))
     (should (equal result nil)))))

(provide 'imenu-inplace-integration-test)
;;; js2-imenu-mocha-integration-test.el ends here
