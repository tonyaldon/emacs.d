;;; setup-sgml-test.el --- Tests for setup-sgml.el           -*- lexical-binding: t; -*-

;; Copyright (C) 2020 - Tony Aldon - MIT License

;; Author: Tony Aldon <aldon.tony@gmail.com>

;;; Code:

(require 'ert)
(eval-when-compile (require 'cl-lib))

(defun setup-sgml-test--buffer-substrings ()
  "Return cons of buffer substrings before and after point."
  (cons (buffer-substring (point-min) (point))
        (buffer-substring (point) (point-max))))

(defmacro setup-sgml-test--dummy-buffer (pos &rest body)
  (declare (indent 1))
  `(with-temp-buffer
     (sgml-mode)
     (save-excursion (insert "<tag att1=\"some value\" att2=\"another value\"><tag att=\"some value\"/></tag>"))
     (goto-char ,pos)
     ,@body
     (with-no-warnings (setup-sgml-test--buffer-substrings))))

(ert-deftest ta-next-attribute ()
  (should (equal
           (setup-sgml-test--dummy-buffer 1 (ta-next-attribute))
           '("<tag " . "att1=\"some value\" att2=\"another value\"><tag att=\"some value\"/></tag>")))
  (should (equal
           (setup-sgml-test--dummy-buffer 1 (call-interactively #'ta-next-attribute))
           '("<tag " . "att1=\"some value\" att2=\"another value\"><tag att=\"some value\"/></tag>")))
  (should (equal
           (setup-sgml-test--dummy-buffer 2
             (call-interactively #'ta-next-attribute))
           '("<tag " . "att1=\"some value\" att2=\"another value\"><tag att=\"some value\"/></tag>")))
  (should (equal
           (setup-sgml-test--dummy-buffer 7
             (call-interactively #'ta-next-attribute))
           '("<tag att1=\"some value\" " . "att2=\"another value\"><tag att=\"some value\"/></tag>")))
  (should (equal
           (setup-sgml-test--dummy-buffer 11
             (call-interactively #'ta-next-attribute))
           '("<tag att1=\"some value\" " . "att2=\"another value\"><tag att=\"some value\"/></tag>")))
  (should (equal
           (setup-sgml-test--dummy-buffer 24
             (call-interactively #'ta-next-attribute))
           '("<tag att1=\"some value\" att2=\"another value\"" . "><tag att=\"some value\"/></tag>")))
  (should (equal
           (setup-sgml-test--dummy-buffer 44
             (call-interactively #'ta-next-attribute))
           '("<tag att1=\"some value\" att2=\"another value\">" . "<tag att=\"some value\"/></tag>")))
  (should (equal
           (setup-sgml-test--dummy-buffer 66
             (call-interactively #'ta-next-attribute))
           '("<tag att1=\"some value\" att2=\"another value\"><tag att=\"some value\"/>" . "</tag>")))
  (should (equal
           (setup-sgml-test--dummy-buffer 68
             (call-interactively #'ta-next-attribute))
           '("<tag att1=\"some value\" att2=\"another value\"><tag att=\"some value\"/></tag>" . ""))))

(ert-deftest ta-previous-attribute ()
  (should (equal
           (setup-sgml-test--dummy-buffer 2
             (call-interactively #'ta-previous-attribute))
           '("" . "<tag att1=\"some value\" att2=\"another value\"><tag att=\"some value\"/></tag>")))
  (should (equal
           (setup-sgml-test--dummy-buffer 44
             (call-interactively #'ta-previous-attribute))
           '("<tag att1=\"some value\" " . "att2=\"another value\"><tag att=\"some value\"/></tag>")))
  (should (equal
           (setup-sgml-test--dummy-buffer 24
             (call-interactively #'ta-previous-attribute))
           '("<tag " . "att1=\"some value\" att2=\"another value\"><tag att=\"some value\"/></tag>")))
  (should (equal
           (setup-sgml-test--dummy-buffer 45
             (call-interactively #'ta-previous-attribute))
           '("<tag att1=\"some value\" att2=\"another value\"" . "><tag att=\"some value\"/></tag>")))
  (should (equal
           (setup-sgml-test--dummy-buffer 68
             (call-interactively #'ta-previous-attribute))
           '("<tag att1=\"some value\" att2=\"another value\"><tag att=\"some value\"" . "/></tag>")))
	(should (equal
           (setup-sgml-test--dummy-buffer 74
             (call-interactively #'ta-previous-attribute))
           '("<tag att1=\"some value\" att2=\"another value\"><tag att=\"some value\"/>" . "</tag>")))))
