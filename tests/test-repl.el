;;; test-repl.el ---  -*- lexical-binding: t; -*-
;;
;; Filename: unrepl-connect.el
;; Author: Daniel Barreto
;; Copyright (C) 2017 Daniel Barreto
;; Created: Sun Dec 17 23:42:58 2017 (+0100)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Test REPL stuff
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(add-to-list 'load-path (expand-file-name "parseclj/"))
;; HACK: this is a temporary meassure while MELPA/package-build.el fixes:
;;       https://github.com/melpa/package-build/commit/063e64ad55ac07a348af0373605fd90348a8bc11#r26028026

(require 'buttercup)

(require 'unrepl)


(defmacro describe-evaluation (&rest opts)
  "Expand to a buttercup `it' form that ensures correct behavior for an input.
OPTS should be a plist that contains `:input' and `:expected' properties.
The `:input' property is what is going to be send through the wire to the
connected REPL, as if it were typed by a user.
The `:expected' property is the string that should appear in the
REPL (without text properties) after the input is sent, and before the
start of the next prompt."
  (let* ((input (plist-get opts :input))
         (expected (plist-get opts :expected))
         (test-name (format "correctly evaluates $> %s" input)))
    `(it ,test-name
       (with-current-buffer "UNREPL[localhost:5555]"
         (goto-char (point-max))
         (insert ,input)
         (let ((history-count (length unrepl-repl-history))
               (end-of-input (point)))
           (unrepl-repl-return)
           ;; There should be a client pending evaluation
           (with-process-buffer 'localhost:5555 :client
             (expect (length unrepl-pending-evals) :to-equal 1))
           ;; Wait til the next prompt is there
           (while unrepl-repl-inputting
             (accept-process-output nil 0.1))
           ;; And after the prompt, no more pending evaluations
           (with-process-buffer 'localhost:5555 :client
             (expect (length unrepl-pending-evals) :to-equal 0))
           ;; Check history
           (let ((he (car unrepl-repl-history)))
             (expect (unrepl-repl--history-entry-idx he) :to-equal (1+ history-count))
             (expect (unrepl-repl--history-entry-str he) :to-equal ,input)
             (expect (unrepl-repl--history-entry-prompt-marker he) :to-equal unrepl-repl-prompt-start-mark))
           ;; Get evaluation result, without really paying attention to text
           ;; properties.
           (expect (buffer-substring-no-properties
                    (1+ end-of-input)
                    (1- unrepl-repl-prompt-start-mark))
                   :to-equal
                   ,expected))))))


(describe "REPL"
  (before-all
    (unrepl--connect-to "localhost" 5555)
    (with-current-buffer "UNREPL[localhost:5555]"  ;; wait for it to start.
      (while (null (marker-buffer unrepl-repl-prompt-start-mark))
        (accept-process-output nil 0.1))))

  (after-all
    (unrepl-quit 'do-it 'localhost:5555))

  (describe-evaluation
   :input ":foo"
   :expected "> :foo")

  (describe-evaluation
   :input "{:foo 'bar}"
   :expected "> {:foo bar}")

  (describe-evaluation
   :input "(+ 1 1)"
   :expected "> 2")

  (describe-evaluation
   :input "(def square #(* % %))"
   :expected "> user/square")

  (describe-evaluation
   :input "(square 5)"
   :expected "> 25")

  (describe-evaluation
   :input "(/ 1 2)"
   :expected "> 1/2")

  (describe-evaluation
   :input "(range 100)"
   :expected "> (0 1 2 3 4 5 6 7 8 9  ...)")

  (describe-evaluation
   :input "(into [] (range 100))"
   :expected "> [0 1 2 3 4 5 6 7 8 9  ...]")

  (describe-evaluation
   :input "(into #{} (range 100))"
   :expected "> #{0 65 70 62 74 7 59 86 20 72  ...}")

  (describe-evaluation
   :input "(str (apply str (repeat 27 \"Na \")) \"Batman!\")"
   :expected "> \"Na Na Na Na Na Na Na Na Na Na Na Na Na Na Na Na Na Na Na Na Na Na Na Na Na Na Na\" ...")

  (describe-evaluation
   :input "(println \"spiral?\")"
   :expected "spiral?\n> nil")

  (describe-evaluation
   :input "(print \"stroem?\")"
   :expected "stroem?%\n> nil")

  (describe-evaluation
   :input "(binding [*out* *err*] (println \"oh noes...\"))"
   :expected "oh noes...\n> nil")

  (describe-evaluation
   :input "(zipmap (map char (range 97 (+ 97 26))) (range 26))"
   :expected "> {\\a 0 \\b 1 \\c 2 \\d 3 \\e 4 \\f 5 \\g 6 \\h 7 \\i 8 \\j 9  ...}")

  (describe-evaluation
   :input "1 2 3"
   :expected "> 1\n> 2\n> 3")

  (describe-evaluation
   :input "(/ 1 0)"
   :expected "~ Unhandled Exception
  java.lang.ArithmeticException: Divide by zero

 [Show Trace]
")

  (describe-evaluation
   :input "(map / (iterate dec 3))"
   :expected "> (1/3 1/2 1 ~lazy-error \"Divide by zero\" [Inspect]~)")

  ;; (describe-evaluation
  ;;  :input ""
  ;;  :expected "")

  ;; Test clicking elisions
  )


;;; test-repl.el ends here
