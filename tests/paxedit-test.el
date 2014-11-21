;; Copyright 2014 Mustafa Shameem

;; Author: Mustafa Shameem
;; Maintainer: Mustafa Shameem
;; URL: https://github.com/promethial/paxedit

;;; This file is part of Paxedit.

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

(require 'cl-lib)
(require 'xtest)
(require 'paxedit)

(defun paxedit-test-elisp-setup ()
  "Elisp test setup for Paxedit."
  (emacs-lisp-mode)
  (paxedit-mode 1))

(defun paxedit-test-clojure-setup ()
  "Clojure test setup for Paxedit."
  (clojure-mode)
  (paxedit-mode 1))

(xt-deftest paxedit-region-contains
  (xtd-should 'paxedit-region-contains
              ((1 . 5) (1 . 5))
              ((1 . 10) (2 . 3))
              ((1 . 10) (1 . 5)))
  (xtd-should! 'paxedit-region-contains
               ((1 . 10) (11 . 12))
               ((1 . 10) (5 . 15))))

(xt-deftest paxedit-nth-satisfies
  (xtd-should (lambda (func data result) (equal (paxedit-nth-satisfies data func) result))
              ((lambda (n) (> n 4)) (1 2 3 4 5) 4)
              ((lambda (s) (equal "cat" s)) (1 2 "shark" 3 "cat") 4)
              ((lambda (n) (= n 1)) (1) 0))
  (xtd-should! (lambda (func data) (paxedit-nth-satisfies data func))
               ((lambda (n) (> n 8)) (1 2 3 4 5))
               ((lambda (n) (> n 8)) nil)))

(xt-deftest paxedit-region-contains-point
  (xtd-should (lambda (region point)
                (paxedit-region-contains-point region point))
              ((1 . 10) 1)
              ((5 . 10) 5)
              ((5 . 10) 10)
              ((1 . 1) 1))
  (xtd-should! (lambda (region point)
                 (paxedit-region-contains-point region point))
               ((6 . 10) 5)
               ((6 . 10) 11)))

(xt-deftest paxedit-region-kill
  (xtd-setup= (lambda (x) (paxedit-region-kill x))
              ("Test -!-Me" "Test -!-Me" (1 . 1))
              ("Test -!-Me" "est -!-Me" (1 . 2)))
  (xtd-setup= (lambda (x) (paxedit-region-kill x)
                (end-of-buffer)
                (yank))
              ("Test -!-Me" "MeTest -!-" (1 . 6))
              ("Test -!-Me" " MeTest-!-" (1 . 5))))

(xt-deftest paxedit-region-modify
  (xtd-setup= (lambda (_) (paxedit-region-modify '(9 . 21) ; This region will be capitalized
                                            (lambda (x) (capitalize x))))
              ("we hold these truths" "-!-we hold These Truths")
              ("we hold these-!- truths" "we hold -!-These Truths")))

(xt-deftest paxedit-sexp-move-to-start
  (xtd-setup= (lambda (_) (paxedit-sexp-move-to-core-start))
              ("(message -!-\"hello\")" "-!-(message \"hello\")")
              ("'(message -!-\"hello\")" "'-!-(message \"hello\")")
              ("[message -!-\"hello\"]" "-!-[message \"hello\"]")
              ("{message -!-\"hello\"}" "-!-{message \"hello\"}")
              ("(if x b (message -!-\"hello\"))" "(if x b -!-(message \"hello\"))")))

(xt-deftest paxedit-sexp-forward
  (xtd-setup= (lambda (_) (paxedit-sexp-forward))
              ("(if t -!-(message \"hello\"))" "(if t (message \"hello\")-!-)")
              ("(if t (message \"hello\")-!- test)" "(if t (message \"hello\") test-!-)")))

(xt-deftest paxedit-sexp-core-get-start-point
  (xtd-return= (lambda (_) (paxedit-sexp-core-get-start-point))
               ("(when t '-!-(+ 1 2))" 9)))

(xt-deftest paxedit-sexp-core-move-istart
  (xtd-setup= (lambda (_) (paxedit-sexp-move-to-core-start)
                (paxedit-sexp-core-move-istart))
              ("(when t '-!-(+ 1 2))" "(-!-when t '(+ 1 2))")
              ("(    when t '-!-(+ 1 2))" "(    -!-when t '(+ 1 2))")
              ("(
when t '-!-(+ 1 2))" "(
-!-when t '(+ 1 2))")))

(xt-deftest paxedit-sexp-core-get-type
  (xtd-return= (lambda (_) (paxedit-sexp-move-to-core-start)
                 (paxedit-sexp-core-get-type))
               ("(when (< x 10) (message-!- \"Guten Tag\"))" ?\()
               ("(when (< x 10) [1 -!-2 3])" ?\[)))

(xt-deftest paxedit-sexp-core-get-functional-symbol
  (xtd-return= (lambda (_) (paxedit-sexp-core-get-functional-symbol))
               ("(when (< x 10) -!-(zip 100))" 'zip)
               ("(when (< x 10) -!-(    zip 100))" 'zip)
               ("(when (< x 10) -!-(


   zip 100))" 'zip)
               ("(if t
    -!-(message \"It's true watya know\")
  (error \"oh no\"))" 'message)))

(xt-deftest paxedit-comment-valid?
  (xtd-return= (lambda (_) (paxedit-comment-valid?))
               ("-!-" nil)
               ("-!-(+ 1 2 3)" nil)
               ("\"-!-;\"" nil)
               ("(+ 1 2 3) -!-;; some math" t)
               ("-!-;1" t)))

(xt-deftest paxedit-comment-check-context
  (xtd-return= (lambda (_) (paxedit-comment-check-context))
               ("-!-" nil)
               ("(+ 1 2 3) -!-;; some math" '(11 . 23))
               ("-!-;; some math" '(1 . 13))
               ("-!-;" '(1 . 2))
               ("-!-(+ 1 2 3)" nil)
               ("(+ 1 2
3 4-!- ;; some math
)" '(12 . 24))
               ("(+ 1 2
3 4 ;; some math-!-
)" '(12 . 24))
               ("(+ 1 2
-!-3 4 ;; some math
)" '(12 . 24))))

(xt-deftest paxedit-comment-internal-region
  (xtd-return= (lambda (_) (paxedit-comment-internal-region (paxedit-comment-check-context)))
               ("; Hello" '(2 . 8))
               ("; H" '(2 . 4))
               (";;" '(3 . 3))
               (";; yellow-!-" '(3 . 10))
               ("" nil)))

(xt-deftest paxedit-next-symbol
  (xtd-setup= (lambda (_) (call-interactively 'paxedit-next-symbol))
              ("-!-move forward" "move -!-forward")
              ("-!-move_32342423*^&%$, forward" "move_32342423*^&%$, -!-forward")
              ("[-!-] art" "[] -!-art"))
  (xtd-setup= (lambda (_) (call-interactively 'paxedit-previous-symbol))
              ("move -!-forward" "move-!- forward")
              ("move_32342423*^&%$-!- forward" "-!-move_32342423*^&%$ forward")))

(xt-deftest paxedit-kill
  (xtd-setup= (lambda (_) (paxedit-test-elisp-setup)
                (call-interactively 'paxedit-kill)
                (goto-char (point-max))
                (yank))
              ;; SEXP Kill
              ("(+ 1 2 (- 3 -!-6))" "(+ 1 2)(- 3 6)-!-")
              ("(+ 1 2
                (- 3 -!-6))" "(+ 1 2)(- 3 6)-!-")
              ;; SEXP kill, comment same line
              ("(+ 1 2
 (+ -!-1 2) ;; Special Number
 4)"
               "(+ 1 2
   ;; Special Number
 4)(+ 1 2)-!-")
              ;; SEXP Implicit Kill
              ("(paxedit-put (paxedit-new)
                        :one -!-1234
                        :two 2345)"
               "(paxedit-put (paxedit-new)
                        :two 2345):one 1234-!-")
              ("(paxedit-put (paxedit-new)
                   -!-     :one 1234
                        :two 2345)'end"
               "'end(paxedit-put (paxedit-new)
                        :one 1234
                        :two 2345)-!-")
              ;; Comment Kill
              ("-!-;; one
                (message \"hello\")"
               "
                (message \"hello\");; one-!-")
              ("'art ;; -!-My Comment
                'project"
               "'art
                'project;; My Comment-!-"))
  (xtd-return= (lambda (_) (cl-letf (((symbol-function 'message) (lambda (output) (concat "message:: " output))))
                             (paxedit-test-elisp-setup)
                             (call-interactively 'paxedit-kill)))
               ("-!-" "message:: Nothing found to kill in this context.")
               ("-!-(+ 1 2 3)" "message:: Nothing found to kill in this context.")))

(xt-deftest paxedit-delete
  (xtd-setup= (lambda (_) (paxedit-test-elisp-setup)
                (let ((kill-ring nil))
                  (call-interactively 'paxedit-delete)
                  (ignore-errors (yank))))
              ;; SEXP Delete
              ("(+ 1 2 (- 3 -!-6))" "(+ 1 2-!-)")
              ("(+ 1 2
                (- 3 -!-6))" "(+ 1 2-!-)")
              ;; SEXP Implicit Delete
              ("(paxedit-put (paxedit-new)
                        :one -!-1234
                        :two 2345)"
               "(paxedit-put (paxedit-new)-!-
                        :two 2345)")
              ("(paxedit-put (paxedit-new)
                   -!-     :one 1234
                        :two 2345)'end"
               "-!-'end")
              ;; Comment Delete
              ("-!-;; one
                (message \"hello\")"
               "-!-
                (message \"hello\")")
              ("'art ;; -!-My Comment
                'project"
               "'art-!-
                'project"))
  (xtd-return= (lambda (_) (cl-letf (((symbol-function 'message) (lambda (output) (concat "message:: " output))))
                             (paxedit-test-elisp-setup)
                             (call-interactively 'paxedit-delete)))
               ("-!-" "message:: Nothing found to delete in this context.")
               ("-!-(+ 1 2 3)" "message:: Nothing found to delete in this context.")))

(xt-deftest paxedit-transpose-forward
  (xtd-setup= (lambda (_) (paxedit-test-elisp-setup)
                (call-interactively 'paxedit-transpose-forward))
              ;; Symbol Refactoring
              (":o-!-ne :two :three" ":two :o-!-ne :three")
              ;; SEXP Refactoring
              ("(+ 12-!-3 456 234)" "(+ 456 12-!-3 234)")
              ("(+ 12-!-3 (+ 1 1) 234)" "(+ (+ 1 1) 12-!-3 234)")
              ("(+ (- 12 -!-3) (+ 1 1) 234)" "(+ (+ 1 1) (- 12 -!-3) 234)")
              ("(+ (- 12 3-!-) (+ 1 1) 234)" "(+ (+ 1 1) (- 12 3-!-) 234)")
              ("(+-!- 1 1) (+ 3 2)" "(+ 3 2) (+-!- 1 1)")
              ("(+-!- 1 1) 'art (+ 3 2)" "'art (+-!- 1 1) (+ 3 2)")
              ("(+-!- 1 1)
                ;; Misc Comment
                (+ 3 2)"
               "(+ 3 2)
                ;; Misc Comment
                (+-!- 1 1)")
              ;; Implicit SEXP Refactoring
              ("(paxedit-put (paxedit-new)
                        :one -!-123
                        :two 234)"
               "(paxedit-put (paxedit-new)
                        :two 234
                        :one -!-123)")
              ("(paxedit-put (paxedit-new)
                        :setup \"na\"
                        :one -!-123
                        :two 234)"
               "(paxedit-put (paxedit-new)
                        :setup \"na\"
                        :two 234
                        :one -!-123)")
              ("(paxedit-put (paxedit-new)
                        -!-:one 123
                        :two 234)"
               "(paxedit-put (paxedit-new)
                        :two 234
                        -!-:one 123)")
              ("(paxedit-put (paxedit-new)
                        :on-!-e 123
                        :two 234)"
               "(paxedit-put (paxedit-new)
                        123 :on-!-e
                        :two 234)")
              ("(paxedit-put (paxedit-new)
                        (one 1)-!- 1234
                        :two 2345)"
               "(paxedit-put (paxedit-new)
                        :two 2345
                        (one 1)-!- 1234)")
              ("(paxedit-put-!- (paxedit-new) :one 1)
                (print 'x)"
               "(print 'x)
                (paxedit-put-!- (paxedit-new) :one 1)")
              ;; Comment Refactoring
              ("(+ 1 1)
                ;; -!-Hello World
                (+ 1 2)
                ;; Greeting"
               "(+ 1 1)
                ;; Greeting
                (+ 1 2)
                ;; -!-Hello World")
              ("(+ 1 1)
                (+ 9 9 9);; -!-Hello World
                (+ 1 2)
                ;; Greeting"
               "(+ 1 1)
                (+ 9 9 9);; Greeting
                (+ 1 2)
                ;; -!-Hello World"))
  (xtd-return= (lambda (_) (paxedit-test-elisp-setup)
                 (cl-letf (((symbol-function 'message) (lambda (output) (error (concat "message:: " output)))))
                   (condition-case ex
                       (call-interactively 'paxedit-transpose-forward)
                     (error (cadr ex)))))
               ("" "message:: No action found for this context.")
               ("(somefunc 1 2 3 (+ 2 -!-))" "message:: Nothing found to swap with.")
               ("(paxedit-put (paxedit-new)
                        (one 1) 1234
                        :two-!- 2345)" "message:: Nothing found to swap with.")))

(xt-deftest paxedit-tranpose-backward
  (xtd-setup= (lambda (_) (paxedit-test-elisp-setup)
                (paxedit-transpose-backward))
              (";; world hell-!-o" ";; hell-!-o world")
              (";; hell-!-o world" ";; hell-!-o world")
              (";; other message
;; hell-!-o world" ";; other message
;; hell-!-o world")))

(xt-deftest paxedit-transpose-backward-clojure
  (xtd-setup= (lambda (_) (paxedit-test-clojure-setup)
                (paxedit-transpose-backward))
              ("{:a 1 :b-!- 2}" "{:b-!- 2 :a 1}")
              ("(let [:a 1 :b-!- 2])" "(let [:b-!- 2 :a 1])")
              ("(art [:a 1 :b-!- 2])" "([:a 1 :b-!- 2] art)")))

;;; Context Tests

(xt-deftest paxedit-cxt-sexp-enumerate
  (xtd-return= (lambda (_) (paxedit-cxt-sexp-enumerate (paxedit-context-generate :forward 1)))
               ("(-!-1 2 3)" '((2 . 3) (4 . 5) (6 . 7)))
               ("(-!-1 2 \"3\")" '((2 . 3) (4 . 5) (6 . 9)))))

(xt-deftest paxedit-cxt-sexp?
  (xtd-return= (lambda (_) (paxedit-cxt-sexp? (paxedit-context-generate)))
               ("(+ 1 -!-2)" t)
               ("{:one 1 -!-:two 2}" t)
               ("-!-(+ 1 2)" nil)
               ("" nil)))

(xt-deftest paxedit-cxt-topmost-sexp?
  (xtd-return= (lambda (_) (paxedit-cxt-topmost-sexp? (paxedit-context-generate)))
               ("(+ 1 -!-2)" t)
               ("(+ 1 (1- -!-20))" nil)
               ("-!-()" nil)))

;;; Interactive Functions

(xt-deftest paxedit-backward-up
  (xtd-setup= (lambda (_) (paxedit-test-elisp-setup)
                (call-interactively 'paxedit-backward-up))
              ("(when (+ 1-!- 2) t)" "(when -!-(+ 1 2) t)")
              ;; Implicit SEXP backward up tests
              ("(paxedit-put (paxedit-new) :one-!- 1)" "(paxedit-put (paxedit-new) -!-:one 1)")
              ("(paxedit-put (paxedit-new) :o-!-ne 1)" "(paxedit-put (paxedit-new) -!-:one 1)")
              ("(paxedit-put (paxedit-new) -!-:one 1)" "-!-(paxedit-put (paxedit-new) :one 1)")
              ("(paxedit-put (paxedit-new) :one 1-!-)" "-!-(paxedit-put (paxedit-new) :one 1)")
              ;; Comment backward up
              (";; hello -!-world" "-!-;; hello world"))
  (xtd-setup= (lambda (_) (paxedit-test-elisp-setup)
                (paxedit-backward-up 2))
              ("(when (+ 1-!- 2) t)" "-!-(when (+ 1 2) t)")
              ;; Implicit SEXP backward up
              ("(paxedit-put (paxedit-new) :one-!- 1)" "-!-(paxedit-put (paxedit-new) :one 1)")))

(xt-deftest paxedit-backward-end
  (xtd-setup= (lambda (_) (paxedit-test-elisp-setup)
                (call-interactively 'paxedit-backward-end))
              ("(when (+ 1-!- 2) t)" "(when (+ 1 2)-!- t)")
              ;; Implicit SEXP backward end
              ("(paxedit-put (paxedit-new) :one-!- 1)" "(paxedit-put (paxedit-new) :one 1-!-)")
              ("(paxedit-put (paxedit-new) :o-!-ne 1)" "(paxedit-put (paxedit-new) :one 1-!-)")
              ("(paxedit-put (paxedit-new) -!-:one 1)" "(paxedit-put (paxedit-new) :one 1)-!-")
              ("(paxedit-put (paxedit-new) :one 1-!-)" "(paxedit-put (paxedit-new) :one 1)-!-")
              ;; Comment backward end
              (";; hello-!- world, :D" ";; hello world, :D-!-"))
  (xtd-setup= (lambda (_) (paxedit-test-elisp-setup)
                (paxedit-backward-end 2))
              ("(when (+ 1-!- 2) t)" "(when (+ 1 2) t)-!-")
              ;; Implicit SEXP backward end tests
              ("(paxedit-put (paxedit-new) :one-!- 1)" "(paxedit-put (paxedit-new) :one 1)-!-")))

(xt-deftest paxedit-sexp-raise
  (xtd-setup= (lambda (_) (paxedit-sexp-raise))
              ("(+ 1 2 (+ 2-!- 3))" "(+ 2-!- 3)")
              ("(+ 1 2
 (+ 2-!- 3))" "(+ 2-!- 3)")
              ("(1+ (+ 1 2
 (+ 2-!- 3)))" "(1+ (+ 2-!- 3))")
              ("-!-" "-!-")
              ("(concat user-me-!-ssage name)" "user-me-!-ssage"))
  (xtd-return= (lambda (_) (cl-letf (((symbol-function 'message) (lambda (output) (concat "message:: " output))))
                        (paxedit-sexp-raise)))
               ("-!-" "message:: No SEXP found to raise.")))

(xt-deftest paxedit-wrap-comment
  (xtd-setup= (lambda (_) (let ((paxedit-alignment-cleanup nil))
                       (paxedit-wrap-comment)))
              ("(message-!- \"hello\")" "(comment (message-!- \"hello\"))")
              ("(message
\"hello\"-!-)" "(comment (message
\"hello\"-!-))")))

(xt-deftest paxedit-delete-whitespace
  (xtd-setup= (lambda (_) (paxedit-delete-whitespace))
              ("(hello    -!-  world)" "(hello-!-world)")
              ("(+ 1-!-


3)" "(+ 1-!-3)")))

(xt-deftest paxedit-wrap-function
  (xtd-setup= (lambda (_) (paxedit-wrap-function "1+" '(1 . 6)))
              ("(func-!-)" "(1+ (func-!-))")
              ("-!-(func)" "(1+ -!-(func))")))

(xt-deftest paxedit-copy
  (xtd-setup= (lambda (_) (call-interactively 'paxedit-copy)
                (goto-char (point-max))
                (insert " ")
                (call-interactively 'yank))
              ("(+ 1 2-!-)" "(+ 1 2) (+ 1 2)-!-")))

(xt-deftest paxedit-symbol-change-case
  (xtd-setup= (lambda (_) (call-interactively 'paxedit-symbol-change-case))
              ("hell-!-o" "HELL-!-O")
              ("HELL-!-O" "hell-!-o")))
