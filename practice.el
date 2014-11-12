;;; Paxedit
;;; practice.el, place to experiment with Paxedit

;;; Why Paxedit rocks compared to manual refactoring,
;;; transpose-sexps, and other tranpose-* functions

;;; 1. One command works everywhere
;;; 2. Do the 'right thing' in the right context
;;; 3. Preserve the cursor position
;;; 4. Clean up whitespace and indentation

(concat "!!"
        ", this is an interesting program!"
        "world Hello")

(concat "!!"
        ", this is an interesting program!"
        "world Hello")

;;; second
;;; first

(message (concat "Having "
                 "a good day?"))
(message "Hello Tom!")

(setf x 10
      y (+ 1 2
           3 4)
      g 20)

(paxedit-put (paxedit-new)
             :lead-actor "Tom Hanks"
             :movie-title (concat "Apollo 13"
                                  ", The Movie")
             :cost 100000000)

;;; Killing / Deleting expressions

(paxedit-put (paxedit-new) :one 1 :two 2 :delete-me 123123123123 :three 3)

;;; Unnecessary comment

(paxedit-put (paxedit-new)
             :movie "Apollo 13"
             :cost 100000000
             "Tom Hanks" :lead-actor)

;;; Killing / deleting symbols

(concat hello-message
        other-message)

(concat hello-message
        other-message second-message)

;;; Navigating

;;; Commenting / Uncommenting expressions

(message "Demo complete!")

;;; Smart Upcase / Downcase

(defun my-function (the-message)
  "Prints the-message to the MINIBUFFER."
  (message the-message))

;;; Expand macros in place

(comment (+ 1 2))

(paxedit-aif (+ 1 2)
    (+ 2 it)
  (+ 3 it))
