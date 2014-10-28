# TODO:
    # Read command line input
    # Print "No equation found" when no valid equations are found
    # Cite in proper MLA format
    # Add more comments, clean up current comments

# Authors: Eric Hebert, <insert names here>
# Run with:
    # pil assignment4.l
    # (equations (1 2 3 4))

# This code was derived from http://picolisp.com/wiki/?99pp93

# Converts a list between infix and prefix notation
(de infix (E)
   (if (atom E)
      E
      (list
         (infix (cadr E))
         (car E)
         (infix (caddr E)) ) ) )

# Generate all possible combinations of the expressions
(de expressions (X)
   (if (cdr X)
      (mapcan
         '((I)
            (mapcan
               '((A)
                  (mapcan
                     '((B)
                        (mapcar
                           '((Op) (list Op A B))
                           '(+ - * /) ) )
                     (expressions (tail (- I) X)) ) )
               (expressions (head I X)) ) )
         (range 1 (dec (length X))) )
      (list (car X)) ) )

# Find all valid expressions that evaluate to true
(de equations (Lst)
   (use /
      (redef / (A B)
         (and (n0 B) (=0 (% A B)) (/ A B)) )
      (for (I 1  (> (length Lst) I)  (inc I))
         (for A (expressions (head I Lst))
            (for B (expressions (tail (- I) Lst))
               (let? N (eval A)
                  (when (= N (eval B))
                     (println (infix A) '= (infix B))
                     (quit) ) ) ) ) ) ) )

(de a4 (lst)
    (unless (equations lst)
        (println "No equation found.")
        (quit)))
