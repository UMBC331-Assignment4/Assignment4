# Authors: Eric Hebert, Navneet Kaur, Dong Sun, Hamza Saeed

# Run with:
	# pil assignment4.l
	# (assignment4 (1 2 3 4))

# This code was derived from http://picolisp.com/wiki/?99pp93
# Source: "PicoLisp Wiki: 99pp93." PicoLisp Wiki: 99pp93. Alexander Burger, 23 July 2010. Web. 28 Oct. 2014.

(de infix (expression)
	“Converts a list between infix and prefix notation.”
	(if (atom expression) # If the expression passed in is an atom (not a list)
	expression # Return the atom
	(list # Otherwise, return the infix representation of the list.
		(infix (cadr expression)) # List of the second element
		(car expression) # List of the first element
		(infix (caddr expression))))) # List of the third element

(de expressions (numbers)
	“Generates all possible combinations of arithmetic expressions.”
	(if (cdr numbers) # Continue recursing if there is more than one number in the list
	(mapcan 
		'((split-index) # Pass split-index to operator function
			(mapcan
			'((left-list) # Apply the left side elements from the list
				(mapcan
					'((right-list) # Apply the right side elements from the list
						(mapcar
						'((operator) (list operator left-list right-list)) # Call operator for the first element of list, then the second element and so on.
						'(+ - * /))) # List of operators to use for generating equations
					(expressions (tail (- split-index) numbers)))) # Recurse to get more combinations
			(expressions (head split-index numbers)))) # Recurse to get more combinations
		(range 1 (dec (length numbers))))
	(list (car numbers)))) # Stop recursing, and return a list of the first element in the list

(de equations (Lst)
	“Finds the first valid expression that evaluates to true, and prints it.”
	(use /
	(redef / (left-list right-list)
		(and (n0 right-list) (=0 (% left-list right-list)) (/ left-list right-list)))
	# Loop through the list of combinations, insert the “=” operator into different locations, and find the first equation that evaluates to true.
	(for (split-index 1 (> (length Lst) split-index) (inc split-index)) # Loops through all of the places that the equals sign can go
		(for left-list (expressions (head split-index Lst)) # Get all of the left half lists
			(for right-list (expressions (tail (- split-index) Lst)) # Get all of the right half lists
			(let? left-list-value (eval left-list) # Evaluate the left list and save the result
				(when (= left-list-value (eval right-list)) # Check if this equation is true
					(println (infix left-list) '= (infix right-list)) # Print the equation in infix notation
					(quit)))))))) # Quit after finding the first valid equation

(de assignment4 (lst)
	“Displays a valid equation from a list of numbers, or an error message if nothing was found.”
	(unless (equations lst)
		(println 'No 'equation 'found)
		(quit)))
