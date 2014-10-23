Assignment4
===========

Due Oct 28
----------

Given a sequence of numbers, it is sometimes possible to place the basic operations of =, +, −, ×, and ÷ between those number to build a complete, correct equation. For instance, the sequence [2, 3, 8, 4, 22] can be made into an equation by putting the symbols in the following places:

```
2 + 3 × 8 − 4 = 22
```

Note how the order of operations is followed. Write a program that will build such a number sentence, given the list of numbers. An ideal solution would allow the numbers to be specified on the command line, but prompting the user for a list is also acceptable. If no true sentence can be found, print such a message to the user. Assume only one = will be used (allowing for multiple equal signs makes the assignment a lot harder, but is fun!)

You may present the sentence using prefix, infix, or postfix operators.

Sample Output
-------------

Note that some of these sequences could form multiple sentences. Only one need be printed.

```
> assignment-4 1 2 3 4 
1 - 2 = 3 - 4 
> assignment-4 1 1 1
 1 / 1 = 1 
> assignment-4 0 18 1 2 1 1 
0 / 18 = 1 - 2 + 1 * 1 
> assignment-4 2 2 2 
No equation found 
```
