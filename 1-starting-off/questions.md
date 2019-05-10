Questions

1.What are the types of the following expressions and what do they evaluate to, and why?

17 `(int and this expression is a value already)`

1 + 2 * 3 + 4 `(int) and evaluates to 11 since multiplication is done first`

800 / 80 / 8 `(int) and evaluates to 1 as / returns ints`

400 > 200 `(bool) and evaluates to true as 400 is greater than 200`

1 <> 1 `(bool) and evaluates to false as 1 is equal to 1`

true || false `(bool) and evaluates to true since one of the operands is true`

true && false `(bool) and evaluates to false since not both of the operands is true`

if true then false else true `(bool) and evaluates to false since condition is true and it returns then clause`

'%' `(char) and it's already a value`

'a' + 'b' `(no type/type-error) since the + operator does not work on char's`

2.Consider the evaluations of the expressions 1 + 2 mod 3 , (1 + 2) mod 3 , and 1 + (2 mod 3) . What
can you conclude about the + and mod operators?

The mod operator has a higher precedence than the + operator.

3.A programmer writes 1+2 * 3+4 . What does this evaluate to? What advice would you give him?

It would give the programmer 11. From the spacing between the * operator it seems like he/she wants 1 + 2 and 3 + 4 to be evaluated before multiplying the results of those two sub-expressions. I'd advice them to put parentheses around those two sub expressions as spacing does not affect evaluation order.

4.The range of numbers available is limited. There are two special numbers: min_int and max_int .
What are their values on your computer? What happens when you evaluate the expressions max_int
+ 1 and min_int - 1 ?

+/-4611686018427387903
The number line seems to wrap around.

5.What happens when you try to evaluate the expression 1 / 0 ? Why?

It throws an exception once the program is run. Probably because any number divided by zero tends to infinity and there's probably no representation for infinity in Ocaml std lib. 

6.Can you discover what the mod operator does when one or both of the operands are negative? What
about if the first operand is zero? What if the second is zero?

For x mod y: 
  
  when y = 0, Ocaml prints Exception: Division_by_zero

  when y <> 0 && x < 0, the result will be be negative
  

  when y <> 0 && x = 0, the result will be zero. 

7.Why not just use, for example, the integer 0 to represent false and the integer 1 for true? Why have
a separate bool type at all?

It prevents unexpected values and allows us more easily to show that a program is correct. 

8.What is the effect of the comparison operators like < and > on alphabetic values of type char? For
example, what does 'p' < 'q' evaluate to? What is the effect of the comparison operators on the
booleans, true and false ?

'p' < 'q' evaluates to true. Lowercase characters are in alphabetical order and uppercase characters are bigger than lowercase characters. False is considered "less than" true. 