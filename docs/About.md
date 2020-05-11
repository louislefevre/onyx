# What is the Onyx Compiler for?
The Onyx Compiler is a programming language designed to be used at a beginner programming level, regardless of age. The idea is that it uses clear terminology to help users better understand what each piece of functionality does, with detailed error message feedback for guiding learners in solving syntax problems. Hidden inbuilt functions are included to help deal with more complex tasks, allowing users to still achieve goals without having to know how to fully implement their functionality.  

The goal is that once a user can implement basic programs in this language and understand how each part works, they have learned the basic foundations of programming concepts. As its primary use would be in lessons, students would be given tasks to complete in the programming language to demonstrate their understanding. These tasks are done as a series of hidden functions, with the user passing their answer as input. The function then outputs whether or not their answer is correct or not, and gives them information on improving if possible. Each task will be designed to test various aspects of foundational programming, with the user being done when completing all the tasks. The language comes with pre-packaged documentation, detailing each task and how to use the language in general.  

# What are the features of the compiler?
The language is limited to the main features found in common programming languages: functions, variables/data types, operators, conditionals and loops. In order to keep things simple and avoid learning the differences between features with similar functionality, it contains only the primary members of these features. For example:  
- Variables
  - Variables do not need to be declared and may be assigned different types.
  - Doesn't support use of different types together.
- Data Types
  - Supports int, double, boolean and string.
  - Doesn't support have float, char, short, byte, long.
- Operators
  - Supports equals, plus, minus, divide, multiply, modulo, power (and their assignment variations).
  - Doesn't support increment and decrement.
- Conditionals
  - Supports if statements.
  - Doesn't support switch/case statements.
- Loops
  - Supports for-style loops.
  - Doesn't support while loops or for each loops.

The following is a brief overview of the syntax for the language:
- Operators
  - =
    - Assignment operator
    - ```a = 10```
  - ==
    - Equality operator
    - ```a == 10```
  - \+
    - Addition operator
    - ```a + 10```
  - \-
    - Subtraction operator
    - ```a - 10```
  - \*
    - Multiplication operator
    - ```a * 10```
  - /
    - Division operator
    - ```a / 10```
  - %
    - Modulo operator
    - ```a % 10```
  - ^
    - Power operator
    - ```a ^ 10```
  - \>
    - Greater operator
    - ```a > 10```
  - <
    - Less operator
    - ```a < 10```
  - +=, -=, *=, /=, %=, ^=
    - Assignment operators
    - ```a += 10```
- Loops
  - For loop
  - ```loop myNumber from 0 to 100```
- Conditionals
  - If statement
  - ```If a == 10```
- Integer
  - Integer number
  - ```10```
- Double
  - Decimal number
  - ```10.0```
- Boolean
  - Boolean value
  - ```true```
- Strings
  - Text string
  - ```"string"```

Example program written in Onyx:
```
a = 10
b = 20
c = true
d = false
e = "string"
var = 0

if a <= b  # if a is less than or equal to b
{
    loop i from 1 to 10  # inclusive
    {
        var += i
        if i == 10
            var *= 5
    }
}
else
{
    var = 10
}

var  # prints var
```

