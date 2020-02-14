# Onyx Compiler
Custom compiler written in Java.  

The Onyx Compiler is a programming language designed to be used at a beginner programming level, regardless of age. The idea is that it uses clear terminology to help users better understand what each piece of functionality does, with detailed error message feedback for guiding learners in solving syntax problems. Hidden inbuilt functions are included to help deal with more complex tasks, allowing users to still achieve goals without having to know how to fully implement their functionality.  

The goal is that once a user can implement basic programs in this language and understand how each part works, they have learned the basic foundations of programming concepts. As its primary use would be in lessons, students would be given tasks to complete in the programming language to demonstrate their understanding. These tasks are done as a series of hidden functions, with the user passing their answer as input. The function then outputs whether or not their answer is correct or not, and gives them information on improving if possible. Each task will be designed to test various aspects of foundational programming, with the user being done when completing all the tasks. The language comes with pre-packaged documentation, detailing each task and how to use the language in general.  

The language is limited to the main features found in common programming languages: functions, variables/data types, operators, conditionals and loops. In order to keep things simple and avoid learning the differences between features with similar functionality, it contains only the primary members of these features. For example:  
- Variables/Data Types
  - Does have int, string and boolean.
  - Doesn't have double, float, char, short, byte, long.
- Operators
  - Does have equals, plus, minus, divide, multiply.
  - Doesn't have modulo, increment, decrement.
- Conditionals
  - Does have if statements.
  - Doesn't have switch/case statements.
- Loops
  - Does have for loops.
  - Doesn't have while, for each.

The following is a brief overview of the syntax for the language:
- Operators
  - =
    - is
    - "Number num is 5"
  - ==
    - equals
    - "If num equals 5"
  - \+
    - plus
    - "num plus 10"
  - \-
    - minus
    - "num minus 10"
  - /
    - divide
    - "num divide 2"
  - \*
    - multiply
    - "num multiply 5"
  - >
    - greater than
    - "If myNum greater than num"
  - <
    - less than
    - "If myNum less than num"
- Functions
  - Task
  - "Task addNumbers():"
- Loops
  - Loop
  - "Loop myNumber from 0 to 100"
- Conditionals
  - If
  - "If myNumber equals num"
- Ints
  - Number
  - "Number num is 5"
- Strings
  - Text
  - "Text text is "Hello world""
- Booleans
  - Bool
  - "Bool bool is true"

Example program written in Onyx:
<pre>
Task addNumbers()
{
  Number myNum is 0
  Loop loopNum from 0 to 100:
    myNum plus loopNum
}
</pre>


## Conventions
### Class Structure
1. Variables  
  a. Static variable declarations  
  b. Instance variable declarations  
2. Static constructor  
  a. Static variable initialization  
3. Static methods  
  a. Other static methods  
  b. Static variable getters & setters  
4. Instance constructor  
  b. Instance variable initialization  
5. Instance methods  
  a. Other instance methods  
  b. Instance variable getters & setters  

i. All variables must be declared private, and any final variables come first in their corresponding section.  
ii. Reference variables should be declared first, and primitive variables second.  
iii. Setters and getters should be ordered in the same order as they declaration.  
iv. Scope of methods should limit access as much as possible.  
v. Other methods should be ordered based on access modifiers, followed by workflow order.


## To-Do
- Move addTokens() from Parser into Lexer.
- Move diagnostics into own ErrorHandler.class from the Evaluator.class.
- Move Syntax/Token information into own package.
- Modularise Parser parsePrimaryExpression() and parseExpression() code.
- Declare and initialise necessary class variables simultaneously, rather than initialise in constructor.
- Put all syntax into a single class of constants.
- Create class for calling error messages (as constants?).
- Clean up variable names (.e.g change 'type' to 'class').
- Move around SyntaxTree class (and remove its redundant variables).

## Notes
- Using [Mavens Standard Directory Layout](http://maven.apache.org/guides/introduction/introduction-to-the-standard-directory-layout.html) for project structure.  
