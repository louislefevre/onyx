## Chapter 1 - Introduction
Onyx is a programming language designed to be used by beginners in attempt to simplify the foundations of writing code, allowing for a more straightforward learning experience. Coupled with the language is the Onyx Compiler, a written from scratch program that compiles source code written in unique Onyx syntax down into Java.

The primary goal of the project is to provide those who are learning programming for the first time with a tool specific to their needs. That is, a language that contains only the bare minimum amount of features required to learn basic coding skills, as to avoid overwhelming the user with unnecessary components. Whilst languages such as Python are often seen as a good starting language, they regularly require the user to deal with more complex functionality in order to simply set up the program. For example, handling imports, using a 'main' function, and understanding new file types. The large amount of features packaged with traditional languages makes everything infinitely more complex, and the scale of it can be overbearing for many. Onyx seeks to circumvent these issues and provide a language that plays to users familiarity’s, adopting a start-and-go attitude where they can instantly begin writing syntax with no setup.

This report serves as a deeper explanation into the development of Onyx, providing insight into the inner workings of the program and the steps taken throughout its progression. Whilst this report will explain the more complex aspects of compilers and how they work in detail, it is assumed that the reader has a substantial level of knowledge with computers and in computer science as a whole.


## Chapter 3 - Requirements
Before planning the project, it needed to be decided what functionality the compiler would actually include. The original goal was to keep the language simple with only the bare minimum amount of features, typically those taught in introductory programming courses, so that precondition had to be kept in mind. After reviewing such courses and consulting with tutors, there were five main features to be included: variables, operators, conditionals, loops, and functions. These were perhaps the most common pieces of functionality taught to beginners in a learning environment, so it become vital that the compiler contained these components.

To compliment this it was required to include data types. Originally the goal was to only allow explicit variable declarations, where the user would have to define the type of a variable during declaration before it could be used. However, this idea was abandoned as providing the user with another thing to be concerned with was outside the boundaries of the projects goal of minimalism. Instead variables are implicit and do not need to be declared or given a type, only assigned. It would be possible for variables to be assigned values of different data types regardless of the type the previous value was, though it would not be possible to use contrasting data types with one another.

A greatly important feature was the addition of detailed error messages. One of the greatest pitfalls among novice programmers is their inability to read and understand error messages, often due to their verbose and jargon-filled nature. It is common among popular languages for error messages to be returned as a long and confusing mess, which while useful for experienced users, can be devastatingly difficult to decipher for learners. Its a goal of Onyx to instead provide the user with simple yet explanatory error messages; giving a clear indication for where the error occurred, what caused it, and a possible explanation for how to fix it.

The compiler also removes a number of features typically built into languages, such as that of scope. All variables can be accessed globally, with no such thing as local variables. Whilst this would be an issue in a more large scale language, the simple nature of Onyx makes this viable whilst removing the need for the user to learn about scoping at this level. 


## Chapter 4 - Design
Designing of the project involved planning in two areas: the layout and structure of the compilers source code, and the syntax of the language. Its important that considerable consideration was taken for both of these aspects, as it made objectives far clearer and removed the need for consistent revisions during development.

### 4.1 Compiler Design
A compiler goes through various stages when processing syntax, with each stage transforming the source programs representation in some way. In the form of a pipeline, each component takes input from its predecessor, transforms it, and feeds the output forward to the next component [1]. The amount of stages within a compiler can vary, but will generally contain the following.

#### 4.1.1 Lexical Analysis
Any compiler will always start with lexical analysis, which is performed by the lexer. It takes the source code as input and scans over it, typically left to right, and groups the characters into lexemes [1]. The lexer represents these lexemes in the form of tokens [2], with each token containing information about the type of data it holds. For example, given the input of an integer, the lexer would output a token that identifies the characters location in the text, its type (e.g. an integer token), and its value. This process is performed for all the characters in a given text, and the lexer outputs a stream of tokens [3]. During this phase, the lexer would also be responsible for reporting any invalid characters located in the source code.

The lexer was designed to be capable of handling all types of characters, whilst also taking into account their context. It had to be capable of consistently taking in new characters and identifying them correctly, whilst also adapting to deal with characters found within particular boundries. For example, recognising that a number occurring after quotations is a part of a string, rather than a token in its own right. The handling of invalid characters must also be considered, as otherwise the lexer would become stuck and be unable to finish, and having a robust design means never failing to take in characters and output valid tokens.

In summary, the primary functions of this phase is to:
- Take a string of text as input.
- Inspect each individual character, classifying each token with its corresponding type, whilst also recognising invalid characters.
- Output a stream of tokens, often in the form of a list.
- Identify invalid characters.

##### Example
| Tokens      |     Token Type      |
| ----------- | :-----------------: |
| var         |     Identifier      |
| =           | Assignment operator |
| "my string" |       String        |
| +           |    Plus operator    |
| 10          |       Integer       |

#### 4.1.2 Syntax Analysis
The second stage of compilation is syntax analysis, also known as parsing, and is performed by the parser. Its role is to take the list of tokens produced by the lexer as input, validate the arrangement of tokens against the programming languages grammatical rules, and from that generate a parse tree that represents the structure of the source program. The primary purpose of this component is to ensure the expressions made by the arrangement of tokens is syntactically correct, following a format defined by the language being used [2]. From this a parse tree can be generated, which demonstrates how each token is grouped together in each individual statement. The parser would also identify any syntactical errors found within the source code.

The parser was designed as a recursive descent parser, which adopts a top-down parsing strategy where it begins at the highest level of the parse tree and works its way down, building the parse tree as it goes [4]. The input is read from left to right, taking in tokens until it reaches the end of the file. Each token would be identified by its type, sending the parser flow in a different direction depending on the result. The following tokens would then be checked to ensure they appear as expected, such as an equals operator token appearing after an identifier token, and an expression would be returned. This expression would contain all its relevant tokens, such as in the case of the previous example: an identifier, an equals operator, and the assignment. In the original design the parser was only able to handle expressions, but this was later extended to also process statements so that full programs could be written all at once since the former only allowed REPL-like behaviour.

Another aspect that had to be considered is precedence for operators. Most parsers define a priority value for each operator, and during parsing they are shuffled around with the parser being data driven by those priorities, providing an indicator for which expressions to parse next. This makes the adding of additional operators much easier, as it provides the ability to view operators and see their priority order.

In summary, the primary functions of this phase is to:
- Retrieve the tokens from the lexer.
- Checks if the source code is syntactically correct or not by comparing it against the grammatical rules of the programming language.
- Construct and output a parse tree representing the syntactic structure of the program.
- Identify invalid expressions and statements.

##### Example
```
(10 + 20) * 5

    *
   / \
  +   5
 / \  
10 20
```


## References
1. https://www.guru99.com/compiler-design-phases-of-compiler.html
2. https://www.tutorialspoint.com/compiler_design/compiler_design_phases_of_compiler.htm
3. https://www.kttpro.com/2017/02/09/six-phases-of-the-compilation-process
4. https://www.geeksforgeeks.org/recursive-descent-parser/

## Glossary of Terms
- Token
- Lexer
- Lexemes
- Source code
- Integer
- Parse tree
- Expression
- Statement
- REPL
- Identifier
- Data driven
- Recursive Descent Parser