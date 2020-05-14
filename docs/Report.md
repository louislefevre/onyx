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
A compiler goes through various stages when processing syntax, with each stage transforming the source programs representation in some way. In the form of a pipeline, each component takes input from its predecessor, transforms it, and feeds the output forward to the next component [1]. The amount of stages within a compiler can vary, but in the case of Onyx contains the following.

#### 4.1.1 Lexical Analysis
Any compiler will always start with lexical analysis, which is performed by the lexer. It takes the source code as input and scans over it, typically left to right, and groups the characters into lexemes [1]. The lexer represents these lexemes in the form of tokens [2], with each token containing information about the type of data it holds. For example, given the input of an integer, the lexer would output a token that identifies the characters location in the text, its type (e.g. an integer token), and its value. This process is performed for all the characters in a given text, and the lexer outputs a stream of tokens [3]. During this phase, the lexer would also be responsible for reporting any invalid characters located in the source code.

The lexer was designed to be capable of handling all types of characters, whilst also taking into account their context. It had to be capable of consistently taking in new characters and identifying them correctly, whilst also adapting to deal with characters found within particular boundaries. For example, recognising that a number occurring after quotations is a part of a string, rather than a token in its own right. The handling of invalid characters must also be considered, as otherwise the lexer would become stuck and be unable to finish, and having a robust design means never failing to take in characters and output valid tokens.

In summary, the primary functions of this phase is to:
- Take a string of text as input.
- Inspect each individual character, classifying each token with its corresponding type, whilst also recognising invalid characters.
- Output a stream of tokens, often in the form of a list.
- Identify invalid characters.

##### Example
| Token       |     Token Type      |
| ----------- | :-----------------: |
| var         |     Identifier      |
| =           | Assignment operator |
| "my string" |       String        |
| +           |    Plus operator    |
| 10          |       Integer       |

#### 4.1.2 Syntax Analysis
The second stage of compilation is syntax analysis, also known as parsing, and is performed by the parser. Its role is to take the list of tokens produced by the lexer as input, validate the arrangement of tokens against the programming languages grammatical rules, and from that generate a parse tree that represents the structure of the source program. The primary purpose of this component is to ensure the expressions made by the arrangement of tokens is syntactically correct, following a format defined by the language being used [2]. From this a parse tree can be generated, which demonstrates how each token is grouped together in each individual statement. The parser would also identify any syntactical errors found within the source code.

The parser is designed as a recursive descent parser, which adopts a top-down parsing strategy where it begins at the highest level of the parse tree and works its way down, building the parse tree as it goes [4]. The input is read from left to right, taking in tokens until it reaches the end of the file. Each token would be identified by its type, sending the parser flow in a different direction depending on the result. The following tokens would then be checked to ensure they appear as expected, such as an equals operator token appearing after an identifier token, and an expression would be returned. This expression would contain all its relevant tokens, such as in the case of the previous example: an identifier, an equals operator, and the assignment. In the original design the parser was only able to handle expressions, but this was later extended to also process statements so that full programs could be written all at once since the former only allowed REPL-like behaviour.

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

#### 4.1.3 Semantic Analysis
Next comes semantic analysis, which is another stage of parsing. Directly after syntax analysis, semantic analysis takes place as a means of gathering semantic information about the source code [5], with this phase being performed by the type checker in the case of Onyx. It receives the parse tree from the previous stage and adds extra information to it, such as: type checking to ensure type conversions are valid, and object binding for associating variable and function references with their definitions [6]. This is also where the symbol table is built; adding variables coupled with their values and types. An annotated parse tree is output as a result (annotated referring to the extra data having been added to the tree). It is during this phase that any errors relating to type incompatibility are identified.

Initially, it was designed so that semantic analysis would be performed within the parser alongside syntax analysis. It was soon found that this would be implausible since it typically requires a complete parse tree before being able to add annotations properly, so it was instead moved to the type checker as an individual component. The type checker functions by reviewing the type of expression or statement that is being executed and comparing the data types of each operand, as well as reviewing whether the operator being used is compatible. It contains a series of defined compatibility rules which must be adhered to, the prevailing rule of which is that only values of the same type can be used with one another. An error will be returned should there be an expression that fails to meet these rules. Any undeclared variables will also be identified during this stage, and be returned as errors.

In summary, the primary functions of this phase is to:
- Get the parse tree from the parser.
- Validate data type compatibility for operands and operators, ensure variables have been declared, and store variable information within the symbol table.
- Annotate the parse tree with data type information, producing an annotated parse tree.
- Identify mismatched data types, incompatible operands, and undeclared variables.

##### Example
```
x = 30
y = true
x * y
This will result in a type-mismatch error as integers and booleans are not compatible.
```

#### 4.1.4 Evaluation
The final stage is the evaluation, performed by the aptly named evaluator. The purpose of this phase is to calculate the final outputs of each statement, revealing the final result. It reviews the annotated parse tree and with the extra information gathered by the type checker, is able to execute each expression in Java and return their values. The output is the final value of the statement being evaluated. Its also worth noting that this is where Onyx diverges when compared to other compilers, as they often instead follow semantic analysis with: intermediate code generation, code optimisation, and code generator. However, these stages are instead handled by Java and are not implemented by Onyx directly, and therefore will not be discussed here.

The design of the evaluator is opposite to the parser in regards to the fact that it works through the tree bottom-up; searching the tree by beginning at the furthest nodes, carrying their results up the tree as it works through to the larger expressions, using the newly found values to do so. The result returned is only ever stored either within another statement, or if its a top level statement within a variable. Any expressions that fails to store or use its result in one form or another is discarded. Also, a unique aspect of this stage is the fact no errors occur here, as all the necessary error identification should've happened in the prior stages. However, there are fail-safe exceptions thrown should an error slip through the cracks, as it helps catch out unhandled issues during development.

In summary, the primary functions of this phase is to:
- Obtain the annotated parse tree from the type checker.
- Evaluate each expression and statement.
- Return the resulting value(s).
- Identify any unhandled issues that have gone undetected during the previous stages.

##### Example
```
x = 30
y = 10
x * y
300
```

#### 4.1.5 Symbol Table
The symbol table is a data structure that is maintained throughout every phase of a compilers lifecycle, responsible for storing the names of identifiers, as well as their respective values and data types [2]. It is also often used for scope management, but this is not present in Onyx due to the fact it only implements global variables.

The symbol table is designed to be implemented in the form of a hash map, with the name of the identifier as the key and the value and type as the value. Each of the previous stages has access to the symbol table, and are able to use it to check whether or not a symbol is contained within the table, as well as retrieve any information about a particular one. This is most prominent during the type checker and evaluator, as during these two stages is when types begin to become relevant for the reasons previously described.

In summary, the primary functions of this phase is to:
- Store the names of identifiers, along with their value and type.
- Provide a method for adding and removing symbols.
- Allow retrieval of information on symbols contained within the symbol table.

##### Example
```
x = 30
y = true

Symbol Table:
x = {
   name: "x",
   value: 5,
   type: integer
}
y = {
   name: "y",
   value: true,
   type: boolean
}
```

#### 4.1.6 Error Handler
The error handler is responsible for handling errors before continuing with the compilation process, and like the symbol table is also accessible to every stage. Throughout each phase should an error occur, it is reported to the error handler and reported to the user in the form of an appropriately formated error message. Errors are capable of occurring in every stage of the compiler with the exception of the evaluator, where only exceptions can occur.

In order to prevent interruption, Onyx is designed so that it may continue should an error occur. While this makes no difference in terms of the output (as only the error message is returned rather than any calculated result), it allows certain processes to continue being performed. For example during the generation of the parse tree, when an error occurs the invalid element is replaced by a placeholder (holding a null value), allowing the parse tree to still be built despite the invalid syntax. This means the compiler can still provide information regarding the tree for the rest of the syntax, as well as things like data types. By not having the compiler quit dead in its tracks during compilation it opens up the possibility to gather more information, as well as potentially provide tooling in the future.

In summary, the primary functions of this phase is to:
- Receive errors from each stage of compilation.
- Format those errors and generate a detailed message.
- Output the error message to the user.

##### Example
```
x = true
x * 10
Error (2, 2): Binary operator '*' is not defined for type 'boolean' and 'integer'.
	x * 10
```

## References
1. https://www.guru99.com/compiler-design-phases-of-compiler.html
2. https://www.tutorialspoint.com/compiler_design/compiler_design_phases_of_compiler.htm
3. https://www.kttpro.com/2017/02/09/six-phases-of-the-compilation-process
4. https://www.geeksforgeeks.org/recursive-descent-parser/
5. https://en.wikipedia.org/wiki/Semantic_analysis_(compilers)
6. https://en.wikipedia.org/wiki/Compiler

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
- Parsing
- Semantic information
- Type checker
- Annotated parse tree
- Object binding
- Symbol table
- Symbol
- Hash map
- Data structure
- Identifier
- Scope
- Variable
- Error
- Exception