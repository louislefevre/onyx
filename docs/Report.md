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
Designing of the project involved planning in two areas: the layout and structure of the compilers source code, and the syntax of the language (also known as the language specification). Its important that considerable consideration was taken for both of these aspects, as it made objectives far clearer and removed the need for consistent revisions during development.

### 4.1 Compiler Design
The goal of the compiler is to translate between the original syntax and Java, which means primarily focusing on front-end compiler construction. The middle-end and back-end portions are instead handled by Java, with such a design allowing the development of a language with unique syntax and functionality, without having to worry about the tricky implementation of close-to-machine-level aspects. A compiler goes through various stages when processing syntax, with each stage transforming the source programs representation in some way. In the form of a pipeline, each component takes input from its predecessor, transforms it, and feeds the output forward to the next component [1]. The amount of stages within a compiler can vary, but in the case of Onyx contains the following.

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

Another aspect that had to be considered is precedence for operators. Parsers sometimes define a priority value for each operator, and during parsing they are shuffled around with the parser being data driven by those priorities, providing an indicator for which expressions to parse next. This makes the adding of additional operators much easier, as it provides the ability to view operators and see their priority order.

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
The symbol table is a data structure that is maintained throughout every phase of a compilers life-cycle, responsible for storing the names of identifiers, as well as their respective values and data types [2]. It is also often used for scope management, but this is not present in Onyx due to the fact it only implements global variables.

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
The error handler is responsible for handling errors before continuing with the compilation process, and like the symbol table is also accessible to every stage. Throughout each phase should an error occur, it is reported to the error handler and reported to the user in the form of an appropriately formatted error message. Errors are capable of occurring in every stage of the compiler with the exception of the evaluator, where only exceptions can occur.

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

### 4.2 Language Specification
The language specification is what defines a programming language, detailing what valid syntax is and the behaviour comes from it. Onyx was designed with simplicity in mind, so the requirement for maintaining a simple specification was perhaps the most vital aspect during design. The main idea was to ensure each piece of functionality would be written and work as intuitively as possible, in order to leave little room for the user to be confused as to why something was working the way it was.

#### 4.2.1 Data Types
Onyx was designed to use only the most basic and necessary data types. Languages such as Java include a large amount of variations for essentially what is the same data type, except with varying amounts of memory. For example, integers along with bytes, shorts, and longs. It was prudent to not consider these redundancies for use, due to the fact that their benefit of using less memory would not prove useful in the context of learning. Instead the compiler only contains the following types: integer, double, boolean, string. This allows the compiler to still provide all the necessary functionality typically found in most languages, except without the extra cruft that would do nothing except make learning more difficult.

Its worth noting that the compiler does not allow different data types to be used together. For example, an integer cannot be used with a double in any operations. The purpose of this is to help provide a clear distinction between data types and avoid unexpected results that are difficult to debug for novices, since type errors are not always obvious particularly when used with variables.

##### Example
```
a = 10     (integer)
b = 20.0   (double)
c = true   (boolean)
d = "text" (string)
```

#### 4.2.2 Variables
In the original design it was thought that the language would be statically typed, requiring all variables to have their data types declared before use. This was later changed, however, so that the language was dynamically typed. The reason for this is that it made things far more simple from a user perspective, as variables could be reassigned at will without having to be concerned about the declared type, whilst also avoiding the worry of unexpected results due to the fact different types are incompatible with one enough when operated on. Not requiring users to declare variables before use was a good method of simplifying the language even further, since it removed the need to understand why declaring is necessary.

Variable scope in Onyx is also always kept global, no matter where a variable is declared. This design choice was primarily made due to the fact it removed the need for users to learn about scoping, and thus avoiding issues related to variables being out of bounds. In a much larger language this would of course be a major issue, but due to the fact programs written in Onyx will typically be very short in length and its purpose is simply to learn basic programming functionality, its not expected to cause problems.

##### Example
```
a = 10   (a is an integer)
b = true (a is now a boolean)
```

#### 4.2.3 Operators
The compiler specifies use for all the standard mathematical operators you would typically require to write expressions, but also includes some extra ones such as modulo and power. These two were not going to be provided initially, but was later added since they could provide significantly more functionality without being too overbearing for the user. What was not implemented, however, was the use of increment and decrements operators. This is mainly due to the difference between having the unary operators as a prefix and post-fix, which changes the order in which a value is incremented and returned. Also it is not as intuitive to recognise for beginners unlike other operators, so its use would likely lead to confusion all round.

As a slight remedy for the lack of the previously mentioned, various assignment operators were specified instead. These would allow variables to use operators on themselves, providing a shorthand form to prevent users being required to write out the full expression that includes the identifier name. It was originally unclear whether this would be added at all since the same operation is still possible with the longhand form, but it was decided to be intuitive enough to keep in.

##### Example
```
a = 10
a = a + 15 (longhand form)
a += 15    (shorthand form using plus-assignment operator)
```

#### 4.2.4 Conditionals
Conditional statements in Onyx are not too different from traditional languages and combines attributes from both Python and Java. For example, the specification doesn't require the use of parentheses for the condition but still allows them, giving the user the opportunity to use whichever they feel most comfortable with. However, a more imperative property is the presence of braces; they are required when declaring a block statement (code greater than 1 line), but can be avoided when encompassing only a single statement. In the former case the entire block will be executed, but in the latter only the next statement is executed. It was made a point that Onyx would not feature block statements that lack the use of braces, which is allowed in Python. The reason for this is that it can become very unclear which code belongs where, particularly when block groupings depend solely on indentation as in indicator. Whilst this is not an issue when only a single statement is involved, more than that can become confusing for users and thus has been disallowed entirely.

Another notable design feature is the enforcement of Allman style indentation, which means open braces must begin on a new line and the use of single-line conditional statements is disallowed. This is intended to encourage the use of writing clearer, more modular code, whilst also providing the additional benefit of having all code written in Onyx look the same, allowing for easier reviewing of other users work.

##### Example
```
a = 0

if 1 < 2
    a = 10
else
    a = 20

if a < 20
{
    a = 30
    a *= 5
}
```

#### 4.2.5 Loops
Loops are designed with intuition in mind, and attempts to adopt Pythons style of looping by having the syntax read more as a sentence. The purpose is to have loops be clearer in their functionality just by reading the syntax, unlike in Java where its not easily understandable to the untrained eye how loops are working without a detailed explanation. Many of the other properties of loops are the same as conditionals, such as the enforcement of Allman style indentation and the use of braces in block statements, the reasons for which were explained in the previous section. Though a feat unique to Onyx loops is the inclusion of the upper bound value, which is uncommon in traditional language specifications. When looping it is often the case that the final upper bound value is not executed in the loops body, which is one of the more unintuitive aspects of programming. However, the compiler remedies this by always including the upper value in the execution of the code.

##### Example
```
var = 0

loop i from 1 to 10
{
    var += i
    var *= 2
}
```


## Chapter 6 - Testing
To test that Onyx successfully meets the requirements and specifications originally set, the independent procedures of verification and validation were used as a means of quality assurance.

### 6.1 Validation
"Validation. The assurance that a product, service, or system meets the needs of the customer and other identified stakeholders. It often involves acceptance and suitability with external customers. Contrast with verification." - Project Management Body of Knowledge [7]

The objective of this procedure is to discover whether or not the compiler is successfully able to meet the needs and requirements previously defined, in order to prove itself as a useful product amongst its stakeholders.

### 6.2 Verification
"Verification. The evaluation of whether or not a product, service, or system complies with a regulation, requirement, specification, or imposed condition. It is often an internal process. Contrast with validation." - Project Management Body of Knowledge [7]

The objective of this procedure is to discover whether or not the compiler successfully meets the requirements set in terms of functionality, design and quality, demonstrating that its goals have been met and is capable of performing to a high standard. To help achieve this objective unit tests have been developed for the major components of the compilers pipeline, each one using specialised inputs that help identify errors specific to section being tested. The following explains the testing that took place, and identifies how it ensures requirements have been met.

#### 6.2.1 Lexer Testing
In the original design of the lexer its primary requirement was to be capable of taking a string as input, inspecting each character and categorising it into a token with the corresponding type, and then outputting a list of tokens. Since all of the tests assess by default the input of strings and output of tokens, the main focus becomes the categorising of tokens. This was rather trivial as it simply required the input of single character or word, and then examining the type of the single token found within the returned list, seeing if it matched what was expected. This was repeated for every character that was defined within the language, and the tests passing successfully meant that they were all being identified correctly.

#### 6.2.2 Parser Testing
The parsers function is to check if the source code is syntactically correct by comparing it against the grammatical rules of the language. Given the large amount of possibilities in terms of combinations of tokens, the first step was to break down the tests into different expression categories: literal, unary, binary, identifier, assignment, and parenthesised expressions. Each one would be responsible for implementing tests for its assigned expression type, making it easy to discover which expression failed and the location the issue occurred in the parser. The tests would simply run the input through the parser and examine the type of expression that was returned, and check that it matched against the expected outcome.

In order to expand these test cases they were also used in conjunction with one another, for example all the tests were wrapped in parentheses and then passed as parenthesised expressions, allowing the amount of inputs to expand massively without the need to manually add extra cases. This was rather vital to include, as otherwise it would have meant either a lot of boilerplate code or lack of edge case testing. In any case, the passing of the tests provided clarity in the fact that the parser would correctly verify the syntactic precision of the source code.

#### 6.2.3 Type Checker Testing
Validating the type checker was rather similar to the parser in terms of how it was implemented, as not much changes outside of data types. Again each test was compartmentalised into various categories, with the result being compared against the expected expression type. Though since the role of the type checker is to validate type compatibility between operands and operators, the successful return of the correct expression type meant that type validation was achieved and its function was performed correctly.

#### 6.2.4 Evaluator Testing
The evaluators main job is to evaluate each expression and statement, returning the resulting values. This was particularly simple to test since it only required surveying the final result. Each unit test would take expressions of a particular type, same as previously, and evaluate the results by running them through the evaluator. The values returned were then compared against the expected outcome, and if they matched the tests passed. It was again rather imperative that this stage used a large pool of inputs for testing, as the amount of possibilities is extremely large and would potentially reveal a significant number of edge case issues.

#### 6.2.5 Error Handler Testing
The final stage of unit testing takes place within the error handler, whose primary responsibility is to receive errors and output them to the user. Up until this point the unit tests have focused on correct inputs rather than incorrect ones, so this is where invalid syntax is purposely passed to see if the expected error message is returned. This was broken down into three categories: lexical errors occurring in the lexer, syntax errors occurring in the parser, and semantic errors occurring in the type checker. If the broken syntax resulted in the expected error occurring, it can be verified that the compiler successfully handles errors should they transpire.

## References
1. https://www.guru99.com/compiler-design-phases-of-compiler.html
2. https://www.tutorialspoint.com/compiler_design/compiler_design_phases_of_compiler.htm
3. https://www.kttpro.com/2017/02/09/six-phases-of-the-compilation-process
4. https://www.geeksforgeeks.org/recursive-descent-parser/
5. https://en.wikipedia.org/wiki/Semantic_analysis_(compilers)
6. https://en.wikipedia.org/wiki/Compiler
7. https://ieeexplore.ieee.org/document/5937011

## Glossary of Terms
- Token
- Lexemes
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
- Syntax
- Java
- Machine level
- Pipeline
- Source code/program
- Static typing
- Dynamic typing
- Allman style
- Block statement
- Unit test(ing)