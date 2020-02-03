# onyx-compiler
Custom compiler written in Java.

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
v. Other methods should be ordered based on functionality order and/or access modifiers.


## Blog Notes
- Using [Mavens Standard Directory Layout](http://maven.apache.org/guides/introduction/introduction-to-the-standard-directory-layout.html) for project structure.  
