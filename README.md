# COP4020-Alternative-Language-Project-in-R
## Notes on Running this Code
### Per a conversation with Krishna, this code has been written as follows and in two ways: 
- (i) this code can run as-is in a terminal, whereby all uncommented functions are non-interactive and the code runs with hard-coded additions/changes/deletions and
- (ii) this code can be run interactively, requiring a local R environment and Rscript, whereby running Rscript.exe and the file is done as discussed in the section below.

---

### __To run this code non-interactively, with hard-coded inputs, and to use the unit-tests:__
- Simply run it as-is, that's all!

---

### __To run this code interactively:__
- First, comment out everything in the non-interactive section.
- Second, uncomment the menu and the unit tests.
- Third, uncomment the code at the top of the program, which installs the necessary unit test library (if not present--it checks for this).
- Finally, run the code!

---

### **NOTE FOR VS CODE**: 

Use **Run Source**, and **DO NOT** use Run Code. Run Source runs the code interactively. Run Source is non-interactive.
Running the code in "interactive mode" in a non-interactive shell can cause issues.

![image](https://github.com/WJGardnerJr/COP4020-Alternative-Language-Project-in-R/assets/135628958/f69390d3-f0db-4626-9514-b10ca10aed8b)

## Report

I chose to write this program in R and used version 4.3.3. At the time of this writing, this is the most recent
version to date. R was chosen specifically for the nature of this project becuase this project is inherently about the
manipulation and transformation of data. R is a data-science-oriented language, or so I had heard, and I felt that it would
thus be appropriate for use here. Like Python, it is interpreted, and this made for excellent flexibility becuase I could easily
change and manipulate data on the fly. I found no issues in using this langauge for data manipulation, but I do think it is 
worthwhile to note that this is not a great langauge for object-oriented programming (hereinafter "OOP").

OOP is supported in R. OOP is not well-implemented in R for the basis of this project because R has various versions of objects from
which you can choose to work. I chose to work with the most common version because of simplicity; this means that all objects in the 
above code are "S4" objects, and all methods for said objects are thus S4-object-derived methods. There are limitations on S4 objects vs,
say, S3 objects because S3 objects use syntax more like C++, but are less mutable and not quite as easily fungible in R. S4 objects are
likewise more robust, but their related code syntax is more of scripting-language like syntax, in that code is not expressed in a way that
deals with the objects directly, but rather with their parameters or specific instances at the time. For file ingestion, R is great. R
can natively read and write CSV files, and is meant for data set manipulation.

Conditional statements are mostly standard in R, as you can see by the plethora of logically-controlled loops and expressions my code contains.
Likewise, assignment statements in R are very strange. The R assignment operator "<-" is not unlike C++'s assignment operator "=", but its direction
and implementation in typed form are a bit odd. It is, however, functionally just a standard assignment operator. R does have two other assignment operators,
but they are not used here. They are "=", which is only allowed at top-levels, so it is not as common. It is used here, for things like oem = { ... }.
There are also "<<-" assignments, and those are only used in functions, and search through parent environemnts for a definition of a variable to be assigned and, if found, they redefine it. There are also rightwards versions of "<-" and "<<-", which work as expected in the opposing direction. Loops are handled well in R, and
R even has a nice infinite loop structure called "Repeat" that allows for repetition until broken. This is advantageous when things like iteration
over a code block multiple times is needed. For and while loops exist as standard. 

Subprograms in R are strange. They are not quite as normal, defined as a sort of assignment of a block to a sub-program. They work as normal, but their syntax is a bit odd. The semantic implementation is not abnormal, though, and things work normally when calling or passing parameters to them, e.g., calling a function like foo(bar). Unit testing is done through testthat, a library that externally installs in the environment. It is quite simple, and allows for testing functions via test_that ("Test name", { ...}). This is the main means of R unit testing. Exception handling is also odd, but simple. It is done via tryCatch, which tries and catches any errors and prints them via error = function(e) { ... }, where the error-handling code is in the catch portion of the loop after the function(e), which is just a function to which the error is passed. Exceptions thus are thrown here, not raised.

The only library used here is testthat, which is installed for unit testing only. It was chosen for its simplicity and its utility; testthat provides built in ways to check for boolean true or false outcomes from running a code block. **NOTE THAT THIS DOES NOT WORK IN REPLIT! SEE ABOVE!**

The company that has the highest weight of the phone body is Motorola, as seen in the figure below:
![image](https://github.com/WJGardnerJr/COP4020-Alternative-Language-Project-in-R/assets/135628958/1e876632-c8f4-45bc-8d44-c394f7c72ad2)

The following phones were found to have been announced in one year and released in another:
![image](https://github.com/WJGardnerJr/COP4020-Alternative-Language-Project-in-R/assets/135628958/03c978b7-c379-4744-8d70-728980af3ecf)

The following is the count of the number of phones with only one feature or sensor:
![image](https://github.com/WJGardnerJr/COP4020-Alternative-Language-Project-in-R/assets/135628958/169a65c2-47b4-44ba-94ad-35ed35a65c9d)

The following is an account of the year (post-1999) where the most number of phones was launched, and also a count of how many were launched that year:

![image](https://github.com/WJGardnerJr/COP4020-Alternative-Language-Project-in-R/assets/135628958/d021103c-fce8-4535-9fce-30ccdd30923f)

Big-O runtime analysis is included in the comments. Feel free to use Ctrl-F to find "Runtime." It is present for all major functions. For all intents and purposes, the non-interactive versions possess the same runtime.

