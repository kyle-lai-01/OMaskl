# OMaskl
An interpreter, translator, and evaluator written for OMaskl. Built as a final project for CS 3110.


### Built With
* OCaml


## Table of Contents

* [About OMaskl](#about-omaskl)
  * [Built With](#built-with)
* [Getting Started](#getting-started)
  * [Installation](#installation)
  * [Usage](#usage)
* [Contact](#contact)
* [Acknowledgements](#acknowledgements)


## About OMaskl
OMaskl is a functional and object oriented language created for this project. <br/>
The syntax for writing OMaskl code is found in Language Ideas.pdf <br/>
With just the syntax for OMaskl, you can easily evaluate the code and translate it to OCaml!
<br />


## Getting Started
You must have a working Unix environment with OCaml installed, and the OPAM package manager. 

### Installation 
1. Clone the repo
```sh
git clone https://github.com/kyle-lai-01/OMaskl
```

2. Start the project in the terimal with 
```sh
cd OMaskl
make run
```

3. Write the OMaskl code in test.txt using the syntax outlined in the pdf and save test.txt

### Usage

When the program asks for commands, type in one of the following commands.

1. 
```sh
translate
```
When the program prompts you for a file name, enter "test.txt"
and the program will tell you the name of the .ml file produced containing 
the OCaml translation. 

2. 
```sh 
translate interp
``` 
When the program prompts you for a file name, enter
any .txt file you have created containing OMaskl code. The program will 
print the OCaml translation and prompt you for another file name. 
Enter "quit" to leave the translate interpreter. 

3. 
```sh 
evaluate
```
When the program prompts you for a file name, enter any 
.txt file you have created containing OMaskl code. The program will evaluate
the code you have entered. It will tell you if the code was run successfully. 
You will only see something printed if you call the print or print_endl 
functions. 

4. 
```sh 
evaluate interp
```
This starts the OMaskl interpreter. Enter any expression
and it will evaluate that expression and produce the result. 
You must end the expression with ";_;" (without the quotes). There cannot
be any characters after ";_;" or the program will not recognize 
it. You can use any number of lines to enter an expression as long as you
add ";_;" to the end. To quit type "#quit;_;" (without the quotes).

5. 
```sh
commands
```
Prints out a list of all commands 

6. 
```sh
quit
```
Terminates the program. 

## Contact
This project was created by Tomer Shamir, Kyle Lai, Andrey Yao. <br />

Kyle Lai - [@kyle-lai-01](https://github.com/kyle-lai-01) - kl655@cornell.edu <br />
Tomer Shamir - tys7@cornell.edu <br />
Andrey Yao - awy32@cornell.edu


Original Repository Link: [https://github.coecis.cornell.edu/awy32/3110_Project](https://github.coecis.cornell.edu/awy32/3110_Project)

## Acknowledgements
* [CS3110](https://www.cs.cornell.edu/courses/cs3110/2020sp/)
* [Visual Studio Live Share](https://marketplace.visualstudio.com/items?itemName=MS-vsliveshare.vsliveshare)
* [ocamllex, ocamlyacc](https://ocaml.org/releases/4.07/htmlman/lexyacc.html)
