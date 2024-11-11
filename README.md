# EC3204: Programming Languages and Compilers <br> (프로그래밍 언어 및 컴파일러)
* Instuctor: [Sunbeom So](https://gist-pal.github.io)
* TA: [Sunwoo Lee](https://sites.google.com/view/sunwoo-lee/)
* Location: Haerim hall, EECS-B building
* Time: Mon/Wed 16:00-17:30

## About The Course
A compiler is a software system that translates a program written in one language (source language) into a semantically equivalent program written in another language (target language).
The goal of this course is to learn principles of compiler construction and related programming language theories.

## Configuring the Programming Environment
We will use [OCaml](https://ocaml.org/install) programming language for our programming exercises. This instruction assumes that you are using the Linux command line interface.

### macOS
To install OCaml, simply copy and run the following commands in the terminal.
```
$ chmod +x setup/install_ocaml_mac.sh; ./setup/install_ocaml_mac.sh; eval $(opam env)
```

### Ubuntu
To install OCaml, simply copy and run the following commands in the terminal. I checked that the following commands successfully work for the clean docker image ``python:3.9.19-slim``.
```
$ chmod +x setup/install_ocaml_ubuntu.sh; ./setup/install_ocaml_ubuntu.sh; eval $(opam env)
```

### Windows
For Windows users, I recommend using [WSL](https://learn.microsoft.com/en-us/windows/wsl/install).
  * [WSL setup](setup/wsl-install-guideline.pdf): We provide a PDF guideline that explains how to install WSL and how to share files between the host OS and the guest OS.
  * [script](setup/wsl_install.ps1): For running this installation script, please refer to p.02 of the above PDF guideline.
    
After setting up WSL, simply copy and run the following commands in the terminal to install OCaml.
```
$ chmod +x setup/install_ocaml_ubuntu.sh; ./setup/install_ocaml_ubuntu.sh; eval $(opam env)
```

### Installation Verification
For any OS, if the installation was successful, you should see the message ``The OCaml toplevel, version 5.1.1``
by running the command ``ocaml --version`` in the terminal.
```
$ ocaml --version
The OCaml toplevel, version 5.1.1
```

## Textbook
* [Compilers: Principles, Techniques, and Tools](https://www.amazon.com/Compilers-Principles-Techniques-Tools-2nd/dp/0321486811) (CPTT)

## Schedule
|#|Date|Topics|Recommended Reading|
|-|-|------|------|
|0|9/2|[Course Overview](slides/lec0.pdf)|CPTT Ch.1|
|1|9/2|[Overview of Compilers](slides/lec1.pdf)||
|2|9/4|[Lexical Analysis (1)](slides/lec2.pdf)|CPTT Ch.3.3|
|3|9/9|[Lexical Analysis (2)](slides/lec3.pdf)|CPTT Ch.3.4, 3.6, 3.7.5|
|4|9/11,9/23|[Lexical Analysis (3)](slides/lec4.pdf)|CPTT Ch.3.7.4, 3.7.1|
|-|9/25,9/30,10/2|[Functional Programming in OCaml](slides/lec-ocaml.pdf)|[Exercise solutions](ocaml-examples/)|
|5|10/7|[Syntax Analysis (1)](slides/lec5.pdf)|CPTT Ch.4.2|
|6|10/14,10/16|[Syntax Analysis (2)](slides/lec6.pdf)|CPTT Ch.2.2, 4.3, 4.4|
|7|10/21,10/23|[Syntax Analysis (3)](slides/lec7.pdf)|CPTT Ch.4.5, 4.6|
|-|10/28|Mid-term Exam||
|8|11/4|[Syntax Analysis (4)](slides/lec8.pdf)|CPTT Ch.4.8|
|9|11/6|[Lexer & Parser Generators](slides/lec9.pdf)|CPTT Ch.4.9, [Tutorial for ocamllex, ocamlyacc](https://ocaml.org/manual/5.2/lexyacc.html)|
|10|11/11|[Semantic Analysis (1)](slides/lec10.pdf)||

## Academic Integrity
By registering for this course, I will assume you agree with the policy below.
* All assignments (i.e., writing code) must be your own work. No discussions are allowed.
  * You should not share/show your code.
  * You should not post your code on public websites.
  * You should not modify other students’ code.
* I will have a one-on-one meeting with each student under suspicion. If you fail to prove your integrity in the meeting (e.g., failing to answer my questions about the details of your submissions), I will consider that you cheated on your assignments, even if you do not admit to your cheating.
* Cheating on assignments will result in an F.

## References & Acknowledgements
My lecture slides are based on the slides from the following courses.

* [COSE312: Compilers] course taught by Prof. Hakjoo Oh at Korea University
* [[CS143: Compilers](https://web.stanford.edu/class/cs143/)] from Stanford University
