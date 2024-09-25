# EC3204: Programming Languages and Compilers <br> (프로그래밍 언어 및 컴파일러)
* Instuctor: [Sunbeom So](https://gist-pal.github.io)
* Location: Haerim hall, EECS-B building
* Time: Mon/Wed 16:00-17:30

## About The Course
A compiler is a software system that translates a program written in one language (source language) into a semantically equivalent program written in another language (target language).
The goal of this course is to learn principles of compiler construction and related programming language theories.

## Configuring the Programming Environment
**Note1**: This instruction assumes that you are using the Linux command line interface. For Windows users, I recommend using [WSL](https://learn.microsoft.com/en-us/windows/wsl/install). <br>
**Note2**: I checked that the following commands successfully work for the clean docker image ``python:3.9.19-slim``.

We will use [OCaml](https://ocaml.org/install) programming language for our programming exercises.
To install OCaml, simply copy and run the following commands in the terminal.

### Ubuntu (or WSL)
```
$ chmod +x setup/install_ocaml_ubuntu.sh; ./setup/install_ocaml_ubuntu.sh; eval $(opam env)
```

### macOS
```
$ chmod +x setup/install_ocaml_mac.sh; ./setup/install_ocaml_mac.sh; eval $(opam env)
```

### Testing
For any OS,
if the installations were successful,
you should see the message ``The OCaml toplevel, version 5.1.1``
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
|4|9/11|[Lexical Analysis (3)](slides/lec4.pdf)|CPTT Ch.3.7.4, 3.7.1|
|5|9/23|Lexical Analysis (3) | CPTT Ch.3.7.4, 3.7.1|
|-|9/25|[Functional Programming in OCaml](slides/lec-ocaml.pdf)| |

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
