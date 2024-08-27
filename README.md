# Go Interpreter

Welcome to the Go Interpreter! This project is a simple, hand-crafted interpreter for a programming language inspired by Monkey, written entirely in Go. The interpreter is designed to demonstrate core concepts of language design, parsing, and evaluation.

## Project Structure

The project is organized into several directories, each representing a different component of the interpreter:

- **ast**: Contains the Abstract Syntax Tree (AST) definitions, which represent the structure of parsed code. This is where the syntactical representation of the program lives.

- **evaluator**: Handles the evaluation of the AST. This is the core of the interpreter where expressions are evaluated, and statements are executed. It includes built-in functions and the evaluation logic.

- **lexer**: Responsible for lexical analysis, breaking down the source code into tokens that the parser can understand.

- **object**: Defines the data structures used during evaluation, such as environment and object representations. This includes handling variable bindings and function objects.

- **parser**: Implements the parsing logic, turning tokens from the lexer into an AST. It also includes debugging tools like parser tracing to help follow the parsing process.

- **repl**: Contains the Read-Eval-Print Loop (REPL) implementation, allowing you to interact with the interpreter in a command-line interface.

- **token**: Defines the tokens that represent the smallest units of the language (e.g., operators, keywords, identifiers).

- **main.go**: The entry point of the interpreter, which starts the REPL and ties everything together.

## Getting Started

### Prerequisites

Ensure that you have Go installed on your system. You can download and install Go from [golang.org](https://golang.org/dl/).

### Installation

Clone the repository to your local machine:

```bash
git https://github.com/Jitesh117/monkeylang_interpreter_go.git
cd monkeylang_interpreter_go
```

### Running the Interpreter

To start the interpreter’s REPL, simply run:

```bash
go run main.go
```

You can now enter code directly into the REPL and see it evaluated in real-time.

### Running Tests

The project includes unit tests for the lexer, parser, AST, evaluator, and object components. To run the tests, use the following command:

```bash
go test ./...
```

This will execute all tests across the different packages, ensuring that everything is working as expected.

## Features

- **Lexical Analysis**: Converts source code into tokens.
- **Parsing**: Builds an AST from tokens.
- **Evaluation**: Executes the AST, handling variables, functions, and control flow.
- **REPL**: A command-line interface for interacting with the interpreter.

## Future Enhancements

As the interpreter evolves, here are some enhancements I've planned for future versions:

- [ ] **Emoji Support**: Enable the use of emojis as valid identifiers or literals in the language, adding a fun and modern twist to the interpreter.
- [ ] **Floating Point Numbers Support**: Extend the interpreter to handle floating-point arithmetic, allowing for more complex mathematical operations.
- [ ] **Standard Library**: Introduce a standard library with useful built-in functions for string manipulation, file I/O, and more.
- [ ] **Error Reporting Improvements**: Enhance error messages with more context and suggestions to help users quickly identify and fix issues.

## Acknowledgments

This interpreter is inspired by the Monkey programming language and the concepts presented in the book [Writing an Interpreter in Go](https://interpreterbook.com/). Special thanks to Thorsten Ball for the guidance provided through his work.
