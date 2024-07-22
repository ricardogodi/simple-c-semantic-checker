## Project Overview

### Description

This F# project focuses on parsing and semantically checking "simple C" programs. It identifies valid syntax and reports syntax errors using a lexical analyzer and a recursive-descent parser. The project performs two main semantic checks: variable definition and type usage. It is designed to simplify complex language rules to enhance understanding of programming constructs and parsing techniques.

### Contributions

The F# framework was provided by Prof. Joe Hummel and later modified by Pat Troy, UIC. My specific contribution as a student was to create the parser and semantic analyzer in `parser.fs`, `analyzer.fs`, and `checker.fs` modules.

## Instructions

### Prerequisites

- .NET 8 SDK installed

### Setup

1. Clone the repository:

    ```sh
    git clone https://github.com/ricardogodi/simple-c-semantic-checker.git
    cd simple-c-semantic-checker
    ```

2. Ensure .NET 8 SDK is installed:

    ```sh
    dotnet --version
    ```
    If not installed, download and install from the [official .NET download page](https://dotnet.microsoft.com/download).

3. Build the project:

    ```sh
    make build
    ```

4. Run the project:

    ```sh
    make run
    ```

### Usage

1. When prompted, enter the filename of the "simple C" program file. Sample files like `main1.c`, `main2.c`, and `main3.c` are provided for testing.

## Lexical Analysis

The `lexer.fs` module converts the source code into tokens essential for parsing. This module handles scanning input text, identifying, and categorizing components like keywords, identifiers, and symbols into tokens.

## Syntax Parsing

The `parser.fs` module checks the tokens against the simplified rules of "simple C" syntax. It assesses whether the token arrangement conforms to the grammar, reporting errors or confirming the syntactic correctness of the code.

## Semantic Analysis

### Analyzer Module (`analyzer.fs`)

This module builds a symbol table containing variable declarations and their types. It ensures all variables are uniquely defined and collects them into a list of tuples of the form (name, type).

### Checker Module (`checker.fs`)

This module performs type-checking to ensure all variables and expressions are type-compatible. It validates assignments, if conditions, and expressions, reporting errors or warnings for any semantic violations.

## Compilation and Execution

- **Compile the Application**: Execute `make build` to compile the source files into an executable.
- **Run the Application**: Use `make run` after compiling, then follow the prompts to provide a filename and receive feedback on the syntax analysis.
- **Clean the Build**: Use `make clean` to remove all compiled files for a fresh start.

## Contributions

While the lexer module and the initial project setup were done by Prof. Joe Hummel of the University of Illinois, Chicago, my specific contribution lies in the development of the `parser.fs`, `analyzer.fs`, and `checker.fs` modules, which involved implementing the parsing and semantic analysis logic.
