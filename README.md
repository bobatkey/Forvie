# Forvie

Forvie is an integrated collection of utilities for implementing small
programming languages, itself implemented in Haskell. The driving
principle behind Forvie is the idea that specifications of programming
languages should be first-class entities within the meta-language,
Haskell. Given this, runtime manipulation of language specifications
is possible, and multiple backends can be generated. For example,
compiler frontends and syntax highlighting code for editors can be
generated from the same specification.

## Current Features

At the moment, Forvie has the following features:

*   A Haskell type of specifications of the lexical structure of a
    programming language. Lexical structure is specified by means of a
    collection of regular expressions with attached tokens. The
    regular expressions language is Unicode-aware and includes
    non-standard features such as complementation (“all strings that
    *do not* match this regular expression“) and conjunction (“all
    strings that match both regular expressions”).

*   Lexer generators, which take a lexical specification and produce
    code that splits text into lexemes according to the
    specification. All the lexer generators currently work by
    translating the specification to a DFA.

    The lexer generators that currently exist are:

    +   A dynamic lexer generator that takes a lexical specification
        during the runtime of a Haskell program and returns a function
        that performs lexing.

    + A static lexer generator that uses Template Haskell to generate
        Haskell code at compile time that does the same job as the
        dynamic lexer, but can benefit from compiler optimisations.

    +   An Emacs Lisp generator that outputs Elisp code. This is used to
        generate Emacs modes with syntax highlighting based on the lexical
        structure of a programming language.

    +   An HTML syntax highlighter that produces HTML syntax-highlighted
        code, based on the lexical structure.

*   A post-processor for Haskell-style whitespace-significant
    layout. This allows automatic insertion of braces (‘{’ and ‘}’) and
    semicolons (’;’) based on the indentation structure of the
    input. The Forvie implementation does not implement all of the
    Haskell-style layout rules, since this requires tight bi-directional
    communication between the parser and the layout processor.

## Future Plans

Future plans for Forvie revolve around two poles:

*   Refinement of the current features. This includes:
    + Addition of anchors to the regular expression language to match line endings and beginings.
    + More support for generation of syntax-highlighting code
*   Extending to cover more features of language specification
    + In particular, the context-free structure of programming languages.
    + The name-binding and scoping rules of programming languages.

Also, optimisation opportunities arising from the declaration
specification of language features are also being explored.