# WIP! Not ready for production
# Siryis

Siryis is a programming language that was originally made for the Truttle 2023 esojam but is now a standalone project.

At first designed to have a weird syntax, it actually ended up being pretty
readable and easy to read.

The language uses a more "natural" syntax which consists of few keywords and
mainly symbols.

## Example: Hello, world!

A simple "Hello, world!" program in Siryis:

```srs
module Main

procedure Lifecycle
    -> Say: ["Hello, world!"]
```

Let's break it down!

`module Main` means that we define a new module called "Main"
in the current project

`procedure Lifecycle` creates a new procedure (a.k.a function)
called "Lifecycle" it is the equivalent of the "main" function
in many languages

`-> Say: ["Hello, world"]` "appends" a new instruction to the procedure    
`->` means a call to a procedure  
`Say` is the name of the procedure to call,
`:` tells to the compiler that this call will contain arguments,  
and `["Hello, world"]` is the list of the elements
we want to print to the output.