~~The equality operators are `=` and `<>` and not `==` and `!=`.~~

**The double semicolon is stricly for toplevel sessions, no need to write it in a .ml file.**

## Build/Compilation instructions using the ocaml build system 

If you want to compile your code you can use the following command: 

**ocamlc -o hello.byte hello.ml**
The `-o hello.byte` option says to name the output executable hello.byte and contains compiled OCaml bytecode. Two other files are also produced, `hello.cmi` and `hello.cmo`. In order to run the produced executable you simply do `./hello.byte`. 

Instead of running the compiler directly, you'd want to use the Ocaml build system by running `ocamlbuild hello.byte.` We're asking the build system to build a file called `hello.byte` and it will automatically figure out that `hello.ml` is the source code. 

You can also clean up the compiled code by running `ocamlbuild -clean`.

Not sure how dune comes into all of this, but will find out!

## Loading code in the toplevel 

You can use _directives_ that are not OCaml code to tell the toplevel to do something. **#use** allows you to load code from a file into the toplevel.