Introduction:
------------
About Haskell's QuickCheck Library: http://www.haskell.org/haskellwiki/Introduction_to_QuickCheck

Installation and Usage: 
--------------------
You will need lasted OCaml release installed: http://caml.inria.fr/ocaml/release.en.html

Follow these steps to "check" a program (in the example below, from the file "property.ml"):

    $ ocamlc -w a -o result BoundedCheck.ml property.ml
    $ ./result
