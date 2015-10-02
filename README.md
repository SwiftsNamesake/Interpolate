Interpolate
===========
Python style string formatting brought to Haskell.

    λ>format "I can't believe {0}"
    "I can't believe"


Format language
---------------
From the [Python docs](https://docs.python.org/3.4/library/string.html#formatspec):

    format_spec ::=  [[fill]align][sign][#][0][width][,][.precision][type]
    fill        ::=  <any character>
    align       ::=  "<" | ">" | "=" | "^"
    sign        ::=  "+" | "-" | " "
    width       ::=  integer
    precision   ::=  integer
    type        ::=  "b" | "c" | "d" | "e" | "E" | "f" | "F" | "g" | "G" | "n" | "o" | "s" | "x" | "X" | "%"


Contributors
------------
Jonatan H Sundqvist ([jonatanhsundqvist@gmail.com](mailto:jonatanhsundqvist@gmail.com))


TODO
----
* Introduce ticket system, invite contributors
* Create dev branch
* Finish the library
* Write documentation
