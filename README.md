# Simple Dependent Types

**REPL sucks and crashes if the input is not well formed or type checked**

Based off of [How to implement dependent types in 80 lines of code](https://gist.github.com/Hirrolot/27e6b02a051df333811a23b97c375196) and [Barebones lambda cube in OCaml](https://gist.github.com/Hirrolot/89c60f821270059a09c14b940b454fd6) which are both written in OCaml.


Syntax:
```
lambdas:
λ var:type.body
^ var:type.body

pi:
Π var:type.body

star (type of normal types):
*
box (type or kind of star *):
☐

application (applys y to identity lambda):
(λx:*.x y)
```

Wanted to understand dependent types more because they are mind bending. Stumbled across this [How to implement dependent types in 80 lines of code](https://gist.github.com/Hirrolot/27e6b02a051df333811a23b97c375196) article implementing them in OCaml. Once you go all the way to CoC in the lambda cube the implementation only becomes slightly more complicated than a regular untyped lambda calculus. Which is pretty neat.
