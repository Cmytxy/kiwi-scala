# kiwi-scala

[![Build Status](https://travis-ci.org/stefanszymanski/kiwi-scala.svg?branch=master)](https://travis-ci.org/stefanszymanski/kiwi-scala)

A Scala port of the [Kiwi Java](https://github.com/alexbirkett/kiwi-java) and 
[Kiwi Haxe](https://github.com/Tw1ddle/haxe-kiwi) implementations of the
Cassowary constraint solving algorithm

## Example usage

```scala
val solver = new Solver
val x = new Variable
val y = new Variable

solver += x == 20
solver += x + 2 == y + 10
!solver

println(x.value, y.value)
// (20.0, 12.0)
```

Those three lines with `solver` use the short form of the following.

```scala
solver.addConstraint(Symbolics.equals(x, 20))
solver.addConstraint(Symbolics.equals(Symbolics.add(x, 2), Symbolics.add(y, 10)))
solver.updateVariables()
```

## Links

* [Kiwi C++](https://github.com/nucleic/kiwi) 
* [Kiwi Java](https://github.com/alexbirkett/kiwi-java)
* [Kiwi Haxe](https://github.com/Tw1ddle/haxe-kiwi)
* [overconstrained.io](https://overconstrained.io)