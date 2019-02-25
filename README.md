# kiwi-scala

A Scala port of the [Kiwi Java](https://github.com/alexbirkett/kiwi-java) and 
[Kiwi Haxe](https://github.com/Tw1ddle/haxe-kiwi) implementations of the
Cassowary constraint solving algorithm

## Example usage

```scala
val solver = new Solver
val x = new Variable
val y = new Variable

solver.addConstraint(x == 20)
solver.addConstraint(x + 2 == y + 10)

solver.updateVariables()

println(x.value, y.value)
// (20.0, 12.0)
```

## Links

* [Kiwi C++](https://github.com/nucleic/kiwi) 
* [Kiwi Java](https://github.com/alexbirkett/kiwi-java)
* [Kiwi Haxe](https://github.com/Tw1ddle/haxe-kiwi)
* [overconstrained.io](https://overconstrained.io)