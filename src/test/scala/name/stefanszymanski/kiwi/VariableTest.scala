package name.stefanszymanski.kiwi

import org.scalatest.FunSuite

class VariableTest extends FunSuite {
  test("Construct with argument") {
    val variable = new Variable(4.2)
    assert(variable.value == 4.2)
  }

  test("Construct without argument") {
    val variable = new Variable
    assert(variable.value == 0.0)
  }

  test("Value is mutable") {
    val variable = new Variable(4.2)
    variable.value = 42.0
    assert(variable.value == 42.0)
  }
}
