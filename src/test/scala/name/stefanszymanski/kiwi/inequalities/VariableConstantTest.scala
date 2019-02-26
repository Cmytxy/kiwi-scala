package name.stefanszymanski.kiwi.inequalities

import name.stefanszymanski.kiwi.Solver
import name.stefanszymanski.kiwi.Variable
import name.stefanszymanski.kiwi.Exception
import org.scalatest.{BeforeAndAfter, FunSuite}

class VariableConstantTest extends FunSuite with BeforeAndAfter {
  var solver: Solver = _
  var epsilon = 1.0e-8

  before {
    solver = new Solver
  }

  test("Variable is less than or equal to constant") {
    val x = new Variable
    solver += x <= 100
    !solver
    assert(x.value <= 100)
    solver += x == 90
    !solver
    assert(x.value == 90)
  }

  test("Variable is less than or equal to constant, unsatisfiable") {
    val x = new Variable
    solver += x <= 100
    !solver
    assert(x.value <= 100)
    intercept[Exception.UnsatisfiableConstraint] {
      solver += x == 110
    }
  }

  test("Variable is greater than or equal to constant") {
    val x = new Variable
    solver += x >= 100
    !solver
    assert(x.value >= 100)
    solver += x == 110
    !solver
    assert(x.value == 110)
  }

  test("Variable is greater than or equals to constant, unsatisfiable") {
    val x = new Variable
    solver += x >= 100
    !solver
    assert(x.value >= 100)
    intercept[Exception.UnsatisfiableConstraint] {
      solver += x == 90
    }
  }
}
