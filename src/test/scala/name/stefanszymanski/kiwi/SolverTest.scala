package name.stefanszymanski.kiwi

import org.scalatest.{BeforeAndAfter, FunSuite}

class SolverTest extends FunSuite with BeforeAndAfter {

  var solver: Solver = _
  var epsilon = 1.0e-8

  before {
    solver = new Solver
  }

  test("Solve equality constraint with one variable") {
    val a = new Variable
    solver += a == 5
    !solver
    assert(a.value == 5)
  }

  test("Solve equality constraints with two variables") {
    val a = new Variable
    val b = new Variable
    solver += a == 20
    solver += a + 2 == b + 10
    !solver
    assert(a.value == 20)
    assert(b.value == 12)
  }

  test("Solve constraints with weak strength") {
    val a = new Variable
    val b = new Variable
    solver += a <= b
    solver += b == a + 3.0
    solver += a == 10.0 ^ Strength.weak
    solver += b == 10.0 ^ Strength.weak
    !solver
    if ((a.value - 10.0).abs < epsilon) {
      assert(a.value == 10.0)
      assert(b.value == 13.0)
    } else {
      assert(a.value == 7)
      assert(b.value == 10)
    }
  }

  test("Add and remove constraints") {
    val a = new Variable
    solver += a <= 100 ^ Strength.weak
    !solver
    assert(a.value == 100)

    val c10 = a <= 10
    val c20 = a <= 20

    solver += c10
    solver += c20
    !solver
    assert(a.value == 10)

    solver -= c10
    !solver
    assert(a.value == 20)

    solver -= c20
    !solver
    assert(a.value == 100)

    val c10again = a <= 10
    solver += c10again
    solver += c10
    !solver
    assert(a.value == 10)

    solver -= c10
    !solver
    assert(a.value == 10)

    solver -= c10again
    !solver
    assert(a.value == 100)
  }

  test("Add and remove constraints 2") {
    val a = new Variable
    val b = new Variable

    solver += a == 100 ^ Strength.weak
    solver += b == 120 ^ Strength.strong

    val c10 = a <= 10
    val c20 = a <= 20

    solver += c10
    solver += c20
    !solver
    assert(a.value == 10)
    assert(b.value == 120)

    solver -= c10
    !solver
    assert(a.value == 20)
    assert(b.value == 120)

    val cxy = a * 2 == b
    solver += cxy
    !solver
    assert(a.value == 20)
    assert(b.value == 40)

    solver -= c20
    !solver
    assert(a.value == 60)
    assert(b.value == 120)

    solver -= cxy
    !solver
    assert(a.value == 100)
    assert(b.value == 120)
  }

  test("Fail when adding an inconsistent constraint") {
    val a = new Variable
    solver += a == 10
    intercept[Exception.UnsatisfiableConstraint] {
      solver += a == 5
    }
  }

  test("Fail when adding an inconstistent constraint 2") {
    val a = new Variable
    solver += a >= 10
    intercept[Exception.UnsatisfiableConstraint] {
      solver += a <= 5
    }
  }

  test("Fail when adding inconsistent constraints") {
    val w = new Variable
    val x = new Variable
    val y = new Variable
    val z = new Variable
    solver += w >= 10
    solver += x >= w
    solver += y >= x
    solver += z >= y
    solver += z >= 8
    intercept[Exception.UnsatisfiableConstraint] {
      solver += z <= 4
    }
  }
}
