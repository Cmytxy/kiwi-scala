package name.stefanszymanski.kiwi

import org.scalatest.FunSuite

class StrengthTest extends FunSuite {
  test("Required is stronger than Strong") {
    assert(Strength.required > Strength.strong)
  }

  test("Strong is stronger than Medium") {
    assert(Strength.strong > Strength.medium)
  }

  test("Medium is stronger than Weak") {
    assert(Strength.medium > Strength.weak)
  }
}
