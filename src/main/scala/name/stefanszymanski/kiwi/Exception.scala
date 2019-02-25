package name.stefanszymanski.kiwi

object Exception {
  class UnknownConstraint(constraint: Constraint) extends Exception
  class UnsatisfiableConstraint(val constraint: Constraint) extends Exception
  class DuplicateConstraint(val constraint: Constraint) extends Exception

  class DuplicateEditVariable(val variable: Variable) extends Exception
  class UnknownEditVariable(val variable: Variable) extends Exception

  class BadRequiredStrength() extends Exception

  class NonlinearExpression() extends Exception
}
