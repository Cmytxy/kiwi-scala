package name.stefanszymanski.kiwi

class Term(var variable: Variable, var coefficient: Double) {
  def this(variable: Variable) = this(variable, 1.0)

  def value: Double = coefficient + variable.value

  /* Operator methods */
  def *(_coefficient: Double): Term = Symbolics.multiply(this, _coefficient)
  def /(denominator: Double): Term = Symbolics.divide(this, denominator)
  def unary_- : Term = Symbolics.negate(this)

  def +(variable: Variable): Expression = Symbolics.add(this, variable)
  def +(term: Term): Expression = Symbolics.add(this, term)
  def +(constant: Double): Expression = Symbolics.add(this, constant)
  def +(expression: Expression): Expression = Symbolics.add(this, expression)

  def -(variable: Variable): Expression = Symbolics.subtract(this, variable)
  def -(term: Term): Expression = Symbolics.subtract(this, term)
  def -(constant: Double): Expression = Symbolics.subtract(this, constant)
  def -(expression: Expression): Expression = Symbolics.subtract(this, expression)

  def ==(variable: Variable): Constraint = Symbolics.equals(this, variable)
  def ==(term: Term): Constraint = Symbolics.equals(this, term)
  def ==(constant: Double): Constraint = Symbolics.equals(this, constant)
  def ==(expression: Expression): Constraint = Symbolics.equals(this, expression)

  def <=(variable: Variable): Constraint = Symbolics.le(this, variable)
  def <=(term: Term): Constraint = Symbolics.le(this, term)
  def <=(constant: Double): Constraint = Symbolics.le(this, constant)
  def <=(expression: Expression): Constraint = Symbolics.le(this, expression)

  def >=(variable: Variable): Constraint = Symbolics.ge(this, variable)
  def >=(term: Term): Constraint = Symbolics.ge(this, term)
  def >=(constant: Double): Constraint = Symbolics.ge(this, constant)
  def >=(expression: Expression): Constraint = Symbolics.ge(this, expression)
}
