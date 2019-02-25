package name.stefanszymanski.kiwi

class Variable(var value: Double) {
  def *(coefficient: Double): Term = Symbolics.multiply(this, coefficient)
  def /(denominator: Double): Term = Symbolics.divide(this, denominator)
  def unary_- : Term = Symbolics.negate(this)

  def +(expression: Expression): Expression = Symbolics.add(this, expression)
  def +(term: Term): Expression = Symbolics.add(this, term)
  def +(variable: Variable): Expression = Symbolics.add(this, variable)
  def +(constant: Double): Expression = Symbolics.add(this, constant)

  def -(expression: Expression): Expression = Symbolics.subtract(this, expression)
  def -(term: Term): Expression = Symbolics.subtract(this, term)
  def -(variable: Variable): Expression = Symbolics.subtract(this, variable)
  def -(constant: Double): Expression = Symbolics.subtract(this, constant)

  def ==(expression: Expression): Constraint = Symbolics.equals(this, expression)
  def ==(term: Term): Constraint = Symbolics.equals(this, term)
  def ==(variable: Variable): Constraint = Symbolics.equals(this, variable)
  def ==(constant: Double): Constraint = Symbolics.equals(this, constant)

  def <=(expression: Expression): Constraint = Symbolics.le(this, expression)
  def <=(term: Term): Constraint = Symbolics.le(this, term)
  def <=(variable: Variable): Constraint = Symbolics.le(this, variable)
  def <=(constant: Double): Constraint = Symbolics.le(this, constant)

  def >=(expression: Expression): Constraint = Symbolics.ge(this, expression)
  def >=(term: Term): Constraint = Symbolics.ge(this, term)
  def >=(variable: Variable): Constraint = Symbolics.ge(this, variable)
  def >=(constant: Double): Constraint = Symbolics.ge(this, constant)
}
