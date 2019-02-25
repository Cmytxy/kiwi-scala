package name.stefanszymanski.kiwi

class Expression(var terms: List[Term], var constant: Double) {
  /* Constructors */
  def this(constant: Double) = this(List.empty[Term], constant)
  def this() = this(0.0)
  def this(term: Term, constant: Double) = this(List[Term](term), constant)
  def this(term: Term) = this(term, 0.0)
  def this(terms: List[Term]) = this(terms, 0.0)

  def value: Double = terms.foldLeft[Double](constant) { (sum: Double, term: Term) => sum + term.value }

  def isConstant: Boolean = terms.isEmpty

  /* Operator methods */
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
