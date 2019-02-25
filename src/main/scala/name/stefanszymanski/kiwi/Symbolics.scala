package name.stefanszymanski.kiwi


object Symbolics {
  def multiply(variable: Variable, coefficient: Double): Term = new Term(variable, coefficient)

  def divide(variable: Variable, denominator: Double): Term = multiply(variable, (1.0 / denominator))

  def negate(variable: Variable): Term = multiply(variable, -1.0)

  def multiply(term: Term, coefficient: Double): Term = new Term(term.variable, term.coefficient * coefficient)

  def divide(term: Term, denominator: Double): Term = multiply(term, (1.0 / denominator))

  def negate(term: Term): Term = multiply(term, -1.0)

  def multiply(expression: Expression, coefficient: Double): Expression = {
    val terms = List[Term]((for (term <- expression.terms) yield multiply(term, coefficient)): _*)
    new Expression(terms, expression.constant * coefficient)
  }

  def multiply(expression1: Expression, expression2: Expression): Expression = {
    if (expression1.isConstant) {
      multiply(expression1.constant, expression2)
    } else if (expression2.isConstant) {
      multiply(expression2.constant, expression1)
    } else {
      throw new Exception.NonlinearExpression()
    }
  }

  def divide(expression: Expression, denominator: Double): Expression = multiply(expression, (1.0 / denominator))

  def divide(expression1: Expression, expression2: Expression): Expression = {
    if (expression2.isConstant) {
      divide(expression1, expression2.constant)
    } else {
      throw new Exception.NonlinearExpression()
    }
  }

  def negate(expression: Expression): Expression = multiply(expression, -1.0)

  def multiply(coefficient: Double, expression: Expression): Expression = multiply(expression, coefficient)

  def multiply(coefficient: Double, term: Term): Term = multiply(term, coefficient)

  def multiply(coefficient: Double, variable: Variable): Term = multiply(variable, coefficient)

  def add(first: Expression, second: Expression): Expression = {
    val terms = first.terms ++ second.terms
    new Expression(terms, first.constant + second.constant)
  }

  def add(first: Expression, second: Term): Expression = {
    val terms = first.terms ++ List(second)
    new Expression(terms, first.constant)
  }

  def add(expression: Expression, variable: Variable): Expression = add(expression, new Term(variable))

  def add(expression: Expression, constant: Double): Expression = new Expression(expression.terms, expression.constant + constant)

  def subtract(first: Expression, second: Expression): Expression = add(first, negate(second))

  def subtract(expression: Expression, term: Term): Expression = add(expression, negate(term))

  def subtract(expression: Expression, variable: Variable): Expression = add(expression, negate(variable))

  def subtract(expression: Expression, constant: Double): Expression = add(expression, -constant)

  def add(term: Term, expression: Expression): Expression = add(expression, term)

  def add(first: Term, second: Term): Expression = new Expression(List(first, second))

  def add(term: Term, variable: Variable): Expression = add(term, new Term(variable))

  def add(term: Term, constant: Double): Expression = new Expression(term, constant)

  def subtract(term: Term, expression: Expression): Expression = add(negate(expression), term)

  def subtract(first: Term, second: Term): Expression = add(first, negate(second))

  def subtract(term: Term, variable: Variable): Expression = add(term, negate(variable))

  def subtract(term: Term, constant: Double): Expression = add(term, -constant)

  def add(variable: Variable, expression: Expression): Expression = add(expression, variable)

  def add(variable: Variable, term: Term): Expression = add(term, variable)

  def add(first: Variable, second: Variable): Expression = add(new Term(first), second)

  def add(variable: Variable, constant: Double): Expression = add(new Term(variable), constant)

  def subtract(variable: Variable, expression: Expression): Expression = add(variable, negate(expression))

  def subtract(variable: Variable, term: Term): Expression = add(variable, negate(term))

  def subtract(first: Variable, second: Variable): Expression = add(first, negate(second))

  def subtract(variable: Variable, constant: Double): Expression = add(variable, -constant)

  def add(constant: Double, expression: Expression): Expression = add(expression, constant)

  def add(constant: Double, term: Term): Expression = add(term, constant)

  def add(constant: Double, variable: Variable): Expression = add(variable, constant)

  def subtract(constant: Double, expression: Expression): Expression = add(negate(expression), constant)

  def subtract(constant: Double, term: Term): Expression = add(negate(term), constant)

  def subtract(constant: Double, variable: Variable): Expression = add(negate(variable), constant)

  def equals(first: Expression, second: Expression): Constraint = new Constraint(subtract(first, second), RelationalOperator.eq)

  def equals(expression: Expression, term: Term): Constraint = equals(expression, new Expression(term))

  def equals(expression: Expression, variable: Variable): Constraint = equals(expression, new Term(variable))

  def equals(expression: Expression, constant: Double): Constraint = equals(expression, new Expression(constant))

  def le(first: Expression, second: Expression): Constraint = new Constraint(subtract(first, second), RelationalOperator.le)

  def le(expression: Expression, term: Term): Constraint = le(expression, new Expression(term))

  def le(expression: Expression, variable: Variable): Constraint = le(expression, new Term(variable))

  def le(expression: Expression, constant: Double): Constraint = le(expression, new Expression(constant))

  def ge(first: Expression, second: Expression): Constraint = new Constraint(subtract(first, second), RelationalOperator.ge)

  def ge(expression: Expression, term: Term): Constraint = ge(expression, new Expression(term))

  def ge(expression: Expression, variable: Variable): Constraint = ge(expression, new Term(variable))

  def ge(expression: Expression, constant: Double): Constraint = ge(expression, new Expression(constant))

  def equals(term: Term, expression: Expression): Constraint = equals(expression, term)

  def equals(first: Term, second: Term): Constraint = equals(new Expression(first), second)

  def equals(term: Term, variable: Variable): Constraint = equals(new Expression(term), variable)

  def equals(term: Term, constant: Double): Constraint = equals(new Expression(term), constant)

  def le(term: Term, expression: Expression): Constraint = le(new Expression(term), expression)

  def le(first: Term, second: Term): Constraint = le(new Expression(first), second)

  def le(term: Term, variable: Variable): Constraint = le(new Expression(term), variable)

  def le(term: Term, constant: Double): Constraint = le(new Expression(term), constant)

  def ge(term: Term, expression: Expression): Constraint = ge(new Expression(term), expression)

  def ge(first: Term, second: Term): Constraint = ge(new Expression(first), second)

  def ge(term: Term, variable: Variable): Constraint = ge(new Expression(term), variable)

  def ge(term: Term, constant: Double): Constraint = ge(new Expression(term), constant)

  // Variable relations
  def equals(variable: Variable, expression: Expression): Constraint = equals(expression, variable)

  def equals(variable: Variable, term: Term): Constraint = equals(term, variable)

  def equals(first: Variable, second: Variable): Constraint = equals(new Term(first), second)

  def equals(variable: Variable, constant: Double): Constraint = equals(new Term(variable), constant)

  def le(variable: Variable, expression: Expression): Constraint = le(new Term(variable), expression)

  def le(variable: Variable, term: Term): Constraint = le(new Term(variable), term)

  def le(first: Variable, second: Variable): Constraint = le(new Term(first), second)

  def le(variable: Variable, constant: Double): Constraint = le(new Term(variable), constant)

  def ge(variable: Variable, expression: Expression): Constraint = ge(new Term(variable), expression)

  def ge(variable: Variable, term: Term): Constraint = ge(term, variable)

  def ge(first: Variable, second: Variable): Constraint = ge(new Term(first), second)

  def ge(variable: Variable, constant: Double): Constraint = ge(new Term(variable), constant)

  def equals(constant: Double, expression: Expression): Constraint = equals(expression, constant)

  def equals(constant: Double, term: Term): Constraint = equals(term, constant)

  def equals(constant: Double, variable: Variable): Constraint = equals(variable, constant)

  def le(constant: Double, expression: Expression): Constraint = le(new Expression(constant), expression)

  def le(constant: Double, term: Term): Constraint = le(constant, new Expression(term))

  def le(constant: Double, variable: Variable): Constraint = le(constant, new Term(variable))

  def ge(constant: Double, term: Term): Constraint = ge(new Expression(constant), term)

  def ge(constant: Double, variable: Variable): Constraint = ge(constant, new Term(variable))

  def modifyStrength(constraint: Constraint, strength: Double): Constraint = new Constraint(constraint, strength)

  def modifyStrength(strength: Double, constraint: Constraint): Constraint = modifyStrength(strength, constraint)
}