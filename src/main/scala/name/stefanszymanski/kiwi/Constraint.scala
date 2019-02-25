package name.stefanszymanski.kiwi

import scala.collection.mutable

class Constraint(var expression: Expression, var operator: RelationalOperator.type, var strength: Double) {
  /* Constructors */
  def this(expression: Expression, operator: RelationalOperator.type) = this(expression, operator, Strength.required)
  def this(other: Constraint, strength: Double) = this(other.expression, other.operator, strength)

  protected def reduce(_expression: Expression): Expression = {
    val variables = mutable.LinkedHashMap.empty[Variable, Double]
    for (term <- _expression.terms) {
      val value = variables.get(term.variable) match {
        case Some(c) => c + term.coefficient
        case None => 0.0
      }
      variables += term.variable -> value
    }
    val reducedTerms = (for ((variable, coefficient) <- variables) yield new Term(variable, coefficient)).toList
    new Expression(reducedTerms, expression.constant)
  }

  /* Operator methods */
  def ^(strength: Double) = Symbolics.modifyStrength(this, strength)
}
