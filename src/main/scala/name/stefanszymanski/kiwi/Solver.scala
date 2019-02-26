package name.stefanszymanski.kiwi

import scala.collection.mutable

class Solver {
  private class Tag {
    var marker: Symbol = Symbol()
    var other: Symbol = Symbol()
  }

  private class EditInfo(var constraint: Constraint, var tag: Tag, var constant: Double)

  private val constraints = mutable.LinkedHashMap.empty[Constraint, Tag]
  private val rows = mutable.LinkedHashMap.empty[Symbol, Row]
  private val variables = mutable.LinkedHashMap.empty[Variable, Symbol]
  private val edits = mutable.LinkedHashMap.empty[Variable, EditInfo]
  private val infeasibleRows = mutable.ArrayBuffer.empty[Symbol]

  private val objective = new Row()
  private var artificial: Row = _

  /*
	 * Add a constraint to the solver.
	 */
  @throws[Exception.DuplicateConstraint]
  @throws[Exception.UnsatisfiableConstraint]
  def addConstraint(constraint: Constraint): Unit = {
    if (hasConstraint(constraint)) {
      throw new Exception.DuplicateConstraint(constraint)
    }

    // Creating a row causes symbols to reserved for the variables in the constraint.
		// If this method exits with an exception, then its possible those variables will linger in the var map.
		// Since it's likely that those variables will be used in other constraints and since exceptional conditions are uncommon,
    // I'm not too worried about aggressive cleanup of the var map.
    val tag = new Tag()
    val row = createRow(constraint, tag)
    var subject = chooseSubject(row, tag)

    // If chooseSubject could find a valid entering symbol, one last option is available if the entire row is composed of dummy variables.
		// If the constant of the row is zero, then this represents redundant constraints and the new dummy marker can enter the basis.
		// If the constant is non-zero, then it represents an unsatisfiable constraint.
    if (subject.kind == Symbol.Type.invalid && allDummies(row)) {
      if (!isNearZero(row.constant)) {
        throw new Exception.UnsatisfiableConstraint(constraint)
      }
      subject = tag.marker
    }

    // If an entering symbol still isn't found, then the row must be added using an artificial variable.
		// If that fails, then the row represents an unsatisfiable constraint.
    if (subject.kind == Symbol.Type.invalid) {
      if (!addWithArtificialVariable(row)) {
        throw new Exception.UnsatisfiableConstraint(constraint)
      }
    } else {
      row.solveFor(subject)
      substitute(subject, row)
      rows += subject -> row
    }

    constraints += constraint -> tag

    // Optimizing after each constraint is added performs less aggregate work due to a smaller average system size.
		// It also ensures the solver remains in a consistent state.
    optimize(objective)
  }

  /*
	 * Remove a constraint from the solver.
	 */
  def removeConstraint(constraint: Constraint): Unit = {
    if (!hasConstraint(constraint)) {
      throw new Exception.UnknownConstraint(constraint)
    }

    val tag = constraints(constraint)
    constraints -= constraint

    // Remove the error effects from the objective function before pivoting, or substitutions into the objective will lead to incorrect solver results.
    removeConstraintEffects(constraint, tag)

    // If the marker is basic, simply drop the row. Otherwise, pivot the marker into the basis and then drop the row.
    rows.get(tag.marker) match {
      case Some(_) => rows -= tag.marker
      case None =>
        val leaving = getMarkerLeavingSymbol(tag.marker)
        if (leaving.kind == Symbol.Type.invalid) {
          throw new InternalSolverError
        }
        val row = rows(leaving)
        rows -= leaving
        row.solveFor(leaving, tag.marker)
        substitute(tag.marker, row)
    }

    // Optimizing after each constraint is removed ensures that the solver remains consistent.
		// It makes the solver API easier to use at a small tradeoff for speed.
    optimize(objective)

  }

  /*
	 * Test whether the solver contains the constraint.
	 */
  def hasConstraint(constraint: Constraint): Boolean = constraints.contains(constraint)

  /*
	 * Add an edit variable to the solver.
	 * This method should be called before the 'suggestValue' method is used to supply a suggested value for the given edit variable.
	 */
  def addEditVariable(variable: Variable, _strength: Double): Unit = {
    if (edits.contains(variable)) {
      throw new Exception.DuplicateEditVariable(variable)
    }

    val strength = Strength.clip(_strength)

    if (strength == Strength.required) {
      throw new Exception.BadRequiredStrength
    }

    val terms = new Term(variable) +: List.empty[Term]
    val constraint = new Constraint(new Expression(terms), RelationalOperator.eq, strength)
    addConstraint(constraint)
    edits += variable -> new EditInfo(constraint, constraints(constraint), 0.0)

  }

  /*
	 * Remove an edit variable from the solver.
	 */
  def removeEditVariable(variable: Variable): Unit = {
    if (!edits.contains(variable)) {
      throw new Exception.UnknownEditVariable(variable)
    }

    removeConstraint(edits(variable).constraint)
    edits -= variable
  }

  def hasEditVariable(variable: Variable): Boolean = edits.contains(variable)

  /*
	 * Suggest the value of an edit variable.
	 * This method should be used after an edit variable as been added to the solver in order to suggest the value for that variable.
	 */
  def suggestValue(variable: Variable, value: Double): Unit = {
    if (!hasEditVariable(variable)) {
      throw new Exception.UnknownEditVariable(variable)
    }

    val info = edits(variable)
    info.constant = value
    val delta = value - info.constant

    // Check if the positive error variable is basic.
    val marker = info.tag.marker
    if (rows.contains(marker)) {
      val row = rows(marker)
      if (row.add(-delta) < 0.0) {
        infeasibleRows += marker
      }
      dualOptimize()
      return
    }

    // Check if the negative error variable is basic.
    val other = info.tag.other
    if (rows.contains(other)) {
      val row = rows(other)
      if (row.add(delta) < 0.0) {
        infeasibleRows += other
      }
      dualOptimize()
      return
    }

    // Otherwise update each row where the error variables exist.
    for ((symbol, row) <- rows) {
      val coefficient = row.coefficientFor(marker)
      if (coefficient != 0.0 && row.add(delta * coefficient) < 0.0 && symbol.kind != Symbol.Type.external) {
        infeasibleRows += symbol
      }
    }
    dualOptimize()
  }

  /*
	 * Update the values of the external solver variables.
	 */
  def updateVariables(): Unit = {
    for ((variable, symbol) <- variables) {
      variable.value = if (rows.contains(symbol)) rows(symbol).constant else 0.0
    }
  }

  /*
	 * Remove the effects of a constraint on the objective function.
	 */
  protected def removeConstraintEffects(constraint: Constraint, tag: Tag): Unit = {
    if (tag.marker.kind == Symbol.Type.error) {
      removeMarkerEffects(tag.marker, constraint.strength)
    } else if (tag.other.kind == Symbol.Type.error) {
      removeMarkerEffects(tag.other, constraint.strength)
    }
  }

  protected def removeMarkerEffects(marker: Symbol, strength: Double): Unit = rows.get(marker) match {
    case Some(row) => objective.insert(row, -strength)
    case None => objective.insert(marker, -strength)
  }

  /*
	 * Compute the leaving symbol for a marker variable.
	 * This method will return a symbol corresponding to a basic row which holds the given marker variable.
	 * The row will be chosen according to the following precedence:
	 * 1) The row with a restricted basic variable and a negative coefficient for the marker with the smallest ratio of -constant / coefficient.
	 * 2) The row with a restricted basic variable and the smallest ratio of constant / coefficient.
	 * 3) The last unrestricted row which contains the marker.
	 * If the marker does not exist in any row, an invalid symbol will be returned.
	 * This indicates an internal solver error since the marker should exist somewhere in the tableau.
	 */
  protected def getMarkerLeavingSymbol(marker: Symbol): Symbol = {
    var ratio1 = Double.MaxValue
    var ratio2 = Double.MaxValue

    var first = Symbol.Invalid()
    var second = Symbol.Invalid()
    var third = Symbol.Invalid()

    for ((symbol, row) <- rows) {
      val coefficient = row.coefficientFor(marker)
      if (coefficient != 0.0) {
        if (symbol.kind == Symbol.Type.external) {
          third = symbol
        } else if (coefficient < 0.0) {
          val ratio = -row.constant / coefficient
          if (ratio < ratio1) {
            ratio1 = ratio
            first = symbol
          }
        } else {
          val ratio = row.constant / coefficient
          if (ratio < ratio2) {
            ratio2 = ratio
            second = symbol
          }
        }
      }
    }

    if (first.kind != Symbol.Type.invalid) {
      return first
    }
    if (second.kind != Symbol.Type.invalid) {
      return second
    }
    third
  }

  /*
	 * Create a new Row object for the given constraint.
	 * The terms in the constraint will be converted to cells in the row.
	 * Any term in the constraint with a coefficient of zero is ignored.
	 * This method uses the 'getVarSymbol' method to get the symbol for the variables added to the row.
	 * If the symbol for a given cell variable is basic, the cell variable will be substituted with the basic row.
	 * The necessary slack and error variables will be added to the row.
	 * If the constant for the row is negative, the sign for the row will be inverted so the constant becomes positive.
	 * The tag will be updated with the marker and error symbols to use for tracking the movement of the constraint in the tableau.
	 */
  protected def createRow(constraint: Constraint, tag: Tag): Row = {
    val expression = constraint.expression
    val row = new Row(expression.constant)

    // Substitute the current basic variables into the row.
    for (term <- expression.terms if !isNearZero(term.coefficient)) {
      val symbol = getVariableSymbol(term.variable)
      rows.get(symbol) match {
        case Some(otherRow) => row.insert(otherRow, term.coefficient)
        case None => row.insert(symbol, term.coefficient)
      }
    }

    // Add the necessary slack, error, and dummy variables.
    val strength = constraint.strength
    constraint.operator match {
      case RelationalOperator.le | RelationalOperator.ge =>
        val coefficient = if (constraint.operator == RelationalOperator.le) 1.0 else -1.0
        val slack = Symbol.Slack()
        tag.marker = slack
        row.insert(slack, coefficient)
        if (strength < Strength.required) {
          val error = Symbol.Error()
          tag.other = error
          row.insert(error, -coefficient)
          objective.insert(error, strength)
        }
      case RelationalOperator.eq if constraint.strength < Strength.required =>
        val errorPlus = Symbol.Error()
        val errorMinus = Symbol.Error()
        tag.marker = errorPlus
        tag.other = errorMinus
        row.insert(errorPlus, -1.0)
        row.insert(errorMinus, 1.0)
        objective.insert(errorPlus, strength)
        objective.insert(errorMinus, strength)
      case RelationalOperator.eq =>
        val dummy = Symbol.Dummy()
        tag.marker = dummy
        row.insert(dummy)
    }

    // Ensure the row as a positive constant.
    if (row.constant < 0.0) {
      row.reverseSign()
    }

    row
  }

  /*
	 * Choose the subject for solving for the row.
	 * This method will choose the best subject for using as the solve target for the row.
	 * An invalid symbol will be returned if there is no valid target.
	 * The symbols are chosen according to the following precedence:
	 * 1) The first symbol representing an external variable.
	 * 2) A negative slack or error tag variable.
	 * If a subject cannot be found, an invalid symbol will be returned.
	 */
  protected def chooseSubject(row: Row, tag: Tag): Symbol = {
    for (symbol <- row.cells.keys if symbol.kind == Symbol.Type.external) {
      return symbol
    }
    if (tag.marker.kind == Symbol.Type.slack || tag.marker.kind == Symbol.Type.error) {
      if (row.coefficientFor(tag.marker) < 0.0) {
        return tag.marker
      }
    }
    if (tag.other.kind == Symbol.Type.slack || tag.other.kind == Symbol.Type.error) {
      if (row.coefficientFor(tag.other) < 0.0) {
        return tag.other
      }
    }
    Symbol.Invalid()
  }

  /*
	 * Add the row to the tableau using an artificial variable.
	 * This will return false if the constraint cannot be satisfied.
 	 */
  protected def addWithArtificialVariable(row: Row): Boolean = {
    // Create and add the artificial variable to the tableau.
    val art = Symbol.Slack()
    rows += art -> new Row(row)
    artificial = new Row(row)

    // Optimize the artificial objective.
		// This is successful only if the artificial objective is optimized to zero.
    optimize(artificial)
    val success = isNearZero(artificial.constant)
    artificial = null

    // If the artificial variable is basic, pivot the row so that it becomes non-basic.
		// If the row is constant, exit early.
    if (rows.contains(art)) {
      val row = rows(art)
      rows -= art
      if (row.isConstant) {
        return success
      }
      val entering = anyPivotableSymbol(row)
      if (entering.kind == Symbol.Type.invalid) {
        return false
      }
      row.solveFor(art, entering)
      substitute(entering, row)
      rows += entering -> row
    }

    // Remove the artificial variable from the tableau.
    for (row <- rows.values) row.remove(art)
    objective.remove(art)

    success
  }

  /*
	 * Substitute the parametric symbol with the given row.
	 * This method will substitute all instances of the parametric symbol in the tableau and the objective function with the given row.
	 */
  protected def substitute(symbol: Symbol, row: Row): Unit = {
    for ((_symbol, _row) <- rows) {
      _row.substitute(symbol, row)
      if (_symbol.kind == Symbol.Type.external && _row.constant < 0.0) {
        infeasibleRows += _symbol
      }
    }

    objective.substitute(symbol, row)
    if (artificial != null) {
      artificial.substitute(symbol, row)
    }
  }

  /*
	 * Optimize the system for the given objective function.
	 * This method performs iterations of Phase 2 of the simplex method until the objective function reaches a minimum.
	 */
  protected def optimize(objective: Row): Unit = {
    while (true) {
      val entering = getEnteringSymbol(objective)
      if (entering.kind == Symbol.Type.invalid) {
        return
      }
      val leaving = getLeavingSymbol(entering)
      if (leaving.kind == Symbol.Type.invalid) {
        throw new InternalSolverError
      }

      // Pivot the entering symbol into the basis.
      val row = rows(leaving)
      rows -= leaving
      row.solveFor(leaving, entering)
      substitute(entering, row)
      rows += entering -> row
    }
  }

  /*
	 * Optimize the system using the dual of the simplex method.
	 * The current state of the system should be such that the objective function is optimal, but not feasible.
	 * This method will perform an iteration of the dual simplex method to make the solution both optimal and feasible.
	 */
  protected def dualOptimize(): Unit = {
    while (infeasibleRows.nonEmpty) {
      val leaving = infeasibleRows.remove(0)
      rows.get(leaving) match {
        case Some(row) if row.constant < 0.0 =>
          val entering = getDualEnteringSymbol(row)
          if (entering.kind == Symbol.Type.invalid) {
            throw new InternalSolverError
          }

          // Pivot the entering symbol into the basis.
          rows -= leaving
          row.solveFor(leaving, entering)

          substitute(entering, row)
          rows += entering -> row
      }

    }
  }

  /*
	 * Compute the entering variable for a pivot operation.
	 * This method will return first symbol in the objective function which is non-dummy and has a coefficient less than zero.
	 * If no symbol meets the criteria, it means the objective function is at a minimum, and an invalid symbol is returned.
	 */
  protected def getEnteringSymbol(objective: Row): Symbol = {
    for ((symbol, constant) <- objective.cells if symbol.kind != Symbol.Type.dummy && constant < 0.0) {
      return symbol
    }
    Symbol.Invalid()
  }

  /*
	 * Compute the entering symbol for the dual optimize operation.
	 * This method will return the symbol in the row which has a positive coefficient and yields the minimum ratio for its respective symbol in the objective function.
	 * The provided row must be infeasible.
	 * If no symbol is found which meets the criteria, an invalid symbol is returned.
	 */
  protected def getDualEnteringSymbol(row: Row): Symbol = {
    var entering = Symbol.Invalid()
    var ratio = Double.MaxValue

    for ((symbol, constant) <- row.cells) {
      if (symbol.kind != Symbol.Type.dummy && constant > 0.0) {
        val coefficient = objective.coefficientFor(symbol)
        val _ratio = coefficient / constant
        if (_ratio < ratio) {
          ratio = _ratio
          entering = symbol
        }
      }
    }

    entering
  }

  /*
	 * Get the first Slack or Error symbol in the row.
	 * If no such symbol is present, an Invalid symbol will be returned.
	 */
  protected def anyPivotableSymbol(row: Row): Symbol = {
    for (symbol <- row.cells.keys if symbol.kind == Symbol.Type.slack || symbol.kind == Symbol.Type.error) {
      return symbol
    }
    Symbol.Invalid()
  }

  /*
	 * Compute the symbol for pivot exit row.
	 * This method will return the symbol for the exit row in the row map.
	 * If no appropriate exit symbol is found, an invalid symbol will be returned.
	 * This indicates that the objective function is unbounded.
	 */
  protected def getLeavingSymbol(entering: Symbol): Symbol = {
    var ratio = Double.MaxValue
    var leaving = Symbol.Invalid()

    for ((symbol, row) <- rows if symbol.kind != Symbol.Type.external) {
      val coefficient = row.coefficientFor(entering)
      if (coefficient < 0.0) {
        val _ratio = -row.constant / coefficient
        if (_ratio < ratio) {
          ratio = _ratio
          leaving = symbol
        }
      }
    }

    leaving
  }

  /*
	 * Get the symbol for the given variable.
	 * If a symbol does not exist for the variable, one will be created.
	 */
  protected def getVariableSymbol(variable: Variable): Symbol = variables.get(variable) match {
    case Some(symbol) => symbol
    case None =>
      val symbol = Symbol.External()
      variables += variable -> symbol
      symbol
  }

  /*
	 * Test whether a row is composed of all dummy variables.
	 */
  protected def allDummies(row: Row): Boolean = {
    for (symbol <- row.cells.keys) {
      if (symbol.kind != Symbol.Type.dummy) {
        return false
      }
    }
    true
  }

  def +=(constraint: Constraint) = addConstraint(constraint)

  def -=(constraint: Constraint) = removeConstraint(constraint)

  def unary_! = updateVariables()
}
