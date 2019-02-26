package name.stefanszymanski.kiwi

import scala.collection.mutable

class Row(var cells: mutable.LinkedHashMap[Symbol, Double], var constant: Double) {
  /* Constructors */
  def this(constant: Double) = this(mutable.LinkedHashMap.empty, constant)
  def this() = this(0.0)
  def this(other: Row) = this(other.cells.clone(), other.constant)

  def add(value: Double): Double = {
    constant += value
    constant
  }

  def insert(symbol: Symbol, coefficient: Double): Unit = {
    val _coefficient = coefficient + (cells.get(symbol) match {
      case Some(c) => c
      case None => 0.0
    })
    if (isNearZero(_coefficient)) {
      cells -= symbol
    } else {
      cells += symbol -> _coefficient
    }
  }

  def insert(symbol: Symbol): Unit = insert(symbol, 1.0)

  def insert(other: Row, coefficient: Double): Unit = {
    constant += other.constant * coefficient
    for ((otherSymbol, otherCoefficient) <- other.cells) {
      insert(otherSymbol, otherCoefficient * coefficient)
    }
  }

  def insert(other: Row): Unit = insert(other, 1.0)

  def reverseSign(): Unit = {
    constant = -constant
    cells = for ((symbol, coefficient) <- cells) yield symbol -> -coefficient
  }

  def solveFor(symbol: Symbol): Unit = {
    val coefficient = -1.0 / cells(symbol)
    cells -= symbol
    constant *= coefficient
    cells = for ((_symbol, _coefficient) <- cells) yield _symbol -> _coefficient * coefficient
  }

  def solveFor(lhs: Symbol, rhs: Symbol): Unit = {
    insert(lhs, -1.0)
    solveFor(rhs)
  }

  def coefficientFor(symbol: Symbol): Double = cells.get(symbol) match {
    case Some(c) => c
    case None => 0.0
  }

  def substitute(symbol: Symbol, row: Row): Unit = {
    if (cells.contains(symbol)) {
      val coefficient = cells(symbol)
      cells -= symbol
      insert(row, coefficient)
    }
  }

  def remove(symbol: Symbol): Unit = cells -= symbol

  def isConstant: Boolean = cells.isEmpty
}
