package name.stefanszymanski

package object kiwi {
  val epsilon = 1.0e-8

  def isNearZero(value: Double): Boolean = if (value < 0.0) -value < epsilon else value < epsilon
}
