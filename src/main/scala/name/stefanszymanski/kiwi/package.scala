package name.stefanszymanski

package object kiwi {
  val eps = 1.0e-8

  def isNearZero(value: Double): Boolean = if (value < 0.0) -value < eps else value < eps
}
