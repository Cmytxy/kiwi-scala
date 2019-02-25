package name.stefanszymanski.kiwi

object Strength {

  val required: Double = create(1000.0, 1000.0, 10000.0)
  val strong: Double = create(1.0, 0.0, 0.0)
  val medium: Double = create(0.0, 1.0, 0.0)
  val weak: Double = create(0.0, 0.0, 1.0)

  def create(a: Double, b: Double, c: Double, w: Double): Double = {
    var result = 0.0
    result += Math.max(0.0, Math.min(1000.0, a * w)) * 1000000.0
    result += Math.max(0.0, Math.min(1000.0, b * w)) * 1000.0
    result += Math.max(0.0, Math.min(1000.0, c * w))
    result
  }

  def create(a: Double, b: Double, c: Double): Double = create(a, b, c, 1.0)

  def clip(value: Double): Double = Math.max(0.0, Math.min(value, required))
}
