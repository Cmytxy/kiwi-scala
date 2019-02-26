package name.stefanszymanski.kiwi

object Symbol {
  def apply(): Symbol = Invalid()

  object Type extends Enumeration {
    type Type = Value
    val invalid = Value
    val external = Value
    val slack = Value
    val error = Value
    val dummy = Value
  }

  def Invalid(): Symbol = new Symbol(Type.invalid)
  def External(): Symbol = new Symbol(Type.external)
  def Slack(): Symbol = new Symbol(Type.slack)
  def Error(): Symbol = new Symbol(Type.error)
  def Dummy(): Symbol = new Symbol(Type.dummy)
}

class Symbol(val kind: Symbol.Type.Value)
