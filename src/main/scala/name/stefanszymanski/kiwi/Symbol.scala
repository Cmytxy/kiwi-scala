package name.stefanszymanski.kiwi

object Symbol {
  def apply(): Symbol = Invalid()

  object Type extends Enumeration {
    type Type = Value
    val invalid = Type
    val external = Type
    val slack = Type
    val error = Type
    val dummy = Type
  }

  def Invalid(): Symbol = new Symbol(Type.invalid)
  def External(): Symbol = new Symbol(Type.external)
  def Slack(): Symbol = new Symbol(Type.slack)
  def Error(): Symbol = new Symbol(Type.error)
  def Dummy(): Symbol = new Symbol(Type.dummy)
}

class Symbol(val kind: Symbol.Type.type)
