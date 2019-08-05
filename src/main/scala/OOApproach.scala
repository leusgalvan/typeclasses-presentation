object OOApproach {
  sealed trait JsValue
  case class JsString(value: String) extends JsValue
  case class JsNumber(int: Int) extends JsValue
  case class JsObject(entries: Map[String, JsValue]) extends JsValue

  trait JsConvertible {
    def toJson(): JsValue
  }

  def write(jsValue: JsValue): String = jsValue match {
    case JsString(v) => v
    case JsNumber(n) => n.toString
    case JsObject(entries) => s"""{${entries.map(p => p._1 + ": " + write(p._2)).mkString(", ")}}"""
  }

  def write(jsConvertible: JsConvertible): String = write(jsConvertible.toJson())

  sealed trait Operation extends JsConvertible
  case class Withdraw(accountNumber: String, amount: Int) extends Operation {
    override def toJson(): JsValue = JsObject(Map(
      "operation" -> JsString("withdraw"),
      "accountNumber" -> JsString(accountNumber),
      "amount" -> JsNumber(amount)
    ))
  }
  case class Transfer(fromAccountNumber: String, toAccountNumber: String) extends Operation {
    override def toJson(): JsValue = JsObject(Map(
      "operation" -> JsString("transfer"),
      "fromAccountNumber" -> JsString(fromAccountNumber),
      "toAccountNumber" -> JsString(fromAccountNumber)
    ))
  }
  case class Deposit(accountNumber: String, amount: Int) extends Operation {
    override def toJson(): JsValue = JsObject(Map(
      "operation" -> JsString("deposit"),
      "accountNumber" -> JsString(accountNumber),
      "amount" -> JsNumber(amount)
    ))
  }
}
