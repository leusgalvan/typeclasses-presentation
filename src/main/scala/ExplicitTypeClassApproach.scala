object ExplicitTypeClassApproach {

  sealed trait JsValue

  case class JsString(value: String) extends JsValue

  case class JsNumber(int: Int) extends JsValue

  case class JsObject(entries: Map[String, JsValue]) extends JsValue

  // Typeclass: trait with one type parameter, and some operations on that type.
  trait Json[A] {
    def toJson(value: A): JsValue
  }

  def write(jsValue: JsValue): String = jsValue match {
    case JsString(v) => v
    case JsNumber(n) => n.toString
    case JsObject(entries) => s"""{${entries.map(p => p._1 + ": " + write(p._2)).mkString(", ")}}"""
  }

  def write[A](value: A, jsConverter: Json[A]): String =
    write(jsConverter.toJson(value))

  // Typeclass instance: Its the assertion that Operation belongs in the JsConverter class
  val operationJsConverter = new Json[Operation] {
    override def toJson(value: Operation): JsValue = value match {
      case Withdraw(accountNumber, amount) =>
        JsObject(Map(
          "operation" -> JsString("withdraw"),
          "accountNumber" -> JsString(accountNumber),
          "amount" -> JsNumber(amount)
        ))

      case Transfer(fromAccountNumber, toAccountNumber) =>
        JsObject(Map(
          "operation" -> JsString("transfer"),
          "fromAccountNumber" -> JsString(fromAccountNumber),
          "toAccountNumber" -> JsString(fromAccountNumber)
        ))
      case Deposit(accountNumber, amount) =>
        JsObject(Map(
          "operation" -> JsString("deposit"),
          "accountNumber" -> JsString(accountNumber),
          "amount" -> JsNumber(amount)
        ))
    }
  }

  sealed trait Operation

  case class Withdraw(accountNumber: String, amount: Int) extends Operation

  case class Transfer(fromAccountNumber: String, toAccountNumber: String) extends Operation

  case class Deposit(accountNumber: String, amount: Int) extends Operation

}
