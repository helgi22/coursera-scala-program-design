
trait Subscriber {
  def handler(pub: Publisher)
}

trait Publisher {
  private var subscribers: Set[Subscriber] = Set()

  def subscriber(subscriber: Subscriber): Unit = {
    subscribers += subscriber
  }

  def unsubscribe(subscriber: Subscriber): Unit = {
    subscribers -= subscriber
  }

  def publish(): Unit = {
    subscribers.foreach(_.handler(this))
  }
}

/** *************************************/
class BankAccount extends Publisher {
  private var balance = 0

  def currentBalance() = balance

  def deposit(amount: Int): Unit = {
    if (amount > 0) balance = balance + amount
    publish()
  }

  def withdraw(amount: Int): Unit =
    if (0 < amount && amount <= balance) {
      balance = balance - amount
      publish()
    } else throw new Error("insufficient funds")
}

/** ***********************************/
class Consolidator(observed: List[BankAccount]) extends Subscriber {
  observed.foreach(_.subscriber(this))

  private var total: Int = _
  compute()

  private def compute(): Unit =
    total = observed.map(_.currentBalance()).sum

  override def handler(pub: Publisher): Unit = compute()

  def totalBalance = total
}

/** **************************************/
object observed {

  def run(): Unit = {
    val a = new BankAccount
    val b = new BankAccount
    val c = new Consolidator(List(a, b))
    println(c.totalBalance)
    a deposit 20
    println(c.totalBalance)

  }
}

observed.run()