import calculator.{Signal, Var}

class BankAccount {
  val balance = Var(0)

  def deposit(amount: Int): Unit =
    if (amount > 0) {
      val b = balance()
      balance() = b + amount
    }

  def withdraw(amount: Int): Unit =
    if (0 < amount && amount <= balance()) {
      val b = balance()
      balance() = b - amount
    } else throw new Error("inefficient funds")
}


object account {
  def consolidate(accts: List[BankAccount]): Signal[Int] =
    Signal(accts.map(_.balance()).sum)

  val a = new BankAccount()
  val b = new BankAccount()
  val c = consolidate(List(a, b))
  println(c())
  a deposit 20
  println(c())
  b deposit 30
  println(c())

  val xchange = Signal(246.00)
  val inDollar = Signal(c() * xchange())
  println(inDollar())
  b withdraw 10
  println(inDollar())
}

account

/** *************/
val num = Var(1)
val twice = Signal(num() * 2)
num() =2
println(twice)
/*************/
val nam = Var(1)
val twice1 = Signal(nam() * 2)
nam() =2
println(twice1)