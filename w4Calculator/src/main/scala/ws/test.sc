package ws

trait Subscriber{
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

  def publisher(): Unit = {
  subscribers.foreach(_.handler(this))
  }
}