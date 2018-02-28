trait Generator[+T] {
  val rand = new java.util.Random

  def generate: T
}

val integers = new Generator[Int] {
  override def generate = rand.nextInt()
}

(1 to 10).foreach(n => println(s"N$n  ${integers.generate}"))

val booleans = new Generator[Boolean] {
  override def generate = rand.nextBoolean()
}

(1 to 10).foreach(n => println(s"N$n  ${booleans.generate}"))

val pairs = new Generator[(Int, Int)] {
  override def generate = (rand.nextInt, rand.nextInt)
}

(1 to 10).foreach(n => println(s"N$n  ${pairs.generate}"))

/** ***********************************************************/

trait Generators[+T] {
  self => def generate: T // an alias for "this"
  def map[S](f: T => S): Generators[S] = new Generators[S] {
    override def generate = f(self.generate)
  }

  def flatMap[S](f: T => Generators[S]): Generators[S] = new Generators[S] {
    override def generate = f(self.generate).generate
  }
}

val booleanss = new Generators[Boolean] {
  def generate = integers.generate > 0
}

