case class Pos(row: Int, col: Int) {
  /** The position obtained by changing the `row` coordinate by `d` */
  def deltaRow(d: Int): Pos = copy(row = row + d)

  /** The position obtained by changing the `col` coordinate by `d` */
  def deltaCol(d: Int): Pos = copy(col = col + d)
}

type Terrain = Pos => Boolean

val level =
  """ST
    |oo
    |oo""".stripMargin

lazy val vector: Vector[Vector[Char]] =Vector(level.split("\n").map(str => Vector(str: _*)): _*)

/**
  * This method returns terrain function that represents the terrain
  * in `levelVector`. The vector contains parsed version of the `level`
  * string. For example, the following level
  *
  *   val level =
  *     """ST
  *       |oo
  *       |oo""".stripMargin
  *
  * is represented as
  *
  *   Vector(Vector('S', 'T'), Vector('o', 'o'), Vector('o', 'o'))
  *
  * The resulting function should return `true` if the position `pos` is
  * a valid position (not a '-' character) inside the terrain described
  * by `levelVector`.
  */
def terrainFunction(levelVector: Vector[Vector[Char]]): Pos => Boolean = {
  case Pos(x,y) => (for {
    row <- levelVector lift x
    ch  <- row lift y
    if ch != '-'
  } yield ch).isDefined
}



lazy val terrain: Terrain = terrainFunction(vector)
terrain