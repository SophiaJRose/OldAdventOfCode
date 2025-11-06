//> using toolkit default

@main
def day9(): Unit = {
  val path = os.pwd / "day9Input.txt"
  solutionPart1(path)
  solutionPart2(path)
}

case class Coord(val x: Int, val y: Int) {
  def +(that: Coord): Coord =
    return Coord(x + that.x, y + that.y)

  def -(that: Coord): Coord =
    return Coord(x - that.x, y - that.y)

  def half: Coord =
    return Coord(x / 2, y / 2)
}

object Distance:
  def unapply(s: String): Option[Int] = s.toIntOption

def solutionPart1(path: os.Path): Unit = {
  val lines = os.read.lines(path)
  var head = Coord(0,0)
  var tail = Coord(0,0)
  var tailVisited: Set[Coord] = Set[Coord](tail)
  lines.foreach {
    (line) =>
      val (dir, dist) = line match
        case s"U ${Distance(dist)}" => (Coord(0,-1), dist)
        case s"R ${Distance(dist)}" => (Coord(1,0), dist)
        case s"D ${Distance(dist)}" => (Coord(0,1), dist)
        case s"L ${Distance(dist)}" => (Coord(-1,0), dist)
        case _ => (Coord(0,0), 0)
      for (_ <- 0 until dist) {
        head = head + dir
        var tailDist = head - tail
        if tailDist.x.abs > 1 || tailDist.y.abs > 1 then {
          // Take advantage of integer division flooring to get desired distance from head to tail
          var halfDist = tailDist.half
          tail = head - halfDist
          tailVisited = tailVisited.concat(Set(tail))
        }
      }
  }
  println(tailVisited.size)
}

def solutionPart2(path: os.Path): Unit = {
  val lines = os.read.lines(path)
  var knots: Array[Coord] = Array.fill(10)(Coord(0,0))
  var tailVisited: Set[Coord] = Set[Coord](Coord(0,0))
  lines.foreach {
    (line) =>
      val (dir, dist) = line match
        case s"U ${Distance(dist)}" => (Coord(0,-1), dist)
        case s"R ${Distance(dist)}" => (Coord(1,0), dist)
        case s"D ${Distance(dist)}" => (Coord(0,1), dist)
        case s"L ${Distance(dist)}" => (Coord(-1,0), dist)
        case _ => (Coord(0,0), 0)
      for (_ <- 0 until dist) {
        knots(0) = knots(0) + dir
        for (i <- 1 until knots.size) {
          val knotDist = knots(i-1) - knots(i)
          if knotDist.x.abs > 1 || knotDist.y.abs > 1 then {
            // Take advantage of integer division flooring to get desired distance from head to tail
            var halfDist = knotDist.half
            knots(i) = knots(i-1) - halfDist
            if i == 9 then
              tailVisited = tailVisited.concat(Set(knots(i)))
          }
        }
      }
  }
  println(tailVisited.size)
}