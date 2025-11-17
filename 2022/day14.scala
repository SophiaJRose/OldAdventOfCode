//> using toolkit default

@main
def day14(): Unit = {
  val path = os.pwd / "day14Input.txt"
  solution(path, false)
  solution(path, true)
}

case class Coord(val x: Int, val y: Int)
object Value:
  def unapply(s: String): Option[Int] = s.toIntOption

def solution(path: os.Path, part2: Boolean): Unit = {
  var lines: IndexedSeq[String] = os.read.lines(path)
  var grid: Map[Coord, Char] = Map[Coord, Char]()
  // Draw rock lines
  lines.foreach {
    (l) =>
      var line = l
      val startCoordStr = line.takeWhile((c) => c != ' ')
      line = line.dropWhile((c) => c != ' ')
      var startCoord = startCoordStr match
        case s"${Value(x)},${Value(y)}" => Coord(x,y)
        case _ => Coord(-1,-1)
      line = line.drop(4)
      while line.nonEmpty do {
        val endCoordStr = line.takeWhile((c) => c != ' ')
        line = line.dropWhile((c) => c != ' ')
        val endCoord = endCoordStr match
          case s"${Value(x)},${Value(y)}" => Coord(x, y)
          case _ => Coord(-1, -1)
        // Draw line of rock
        if startCoord.x == endCoord.x then {
          // Vertical, check if up or down
          if startCoord.y < endCoord.y then {
            for (y <- startCoord.y to endCoord.y) {
              grid = grid + (Coord(startCoord.x, y) -> '#')
            }
          } else
            for (y <- endCoord.y to startCoord.y) {
              grid = grid + (Coord(startCoord.x, y) -> '#')
            }
        } else if startCoord.y == endCoord.y then {
          // Horizontal, check if left or right
          if startCoord.x < endCoord.x then {
            for (x <- startCoord.x to endCoord.x) {
              grid = grid + (Coord(x, startCoord.y) -> '#')
            }
          } else
            for (x <- endCoord.x to startCoord.x) {
              grid = grid + (Coord(x, startCoord.y) -> '#')
            }
        }
        // If end of input line not reached, draw next rock line
        if line.nonEmpty then {
          line = line.drop(4)
          startCoord = endCoord
        }
      }
  }
  // Simulate sand falling
  val lowestRockHeight = grid.keys.maxBy((coord) => coord.y).y
  val floorHeight = lowestRockHeight+2
  val sandStart = Coord(500,0)
  var simulate = true
  while simulate do {
    var sand = sandStart
    var falling = true
    while falling do {
      // Check if on floor in part 2
      if part2 && sand.y == floorHeight-1 then {
        grid = grid + (sand -> 'o')
        falling = false
      }
      val down = grid.get(Coord(sand.x, sand.y+1))
      down match
        case None => sand = Coord(sand.x, sand.y+1)
        case Some(_) => {
          val downLeft = grid.get(Coord(sand.x-1, sand.y+1))
          downLeft match
            case None => sand = Coord(sand.x-1, sand.y+1)
            case Some(_) => {
              val downRight = grid.get(Coord(sand.x+1, sand.y+1))
              downRight match
                case None => sand = Coord(sand.x+1, sand.y+1)
                case Some(_) => {
                  grid = grid + (sand -> 'o')
                  // Stop simulating in part2 if sandStart has been filled
                  if sand == sandStart then
                    simulate = false
                  falling = false
                }
            }
        }
      // Only check for falling forever in part 1
      if !part2 && sand.y > lowestRockHeight then {
        falling = false
        simulate = false
      }
    }
  }
  println(grid.values.count((c) => c == 'o'))
}