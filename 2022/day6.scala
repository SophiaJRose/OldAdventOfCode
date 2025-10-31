//> using toolkit default

@main
def day6(): Unit = {
  val path = os.pwd / "day6Input.txt"
  solution(path, false)
  solution(path, true)
}

def solution(path: os.Path, part2: Boolean): Unit = {
  val input = os.read(path)
  var markerSize = 4
  if part2 then markerSize = 14
  val potentialMarkers = input.sliding(markerSize,1)
  println(potentialMarkers.indexWhere((mark) => mark.distinct.size == markerSize) + markerSize)
}