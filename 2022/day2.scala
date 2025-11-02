//> using toolkit default

@main
def day2(): Unit = {
  val path = os.pwd / "day2Input.txt"
  solution(path, false)
  solution(path, true)
}

def solution(path: os.Path, part2: Boolean): Unit = {
  val part1Values = Map("A X" -> 4, "A Y" -> 8, "A Z" -> 3, "B X" -> 1, "B Y" -> 5, "B Z" -> 9, "C X" -> 7, "C Y" -> 2, "C Z" -> 6)
  val part2Values = Map("A X" -> 3, "A Y" -> 4, "A Z" -> 8, "B X" -> 1, "B Y" -> 5, "B Z" -> 9, "C X" -> 2, "C Y" -> 6, "C Z" -> 7)
  val lines = os.read.lines(path)
  var total = 0
  lines.foreach {
    (line) =>
      if !part2 then
        total += part1Values.get(line).getOrElse(0)
      else
        total += part2Values.get(line).getOrElse(0)
  }
  println(total)
}