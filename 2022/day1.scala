//> using toolkit default

@main
def day1(): Unit = {
  val path = os.pwd / "day1Input.txt"
  solution(path, false)
  solution(path, true)
}

def solution(path: os.Path, part2: Boolean): Unit = {
  val lines = os.read.lines(path)
  var elves: List[Int] = List()
  var cumul = 0
  lines.foreach {
    (line) => {
      if line == "" then {
        elves = cumul :: elves
        cumul = 0
      } else {
        cumul += line.toInt
      }
    }
  }
  elves = cumul :: elves
  if part2 then
    println(elves.sortWith((x, y) => x > y).take(3).sum)
  else
    println(elves.sortWith((x, y) => x > y).head)
}