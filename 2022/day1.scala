//> using toolkit default

@main
def solution(): Unit = {
  part1()
  part2()
}

def part1(): Unit = {
  val path = os.pwd / "day1Input.txt"
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
  println(elves.sortWith((x, y) => x > y).head)
}

def part2(): Unit = {
  val path = os.pwd / "day1Input.txt"
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
  println(elves.sortWith((x, y) => x > y).take(3).sum)
}