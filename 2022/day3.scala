//> using toolkit default

@main
def day3(): Unit = {
  val path = os.pwd / "day3Input.txt"
  solutionPart1(path)
  solutionPart2(path)
}

def solutionPart1(path: os.Path): Unit = {
  val lines = os.read.lines(path)
  val priorityString = " abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
  var total = 0
  lines.foreach {
    (line) =>
      val (leftHalf, rightHalf) = line.splitAt(line.size / 2)
      val intersection = leftHalf.intersect(rightHalf)
      if intersection.nonEmpty then {
        val item = intersection.head
        total += priorityString.indexWhere((x) => x == item, 0)
      }
  }
  println(total)
}

def solutionPart2(path: os.Path): Unit = {
  val lines = os.read.lines(path)
  val groups = lines.grouped(3)
  val priorityString = " abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
  var total = 0
  groups.foreach {
    (group) =>
      val commonItem = group.fold(priorityString)((a, b) => a.intersect(b))
      if commonItem.nonEmpty then {
        val item = commonItem.head
        total += priorityString.indexWhere((x) => x == item, 0)
      }
  }
  println(total)
}