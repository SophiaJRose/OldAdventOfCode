//> using toolkit default

@main
def day5(): Unit = {
  val path = os.pwd / "day5Input.txt"
  solution(path, false)
  solution(path, true)
}

object InstrNumber:
  def unapply(s: String): Option[Int] = s.toIntOption

def solution(path: os.Path, part2: Boolean): Unit = {
  val lines = os.read.lines(path)
  val initialState = lines.takeWhile((line) => line.contains('[')).reverse
  val instructions = lines.dropWhile((line) => line.nonEmpty).tail
  // Line length is 4n - 1 for n stacks
  val numStacks = (initialState.head.size + 1) / 4
  var stacks: Array[Array[Char]] = Array[Array[Char]]()
  var i = 0
  // Build initial state of stacks
  while (i < numStacks) {
    var stack: Array[Char] = Array[Char]()
    initialState.foreach {
      (line) =>
        // label of crate is at index 4i + 1 for ith crate
        if line.size > (4*i + 1) && line(4*i + 1) != ' ' then {
          stack = stack.appended(line(4*i + 1))
        }
    }
    stacks = stacks.appended(stack)
    i += 1
  }
  // Perform instructions
  instructions.foreach {
    (line) =>
      val (amount, source, dest) = line match
        case s"move ${InstrNumber(amount)} from ${InstrNumber(source)} to ${InstrNumber(dest)}" => (amount, source-1, dest-1) // Convert from 1-indexing to 0-indexing
        case _ => (0,0,0)
      if part2 then {
        val movedCrates = stacks(source).takeRight(amount)
        stacks(source) = stacks(source).dropRight(amount)
        stacks(dest) = stacks(dest).appendedAll(movedCrates)
      } else {
        var i = 0
        while (i < amount) {
          val movedCrate = stacks(source).last
          stacks(source) = stacks(source).dropRight(1)
          stacks(dest) = stacks(dest).appended(movedCrate)
          i += 1
        }
      }
  }
  val tops: Array[Char] = stacks.map(_.last)
  println(tops.addString(StringBuilder()))
}