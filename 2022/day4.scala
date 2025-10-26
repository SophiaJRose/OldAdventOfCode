//> using toolkit default

@main
def day4(): Unit = {
  val path = os.pwd / "day4Input.txt"
  solution(path, false)
  solution(path, true)
}

object Section:
  def unapply(s: String): Option[Int] = s.toIntOption

def solution(path: os.Path, part2: Boolean): Unit = {
  val lines = os.read.lines(path)
  var count = 0
  lines.foreach {
    (line) =>
      val (elf1Start, elf1End, elf2Start, elf2End) = line match
        case s"${Section(elf1Start)}-${Section(elf1End)},${Section(elf2Start)}-${Section(elf2End)}" => (elf1Start, elf1End, elf2Start, elf2End)
        case _ => (-4,-3,-2,-1)
      if (part2 && ((elf1Start <= elf2Start && elf2Start <= elf1End) || (elf2Start <= elf1Start && elf1Start <= elf2End))) ||
        ((elf1Start <= elf2Start && elf1End >= elf2End) || (elf2Start <= elf1Start && elf2End >= elf1End)) then {
        count += 1
      }
  }
  println(count)
}