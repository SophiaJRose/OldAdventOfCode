//> using toolkit default

@main
def day10(): Unit = {
  val path = os.pwd / "day10Input.txt"
  solutionPart1(path)
  solutionPart2(path)
}

object Value:
  def unapply(s: String): Option[Int] = s.toIntOption

def solutionPart1(path: os.Path): Unit = {
  val lines = os.read.lines(path)
  var x = 1
  var cycles = 0
  var signalStrengthSum = 0
  lines.foreach {
    (line) =>
      // Regardless of instruction, cycles should be incremented once and signal strength checked if on 20th/60th/etc cycle
      cycles += 1
      if ((cycles % 40) == 20) then {
        signalStrengthSum += cycles * x
      }
      line match
        case "noop" => ()
        case s"addx ${Value(value)}" => {
          // Increment cycles and check signal strength again for second cycle of addx, then update value of x
          cycles += 1
          if ((cycles % 40) == 20) then {
            signalStrengthSum += cycles * x
          }
          x += value
        }
  }
  println(signalStrengthSum)
}

def solutionPart2(path: os.Path): Unit = {
  val lines = os.read.lines(path)
  var x = 1
  var cycles = 0
  var output = ""
  lines.foreach {
    (line) =>
      // Regardless of instruction, pixel should be drawn and cycles should be incremented once
      if (x == (cycles % 40) - 1 || x == (cycles % 40) || x == (cycles % 40) + 1) then {
        output = output ++ "#"
      } else {
        output = output ++ "."
      }
      cycles += 1
      line match
        case "noop" => ()
        case s"addx ${Value(value)}" => {
          // Draw pixel and increment cycles again for second cycle of addx, then update value of x
          if (x == (cycles % 40) - 1 || x == (cycles % 40) || x == (cycles % 40) + 1) then {
            output = output ++ "#"
          } else {
            output = output ++ "."
          }
          cycles += 1
          x += value
        }
  }
  var outputLines = output.grouped(40)
  outputLines.foreach((line) => println(line))
}