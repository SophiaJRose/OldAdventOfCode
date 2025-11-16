//> using toolkit default

@main
def day13(): Unit = {
  val path = os.pwd / "day13Input.txt"
  solutionPart1(path)
  solutionPart2(path)
}

def solutionPart1(path: os.Path): Unit = {
  val lines = os.read.lines(path)
  val pairs = lines.grouped(3).toArray
  var totalIndices = 0
  for (i <- 0 until pairs.size) {
    val packet1 = pairs(i)(0)
    val packet2 = pairs(i)(1)
    if rightOrder(packet1, packet2) then {
      totalIndices += i+1
    } else
      ()
  }
  println(totalIndices)
}

def solutionPart2(path: os.Path): Unit = {
  val lines = os.read.lines(path)
  var packets: IndexedSeq[String] = lines.filter((l) => l != "")
  packets = packets.appendedAll(Array[String]("[[2]]","[[6]]"))
  packets = packets.sortWith(rightOrder)
  val dividerIndex1 = packets.indexOf("[[2]]")+1
  val dividerIndex2 = packets.indexOf("[[6]]")+1
  println(dividerIndex1 * dividerIndex2)
}

def rightOrder(packetA: String, packetB: String): Boolean = {
  // Convert parameters to vars
  var packet1: String = packetA
  var packet2: String = packetB
  while packet1.nonEmpty && packet2.nonEmpty do {
    // Match on empty string for better readability then if-then-else chain
    "" match
      case "" if packet1.head == '[' && packet2.head == '[' => {
        // Entering list
        packet1 = packet1.tail
        packet2 = packet2.tail
      }
      case "" if packet1.head == ']' && packet2.head == ']' => {
        // Exiting list
        packet1 = packet1.tail
        packet2 = packet2.tail
      }
      case "" if packet1.head == ',' && packet2.head == ',' => {
        // Go to next item
        packet1 = packet1.tail
        packet2 = packet2.tail
      }
      case "" if packet1.head.isDigit && packet2.head.isDigit => {
        // Compare numbers
        val num1 = packet1.takeWhile((c) => c.isDigit).toInt
        val num2 = packet2.takeWhile((c) => c.isDigit).toInt
        if num1 < num2 then
          return true
        else if num2 < num1 then
          return false
        packet1 = packet1.dropWhile((c) => c.isDigit)
        packet2 = packet2.dropWhile((c) => c.isDigit)
      }
      // One list has number while other has list
      case "" if packet1.head.isDigit && packet2.head == '[' => {
        // Add ']' at end of number in packet 1, enter list in packet 2
        packet1 = packet1.takeWhile((c) => c.isDigit) ++ "]" ++ packet1.dropWhile((c) => c.isDigit)
        packet2 = packet2.tail
      }
      case "" if packet1.head == '[' && packet2.head.isDigit => {
        // Enter list in packet 1, add ']' at end of number in packet 2
        packet1 = packet1.tail
        packet2 = packet2.takeWhile((c) => c.isDigit) ++ "]" ++ packet2.dropWhile((c) => c.isDigit)
      }
      // One list ends before other
      case "" if packet1.head != ']' && packet2.head == ']' => {
        // If packet2 ends list first, wrong order
        return false
      }
      case "" if packet1.head == ']' && packet2.head != ']' => {
        // If packet1 ends list first, right order
        return true
      }
      case _ => ()
  }
  // If packet1 still not empty, wrong order, otherwise right order
  return !packet1.nonEmpty
}