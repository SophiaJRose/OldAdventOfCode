//> using toolkit default

@main
def day7(): Unit = {
  val path = os.pwd / "day7Input.txt"
  solution(path, false)
  solution(path, true)
}

abstract class Node(val name: String, val parent: Option[Directory])
class File(override val name: String, override val parent: Option[Directory], val size: Int) extends Node(name, parent)
class Directory(override val name: String, override val parent: Option[Directory]) extends Node(name, parent) {
  var files: Array[File] = Array[File]()
  var subDirs: Array[Directory] = Array[Directory]()
  var totalSize: Int = 0
  def calcSize: Unit = {
    totalSize = files.map((file) => file.size).sum
    subDirs.foreach((subdir) => {
      subdir.calcSize
      totalSize += subdir.totalSize
    })
  }
}

object Size:
  def unapply(s: String): Option[Int] = s.toIntOption

def solution(path: os.Path, part2: Boolean): Unit = {
  val lines = os.read.lines(path)
  var root = Directory("/", None)
  var currentDir = root
  // Create file tree
  lines.foreach {
    (line) =>
      line match
        case "$ cd /" => currentDir = root
        case "$ cd .." => currentDir = currentDir.parent.get
        case s"$$ cd ${dirName}" => currentDir = currentDir.subDirs.find((dir) => dir.name == dirName).get
        case s"dir ${dirName}" => {
          var dir = Directory(dirName, Option(currentDir))
          currentDir.subDirs = currentDir.subDirs.appended(dir)
        }
        case s"${Size(fileSize)} ${fileName}" => {
          var file = File(fileName, Option(currentDir), fileSize)
          currentDir.files = currentDir.files.appended(file)
        }
        case _ => ()
  }
  root.calcSize
  if !part2 then {
    // Part 1 size checks
    var sumLessThan100k = 0
    var queue: Array[Directory] = Array[Directory](root)
    while (queue.size != 0) {
      val cur = queue.head
      if (cur.totalSize < 100000) then {
        sumLessThan100k += cur.totalSize
      }
      queue = queue.tail.appendedAll(cur.subDirs)
    }
    println(sumLessThan100k)
  } else {
    // Part 2 size checks
    val currentUnused = 70000000 - root.totalSize
    val spaceToBeCleared = 30000000 - currentUnused
    var minClearSize = Int.MaxValue
    var queue: Array[Directory] = Array[Directory](root)
    while (queue.size != 0) {
      val cur = queue.head
      if (cur.totalSize > spaceToBeCleared && cur.totalSize < minClearSize) {
        minClearSize = cur.totalSize
      }
      queue = queue.tail.appendedAll(cur.subDirs)
    }
    println(minClearSize)
  }
}