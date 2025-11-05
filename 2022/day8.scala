//> using toolkit default

@main
def day8(): Unit = {
  val path = os.pwd / "day8Input.txt"
  solutionPart1(path)
  solutionPart2(path)
}

def solutionPart1(path: os.Path): Unit = {
  val lines = os.read.lines(path)
  var grid: Array[Array[Int]] = lines.map((line) => line.toArray.map((x) => x.asDigit)).toArray
  var visible: Array[Array[Boolean]] = grid.map((row) => row.map(_ => false))
  val height = grid.size
  val width = grid(0).size
  // Find visible trees by checking left-right
  for (i <- 0 until height) {
    for (j <- 0 until width) {
      // Mark edges as visible
      if (i == 0 || i == height-1 || j == 0 || j == width-1) then
        visible(i)(j) = true
      else {
        val curTree = grid(i)(j)
        if grid(i).take(j).forall((tree) => tree < curTree) || grid(i).drop(j + 1).forall((tree) => tree < curTree) then {
          visible(i)(j) = true
        }
      }
    }
  }
  // Transpose and then do same check to check up-down
  grid = grid.transpose
  visible = visible.transpose
  // Can skip edges this time
  for (i <- 1 until height-1) {
    for (j <- 1 until width-1) {
      val curTree = grid(i)(j)
      if grid(i).take(j).forall((tree) => tree < curTree) || grid(i).drop(j+1).forall((tree) => tree < curTree) then {
        visible(i)(j) = true
      }
    }
  }
  println(visible.map((row) => row.count((tree) => tree == true)).sum)
}

def solutionPart2(path: os.Path): Unit = {
  val lines = os.read.lines(path)
  var grid: Array[Array[Int]] = lines.map((line) => line.toArray.map((x) => x.asDigit)).toArray
  var scores: Array[Array[Int]] = grid.map((row) => row.map(_ => 0))
  val height = grid.size
  val width = grid(0).size
  for (i <- 0 until height) {
    for (j <- 0 until width) {
      // Check up
      var upScore = 0
      var i2 = i-1
      var viewBlocked = false
      while (i2 >= 0 && !viewBlocked) {
        upScore += 1
        viewBlocked = grid(i2)(j) >= grid(i)(j)
        i2 -= 1
      }
      // Check down
      var downScore = 0
      i2 = i+1
      viewBlocked = false
      while (i2 < height && !viewBlocked) {
        downScore += 1
        viewBlocked = grid(i2)(j) >= grid(i)(j)
        i2 += 1
      }
      // Check left
      var leftScore = 0
      var j2 = j-1
      viewBlocked = false
      while (j2 >= 0 && !viewBlocked) {
        leftScore += 1
        viewBlocked = grid(i)(j2) >= grid(i)(j)
        j2 -= 1
      }
      // Check right
      var rightScore = 0
      j2 = j+1
      viewBlocked = false
      while (j2 < width && !viewBlocked) {
        rightScore += 1
        viewBlocked = grid(i)(j2) >= grid(i)(j)
        j2 += 1
      }
      scores(i)(j) = upScore * downScore * leftScore * rightScore
    }
  }
  println(scores.map((row) => row.max).max)
}