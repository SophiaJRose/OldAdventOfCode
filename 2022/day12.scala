//> using toolkit default

@main
def day12(): Unit = {
  val path = os.pwd / "day12Input.txt"
  solution(path, false)
  solution(path, true)
}

case class Coord(val i: Int, val j: Int)
class Node(var position: Coord, var height: Int, var up: Option[Node], var down: Option[Node], var left: Option[Node], var right: Option[Node], var distance: Int = Int.MaxValue)

def solution(path: os.Path, part2: Boolean): Unit = {
  val lines = os.read.lines(path)
  var grid: Array[Array[Int]] = Array[Array[Int]]()
  var start: Coord = Coord(0,0)
  var end: Coord = Coord(0,0)
  for (i <- 0 until lines.size) {
    var row: Array[Int] = Array[Int]()
    for (j <- 0 until lines(i).size) {
      var char = lines(i)(j)
      if char == 'S' then {
        start = Coord(i,j)
        row = row.appended(1)
      } else if char == 'E' then {
        end = Coord(i,j)
        row = row.appended(26)
      } else
        row = row.appended(char.toInt - '`'.toInt)
    }
      grid = grid.appended(row)
  }
  // Build graph
  var startNode = Node(start, 1, None, None, None, None, 0)
  var graph: Array[Node] = Array[Node](startNode)
  var queue: Array[Node] = Array[Node](startNode)
  while !queue.isEmpty do {
    var curNode = queue.head
    queue = queue.tail
    val i = curNode.position.i
    val j = curNode.position.j
    // Check up
    if curNode.up == None && i != 0 then {
      var upCoord = Coord(i-1, j)
      var heightDiff = grid(i-1)(j) - curNode.height
      if heightDiff <= 1 then {
        var existingUpNode = graph.find((n) => n.position == upCoord)
        existingUpNode match
          case Some(upNode) => curNode.up = Some(upNode)
          case None => {
            var upNode = Node(upCoord, grid(i-1)(j), None, None, None, None)
            // Set two-way connection if height diff allows it
            if heightDiff >= -1 then
              upNode.down = Some(curNode)
            curNode.up = Some(upNode)
            graph = graph.appended(upNode)
            queue = queue.appended(upNode)
          }
      }
    }
    // Check down
    if curNode.down == None && i != grid.size-1 then {
      var downCoord = Coord(i+1, j)
      var heightDiff = grid(i+1)(j) - curNode.height
      if heightDiff <= 1 then {
        var existingDownNode = graph.find((n) => n.position == downCoord)
        existingDownNode match
          case Some(downNode) => curNode.down = Some(downNode)
          case None => {
            var downNode = Node(downCoord, grid(i + 1)(j), None, None, None, None)
            // Set two-way connection if height diff allows it
            if heightDiff >= -1 then
              downNode.up = Some(curNode)
            curNode.down = Some(downNode)
            graph = graph.appended(downNode)
            queue = queue.appended(downNode)
          }
      }
    }
    // Check left
    if curNode.left == None && j != 0 then {
      var leftCoord = Coord(i, j-1)
      var heightDiff = grid(i)(j-1) - curNode.height
      if heightDiff <= 1 then {
        var existingLeftNode = graph.find((n) => n.position == leftCoord)
        existingLeftNode match
          case Some(leftNode) => curNode.left = Some(leftNode)
          case None => {
            var leftNode = Node(leftCoord, grid(i)(j - 1), None, None, None, None)
            // Set two-way connection if height diff allows it
            if heightDiff >= -1 then
              leftNode.right = Some(curNode)
            curNode.left = Some(leftNode)
            graph = graph.appended(leftNode)
            queue = queue.appended(leftNode)
          }
      }
    }
    // Check right
    if curNode.right == None && j != grid(0).size-1 then {
      var rightCoord = Coord(i, j+1)
      var heightDiff = grid(i)(j+1) - curNode.height
      if heightDiff <= 1 then {
        var existingRightNode = graph.find((n) => n.position == rightCoord)
        existingRightNode match
          case Some(rightNode) => curNode.right = Some(rightNode)
          case None => {
            var rightNode = Node(rightCoord, grid(i)(j + 1), None, None, None, None)
            // Set two-way connection if height diff allows it
            if heightDiff >= -1 then
              rightNode.left = Some(curNode)
            curNode.right = Some(rightNode)
            graph = graph.appended(rightNode)
            queue = queue.appended(rightNode)
          }
      }
    }
  }
  // In part 2, all nodes with height 1 have distance 0
  if part2 then {
    graph.foreach {
      (node) =>
        if node.height == 1 then
          node.distance = 0
    }
  }
  // Dijkstra's algorithm
  var unvisited: Array[Node] = graph
  var visited: Array[Node] = Array[Node]()
  while !unvisited.isEmpty do {
    unvisited = unvisited.sortWith((a, b) => a.distance < b.distance)
    var curNode = unvisited.head
    var upOption: Option[Node] = curNode.up
    upOption match
      case Some(upNode) => {
        var newUpDistance = curNode.distance + 1
        if newUpDistance < upNode.distance then
          upNode.distance = newUpDistance
      }
      case None => ()
    var downOption: Option[Node] = curNode.down
    downOption match
      case Some(downNode) => {
        var newDownDistance = curNode.distance + 1
        if newDownDistance < downNode.distance then
          downNode.distance = newDownDistance
      }
      case None => ()
    var leftOption: Option[Node] = curNode.left
    leftOption match
      case Some(leftNode) => {
        var newLeftDistance = curNode.distance + 1
        if newLeftDistance < leftNode.distance then
          leftNode.distance = newLeftDistance
      }
      case None => ()
    var rightOption: Option[Node] = curNode.right
    rightOption match
      case Some(rightNode) => {
        var newRightDistance = curNode.distance + 1
        if newRightDistance < rightNode.distance then
          rightNode.distance = newRightDistance
      }
      case None => ()
    unvisited = unvisited.tail
    visited = visited.appended(curNode)
  }
  val endOption = graph.find((node) => node.position == end)
  endOption match
    case Some(endNode) => println(endNode.distance)
    case None => ()
}