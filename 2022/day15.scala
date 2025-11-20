//> using toolkit default

@main
def day15(): Unit = {
  val path = os.pwd / "day15Input.txt"
  solutionPart1(path)
  solutionPart2(path)
}

case class Sensor(val x: Int, val y: Int, val distance: Int)
case class Coord(val x: Int, val y: Int)
object Value:
  def unapply(s: String): Option[Int] = s.toIntOption

def solutionPart1(path: os.Path): Unit = {
  var lines: IndexedSeq[String] = os.read.lines(path)
  var sensors: Array[Sensor] = Array[Sensor]()
  var beacons: Array[Coord] = Array[Coord]()
  lines.foreach {
    (line) =>
      line match
        case s"Sensor at x=${Value(sensorX)}, y=${Value(sensorY)}: closest beacon is at x=${Value(beaconX)}, y=${Value(beaconY)}" => {
          val sensor = Sensor(sensorX, sensorY, (sensorX - beaconX).abs + (sensorY - beaconY).abs)
          val beacon = Coord(beaconX, beaconY)
          sensors = sensors.appended(sensor)
          beacons = beacons.appended(beacon)
        }
  }
  var beaconSpots: Map[Int,Boolean] = Map[Int,Boolean]()
  val targetRow = 2000000
  sensors.foreach {
    (sensor) =>
      val yDist = (sensor.y - targetRow).abs
      val remainDist = sensor.distance - yDist
      if remainDist >= 0 then
        for (x <- (sensor.x - remainDist) to (sensor.x + remainDist)) {
          beaconSpots = beaconSpots + (x -> false)
        }
  }
  beacons.foreach {
    (beacon) =>
      if beacon.y == targetRow then
        beaconSpots = beaconSpots + (beacon.x -> true)
  }
  println(beaconSpots.count((_,b) => b == false))
}

def solutionPart2(path: os.Path): Unit = {
  val lines = os.read.lines(path)
  var sensors: Array[Sensor] = Array[Sensor]()
  lines.foreach {
    (line) =>
      line match
        case s"Sensor at x=${Value(sensorX)}, y=${Value(sensorY)}: closest beacon is at x=${Value(beaconX)}, y=${Value(beaconY)}" => {
          val sensor = Sensor(sensorX, sensorY, (sensorX - beaconX).abs + (sensorY - beaconY).abs)
          sensors = sensors.appended(sensor)
        }
  }
  // Only one eligible spot, therefore it must be at most 1 out of range of closest sensors
  // For all 8 adjacent spots to be in range, it must be 1 out of range of at least 4 different sensors
  // For each spot 1 out of range of a sensor, count how many sensors it is out of range of
  // For all spots 1 out of range of 4 or more sensors, check if it is out of range of all sensors
  val searchLimits = 4000000
  var perimeterSpots: Map[Coord, Int] = Map[Coord,Int]()
  sensors.foreach {
    (sensor) =>
      val outOfRange = sensor.distance+1
      for (x <- Math.max(0,(sensor.x - outOfRange)) to Math.min(searchLimits,(sensor.x + outOfRange))) {
        val xOffset = (sensor.x - x).abs
        val yOffset = (outOfRange - xOffset).abs
        // Don't add ends twice
        if yOffset == 0 then {
          val perimeterSpot = Coord(x, sensor.y)
          val curCount = perimeterSpots.getOrElse(perimeterSpot, 0)
          perimeterSpots = perimeterSpots + (perimeterSpot -> (curCount + 1))
        } else {
          if sensor.y + yOffset <= searchLimits then {
            val perimeterSpotA = Coord(x, sensor.y + yOffset)
            val curCountA = perimeterSpots.getOrElse(perimeterSpotA, 0)
            perimeterSpots = perimeterSpots + (perimeterSpotA -> (curCountA + 1))
          }
          if sensor.y - yOffset >= 0 then {
            val perimeterSpotB = Coord(x, sensor.y - yOffset)
            val curCountB = perimeterSpots.getOrElse(perimeterSpotB, 0)
            perimeterSpots = perimeterSpots + (perimeterSpotB -> (curCountB + 1))
          }
        }
      }
  }
  val fourOrMoreSensors = perimeterSpots.filter((_,i) => i >= 4).keys
  // For each potential spot, check if out of range of all sensors. Only one should be
  fourOrMoreSensors.foreach {
    (spot) =>
      if sensors.forall((sensor) => (sensor.x - spot.x).abs + (sensor.y - spot.y).abs > sensor.distance) then
        println((spot.x.toLong * 4000000) + spot.y)
  }
}