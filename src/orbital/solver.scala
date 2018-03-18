package orbital

/**
 * @author Timo Haario
 */
import scala.io.Source
import scala.math.{sin, cos, Pi, sqrt}
object solver extends App {

  /**
   * Read the file and assign start, end and all the satellites into
   * a dictionary satellites: Map[String, Satellite]
   */
  val lines = Source.fromFile("src/generate").getLines.toArray
  val seed = lines(0).drop(7).toDouble
  val routeLine = lines.last.split(",").tail
  val startPoint: Point = toXYZ(routeLine(0).toDouble, routeLine(1).toDouble, 0)
  val endPoint: Point = toXYZ(routeLine(2).toDouble, routeLine(3).toDouble, 0)
  val start: Satellite = Satellite("Start", startPoint)
  val end: Satellite = Satellite("End", endPoint)
  /**
   * Dictionary of type String -> Satellite
   * key: name of a satellite (start and end points are considered as satellites too)
   * value: the satellite object that the key represents
   */
  val satellites: Map[String, Satellite] =
    lines.tail.dropRight(1) //leave the seed and route lines out
      .map(line => line.split(","))
      .map(arr => (arr(0), Satellite(arr(0), toXYZ(arr(1).toDouble, arr(2).toDouble, arr(3).toDouble)))) //form satellite objects
      .++(Array((start.name, start), (end.name, end))) //add start and end points
      .toMap

  /**
   * Dictionary of type String -> Array[Satellite]
   * Key: name of a satellite (start and end points are considered as satellites too)
   * Value: All satellites that have an unobstructed line of sight with the key satellite
   */
  val satelliteNeighbors: Map[String, Array[Satellite]] = {
    satellites
      .map(keySat => (keySat._1, satellites
        .map(pair => pair._2)
        .filter(sat => sat.name != keySat._1 && distancePointToSegment(keySat._2.location, sat.location) > 6370.toDouble)
        .toArray))
  }

  /**
   * Converts latitude/longitude/altitude coordinates into points of XYZ form
   * @return a point in Cartesian coordinate system
   */
  def toXYZ(latitude: Double, longitude: Double, alt: Double): Point = {
    val lat = latitude * (Pi / 180)
    val lon = longitude * (Pi / 180)
    val x = (6371 + alt) * cos(lat) * cos(lon)
    val y = (6371 + alt) * sin(lon) * cos(lat)
    val z = (6371 + alt) * sin(lat)
    return new Point(x, y, z)
  }

  /**
   * Calculate distance from center of earth to given line segment ab
   * @param a start of line segment ab
   * @param b end of line segment ab
   * @return smallest distance from center of earth to line segment ab
   */
  def distancePointToSegment(a: Point, b: Point): Double = {

    val center = new Point(0, 0, 0)

    val v: Vector = a - b
    val w: Vector = center - b

    val case1: Double = w dotProduct v
    if (case1 <= 0) return center distance b

    val case2: Double = v dotProduct v
    if (case2 <= case1) return center distance a

    val case3: Double = case1 / case2
    val PointCase3: Point = b + (v * case3)
    return center distance PointCase3
  }

  /**
   * Simple breadth first search for finding a route with least steps
   * @return path of satellites from start to end if a path was found, otherwise just start satellite
   */
  def BFS(): List[Satellite] = {

    val visited = scala.collection.mutable.HashSet[Satellite](satellites("Start"))
    var frontier: Set[(Satellite, List[Satellite])] = Set((satellites("Start"), List(satellites("Start"))))

    while (frontier.nonEmpty) {
      if (frontier.map(sat => sat._1.name).contains("End")) {
        return frontier.filter(endPoint => endPoint._1.name == "End").head._2
      }
      var nextFrontier: Set[(Satellite, List[Satellite])] = Set()
      for (sat <- frontier) {
        for (neighbor <- satelliteNeighbors(sat._1.name)) {
          if (!visited.contains(neighbor)) nextFrontier += ((neighbor, sat._2 ++ List(neighbor)))
        }
        visited ++= nextFrontier.map(x => x._1)
        frontier = nextFrontier
      }
    }
    List(satellites("Start"))
  }

  val finalPath: List[Satellite] = BFS
  println("Path: " + finalPath.tail.dropRight(1).map(x => x.name))

}

/**
 * Represents a satellite in space.
 * @param s name of satellite
 * @param arr coordinates of the satellite as Point object
 */
case class Satellite(s: String, loc: Point) {

  val name = s
  val location = loc

  override def toString = name.toString + " " + location.toString
}

/**
 * Helper class for 3d points in space
 */

case class Point(xx: Double, yy: Double, zz: Double) {

  val (x, y, z) = (xx, yy, zz)

  def +(that: Vector): Point = Point(this.x + that.x, this.y + that.y, this.z + that.z)
  def -(that: Vector): Point = Point(this.x - that.x, this.y - that.y, this.z - that.z)
  def -(that: Point): Vector = Vector(this.x - that.x, this.y - that.y, this.z - that.z)
  def distance(that: Point): Double = (this - that).norm

  override def toString = "[x: " + x.toString + ", " + "y: " + y.toString + ", " + "z: " + z.toString + "]"
}

/**
 *  Helper class for 3d vectors in space
 */

case class Vector(xx: Double, yy: Double, zz: Double) {
  val (x, y, z) = (xx, yy, zz)

  //scalar product
  def *(scalar: Double): Vector = Vector(this.x * scalar, this.y * scalar, this.z * scalar)
  def -(that: Vector): Vector = Vector(this.x - that.x, this.y - that.y, this.z - that.z)

  def dotProduct(that: Vector): Double = (this.x * that.x + this.y * that.y + this.z * that.z)
  def norm: Double = sqrt(this.dotProduct(this))

  override def toString = "[x: " + x.toString + ", " + "y: " + y.toString + ", " + "z: " + z.toString + "]"
}