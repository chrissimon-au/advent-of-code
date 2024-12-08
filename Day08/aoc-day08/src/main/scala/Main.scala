case class Location(val col: Int, val row: Int) {
  def -(that: Location): Location =
    new Location(this.col - that.col, this.row - that.row)
  def +(that: Location): Location =
    new Location(this.col + that.col, this.row + that.row)
}
type Frequency = Char
case class Antenna(val location: Location, val frequency: Frequency)

type Frequencies = Map[Frequency, List[Location]]

type Pair = List[Location]

def getNodes(grid: String): (Frequencies, Location) =
  val gridMap = grid
    .split(System.lineSeparator())
  (gridMap.zipWithIndex.map((row, rowIdx) => {
    row
      .zipWithIndex.map((c, colIdx) => Antenna(Location(colIdx, rowIdx), c))
      .filter(_.frequency != '.')
      .toList
  }).flatMap(a => a).toList
  .groupMap(_.frequency)(_.location),Location(gridMap(0).size,gridMap.size))

def getPairs(nodes: List[Location]): List[Pair] = 
  nodes.toSet.subsets(2).map(_.toList).toList

def getAntiNodes(pairs: List[Pair], gridSize: Location): List[Location] =
  pairs.flatMap((p) => {
    var delta = p(1) - p(0)
    List(
      p(0) - delta,
      p(1) + delta
    )
  }).filter(l => l.col >= 0 && l.col < gridSize.col && l.row >= 0 && l.row < gridSize.row)

@main def countAntiNodeLocation(grid: String): Int =
  val (nodes, gridSize) = getNodes(grid)
  val pairs = nodes.map((frequency, locations) => (frequency, getPairs(locations)))
  println(grid)
  println(gridSize)
  pairs.foreach(println)
  val antiNodes = pairs.flatMap((frequency, pairs) => getAntiNodes(pairs, gridSize))
  println("---")
  antiNodes.foreach(println)
  antiNodes.toSet.size
  
