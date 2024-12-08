case class Location(val col: Int, val row: Int) {}
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
  nodes.toSet.subsets(2).map(_.toList.sortBy(l => l.col * l.row)).toList

def getAntiNodes(pairs: List[Pair], gridSize: Location): List[Location] =
  pairs.flatMap((p) => {
    var delta = Location((p(0).col - p(1).col).abs, (p(0).row - p(1).row).abs)
    List(
      Location(p(0).col - delta.col, p(0).row - delta.row),
      Location(p(1).col + delta.col, p(1).row + delta.row)
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
  
