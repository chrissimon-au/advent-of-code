import scala.collection.immutable.HashMap
case class Location(val col: Int) {
}
type Frequency = Char
case class Antenna(val location: Location, val frequency: Frequency)

type Frequencies = Map[Frequency, List[Location]]

type Pair = List[Location]

def getNodes(grid: String): Frequencies =
  grid
    .zipWithIndex.map((c, i) => Antenna(Location(i), c))
    .filter(_.frequency != '.')
    .toList
    .groupMap(_.frequency)(_.location)


def getPairs(nodes: List[Location]): List[Pair] = 
  nodes.toSet.subsets(2).map(_.toList.sortBy(_.col)).toList

def getAntiNodes(pairs: List[Pair]): List[Location] =
  pairs.flatMap((p) => {
    var delta = (p(0).col - p(1).col).abs
    List(
      Location(p(0).col - delta),
      Location(p(1).col + delta)
    )
  })

@main def countAntiNodeLocation(grid: String): Int =
  val nodes = getNodes(grid)
  val pairs = nodes.map((frequency, locations) => (frequency, getPairs(locations)))
  val antiNodes = pairs.flatMap((frequency, pairs) => getAntiNodes(pairs))
  antiNodes.toSet.size
  
