case class Vector(val col: Int, val row: Int) {
  def -(that: Vector): Vector =
    new Vector(this.col - that.col, this.row - that.row)
  def +(that: Vector): Vector =
    new Vector(this.col + that.col, this.row + that.row)
  def >=(that: Vector): Boolean =
    this.col >= that.col && this.row >= that.row
  def <(that: Vector): Boolean = 
    this.col < that.col && this.row < that.row
}

def Origin = new Vector(0,0)  

type Frequency = Char

case class Antenna(val location: Vector, val frequency: Frequency)

class AntennaMap(val size: Vector, val pairs: Map[Frequency, List[Pair]]) {
  
  def getAntiNodes(antiNodeAlgorithm: (List[Pair], Vector) => List[Vector]): Set[Vector] =
    pairs.flatMap((frequency, pairs) => antiNodeAlgorithm(pairs, size)).toSet
  
}

object AntennaMap {

  def antennas(map: Array[String]): Frequencies =
    map.zipWithIndex.map((row, rowIdx) => {
      row
        .zipWithIndex.map((c, colIdx) => Antenna(Vector(colIdx, rowIdx), c))
        .filter(_.frequency != '.')
        .toList
    })
      .flatMap(a => a).toList
      .groupMap(_.frequency)(_.location)

  def pairs(antennas: Frequencies): Map[Frequency, List[Pair]] = 
    antennas.map((frequency, locations) => (frequency, getPairs(locations)))

  def apply(input: String): AntennaMap =
    val map = input
      .split(System.lineSeparator())
    new AntennaMap(Vector(map(0).size,map.size), pairs(antennas(map)))
}

type Frequencies = Map[Frequency, List[Vector]]

type Pair = List[Vector]

def getPairs(nodes: List[Vector]): List[Pair] = 
  nodes.toSet.subsets(2).map(_.toList).toList

def getPart1AntiNodes(pairs: List[Pair], gridSize: Vector): List[Vector] =
  pairs.flatMap((p) => {
    var delta = p(1) - p(0)
    List(
      p(0) - delta,
      p(1) + delta
    )
  }).filter(l => l >= Origin && l < gridSize)
  

@main
def countAntiNodeLocation(input: String): Int =
  val antennaMap = AntennaMap(input)
  val antiNodes = antennaMap.getAntiNodes(getPart1AntiNodes)
  antiNodes.size