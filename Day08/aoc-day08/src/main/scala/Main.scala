import scala.collection.mutable.ListBuffer
case class Vector(val col: Int, val row: Int) {
  def -(that: Vector): Vector =
    new Vector(this.col - that.col, this.row - that.row)
  def +(that: Vector): Vector =
    new Vector(this.col + that.col, this.row + that.row)
  def >=(that: Vector): Boolean =
    this.col >= that.col && this.row >= that.row
  def <(that: Vector): Boolean = 
    this.col < that.col && this.row < that.row
  def isInMap(size: Vector): Boolean = 
    this >= Vector.Origin && this < size  
}

object Vector {
  val Origin = new Vector(0,0)
}

type Frequency = Char

case class Antenna(val location: Vector, val frequency: Frequency)

type FrequencyAntennas = Map[Frequency, List[Vector]]
type Pair = List[Vector]
type FrequencyAntennaPairs = Map[Frequency, List[Pair]]
type AntiNodeAlgorithm = (List[Pair], Vector) => List[Vector]

class AntennaMap(val size: Vector, val frequencyAntennaPairs: FrequencyAntennaPairs) {
  def getAntiNodes(antiNodeAlgorithm: AntiNodeAlgorithm): Set[Vector] =
    frequencyAntennaPairs.flatMap((frequency, antennaPairs) => antiNodeAlgorithm(antennaPairs, size)).toSet
}

object AntennaMap {

  private def antennas(map: Array[String]): FrequencyAntennas =
    map.zipWithIndex.map((row, rowIdx) => {
      row
        .zipWithIndex.map((c, colIdx) => Antenna(Vector(colIdx, rowIdx), c))
        .filter(_.frequency != '.')
        .toList
    })
      .flatMap(a => a).toList
      .groupMap(_.frequency)(_.location)

  private def getPairs(nodes: List[Vector]): List[Pair] = 
    nodes.toSet.subsets(2).map(_.toList).toList

  private def frequencyPairs(antennas: FrequencyAntennas): FrequencyAntennaPairs = 
    antennas.map((frequency, locations) => (frequency, getPairs(locations)))

  def apply(input: String): AntennaMap =
    val map = input
      .split(System.lineSeparator())
    new AntennaMap(Vector(map(0).size,map.size), frequencyPairs(antennas(map)))
}

def getAntiNodes(pairs: List[Pair], size: Vector): List[Vector] =
  pairs.flatMap((p) => {
    var delta = p(1) - p(0)
    List(
      p(0) - delta,
      p(1) + delta
    )
  }).filter(l => l.isInMap(size))

def getHarmonicAntiNodes(pairs: List[Pair], size: Vector): List[Vector] =
  pairs.flatMap((p) => {
    val delta = p(1) - p(0)
    var n = p(0)
    var l = ListBuffer[Vector]()
    while (n.isInMap(size)) {
      l = l :+ n
      n = n - delta
    }
    n = p(1)
    while (n.isInMap(size)) {
      l = l :+ n
      n = n + delta
    }
    l
  })

@main
def countAntiNodeLocation(input: String): Int =
  val antennaMap = AntennaMap(input)
  antennaMap
    .getAntiNodes(getAntiNodes)
    .size


def countHarmonicAntiNodeLocation(input: String): Int =
  val antennaMap = AntennaMap(input)
  antennaMap
    .getAntiNodes(getHarmonicAntiNodes)
    .size