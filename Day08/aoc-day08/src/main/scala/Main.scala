import scala.collection.mutable.ListBuffer

implicit class RichPipes[Y](y: Y) {
  def |>[Z](f: Y => Z) = f(y)
}

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

type AntennaLocation = Vector
type AntiNodeLocation = Vector
type GridMap = Array[String]
type MapSize = Vector
type FrequencyAntennae = Map[Frequency, List[AntennaLocation]]
type AntennaPair = List[AntennaLocation]
type FrequencyAntennaPairs = Map[Frequency, List[AntennaPair]]
type AntiNodeAlgorithm = (List[AntennaPair], MapSize) => List[AntiNodeLocation]

class AntennaMap(val size: MapSize, val frequencyAntennaPairs: FrequencyAntennaPairs) {
  def getAntiNodes(antiNodeAlgorithm: AntiNodeAlgorithm): Set[AntiNodeLocation] =
    frequencyAntennaPairs.flatMap((frequency, antennaPairs) => antiNodeAlgorithm(antennaPairs, size)).toSet
}

object AntennaMap {

  private def inputToMap(input: String): GridMap = 
    input    
      .split(System.lineSeparator())      

  private def antennae(map: GridMap): FrequencyAntennae =
    map.zipWithIndex.map((row, rowIdx) => {
      row
        .zipWithIndex.map((c, colIdx) => Antenna(Vector(colIdx, rowIdx), c))
        .filter(_.frequency != '.')
        .toList
    })
      .flatMap(a => a).toList
      .groupMap(_.frequency)(_.location)

  private def getPairs(nodes: List[AntennaLocation]): List[AntennaPair] = 
    nodes.toSet.subsets(2).map(_.toList).toList

  private def frequencyPairs(antennas: FrequencyAntennae): FrequencyAntennaPairs = 
    antennas.map((frequency, locations) => (frequency, getPairs(locations)))

  def apply(input: String): AntennaMap =
    val map = inputToMap(input)
    val fPairs = map
      |> antennae
      |> frequencyPairs
    new AntennaMap(Vector(map(0).size,map.size), fPairs)
}

def getAntiNodes(pairs: List[AntennaPair], size: MapSize): List[AntiNodeLocation] =
  pairs.flatMap((p) => {
    var delta = p(1) - p(0)
    List(
      p(0) - delta,
      p(1) + delta
    )
  }).filter(l => l.isInMap(size))

def getHarmonicAntiNodes(pairs: List[AntennaPair], size: MapSize): List[AntiNodeLocation] =
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