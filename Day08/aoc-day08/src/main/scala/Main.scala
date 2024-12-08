case class Location(val col: Int, val frequency: Char) {
}

type Pair = List[Location]

def getNodes(grid: String): List[Location] =
  grid
    .zipWithIndex.map((c, i) => Location(i, c))
    .filter(_.frequency != '.')
    .toList

def getPairs(nodes: List[Location]): List[Pair] = 
  nodes.toSet.subsets(2).map(_.toList.sortBy(_.col)).toList

def getAntiNodes(pairs: List[Pair]): List[Location] =
  pairs.flatMap((p) => {
    var delta = (p(0).col - p(1).col).abs
    List(
      Location(p(0).col - delta, p(0).frequency),
      Location(p(1).col + delta, p(1).frequency)
    )
  })

@main def countAntiNodeLocation(grid: String): Int =
  getAntiNodes(getPairs(getNodes(grid))).toSet.size
  
