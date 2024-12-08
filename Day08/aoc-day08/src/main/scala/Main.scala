class Location(var col: Int, var frequency: Char) {
}

def getNodes(grid: String): List[Location] =
  grid
    .zipWithIndex.map((c, i) => Location(i, c))
    .filter(_.frequency != '.')
    .toList


@main def countAntiNodeLocation(grid: String): Int =
  var nodes = getNodes(grid)
  nodes.size
