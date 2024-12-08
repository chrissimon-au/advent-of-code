class Location(var col: Int, var frequency: Char) {
}

def factorial(n: Int): Int = {  
    var f = 1
    for(i <- 1 to n)
    {
        f = f * i;
    }
    return f
}

def getNodes(grid: String): List[Location] =
  grid
    .zipWithIndex.map((c, i) => Location(i, c))
    .filter(_.frequency != '.')
    .toList


@main def countAntiNodeLocation(grid: String): Int =
  var nodes = getNodes(grid)
  var n = nodes.size
  var divisor = if (n-2 > 0) factorial(n-2) else 1
  factorial(n) / (factorial(2) * divisor) * 2
