// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class Check extends munit.FunSuite {
  test("Simple Case with no antinodes") {    
    assertEquals(0, countAntiNodeLocation("."))
  }

  test("Flat row with single frequency no overlapping antinodes") {    
    assertEquals(2, countAntiNodeLocation("...a.a..."))
  }

  test("Flat row with single frequency more than one pair, no overlapping antinodes") {    
    assertEquals(12, countAntiNodeLocation(".....a..a.a..a....."))
  }

  test("Flat row with single frequency more than one pair, overlapping antinodes") {    
    assertEquals(9, countAntiNodeLocation("....aa.aa...."))
  }

  test("Flat row with two frequencies") {    
    assertEquals(4, countAntiNodeLocation("....a.a.A.A...."))
  }

  test("Multi-dimensional grid") {    
    assertEquals(3, countAntiNodeLocation("""
    ..............
    ..............
    ...o..........
    ......o.......
    .......xx.....
    ..............
    """.stripIndent.trim))
  }

  test("Antinodes outside grid") {    
    assertEquals(1, countAntiNodeLocation("""
    ..............
    ...o..........
    ......o.......
    """.stripIndent.trim))
  }
}
