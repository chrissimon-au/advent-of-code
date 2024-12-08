// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class Check extends munit.FunSuite {
  test("Simple Case with no antinodes") {    
    assertEquals(0, countAntiNodeLocation("."))
  }
}
