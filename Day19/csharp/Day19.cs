namespace Day19;

public class Day19
{

    long CountWaysToMakeDisplay(string input) {
        return 0;
    }

    [Theory()]
    [InlineData(@"a, b, c, ab, bc

abc
cba", 4)]
    public void TestCountWaysToMakeDisplay(string input, long expected)
    {
        Assert.Equal(expected, CountWaysToMakeDisplay(input));
    }
}