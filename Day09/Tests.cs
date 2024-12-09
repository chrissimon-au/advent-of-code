namespace AoC.Day09;

public class Tests
{
    [Theory()]
    [InlineData("1", 0)]
    [InlineData("101", 1)]
    [InlineData("10101", 5)]
    [InlineData("11101", 4)]
    [InlineData("131221", 9)]
    public void test_FileCompaction(string diskMap, long expectedChecksum)
    {
        Assert.Equal(expectedChecksum, Day09.CompactAndGetChecksum(diskMap));
    }
}
