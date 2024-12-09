namespace AoC.Day09;

public class Tests
{
    [Theory()]
    [InlineData("1", 0)]
    [InlineData("101", 1)]
    public void test_FileCompaction(string diskMap, long expectedChecksum)
    {
        Assert.Equal(expectedChecksum, Day09.CompactAndGetChecksum(diskMap));
    }
}
