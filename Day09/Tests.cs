namespace AoC.Day09;

public class Tests
{
    [Theory()]
    [InlineData("1", 0)]
    [InlineData("101", 1)]
    [InlineData("10101", 5)]
    [InlineData("11101", 4)]
    [InlineData("131221", 9)]
    [InlineData("101000000000000000001", 21)]
    public void test_FileCompaction(string diskMap, long expectedChecksum)
    {
        Assert.Equal(expectedChecksum, Day09.CompactAndGetChecksum(diskMap));
    }

    [Theory()]
    [InlineData("sample")]
    [InlineData("test")]
    public void test_FileCompactionFromTestFiles(string fileBase)
    {
        var diskMap = System.IO.File.ReadAllText($"{fileBase}data.txt");
        var expectedChecksum = Convert.ToInt64(File.ReadAllText($"{fileBase}data.answer.txt"));
        Assert.Equal(expectedChecksum, Day09.CompactAndGetChecksum(diskMap));
    }
    
    [Theory()]
    [InlineData("1", 0)]
    public void test_FullFileCompaction(string diskMap, long expectedChecksum)
    {
        Assert.Equal(expectedChecksum, Day09.CompactFullFilesAndGetChecksum(diskMap));
    }
}
