namespace AoC.Day09;
using System.Linq;

public static class Day09
{
    private static IEnumerable<char> ExpandDiskMap(string diskMap) {
        var files = diskMap.Where((_, i) => i % 2 == 0);
        return files;
    }
    public static long CompactAndGetChecksum(string diskMap)
    {
        var expandedDiskMap = ExpandDiskMap(diskMap);
        return expandedDiskMap.Select((c, i) => (c - '0') * i).Sum();
    }
}