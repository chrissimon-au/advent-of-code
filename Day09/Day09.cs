namespace AoC.Day09;
using System.Linq;

public static class Day09
{
    private static IEnumerable<char> ExpandDiskMap(string diskMap) {
        if (diskMap.Length % 2 == 1)
        {
            diskMap += "0";
        }
        var files = diskMap.Where((_, i) => i % 2 == 0);
        var fileAllocation = files.Select((fileSize, fileId) => Enumerable.Repeat(fileId.ToString()[0], fileSize-'0'));
        var spaces = diskMap.Where((_, i) => i % 2 == 1);
        var spaceAllocation = spaces.Select((spaceSize) => Enumerable.Repeat('.', spaceSize - '0'));
        var expanededLayout = fileAllocation.Zip(spaceAllocation).SelectMany((entry) => entry.First.Concat(entry.Second));        
        return expanededLayout;
    }
    public static long CompactAndGetChecksum(string diskMap)
    {
        var expandedDiskMap = ExpandDiskMap(diskMap).ToList();
        var len = expandedDiskMap.Count;
        var compressedBlocks = 0;
        var compressedDiskMap = expandedDiskMap.Select((c, i) =>
        {
            if (c == '.')
            {
                compressedBlocks++;
                return expandedDiskMap[len - compressedBlocks];
            }
            else
            {
                return c;
            }
        }).ToList();
        var activeBlocks = compressedDiskMap.Take(len - compressedBlocks);
        return activeBlocks.Where(c => c != '.')
            .Select((fileNumber, idx) => idx * (fileNumber - '0'))
            .Sum();
    }
}