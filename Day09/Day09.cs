namespace AoC.Day09;
using System.Linq;

public static class Day09
{
    private static String NormalizeLength(this String input) => input.Length % 2 == 1 ? input + "0" : input;
    
    private static IEnumerable<char> ExpandDiskMap(string diskMap) {
        var fileAllocation = diskMap
            .Where((_, i) => i % 2 == 0)
            .Select((fileSize, fileId) => Enumerable.Repeat(fileId.ToString()[0], fileSize-'0'));
        var spaceAllocation = diskMap
            .Where((_, i) => i % 2 == 1)
            .Select((spaceSize) => Enumerable.Repeat('.', spaceSize - '0'));
        var expandedLayout = fileAllocation.Zip(spaceAllocation).SelectMany((entry) => entry.First.Concat(entry.Second));        
        return expandedLayout;
    }

    private static IEnumerable<char> CompressDiskMap(this IEnumerable<char> input)
    {
        var diskMap = input.ToList();
        var len = diskMap.Count;
        var compressedBlocks = 0;
        var compressedDiskMap = diskMap.Select((c, i) =>
        {
            if (c == '.')
            {
                compressedBlocks++;
                return diskMap[len - compressedBlocks];
            }
            else
            {
                return c;
            }
        }).ToList();
        return compressedDiskMap.Take(len - compressedBlocks);
    }
    
    public static long CompactAndGetChecksum(string diskMap) => 
    
        ExpandDiskMap(diskMap.NormalizeLength())
            .CompressDiskMap()
            .Where(c => c != '.')
            .Select((fileNumber, idx) => idx * (fileNumber - '0'))
            .Sum();
    
}