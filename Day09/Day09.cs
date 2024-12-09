using System.Runtime.InteropServices;

namespace AoC.Day09;
using System.Linq;

public static class Day09
{
    private static String NormalizeLength(this String input) => input.Length % 2 == 1 ? input + "0" : input;
    
    private static IEnumerable<long> ExpandDiskMap(string diskMap) {
        var fileAllocation = diskMap
            .Where((_, i) => i % 2 == 0)
            .Select((fileSize, fileId) => Enumerable.Repeat((long)fileId, fileSize-'0'));
        var spaceAllocation = diskMap
            .Where((_, i) => i % 2 == 1)
            .Select((spaceSize) => Enumerable.Repeat((long)-1, spaceSize - '0'));
        var expandedLayout = fileAllocation.Zip(spaceAllocation).SelectMany((entry) => entry.First.Concat(entry.Second));        
        return expandedLayout;
    }

    private static IEnumerable<long> CompressDiskMap(this IEnumerable<long> input)
    {
        var diskMap = input.ToList();
        var len = diskMap.Count;
        var compressedBlocks = 0;
        var compressedDiskMap = diskMap.Select((fileId, idx) =>
        {
            if (idx >= len - compressedBlocks)
            {
                return -1;
            }
            if (fileId < 0)
            {
                compressedBlocks++;
                while (diskMap[len - compressedBlocks] < 0)
                {
                    compressedBlocks++;
                }
                return diskMap[len - compressedBlocks];
            }
            else
            {
                return fileId;
            }
        }).ToList();
        return compressedDiskMap;
    }
    
    public static long CompactAndGetChecksum(string diskMap) => 
        ExpandDiskMap(diskMap.NormalizeLength())
            .CompressDiskMap()
            .Where(c => c >= 0)
            .Select((fileNumber, idx) => idx * fileNumber)
            .Sum();

    public static long CompactFullFilesAndGetChecksum(string diskMap) => 
        ExpandDiskMap(diskMap.NormalizeLength())
            .CompressDiskMap()
            .Select((fileNumber, idx) => idx * fileNumber)
            .Sum();
    
}