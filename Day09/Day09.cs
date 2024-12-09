using System.Collections;
using System.Runtime.InteropServices;
using System.Security.Cryptography;
using Xunit.Sdk;

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

    private record BlockSpace();
    private record File(long id, int len) : BlockSpace;

    private record Empty(int len) : BlockSpace;

    private static IEnumerable<BlockSpace> ExpandIndexedDiskMap(string diskMap)
    {
        IEnumerable<BlockSpace> fileAllocation = diskMap
            .Where((_, i) => i % 2 == 0)
            .Select((fileSize, fileId) => new File(fileId, fileSize - '0'));
        IEnumerable<BlockSpace> spaceAllocation = diskMap
            .Where((_, i) => i % 2 == 1)
            .Select((spaceSize) => new Empty(spaceSize - '0'));
        var expandedLayout = fileAllocation.Zip(spaceAllocation)
            .SelectMany(entry => new List<BlockSpace> { entry.First, entry.Second });        
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

    private static bool BlockCanFitInEmpty(BlockSpace block, Empty empty)
    {
        if (block is Empty)
        {
            return false;
        }

        if (block is File f)
        {
            return f.len <= empty.len;
        }

        return false;
    } 

    private static IEnumerable<BlockSpace> CompressDiskMap(this IEnumerable<BlockSpace> input)
    {
        var diskMap = input.ToList();
        var len = diskMap.Count;
        var compressedBlocks = 0;
        var compressedDiskMap = diskMap.SelectMany((block, idx) =>
        {
            if (idx >= len - compressedBlocks)
            {
                return block switch
                {
                    File f => [new Empty(f.len)],
                    Empty e1 => [e1],
                    _ => throw new ArgumentOutOfRangeException(nameof(block), block, null)
                };
            }
            if (block is Empty e2)
            {
                var potentialCompressedBlocks = 0;
                potentialCompressedBlocks++;
                while (!BlockCanFitInEmpty(diskMap[len - compressedBlocks - potentialCompressedBlocks], e2) && idx < len - compressedBlocks - potentialCompressedBlocks - 1)
                {
                    potentialCompressedBlocks++;
                }

                var b = diskMap[len - compressedBlocks - potentialCompressedBlocks];
                if (BlockCanFitInEmpty(b, e2) && b is File f2 && idx < len - compressedBlocks - potentialCompressedBlocks - 1)
                {
                    compressedBlocks += potentialCompressedBlocks;
                    return
                        new List<BlockSpace>
                        {
                            f2,
                            new Empty(e2.len - (f2?.len ?? 0)),
                        };
                }

                return [e2];
            }
            return [block];
        }).ToList();
        return compressedDiskMap;
    }

    private static IEnumerable<long> Flatten(this IEnumerable<BlockSpace> input) =>
        input.SelectMany(b => b switch
        {
            File f => Enumerable.Repeat(f.id, f.len),
            Empty e => Enumerable.Repeat(0L, e.len)
        });
    
    
    public static long CompactAndGetChecksum(string diskMap) => 
        ExpandDiskMap(diskMap.NormalizeLength())
            .CompressDiskMap()
            .Where(c => c >= 0)
            .Select((fileNumber, idx) => idx * fileNumber)
            .Sum();

    public static long CompactFullFilesAndGetChecksum(string diskMap) => 
        ExpandIndexedDiskMap(diskMap.NormalizeLength())
            .CompressDiskMap()
            .Flatten()
            .Select((fileId, idx) => idx * fileId)
            .Sum();
}