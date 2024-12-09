
namespace AoC.Day09;
using System.Linq;

public static class Common
{
    public static String NormalizeLength(this String input) => input.Length % 2 == 1 ? input + "0" : input;
}

public static class Day09Part1
{
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


}

public static class Day09Part2
{
    private record BlockSpace();

    private record File(long Id, int Len) : BlockSpace
    {
        public bool CanFitInEmpty(Empty empty) => Len <= empty.Len;
    };

    private record Empty(int Len) : BlockSpace;

    private static IEnumerable<BlockSpace> ExpandIndexedDiskMap(string diskMap) =>
        diskMap.Select<char, BlockSpace>((size, idx) =>
            idx % 2 == 0 ? new File(idx / 2, size - '0') : new Empty(size - '0'));
    
    private static IEnumerable<BlockSpace> ReplaceEmptyWithBlocks(Empty empty, List<File> reversedDiskMap, HashSet<File> movedFiles)
    {
        if (empty.Len == 0)
        {
            return [];
        }
        var movableFile = reversedDiskMap.FirstOrDefault(f => !movedFiles.Contains(f) && f.CanFitInEmpty(empty));
        if (movableFile is null)
        {
            return [empty];
            
        }
        movedFiles.Add(movableFile);
        return
            new List<BlockSpace>
            {
                movableFile,
            }.Concat(
                ReplaceEmptyWithBlocks(new Empty(empty.Len - movableFile.Len), reversedDiskMap, movedFiles)
            );
    }

    private static IEnumerable<BlockSpace> CompressDiskMap(this IEnumerable<BlockSpace> input)
    {
        var diskMap = input.ToList();
        var movedFiles = new HashSet<File>();
        var reversedDiskMap = diskMap.AsEnumerable().Reverse().OfType<File>().ToList();
        return diskMap.SelectMany(block =>
        {
            if (block is Empty empty)
            {
                return ReplaceEmptyWithBlocks(empty, reversedDiskMap, movedFiles);
            }

            return block switch
            {
                File f => [movedFiles.Add(f) ? f : new Empty(f.Len)],
                Empty e => [e],
                _ => throw new ArgumentOutOfRangeException(nameof(block), block, null)
            };
        });
    }
    
    private static IEnumerable<long> Flatten(this IEnumerable<BlockSpace> input) =>
        input.SelectMany(b => b switch
        {
            File f => Enumerable.Repeat(f.Id, f.Len),
            Empty e => Enumerable.Repeat(0L, e.Len),
            _ => throw new ArgumentOutOfRangeException(nameof(b), b, null)
        });
    
    public static long CompactFullFilesAndGetChecksum(string diskMap) => 
        ExpandIndexedDiskMap(diskMap.NormalizeLength())
            .CompressDiskMap()
            .Flatten()
            .Select((fileId, idx) => idx * fileId)
            .Sum();
}