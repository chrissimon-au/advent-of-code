namespace Day19;

using Microsoft.VisualStudio.TestPlatform.CommunicationUtilities;
using Xunit.Abstractions;
using Towels = IDictionary<int, List<string>>;

public class Day19
{
    private readonly ITestOutputHelper output;

    public Day19(ITestOutputHelper output)
    {
        this.output = output;
    }

    (Towels, string[]) Parse(string input) {
        var parts = input.Split("\n\n");
        var towels = parts[0].Split(", ");
        var towelsByLength = towels.GroupBy(t => t.Length).ToDictionary(g => g.Key, g => g.ToList());
        var displays = parts[1].Split("\n");
        return (towelsByLength, displays);
    }

    long CountWaysToMakeDisplay(string prefix, string display, Towels towels) {
        return towels.Keys.Aggregate(0L, (counter, wordLength) => {
            int numOptions = towels[wordLength].Where(t => display.StartsWith(t)).Count();
            if (numOptions > 0) {
                if (display.Length > wordLength) {
                    var combinations = numOptions * CountWaysToMakeDisplay(prefix + " ", display.Substring(wordLength), towels);
                    output.WriteLine($"{prefix}{display}: c:{counter}, l:{wordLength}, o:{numOptions}, recreturning {combinations}");
                    return counter + combinations;
                } else {
                    output.WriteLine($"{prefix}{display}: c:{counter}, l:{wordLength}, o:{numOptions}, returning {numOptions}");
                    return counter + numOptions;
                }
            }
            output.WriteLine($"{prefix}{display}: c:{counter}, l:{wordLength}, o:{numOptions}, returning no options");
            return counter;
        });
    }

    long CountWaysToMakeDisplay(string input) {
        var (towels, displays) = Parse(input);

        return displays.Select(d => CountWaysToMakeDisplay("", d, towels)).Sum();
    }

    [Theory()]
    [InlineData(@"a, b, c, ab, bc

abc
cba", 4)]
    [InlineData(@"a, b, c, ab, bc, bb, cc

abbcc", 8)]
    [InlineData(@"x, y, z

efg", 0)]
    public void TestCountWaysToMakeDisplay(string input, long expected)
    {
        Assert.Equal(expected, CountWaysToMakeDisplay(input));
    }

    [Theory()]
    [InlineData("../../../../sampledata.txt", "../../../../sampledata.answer2.txt")]
    public void TestCountWaysToMakeDisplayFromFile(string dataFileName, string answerFileName)
    {
        Assert.Equal(Convert.ToInt64(File.ReadAllText(answerFileName)), CountWaysToMakeDisplay(File.ReadAllText(dataFileName)));
    }
}