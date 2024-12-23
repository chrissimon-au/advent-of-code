defmodule Day23Test do
  use ExUnit.Case
  doctest Day23

  test "constructs a graph of a single connect" do
    g = Day23.make_graph("kh-tc");
    assert Graph.num_vertices(g) == 2
    assert Graph.num_edges(g) == 1
    assert Enum.sort(Graph.vertices(g)) == Enum.sort(["kh", "tc"])
  end

  test "constructs a graph of multiple connects" do
    g = Day23.make_graph("""
kh-tc
tc-ba
bp-kh
""");
    assert Graph.num_vertices(g) == 4
    assert Graph.num_edges(g) == 3
    assert Enum.sort(Graph.vertices(g)) == Enum.sort(["kh", "tc", "ba", "bp"])
  end

  def assert_game_sets(base) do
    g = Day23.make_graph(File.read!("../#{base}data.txt"))
    assert Day23.num_gamesets(g, "t") == File.read!("../#{base}data.answer.txt") |> String.to_integer
  end

  test "AoC Sample: finds number of game-sets with t" do
    assert_game_sets("sample")
  end

  test "AoC Test: finds number of game-sets with t" do
    assert_game_sets("test")
  end
end
