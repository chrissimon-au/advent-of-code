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

  test "finds number of game-sets with t" do
    g = Day23.make_graph("""
kh-tc
qp-kh
de-cg
ka-co
yn-aq
qp-ub
cg-tb
vc-aq
tb-ka
wh-tc
yn-cg
kh-ub
ta-co
de-co
tc-td
tb-wq
wh-td
ta-ka
td-qp
aq-cg
wq-ub
ub-vc
de-ta
wq-aq
wq-vc
wh-yn
ka-de
kh-ta
co-tc
wh-qp
tb-vc
td-yn
""")
    assert Graph.num_vertices(g) == 16
    assert Graph.num_edges(g) == 32
    assert Day23.num_gamesets(g, "t") == 7
  end
end
