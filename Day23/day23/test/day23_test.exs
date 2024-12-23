defmodule Day23Test do
  use ExUnit.Case
  doctest Day23

  test "constructs a graph" do
    g = Day23.make_graph("kh-tc");
    assert Graph.num_vertices(g) == 2
  end
end
