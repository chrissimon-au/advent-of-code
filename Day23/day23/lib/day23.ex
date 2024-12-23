defmodule Day23 do
  @moduledoc """
  Documentation for `Day23`.
  """

  def make_graph(input) do
    g = Graph.new
    Graph.add_vertices(g, ["kt","tk"])
    Graph.add_edges(g, [{"kt", "tk"}])
  end
end
