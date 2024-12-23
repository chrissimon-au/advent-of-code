defmodule Day23 do
  @moduledoc """
  Documentation for `Day23`.
  """

  def make_graph(input) do
    g = Graph.new
    vertices = String.split(input, "-")
    Graph.add_vertices(g, vertices)
    Graph.add_edges(g, [List.to_tuple(vertices)])
  end
end
