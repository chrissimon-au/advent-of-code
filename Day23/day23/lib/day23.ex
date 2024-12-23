defmodule Day23 do
  @moduledoc """
  Documentation for `Day23`.
  """

  def get_edge(connection) do
    String.split(connection,"-")
    |> List.to_tuple
  end

  @spec make_graph(String.t()) :: Graph.t()
  def make_graph(input) do
    g = Graph.new()
    connections = String.split(String.trim(input), "\n")
    edges = Enum.map(connections, &get_edge/1)
    Graph.add_edges(g, edges)
  end
end
