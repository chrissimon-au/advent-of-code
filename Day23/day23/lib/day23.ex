defmodule Day23 do
  @moduledoc """
  Documentation for `Day23`.
  """

  def add_connection(g, connection) do
    IO.puts("h: #{connection}")
    vertices = String.split(connection, "-")
    IO.inspect(vertices)
    edge = List.to_tuple(vertices)
    IO.inspect(edge)
    Graph.add_vertices(g, vertices)
    Graph.add_edges(g, [edge])
    IO.inspect(Graph.vertices(g))
    IO.inspect(Graph.num_edges(g))
  end

  def make_graph(input) do
    g = Graph.new()
    connections = String.split(String.trim(input), "\n")
    add_connection(g, Enum.at(connections, 0))
    # Enum.each(connections, fn c -> add_connection(g, c) end)
    g
  end
end
