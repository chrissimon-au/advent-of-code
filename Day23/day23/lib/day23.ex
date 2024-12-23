defmodule Day23 do
  @moduledoc """
  Documentation for `Day23`.
  """

  def get_edge(connection) do
    String.split(connection,"-")
    |> List.to_tuple
  end

  def make_graph(input) do
    g = Graph.new(type: :undirected)
    connections = String.split(String.trim(input), "\n")
    edges = Enum.map(connections, &get_edge/1)
    Graph.add_edges(g, edges)
  end

  def num_gamesets(g, start_char) do
    cliques = Graph.cliques(g)
      |> Enum.filter(&length(&1)>=3)

    games_in_maximal_clique = cliques
      |> Enum.filter(&length(&1)>3)
      |> Enum.flat_map(&Formulae.combinations(&1, 3))

    games = Enum.concat(Enum.filter(cliques, &length(&1)==3), games_in_maximal_clique)
      |> Enum.filter(fn g -> Enum.any?(g, &String.starts_with?(&1, start_char)) end)
      |> Enum.map(&Enum.sort(&1))
      |> Enum.sort
      |> Enum.dedup
    length(games)
  end

  def get_password(g) do
    biggest_clique = Graph.cliques(g)
      |> Enum.max_by(&length(&1))

    Enum.join(Enum.sort(biggest_clique), ",")
  end

end
