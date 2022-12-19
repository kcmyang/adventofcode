defmodule Mix.Tasks.Part2 do
  use Mix.Task

  @dirs [{0, 0, 1}, {0, 1, 0}, {1, 0, 0}, {0, 0, -1}, {0, -1, 0}, {-1, 0, 0}]

  def line_to_cube(line) do
    line
    |> String.split(",")
    |> Enum.map(&String.to_integer/1)
    |> List.to_tuple()
  end

  def num_neighbours({x, y, z}, cubes) do
    for {dx, dy, dz} <- @dirs,
        MapSet.member?(cubes, {x + dx, y + dy, z + dz}),
        reduce: 0 do
      acc -> acc + 1
    end
  end

  @doc """
  BFS on the points outside of the droplet.
  """
  def calculate_sa(_cubes, _frontier = [], _set, sa) do
    sa
  end

  def calculate_sa(cubes, frontier, set, sa) do
    {frontier_next, set, sa} =
      for point = {x, y, z} <- frontier, reduce: {[], set, sa} do
        {frontier_next, set, sa} ->
          frontier_next =
            for {dx, dy, dz} <- @dirs,
                point_next = {x + dx, y + dy, z + dz},
                MapSet.member?(set, point_next),
                not MapSet.member?(cubes, point_next),
                reduce: frontier_next do
              acc -> [point_next | acc]
            end

          sa = sa + 2 * num_neighbours(point, set) - 6
          set = MapSet.delete(set, point)

          {frontier_next, set, sa}
      end

    frontier_next = MapSet.new(frontier_next)
    set = MapSet.difference(set, frontier_next)
    frontier_next = MapSet.to_list(frontier_next)

    calculate_sa(cubes, frontier_next, set, sa)
  end

  @spec main([String.t()]) :: term()
  def main(lines) do
    cubes = Enum.into(lines, MapSet.new(), &line_to_cube/1)

    {x_min, x_max} = cubes |> Enum.map(&elem(&1, 0)) |> Enum.min_max()
    {y_min, y_max} = cubes |> Enum.map(&elem(&1, 1)) |> Enum.min_max()
    {z_min, z_max} = cubes |> Enum.map(&elem(&1, 2)) |> Enum.min_max()
    x_range = Range.new(x_min - 1, x_max + 1)
    y_range = Range.new(y_min - 1, y_max + 1)
    z_range = Range.new(z_min - 1, z_max + 1)
    w = x_max - x_min + 3
    h = y_max - y_min + 3
    l = z_max - z_min + 3

    frontier = [{x_min - 1, y_min - 1, z_min - 1}]

    set =
      MapSet.new(
        for x <- x_range, y <- y_range, z <- z_range do
          {x, y, z}
        end
      )

    sa = 2 * (w * h + w * l + h * l)

    sa = calculate_sa(cubes, frontier, set, sa)

    IO.puts(sa)
  end

  @impl Mix.Task
  def run([file | _]) do
    lines =
      file
      |> File.stream!()
      |> Stream.map(&String.trim_trailing/1)
      |> Enum.to_list()

    main(lines)
  end
end
