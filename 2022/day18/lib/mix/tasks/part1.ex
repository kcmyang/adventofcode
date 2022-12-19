defmodule Mix.Tasks.Part1 do
  use Mix.Task

  def line_to_cube(line) do
    line
    |> String.split(",")
    |> Enum.map(&String.to_integer/1)
    |> List.to_tuple()
  end

  def num_neighbours({x, y, z}, cubes) do
    dirs = [{0, 0, 1}, {0, 1, 0}, {1, 0, 0}, {0, 0, -1}, {0, -1, 0}, {-1, 0, 0}]

    for {dx, dy, dz} <- dirs,
        MapSet.member?(cubes, {x + dx, y + dy, z + dz}),
        reduce: 0 do
      acc -> acc + 1
    end
  end

  @spec main([String.t()]) :: term()
  def main(lines) do
    {_cubes, sa} =
      for cube <- Enum.map(lines, &line_to_cube/1),
          reduce: {MapSet.new(), 0} do
        {cubes, sa} -> {MapSet.put(cubes, cube), sa + 6 - 2 * num_neighbours(cube, cubes)}
      end

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
