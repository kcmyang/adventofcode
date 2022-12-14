defmodule Mix.Tasks.Main do
  use Mix.Task

  @type point() :: {integer(), integer()}
  @type path() :: [point()]

  @spec line_to_path(String.t()) :: path()
  def line_to_path(line) do
    line
    |> String.split(" -> ")
    |> Enum.map(fn s ->
      [x, y] = String.split(s, ",")
      {String.to_integer(x), String.to_integer(y)}
    end)
  end

  @spec points_on_segment(point(), point()) :: [point()]
  def points_on_segment({x1, y1}, {x2, y2}) do
    cond do
      x1 == x2 -> Enum.map(min(y1, y2)..max(y1, y2), &{x1, &1})
      y1 == y2 -> Enum.map(min(x1, x2)..max(x1, x2), &{&1, y1})
    end
  end

  defmodule Part1 do
    alias Mix.Tasks.Main
    @source {500, 0}

    @type cave_plane() :: MapSet.t(Main.point())
    @type cave_floor() :: %{integer() => integer()}
    @type cave() :: {cave_plane(), cave_floor()}

    @spec make_cave([String.t()]) :: cave()
    def make_cave(lines) do
      for path <- Enum.map(lines, &Main.line_to_path/1),
          [p1, p2] <- Enum.chunk_every(path, 2, 1, :discard),
          point = {x, y} <- Main.points_on_segment(p1, p2),
          reduce: {MapSet.new(), %{}} do
        {plane, floor} -> {MapSet.put(plane, point), Map.update(floor, x, y, &max(&1, y))}
      end
    end

    @spec abyss?(cave_floor(), Main.point()) :: boolean()
    def abyss?(floor, {x, y}) do
      y > Map.get(floor, x, -1)
    end

    @spec drop_sand(cave(), Main.point()) :: {cave(), :ok | :abyss}
    def drop_sand({plane, floor} = cave, point = {x, y}) do
      cond do
        abyss?(floor, point) ->
          {cave, :abyss}

        not MapSet.member?(plane, {x, y + 1}) ->
          drop_sand(cave, {x, y + 1})

        not MapSet.member?(plane, {x - 1, y + 1}) ->
          drop_sand(cave, {x - 1, y + 1})

        not MapSet.member?(plane, {x + 1, y + 1}) ->
          drop_sand(cave, {x + 1, y + 1})

        true ->
          {{MapSet.put(plane, point), floor}, :ok}
      end
    end

    @spec simulate_sand(cave(), integer()) :: integer()
    def simulate_sand(cave, total) do
      {cave, result} = drop_sand(cave, @source)

      case result do
        :ok -> simulate_sand(cave, total + 1)
        :abyss -> total
      end
    end

    @spec run([String.t()]) :: term()
    def run(lines) do
      make_cave(lines) |> simulate_sand(0)
    end
  end

  defmodule Part2 do
    alias Mix.Tasks.Main
    @source {500, 0}

    @type cave_plane() :: MapSet.t(Main.point())
    @type cave_floor() :: integer()
    @type cave() :: {cave_plane(), cave_floor()}

    @spec make_cave([String.t()]) :: cave()
    def make_cave(lines) do
      {plane, floor} =
        for path <- Enum.map(lines, &Main.line_to_path/1),
            [p1, p2] <- Enum.chunk_every(path, 2, 1, :discard),
            point = {_, y} <- Main.points_on_segment(p1, p2),
            reduce: {MapSet.new(), -1} do
          {plane, floor} -> {MapSet.put(plane, point), max(floor, y)}
        end

      {plane, floor + 2}
    end

    @spec drop_sand(cave(), Main.point()) :: cave()
    def drop_sand({plane, floor} = cave, point = {x, y}) do
      cond do
        y + 1 == floor ->
          {MapSet.put(plane, point), floor}

        not MapSet.member?(plane, {x, y + 1}) ->
          drop_sand(cave, {x, y + 1})

        not MapSet.member?(plane, {x - 1, y + 1}) ->
          drop_sand(cave, {x - 1, y + 1})

        not MapSet.member?(plane, {x + 1, y + 1}) ->
          drop_sand(cave, {x + 1, y + 1})

        true ->
          {MapSet.put(plane, point), floor}
      end
    end

    @spec simulate_sand(cave(), integer()) :: integer()
    def simulate_sand({plane, _} = cave, total) do
      if MapSet.member?(plane, @source) do
        total
      else
        cave
        |> drop_sand(@source)
        |> simulate_sand(total + 1)
      end
    end

    @spec run([String.t()]) :: term()
    def run(lines) do
      make_cave(lines) |> simulate_sand(0)
    end
  end

  @impl Mix.Task
  def run([file | _]) do
    lines =
      file
      |> File.stream!()
      |> Stream.map(&String.trim_trailing/1)
      |> Enum.to_list()

    IO.puts(Part1.run(lines))
    IO.puts(Part2.run(lines))
  end
end
