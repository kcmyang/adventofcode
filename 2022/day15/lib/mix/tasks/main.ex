defmodule Mix.Tasks.Main do
  use Mix.Task

  @type point() :: {integer(), integer()}
  @type sensor() :: point()
  @type beacon() :: point()
  @type radius() :: non_neg_integer()
  @type report() :: {sensor(), beacon(), radius()}

  @spec dist(point(), point()) :: non_neg_integer()
  def dist({x1, y1}, {x2, y2}) do
    abs(x1 - x2) + abs(y1 - y2)
  end

  @spec parse_line(String.t()) :: report()
  def parse_line(line) do
    [sensor, beacon] =
      line
      |> String.trim_leading("Sensor at ")
      |> String.split(~r/: closest beacon is at /)

    ["x=" <> sx, "y=" <> sy] = String.split(sensor, ~r/, /)
    ps = {String.to_integer(sx), String.to_integer(sy)}

    ["x=" <> bx, "y=" <> by] = String.split(beacon, ~r/, /)
    pb = {String.to_integer(bx), String.to_integer(by)}

    radius = dist(ps, pb)

    {ps, pb, radius}
  end

  defmodule Part1 do
    alias Mix.Tasks.Main

    @y_query 2_000_000

    @doc """
    List of x-coordinates on the query line where there cannot be another beacon.
    May contain x-coordinates of existing beacons.
    A (much) faster approach would output a list of segments of the line, but this works.
    """
    @spec query_report(Main.report()) :: [integer()]
    def query_report({{sx, sy}, _beacon, radius}) do
      dy = abs(sy - @y_query)

      if dy > radius do
        []
      else
        dx = radius - dy
        Enum.to_list((sx - dx)..(sx + dx))
      end
    end

    @spec run([String.t()]) :: term()
    def run(lines) do
      reports =
        lines
        |> Enum.map(&Main.parse_line/1)

      beacon_x_coords =
        reports
        |> Enum.filter(fn {_sensor, {_bx, by}, _radius} -> by == @y_query end)
        |> Enum.into(MapSet.new(), fn {_sensor, {bx, _by}, _radius} -> bx end)

      reports
      |> Enum.map(&query_report/1)
      |> Enum.map(&MapSet.new/1)
      |> Enum.reduce(&MapSet.union/2)
      |> MapSet.difference(beacon_x_coords)
      |> MapSet.size()
    end
  end

  defmodule Part2 do
    alias Mix.Tasks.Main

    @search_min 0
    @search_max 4_000_000

    @doc """
    Brute force-ish...
    """
    def search(reports, point = {x, y}) do
      cond do
        y > @search_max ->
          raise("no solution!")

        x > @search_max ->
          search(reports, {@search_min, y + 1})

        true ->
          case Enum.find(reports, fn {sensor, _beacon, radius} ->
                 Main.dist(sensor, point) <= radius
               end) do
            {sensor, _beacon, radius} ->
              dx = radius - Main.dist(sensor, point) + 1
              search(reports, {x + dx, y})

            nil ->
              point
          end
      end
    end

    @spec run([String.t()]) :: term()
    def run(lines) do
      {x, y} =
        lines
        |> Enum.map(&Main.parse_line/1)
        |> search({@search_min, @search_min})

      x * 4_000_000 + y
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
