defmodule Day01 do
  @spec lines_to_calorie_totals(Enumerable.t()) :: integer
  def lines_to_calorie_totals(lines) do
    lines
    |> Stream.chunk_by(&(&1 == ""))
    |> Stream.filter(&(&1 != [""]))
    |> Stream.map(fn items ->
      items
      |> Stream.map(&String.to_integer/1)
      |> Enum.sum()
    end)
  end

  @spec part1(Enumerable.t()) :: integer
  def part1(lines) do
    lines
    |> lines_to_calorie_totals()
    |> Enum.max()
  end

  @spec part2(Enumerable.t()) :: integer
  def part2(lines) do
    lines
    |> lines_to_calorie_totals()
    |> Enum.sort(:desc)
    |> Enum.take(3)
    |> Enum.sum()
  end
end

[file | _] = System.argv()

lines =
  file
  |> File.stream!()
  |> Stream.map(&String.trim_trailing/1)

IO.puts(Day01.part1(lines))
IO.puts(Day01.part2(lines))
