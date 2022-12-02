defmodule Day01 do
  @spec lines_to_calorie_totals([String.t()]) :: integer
  def lines_to_calorie_totals(lines) do
    lines
    |> Enum.chunk_by(&(&1 == ""))
    |> Enum.filter(&(&1 != [""]))
    |> Enum.map(fn items ->
      items
      |> Enum.map(&String.to_integer/1)
      |> Enum.sum()
    end)
  end

  @spec part1([String.t()])
  def part1(lines) do
    lines
    |> lines_to_calorie_totals()
    |> Enum.max()
  end

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
  |> Enum.to_list()

IO.puts(Day01.part1(lines))
IO.puts(Day01.part2(lines))
