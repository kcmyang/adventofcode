defmodule Day04 do
  @type assignment() :: {integer(), integer()}

  @spec line_to_assignment_pair(String.t()) :: {assignment(), assignment()}
  def line_to_assignment_pair(line) do
    line
    |> String.split(",")
    |> Enum.map(fn range -> String.split(range, "-") end)
    |> Enum.map(fn [a, b] -> {String.to_integer(a), String.to_integer(b)} end)
    |> List.to_tuple()
  end

  defmodule Part1 do
    @spec fully_overlaps?({Day04.assignment(), Day04.assignment()}) :: boolean()
    def fully_overlaps?({{a, b}, {c, d}}) do
      cond do
        a == c -> b <= d or d <= b
        a < c -> d <= b
        true -> b <= d
      end
    end

    @spec run(Enumerable.t()) :: integer()
    def run(lines) do
      lines
      |> Enum.map(&Day04.line_to_assignment_pair/1)
      |> Enum.count(&Day04.Part1.fully_overlaps?/1)
    end
  end

  defmodule Part2 do
    @spec overlaps?({Day04.assignment(), Day04.assignment()}) :: boolean()
    def overlaps?({{a, b}, {c, d}}) do
      (a <= c and c <= b) or (c <= a and a <= d)
    end

    @spec run(Enumerable.t()) :: integer()
    def run(lines) do
      lines
      |> Enum.map(&Day04.line_to_assignment_pair/1)
      |> Enum.count(&Day04.Part2.overlaps?/1)
    end
  end
end

[file | _] = System.argv()

lines =
  file
  |> File.stream!()
  |> Stream.map(&String.trim_trailing/1)

IO.puts(Day04.Part1.run(lines))
IO.puts(Day04.Part2.run(lines))
