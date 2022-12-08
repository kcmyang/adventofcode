defmodule Day03 do
  @type item() :: char()

  @spec priority(item()) :: integer()
  def priority(item) do
    cond do
      item in ?a..?z -> item - ?a + 1
      item in ?A..?Z -> item - ?A + 27
    end
  end

  defmodule Part1 do
    @type rucksack() :: {String.t(), String.t()}

    @spec line_to_rucksack(String.t()) :: rucksack()
    def line_to_rucksack(line) do
      String.split_at(line, div(String.length(line), 2))
    end

    @spec unique_misplaced_item(rucksack()) :: Day03.item()
    def unique_misplaced_item({comp1, comp2}) do
      comp1 = comp1 |> String.to_charlist() |> Enum.sort()
      comp2 = comp2 |> String.to_charlist()
      Enum.find(comp1, &(&1 in comp2))
    end

    @spec run(Enumerable.t()) :: integer()
    def run(lines) do
      lines
      |> Stream.map(&Day03.Part1.line_to_rucksack/1)
      |> Stream.map(&Day03.Part1.unique_misplaced_item/1)
      |> Stream.map(&Day03.priority/1)
      |> Enum.sum()
    end
  end

  defmodule Part2 do
    @type rucksack() :: String.t()

    @spec badge([rucksack()]) :: Day03.item()
    def badge([r1, r2, r3]) do
      [r1, r2, r3] = [r1, r2, r3] |> Enum.map(&(&1 |> String.to_charlist() |> MapSet.new()))

      MapSet.intersection(MapSet.intersection(r1, r2), r3)
      |> MapSet.to_list()
      |> hd
    end

    @spec run(Enumerable.t()) :: integer()
    def run(lines) do
      lines
      |> Enum.chunk_every(3)
      |> Enum.map(&Day03.Part2.badge/1)
      |> Enum.map(&Day03.priority/1)
      |> Enum.sum()
    end
  end
end

[file | _] = System.argv()

lines =
  file
  |> File.stream!()
  |> Stream.map(&String.trim_trailing/1)

IO.puts(Day03.Part1.run(lines))
IO.puts(Day03.Part2.run(lines))
