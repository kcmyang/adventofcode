defmodule Day06 do
  @spec first_marker_index(Enumerable.t(), non_neg_integer()) :: non_neg_integer()
  def first_marker_index(data, marker_size) do
    index =
      data
      |> Enum.chunk_every(marker_size, 1)
      |> Enum.find_index(fn group -> MapSet.size(MapSet.new(group)) == marker_size end)

    # 1-based index of the end of the marker
    index + marker_size
  end

  defmodule Part1 do
    @spec run(Enumerable.t()) :: integer()
    def run(data) do
      Day06.first_marker_index(data, 4)
    end
  end

  defmodule Part2 do
    @spec run(Enumerable.t()) :: integer()
    def run(data) do
      Day06.first_marker_index(data, 14)
    end
  end
end

[file | _] = System.argv()

data =
  file
  |> File.stream!()
  |> Stream.map(&String.trim_trailing/1)
  |> Enum.to_list()
  |> hd()
  |> String.graphemes()

IO.puts(Day06.Part1.run(data))
IO.puts(Day06.Part2.run(data))
