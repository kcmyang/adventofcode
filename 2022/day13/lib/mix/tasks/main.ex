defmodule Mix.Tasks.Main do
  use Mix.Task

  def parse_packet_data(line) do
    {packet, _} = Code.eval_string(line)
    packet
  end

  def compare([], []) do
    :same
  end

  def compare([], p2) when is_list(p2) and length(p2) > 0 do
    :correct
  end

  def compare(p1, []) when is_list(p1) and length(p1) > 0 do
    :incorrect
  end

  def compare([h1 | t1] = p1, [h2 | t2] = p2) do
    case {is_integer(h1), is_integer(h2)} do
      {true, true} ->
        cond do
          h1 < h2 -> :correct
          h1 > h2 -> :incorrect
          h1 == h2 -> compare(t1, t2)
        end

      {false, false} ->
        case compare(h1, h2) do
          :same -> compare(t1, t2)
          result -> result
        end

      {true, false} ->
        compare([[h1] | t1], p2)

      {false, true} ->
        compare(p1, [[h2] | t2])
    end
  end

  defmodule Part1 do
    alias Mix.Tasks.Main

    @spec run([String.t()]) :: term()
    def run(lines) do
      pairs =
        lines
        |> Enum.filter(&(&1 != ""))
        |> Enum.map(&Main.parse_packet_data/1)
        |> Enum.chunk_every(2)

      for {[p1, p2], index} <- Enum.with_index(pairs, 1), reduce: [] do
        indices ->
          case Main.compare(p1, p2) do
            :correct -> [index | indices]
            :incorrect -> indices
          end
      end
      |> Enum.sum()
    end
  end

  defmodule Part2 do
    alias Mix.Tasks.Main

    @spec run([String.t()]) :: term()
    def run(lines) do
      sorted_packets =
        (lines ++ ["[[2]]", "[[6]]"])
        |> Enum.filter(&(&1 != ""))
        |> Enum.map(&Main.parse_packet_data/1)
        |> Enum.sort(fn p1, p2 -> Main.compare(p1, p2) == :correct end)

      key1 = 1 + Enum.find_index(sorted_packets, &(&1 == [[2]]))
      key2 = 1 + Enum.find_index(sorted_packets, &(&1 == [[6]]))

      key1 * key2
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
