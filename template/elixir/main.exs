defmodule Main do
  defmodule Part1 do
    @spec run(Enumerable.t()) :: integer()
    def run(data) do
      0
    end
  end

  defmodule Part2 do
    @spec run(Enumerable.t()) :: integer()
    def run(data) do
      0
    end
  end
end

[file | _] = System.argv()

data =
  file
  |> File.stream!()
  |> Stream.map(&String.trim_trailing/1)

IO.puts(Main.Part1.run(data))
IO.puts(Main.Part2.run(data))
