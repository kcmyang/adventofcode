defmodule Mix.Tasks.Main do
  use Mix.Task

  defmodule Part1 do
    @spec run([String.t()]) :: term()
    def run(lines) do
      length(lines)
    end
  end

  defmodule Part2 do
    @spec run([String.t()]) :: term()
    def run(lines) do
      length(lines)
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
