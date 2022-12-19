defmodule Mix.Tasks.Part1 do
  use Mix.Task

  @spec main([String.t()]) :: term()
  def main(lines) do
    IO.inspect(nil)
  end

  @impl Mix.Task
  def run([file | _]) do
    lines =
      file
      |> File.stream!()
      |> Stream.map(&String.trim_trailing/1)
      |> Enum.to_list()

    main(lines)
  end
end
