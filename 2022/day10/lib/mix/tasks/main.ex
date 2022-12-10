defmodule Mix.Tasks.Main do
  use Mix.Task

  defmodule Part1 do
    @spec run([String.t()]) :: term()
    def run(lines) do
      {_, _, strengths} =
        for line <- lines, reduce: {1, 1, []} do
          {x, cycle, strengths} ->
            strengths =
              if rem(cycle, 40) == 20 do
                [x * cycle | strengths]
              else
                strengths
              end

            case String.split(line) do
              ["noop"] ->
                {x, cycle + 1, strengths}

              ["addx", v] ->
                cycle = cycle + 1

                strengths =
                  if rem(cycle, 40) == 20 do
                    [x * cycle | strengths]
                  else
                    strengths
                  end

                {x + String.to_integer(v), cycle + 1, strengths}
            end
        end

      IO.inspect(strengths)
      Enum.sum(strengths)
    end
  end

  defmodule Part2 do
    @spec run([String.t()]) :: term()
    def run(lines) do
      {_, _, crt} =
        for line <- lines, reduce: {1, 1, []} do
          {x, cycle, crt} ->
            # draw on the CRT once
            column = rem(cycle - 1, 40)

            crt =
              if abs(x - column) <= 1 do
                ["#" | crt]
              else
                ["." | crt]
              end

            # process the instruction
            case String.split(line) do
              ["noop"] ->
                {x, cycle + 1, crt}

              ["addx", v] ->
                cycle = cycle + 1

                column = rem(cycle - 1, 40)

                crt =
                  if abs(x - column) <= 1 do
                    ["#" | crt]
                  else
                    ["." | crt]
                  end

                {x + String.to_integer(v), cycle + 1, crt}
            end
        end

      crt
      |> Enum.reverse()
      |> Enum.chunk_every(40)
      |> Enum.map_join("\n", &Enum.join/1)
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
