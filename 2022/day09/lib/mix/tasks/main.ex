defmodule Mix.Tasks.Main do
  use Mix.Task

  @spec signum(number()) :: integer()
  def signum(n) do
    cond do
      n == 0 -> 0
      n < 0 -> -1
      n > 0 -> 1
    end
  end

  defmodule Part1 do
    alias Mix.Tasks.Main

    @spec run([String.t()]) :: term()
    def run(lines) do
      {_, _, positions} =
        for [direction, distance] <- Enum.map(lines, &String.split/1),
            _ <- 1..String.to_integer(distance),
            reduce: {{0, 0}, {0, 0}, MapSet.new([{0, 0}])} do
          {{x_head, y_head}, {x_tail, y_tail} = tail, positions} ->
            {x_head, y_head} =
              case direction do
                "R" -> {x_head + 1, y_head}
                "L" -> {x_head - 1, y_head}
                "U" -> {x_head, y_head + 1}
                "D" -> {x_head, y_head - 1}
              end

            tail =
              case {x_head - x_tail, y_head - y_tail} do
                {dx, dy} when abs(dx) <= 1 and abs(dy) <= 1 ->
                  tail

                {0, dy} ->
                  {x_tail, y_tail + div(dy, 2)}

                {dx, 0} ->
                  {x_tail + div(dx, 2), y_tail}

                {dx, dy} ->
                  {x_tail + Main.signum(dx), y_tail + Main.signum(dy)}
              end

            {{x_head, y_head}, tail, MapSet.put(positions, tail)}
        end

      MapSet.size(positions)
    end
  end

  defmodule Part2 do
    alias Mix.Tasks.Main

    @rope_length 10

    @spec run([String.t()]) :: term()
    def run(lines) do
      {_, positions} =
        for [direction, distance] <- Enum.map(lines, &String.split/1),
            _ <- 1..String.to_integer(distance),
            reduce: {
              Enum.into(1..@rope_length, %{}, fn i -> {i, {0, 0}} end),
              MapSet.new([{0, 0}])
            } do
          {rope, positions} ->
            # First, move the head
            rope =
              Map.update!(rope, 1, fn {x_head, y_head} ->
                case direction do
                  "R" -> {x_head + 1, y_head}
                  "L" -> {x_head - 1, y_head}
                  "U" -> {x_head, y_head + 1}
                  "D" -> {x_head, y_head - 1}
                end
              end)

            # Then, move everything else
            rope =
              for segment <- 2..@rope_length, reduce: rope do
                rope ->
                  {x_prev, y_prev} = Map.get(rope, segment - 1)

                  Map.update!(rope, segment, fn {x, y} ->
                    case {x_prev - x, y_prev - y} do
                      {dx, dy} when abs(dx) <= 1 and abs(dy) <= 1 ->
                        {x, y}

                      {0, dy} ->
                        {x, y + div(dy, 2)}

                      {dx, 0} ->
                        {x + div(dx, 2), y}

                      {dx, dy} ->
                        {x + Main.signum(dx), y + Main.signum(dy)}
                    end
                  end)
              end

            # Finally, update the set of tail positions
            {rope, MapSet.put(positions, Map.get(rope, @rope_length))}
        end

      MapSet.size(positions)
    end
  end

  @impl Mix.Task
  def run([file | _]) do
    lines =
      file
      |> File.stream!()
      |> Stream.map(&String.trim_trailing/1)
      |> Enum.to_list()

    IO.puts("Part 1: #{Part1.run(lines)}")
    IO.puts("Part 2: #{Part2.run(lines)}")
  end
end
