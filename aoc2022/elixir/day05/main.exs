defmodule Day05 do
  @type stack() :: [String.t()]
  @type stack_name() :: integer()
  @type stack_map() :: %{stack_name() => stack()}
  @type instruction() :: %{count: integer(), from: stack_name(), to: stack_name()}

  @doc "Expects lines to appear as provided in the puzzle input."
  @spec stack_lines_to_stack_map(Enumerable.t()) :: stack_map()
  def stack_lines_to_stack_map(lines) do
    [header | rest] = Enum.reverse(lines)

    stack_names = header |> String.split() |> Enum.map(&String.to_integer/1)

    item_lists =
      Enum.map(rest, fn line ->
        line
        |> String.graphemes()
        # drop the first '[' (or space)
        |> tl()
        # 4 = width of an item (3) + 1 space for separation
        |> Enum.take_every(4)
      end)

    for item_list <- item_lists,
        {item, index} <- Enum.with_index(item_list),
        reduce: Map.from_keys(stack_names, []) do
      acc ->
        case item do
          " " -> acc
          _ -> Map.update!(acc, index + 1, fn stack -> [item | stack] end)
        end
    end
  end

  @spec instruction_line_to_instruction(String.t()) :: instruction()
  def instruction_line_to_instruction(line) do
    [_move, count, _from, from, _to, to] = String.split(line)
    %{count: String.to_integer(count), from: String.to_integer(from), to: String.to_integer(to)}
  end

  @spec lines_to_stacks_and_instructions(Enumerable.t()) :: {stack_map(), [instruction()]}
  def lines_to_stacks_and_instructions(lines) do
    {stack_lines, rest} = Enum.split_while(lines, &(&1 != ""))

    stack_map = Day05.stack_lines_to_stack_map(stack_lines)
    instruction_lines = rest |> tl() |> Enum.map(&Day05.instruction_line_to_instruction/1)

    {stack_map, instruction_lines}
  end

  defmodule Part1 do
    @spec run(Enumerable.t()) :: String.t()
    def run(lines) do
      {stack_map, instructions} = Day05.lines_to_stacks_and_instructions(lines)

      stack_map =
        for %{count: count, from: from, to: to} <- instructions,
            _ <- 1..count,
            reduce: stack_map do
          map ->
            {[item | _rest], map} =
              Map.get_and_update!(map, from, fn stack -> {stack, tl(stack)} end)

            Map.update!(map, to, fn stack -> [item | stack] end)
        end

      stack_map
      |> Map.to_list()
      |> Enum.sort_by(fn {k, _stack} -> k end)
      |> Enum.map_join(fn {_k, stack} -> hd(stack) end)
    end
  end

  defmodule Part2 do
    @spec run(Enumerable.t()) :: String.t()
    def run(lines) do
      {stack_map, instructions} = Day05.lines_to_stacks_and_instructions(lines)

      stack_map =
        for %{count: count, from: from, to: to} <- instructions, reduce: stack_map do
          map ->
            # Essentially, pop off the items and push them onto a temporary stack first,
            # then pop and push them onto the target stack
            {items, map} =
              for _ <- 1..count, reduce: {[], map} do
                {items, map} ->
                  {[item | _rest], map} =
                    Map.get_and_update!(map, from, fn stack -> {stack, tl(stack)} end)

                  {[item | items], map}
              end

            for item <- items, reduce: map do
              map -> Map.update!(map, to, fn stack -> [item | stack] end)
            end
        end

      stack_map
      |> Map.to_list()
      |> Enum.sort_by(fn {k, _stack} -> k end)
      |> Enum.map_join(fn {_k, stack} -> hd(stack) end)
    end
  end
end

[file | _] = System.argv()

lines =
  file
  |> File.stream!()
  |> Stream.map(&String.trim_trailing/1)

IO.puts(Day05.Part1.run(lines))
IO.puts(Day05.Part2.run(lines))
