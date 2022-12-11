defmodule Mix.Tasks.Main do
  use Mix.Task

  @type monkey() :: %{
          inspected: integer(),
          items: [integer()] | [%{integer() => integer()}],
          operation_fun: (integer() -> integer()),
          divisor: integer(),
          true_index: integer(),
          false_index: integer()
        }

  @spec make_monkey([String.t()]) :: {integer(), monkey()}
  def make_monkey([l1, l2, l3, l4, l5, l6]) do
    index =
      l1
      |> String.trim_leading("Monkey ")
      |> String.trim_trailing(":")
      |> String.to_integer()

    items =
      l2
      |> String.trim_leading("Starting items: ")
      |> String.split(~r/,? /)
      |> Enum.map(&String.to_integer/1)

    operation_fun = fn old ->
      [arg1, op, arg2] =
        l3
        |> String.trim_leading("Operation: new = ")
        |> String.split()

      arg1 =
        case arg1 do
          "old" -> old
          _ -> String.to_integer(arg1)
        end

      arg2 =
        case arg2 do
          "old" -> old
          _ -> String.to_integer(arg2)
        end

      case op do
        "+" -> arg1 + arg2
        "-" -> arg1 - arg2
        "*" -> arg1 * arg2
      end
    end

    divisor =
      l4
      |> String.trim_leading("Test: divisible by ")
      |> String.to_integer()

    true_index =
      l5
      |> String.trim_leading("If true: throw to monkey ")
      |> String.to_integer()

    false_index =
      l6
      |> String.trim_leading("If false: throw to monkey ")
      |> String.to_integer()

    {index,
     %{
       inspected: 0,
       items: items,
       operation_fun: operation_fun,
       divisor: divisor,
       true_index: true_index,
       false_index: false_index
     }}
  end

  defmodule Part1 do
    alias Mix.Tasks.Main

    @num_rounds 20

    @spec run([String.t()]) :: term()
    def run(lines) do
      # make monkeys
      monkeys =
        lines
        |> Enum.filter(&(&1 != ""))
        # each block has length 6
        |> Enum.chunk_every(6)
        |> Enum.into(%{}, &Main.make_monkey/1)
        |> IO.inspect()

      num_monkeys = map_size(monkeys)

      # simulate rounds
      monkeys =
        for _ <- 1..@num_rounds,
            index <- 0..(num_monkeys - 1),
            reduce: monkeys do
          monkeys ->
            monkey = monkeys[index]

            # process and throw each item
            for worry <- monkey.items, reduce: monkeys do
              monkeys ->
                worry = monkey.operation_fun.(worry)
                worry = div(worry, 3)

                throw_index =
                  if rem(worry, monkey.divisor) == 0 do
                    monkey.true_index
                  else
                    monkey.false_index
                  end

                monkeys
                |> Map.update!(index, &Map.update!(&1, :inspected, fn val -> val + 1 end))
                |> Map.update!(index, &Map.update!(&1, :items, fn items -> tl(items) end))
                |> Map.update!(
                  throw_index,
                  &Map.update!(&1, :items, fn items -> items ++ [worry] end)
                )
            end
        end

      monkeys
      |> Enum.map(fn {_k, v} -> v end)
      |> Enum.sort_by(& &1.inspected, :desc)
      |> Enum.map(& &1.inspected)
      |> Enum.take(2)
      |> Enum.reduce(&*/2)
    end
  end

  defmodule Part2 do
    alias Mix.Tasks.Main

    @num_rounds 10_000

    @spec run([String.t()]) :: term()
    def run(lines) do
      # Key observation for part 2: each monkey's divisor is prime.
      # We can keep the worry values small by working modulo each divisor.
      # However, each monkey may interact with any item, so we will need to store the worry value modulo
      # each divisor.

      # make monkeys
      monkeys =
        lines
        |> Enum.filter(&(&1 != ""))
        # each block has length 6
        |> Enum.chunk_every(6)
        |> Enum.into(%{}, &Main.make_monkey/1)
        |> IO.inspect()

      divisors =
        monkeys
        |> Enum.map(fn {k, monkey} -> {k, monkey.divisor} end)
        |> Map.new()

      monkeys =
        monkeys
        |> Enum.map(fn {k, monkey} ->
          {k,
           Map.update!(monkey, :items, fn items ->
             Enum.map(items, fn item ->
               divisors
               |> Enum.map(fn {k2, d} -> {k2, rem(item, d)} end)
               |> Map.new()
             end)
           end)}
        end)
        |> Map.new()

      IO.inspect(monkeys)

      num_monkeys = map_size(monkeys)

      # simulate rounds
      monkeys =
        for _ <- 1..@num_rounds,
            index <- 0..(num_monkeys - 1),
            reduce: monkeys do
          monkeys ->
            monkey = monkeys[index]

            # process and throw each item
            for worry_map <- monkey.items, reduce: monkeys do
              monkeys ->
                worry_map =
                  worry_map
                  |> Enum.map(fn {k, worry} ->
                    worry =
                      worry
                      |> monkey.operation_fun.()
                      |> rem(monkeys[k].divisor)

                    {k, worry}
                  end)
                  |> Map.new()

                throw_index =
                  if worry_map[index] == 0 do
                    monkey.true_index
                  else
                    monkey.false_index
                  end

                monkeys
                |> Map.update!(index, &Map.update!(&1, :inspected, fn val -> val + 1 end))
                |> Map.update!(index, &Map.update!(&1, :items, fn items -> tl(items) end))
                |> Map.update!(
                  throw_index,
                  &Map.update!(&1, :items, fn items -> items ++ [worry_map] end)
                )
            end
        end

      monkeys
      |> Enum.map(fn {_k, v} -> v end)
      |> Enum.sort_by(& &1.inspected, :desc)
      |> IO.inspect()
      |> Enum.map(& &1.inspected)
      |> Enum.take(2)
      |> Enum.reduce(&*/2)
    end
  end

  @impl Mix.Task
  def run([file | _]) do
    lines =
      file
      |> File.stream!()
      |> Stream.map(&String.trim/1)
      |> Enum.to_list()

    IO.puts("Part 1: #{Part1.run(lines)}")
    IO.puts("Part 2: #{Part2.run(lines)}")
  end
end
