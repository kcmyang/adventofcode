defmodule Day02 do
  @type rps() :: :rock | :paper | :scissors

  @type round() :: {theirs :: rps(), yours :: rps()}

  @spec score_shape(rps()) :: integer()
  def score_shape(shape) do
    case shape do
      :rock -> 1
      :paper -> 2
      :scissors -> 3
    end
  end

  @win_score 6
  @draw_score 3
  @loss_score 0

  @spec score_outcome(round()) :: integer()
  def score_outcome(round) do
    case round do
      {:rock, :rock} -> @draw_score
      {:paper, :paper} -> @draw_score
      {:scissors, :scissors} -> @draw_score
      {:rock, :paper} -> @win_score
      {:paper, :scissors} -> @win_score
      {:scissors, :rock} -> @win_score
      _ -> @loss_score
    end
  end

  @spec score_round(round()) :: integer()
  def score_round(round) do
    {_theirs, yours} = round
    score_outcome(round) + score_shape(yours)
  end

  defmodule Part1 do
    @spec string_to_rps(String.t()) :: Day02.rps()
    def string_to_rps(s) do
      case s do
        "A" -> :rock
        "B" -> :paper
        "C" -> :scissors
        "X" -> :rock
        "Y" -> :paper
        "Z" -> :scissors
      end
    end

    @spec line_to_round(String.t()) :: Day02.round()
    def line_to_round(line) do
      [theirs, yours] = String.split(line, " ")
      {string_to_rps(theirs), string_to_rps(yours)}
    end
  end

  defmodule Part2 do
    @spec string_to_rps(String.t()) :: Day02.rps()
    def string_to_rps(s) do
      case s do
        "A" -> :rock
        "B" -> :paper
        "C" -> :scissors
      end
    end

    @spec fulfill_strategy(Day02.rps(), String.t()) :: Day02.rps()
    def fulfill_strategy(theirs, strategy) do
      case strategy do
        # lose
        "X" ->
          case theirs do
            :rock -> :scissors
            :paper -> :rock
            :scissors -> :paper
          end

        # draw
        "Y" ->
          theirs

        # win
        "Z" ->
          case theirs do
            :rock -> :paper
            :paper -> :scissors
            :scissors -> :rock
          end
      end
    end

    @spec line_to_round(String.t()) :: Day02.round()
    def line_to_round(line) do
      [theirs, strategy] = String.split(line, " ")
      theirs = string_to_rps(theirs)
      yours = fulfill_strategy(theirs, strategy)
      {theirs, yours}
    end
  end

  @spec part1(Enumerable.t()) :: integer()
  def part1(lines) do
    lines
    |> Enum.map(&Day02.Part1.line_to_round/1)
    |> Enum.map(&Day02.score_round/1)
    |> Enum.sum()
  end

  @spec part2(Enumerable.t()) :: integer()
  def part2(lines) do
    lines
    |> Enum.map(&Day02.Part2.line_to_round/1)
    |> Enum.map(&Day02.score_round/1)
    |> Enum.sum()
  end
end

[file | _] = System.argv()

lines =
  file
  |> File.stream!()
  |> Stream.map(&String.trim_trailing/1)

IO.puts(Day02.part1(lines))
IO.puts(Day02.part2(lines))
