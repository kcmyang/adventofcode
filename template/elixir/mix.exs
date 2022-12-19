defmodule Solution.MixProject do
  use Mix.Project

  def project do
    [
      app: :solution,
      version: "0.1.0",
      elixir: "~> 1.14",
      deps: deps(),
      aliases: aliases()
    ]
  end

  def application do
    [
      extra_applications: [:logger]
    ]
  end

  defp aliases do
    [
      p1i: "part1 input.txt",
      p1s: "part1 sample.txt",
      p2i: "part2 input.txt",
      p2s: "part2 sample.txt"
    ]
  end

  defp deps do
    []
  end
end
