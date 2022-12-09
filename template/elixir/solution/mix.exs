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
      input: "main input.txt",
      sample: "main sample.txt"
    ]
  end

  defp deps do
    []
  end
end
