defmodule Main do
  # Utils

  @doc "Splits enumerable on every element for which fun returns a truthy value."
  @spec chunk_starting_by(Enumerable.t(), (Enumerable.element() -> as_boolean(any()))) :: [list()]
  def chunk_starting_by(enumerable, fun) do
    enumerable
    |> Enum.with_index()
    |> Enum.scan(fn {elem, index}, {_, chunk_index} ->
      {elem, if(fun.(elem), do: index, else: chunk_index)}
    end)
    |> Enum.chunk_by(fn {_elem, index} -> index end)
    |> Enum.map(fn chunk -> Enum.map(chunk, fn {elem, _index} -> elem end) end)
  end

  # Input parsing

  @type name() :: String.t()
  @type command() ::
          {:cd, name()} | {:ls, [{:dir, name()} | {:file, {name(), non_neg_integer()}}]}

  @spec chunk_to_command([String.t()]) :: command()
  def chunk_to_command([head | rest]) do
    case String.split(head) do
      ["$", "cd", dir] ->
        {:cd, dir}

      ["$", "ls"] ->
        output =
          rest
          |> Enum.map(fn line ->
            case String.split(line) do
              ["dir", dir] -> {:dir, dir}
              [size, file] -> {:file, {file, String.to_integer(size)}}
            end
          end)

        {:ls, output}
    end
  end

  @spec data_to_commands(Enumerable.t()) :: [command()]
  def data_to_commands(data) do
    data
    |> Main.chunk_starting_by(&String.match?(&1, ~r/^\$/))
    |> Enum.map(&Main.chunk_to_command/1)
  end

  # Data representation

  defmodule File do
    @enforce_keys [:name, :size]
    defstruct [:name, :size]

    @type name() :: String.t()
    @type t() :: %File{name: name(), size: non_neg_integer()}
  end

  defmodule Dir do
    @enforce_keys [:name]
    defstruct [:name, :size, children: %{}]

    @type name() :: String.t()
    @type t() :: %Dir{
            name: name(),
            size: non_neg_integer(),
            children: %{optional(name()) => %File{} | %Dir{}}
          }

    @behaviour Access

    @impl Access
    def fetch(dir, key) do
      Map.fetch(dir.children, key)
    end

    @impl Access
    def get_and_update(dir, key, fun) do
      {value, new_children} = Map.get_and_update(dir.children, key, fun)
      {value, %{dir | children: new_children}}
    end

    @impl Access
    def pop(dir, key) do
      {value, new_children} = Map.pop(dir.children, key)
      {value, %{dir | children: new_children}}
    end
  end

  @type filesystem() :: %{}
  @type filesystem_object() :: %File{} | %Dir{}

  @spec make_filesystem(Enumerable.t()) :: filesystem()
  def make_filesystem(data) do
    commands = Main.data_to_commands(data)

    {_path, fs} =
      for command <- commands,
          reduce: {["/"], %{"/" => %Dir{name: "/"}}} do
        {path, fs} ->
          case command do
            {:cd, name} ->
              path =
                case name do
                  "/" -> ["/"]
                  ".." -> Enum.drop(path, -1)
                  _ -> path ++ [name]
                end

              {path, fs}

            {:ls, output} ->
              fs =
                for line <- output, reduce: fs do
                  fs ->
                    case line do
                      {:dir, name} ->
                        update_in(fs, path, fn dir ->
                          %{dir | children: Map.put_new(dir.children, name, %Dir{name: name})}
                        end)

                      {:file, {name, size}} ->
                        update_in(fs, path, fn dir ->
                          %{
                            dir
                            | children: Map.put(dir.children, name, %File{name: name, size: size})
                          }
                        end)
                    end
                end

              {path, fs}
          end
      end

    fs
  end

  @spec with_directory_sizes(f) :: f when f: filesystem() | filesystem_object()
  def with_directory_sizes(f) do
    case f do
      %{"/" => dir = %Dir{}} ->
        %{f | "/" => Main.with_directory_sizes(dir)}

      %File{} ->
        f

      %Dir{children: children} ->
        new_children =
          Enum.map(children, fn {key, child} -> {key, Main.with_directory_sizes(child)} end)

        size = new_children |> Enum.map(fn {_key, child} -> child.size end) |> Enum.sum()

        %{f | size: size, children: new_children}
    end
  end

  @doc "List of directories with children information omitted."
  @spec bare_directories(filesystem() | filesystem_object()) :: [%Dir{}]
  def bare_directories(f) do
    case f do
      %{"/" => dir = %Dir{}} ->
        Main.bare_directories(dir)

      %File{} ->
        []

      %Dir{children: children} ->
        [
          %{f | children: %{}}
          | Enum.flat_map(children, fn {_key, child} -> Main.bare_directories(child) end)
        ]
    end
  end

  defmodule Part1 do
    @max_size 100_000

    @spec run(Enumerable.t()) :: integer()
    def run(data) do
      data
      |> Main.make_filesystem()
      |> Main.with_directory_sizes()
      |> Main.bare_directories()
      |> Enum.map(& &1.size)
      |> Enum.filter(&(&1 <= @max_size))
      |> Enum.sum()
    end
  end

  defmodule Part2 do
    @total_disk_space 70_000_000
    @unused_space_needed 30_000_000

    @spec run(Enumerable.t()) :: String.t()
    def run(data) do
      dirs =
        data
        |> Main.make_filesystem()
        |> Main.with_directory_sizes()
        |> Main.bare_directories()

      used_space = Enum.find(dirs, &(&1.name == "/")).size
      unused_space = @total_disk_space - used_space
      need_to_free = @unused_space_needed - unused_space

      %Dir{size: size} =
        dirs
        |> Enum.filter(&(&1.size >= need_to_free))
        |> Enum.min_by(& &1.size)

      size
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
