defmodule Day14 do
  def get_platform(filename) do
    File.stream!(filename)
    |> Stream.map(&String.trim_trailing/1)
    |> Enum.map(fn line -> String.split(line, "") |> Enum.filter(&(&1 != "")) end)
  end

  def calculate_load(platform) do
    platform
    |> Enum.map(fn line ->
      len = length(line)

      Enum.with_index(line, 0)
      |> Enum.reduce(0, fn e, acc ->
        {elem, i} = e

        case elem do
          "O" -> acc + len - i
          _ -> acc
        end
      end)
    end)
    |> Enum.sum()
  end

  def process1(platform) do
    Enum.zip(platform) |> Enum.map(&Tuple.to_list/1) |> tilt |> calculate_load |> IO.puts()
  end

  def rotate(platform) do
    platform
    |> Enum.zip()
    |> Enum.map(fn x -> Tuple.to_list(x) |> Enum.reverse() end)
  end

  def tilt(platform) do
    reps = length(hd(platform))

    platform
    |> Enum.map(fn line ->
      Enum.with_index(line, 0)
      |> Enum.reduce({0, List.duplicate(".", reps)}, fn e, acc ->
        {elem, i} = e
        {vacant_position, line} = acc

        case elem do
          "O" -> {vacant_position + 1, List.replace_at(line, vacant_position, "O")}
          "#" -> {i + 1, List.replace_at(line, i, "#")}
          "." -> acc
        end
      end)
    end)
    |> Enum.map(fn x -> elem(x, 1) end)
  end

  def process2(platform, seq_id, platform2seq_id, seq_id2platform, num_cycles) do
    if Map.has_key?(platform2seq_id, platform) do
      prev_position = Map.get(platform2seq_id, platform)
      cycle_length = seq_id - prev_position
      remaining_tilts = rem(num_cycles * 4 - prev_position, cycle_length)
      Map.get(seq_id2platform, prev_position + remaining_tilts)
    else
      updated_platform2seq_id = Map.put(platform2seq_id, platform, seq_id)
      updated_seq_id2platform = Map.put(seq_id2platform, seq_id, platform)
      tilted = tilt(platform)
      rotated = rotate(tilted)
      process2(rotated, seq_id + 1, updated_platform2seq_id, updated_seq_id2platform, num_cycles)
    end
  end

  def process2(platform, num_cycles) do
    platform
    |> Enum.zip()
    |> Enum.map(&Tuple.to_list/1)
    |> Enum.reverse()
    |> process2(0, Map.new(), Map.new(), num_cycles)
    |> calculate_load()
    |> IO.puts()
  end
end

filename = System.argv() |> List.first()
platform = Day14.get_platform(filename)
Day14.process1(platform)
Day14.process2(platform, 1_000_000_000)
