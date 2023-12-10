defmodule Day9 do
  def get_reports(filename) do
    File.stream!(filename)
    |> Stream.map(&String.trim_trailing/1)
    |> Enum.map(fn line ->
      String.split(line, " ")
      |> Enum.filter(&(&1 != ""))
      |> Enum.map(&elem(Integer.parse(&1), 0))
    end)
  end

  def predict_next_value(report) do
    if Enum.all?(report, fn x -> x == 0 end) do
      0
    else
      diffs = Enum.zip(Enum.drop(report, 1), Enum.drop(report, -1))
      |> Enum.map(fn e -> elem(e, 0) - elem(e, 1) end)
      List.last(report) + predict_next_value(diffs)
    end
  end

  def process1(reports) do
    predictions = Enum.map(reports, &Day9.predict_next_value/1)
    IO.puts(Enum.sum(predictions))
  end

  def predict_prev_value(report) do
    if Enum.all?(report, fn x -> x == 0 end) do
      0
    else
      diffs = Enum.zip(Enum.drop(report, 1), Enum.drop(report, -1))
      |> Enum.map(fn e -> elem(e, 0) - elem(e, 1) end)
      ret = List.first(report) - predict_prev_value(diffs)
      ret
    end
  end

  def process2(reports) do
    predictions = Enum.map(reports, &Day9.predict_prev_value/1)
    IO.puts(Enum.sum(predictions))
  end
end

filename = System.argv() |> List.first()
reports = Day9.get_reports(filename)
Day9.process1(reports)
Day9.process2(reports)
