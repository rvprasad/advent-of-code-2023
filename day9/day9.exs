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

  def predict_values(report) do
    if Enum.all?(report, fn x -> x == 0 end) do
      {0, 0}
    else
      diffs =
        Enum.zip(Enum.drop(report, 1), Enum.drop(report, -1))
        |> Enum.map(fn e -> elem(e, 0) - elem(e, 1) end)

      predicted_values = predict_values(diffs)

      {List.first(report) - elem(predicted_values, 0),
       List.last(report) + elem(predicted_values, 1)}
    end
  end

  def process(reports) do
    predictions = Enum.unzip(Enum.map(reports, &Day9.predict_values/1))
    IO.puts(Enum.sum(elem(predictions, 0)))
    IO.puts(Enum.sum(elem(predictions, 1)))
  end
end

filename = System.argv() |> List.first()
reports = Day9.get_reports(filename)
Day9.process(reports)
