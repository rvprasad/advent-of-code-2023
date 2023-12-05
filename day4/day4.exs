defmodule Day4 do
  def get_numbers(line) do
    String.split(line, " ")
    |> Enum.filter(&(&1 != ""))
    |> Enum.map(&elem(Integer.parse(&1), 0))
  end

  def get_content(filename) do
    File.stream!(filename)
    |> Stream.map(&String.trim_trailing/1)
    |> Stream.map(&String.replace(&1, ~r/\s+/, " "))
    |> Enum.map(fn line ->
      [card_id_part, card_data_part] = String.split(line, ":")
      [win_part, your_part] = String.split(card_data_part, "|")
      winning_numbers = get_numbers(win_part)
      your_numbers = get_numbers(your_part)
      card_id = elem(Integer.parse(Enum.at(String.split(card_id_part, " "), 1)), 0)
      %{card_id: card_id, winning_numbers: winning_numbers, your_numbers: your_numbers}
    end)
  end

  def calculate_matches(cards) do
    calculate_wins_for_card = fn card ->
      winning_numbers = MapSet.new(card[:winning_numbers])

      Enum.filter(card[:your_numbers], &MapSet.member?(winning_numbers, &1))
      |> Enum.count()
    end

    Enum.map(cards, calculate_wins_for_card)
  end

  def calculate_points(cards) do
    calculate_matches(cards)
    |> Enum.map(fn c -> if(c > 0, do: 2 ** (c - 1), else: 0) end)
  end

  def count_cards([], _, card_counts), do: card_counts

  def count_cards([p | p_tail], matches_and_copies, card_counts) do
    copies =
      if Enum.empty?(matches_and_copies) do
        1
      else
        (matches_and_copies
         |> Enum.map(&elem(&1, 1))
         |> Enum.reduce(fn c, acc -> acc + c end)) + 1
      end

    updated_matches_and_copies =
      matches_and_copies
      |> Enum.map(fn {m, c} -> {m - 1, c} end)
      |> Enum.filter(&(elem(&1, 0) != 0))

    count_cards(
      p_tail,
      if(p == 0,
        do: updated_matches_and_copies,
        else: updated_matches_and_copies ++ [{p, copies}]
      ),
      Enum.concat(card_counts, [copies])
    )
  end
end

filename = System.argv() |> List.first()
cards = Day4.get_content(filename)
points = Day4.calculate_points(cards) |> Enum.reduce(fn a, acc -> a + acc end)
IO.puts(points)
card_counts = Day4.count_cards(Day4.calculate_matches(cards), [], [])
IO.puts(Enum.reduce(card_counts, fn a, acc -> a + acc end))
