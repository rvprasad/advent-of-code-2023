defmodule Trajectory do
  defstruct xp: 0, yp: 0, zp: 0, xv: 0, yv: 0, zv: 0
end

defmodule Day24 do
  require Trajectory

  def get_positions_and_velocities(filename) do
    File.stream!(filename)
    |> Stream.map(&String.trim_trailing/1)
    |> Stream.map(&String.replace(&1, " ", ""))
    |> Stream.map(&String.split(&1, "@"))
    |> Stream.map(fn [pos, vel] ->
      [xp, yp, zp] = String.split(pos, ",") |> Enum.map(fn x -> Integer.parse(x) |> elem(0) end)
      [xv, yv, zv] = String.split(vel, ",") |> Enum.map(fn x -> Integer.parse(x) |> elem(0) end)

      %Trajectory{xp: xp, yp: yp, zp: zp, xv: xv, yv: yv, zv: zv}
    end)
    |> Enum.to_list()
  end

  def solve_part1(trajectories, x, y) do
    for {t1, i} <- Enum.with_index(trajectories) do
      for {t2, j} <- Enum.with_index(trajectories) do
        if i < j do
          calculate_intersection(t1, t2)
        end
      end
    end
    |> List.flatten()
    |> Enum.filter(fn x -> !is_nil(x) end)
    |> Enum.filter(fn {_, _, intersects, ix, iy} ->
      intersects && is_in_test_area(ix, iy, x, y)
    end)
    |> Enum.filter(fn {t1, t2, _, ix, iy} ->
      is_in_future(t1, t2, ix, iy)
    end)
    |> length
    |> IO.puts()
  end

  defp calculate_intersection(t1, t2) do
    {slope1, intercept1} = calculate_slope_and_intercept(t1)
    {slope2, intercept2} = calculate_slope_and_intercept(t2)

    if slope1 == slope2 do
      {t1, t2, false, -1, -1}
    else
      ix = (intercept2 - intercept1) / (slope1 - slope2)
      iy = slope1 * ix + intercept1
      {t1, t2, true, ix, iy}
    end
  end

  defp calculate_slope_and_intercept(%Trajectory{xp: xp, yp: yp, xv: xv, yv: yv}) do
    slope = yv / xv
    intercept = yv * -xp / xv + yp
    {slope, intercept}
  end

  defp is_in_test_area(ix, iy, low, high) do
    low <= ix && ix <= high && low <= iy && iy <= high
  end

  defp is_in_future(t1, t2, ix, iy) do
    x1_is_in_future =
      (t1.xv < 0 && ix < t1.xp) || (t1.xv > 0 && ix > t1.xp) ||
        ix == t1.xp

    y1_is_in_future = (t1.yv < 0 && iy <= t1.yp) || (t1.yv > 0 && iy >= t1.yp) || iy == t1.yp
    x2_is_in_future = (t2.xv < 0 && ix <= t2.xp) || (t2.xv > 0 && ix >= t2.xp) || ix == t2.xp
    y2_is_in_future = (t2.yv < 0 && iy <= t2.yp) || (t2.yv > 0 && iy >= t2.yp) || iy == t2.yp
    x1_is_in_future && y1_is_in_future && x2_is_in_future && y2_is_in_future
  end
end

filename = System.argv() |> List.first()
low = System.argv() |> Enum.drop(1) |> List.first() |> Integer.parse() |> elem(0)
high = System.argv() |> Enum.drop(2) |> List.first() |> Integer.parse() |> elem(0)

trajectories = Day24.get_positions_and_velocities(filename)
Day24.solve_part1(trajectories, low, high)
