defmodule Day19.Part1 do
  def canonical_target(target) do
    case target do
      "R" -> :reject
      "A" -> :accept
      x -> x
    end
  end

  defp create_action_function(guard, splitter, operator, target) do
    [category, value] = String.split(guard, splitter)
    value = Integer.parse(value) |> elem(0)
    target = canonical_target(target)

    fn part ->
      if apply(operator, [part[category], value]), do: target, else: nil
    end
  end

  defp create_workflow_action(action_desc) do
    if String.contains?(action_desc, ":") do
      [guard, target] = action_desc |> String.split(":")

      cond do
        String.contains?(guard, "<") ->
          create_action_function(guard, "<", &Kernel.</2, target)

        String.contains?(guard, ">") ->
          create_action_function(guard, ">", &Kernel.>/2, target)
      end
    else
      case action_desc do
        "R" -> fn _ -> :reject end
        "A" -> fn _ -> :accept end
        x -> fn _ -> x end
      end
    end
  end

  defp parse_workflow(workflow_desc) do
    [name, remainder] = String.split(workflow_desc, "{")

    workflow =
      remainder
      |> String.replace("}", "")
      |> String.split(",")
      |> Enum.map(&create_workflow_action/1)

    {name, workflow}
  end

  defp parse_part(part) do
    part
    |> String.replace("{", "")
    |> String.replace("}", "")
    |> String.split(",")
    |> Enum.map(fn x ->
      [part, rating] = x |> String.split("=")
      {part, elem(Integer.parse(rating), 0)}
    end)
    |> Map.new()
  end

  defp get_workflows_and_parts(filename) do
    [workflows, _, parts] =
      File.stream!(filename)
      |> Stream.map(&String.trim_trailing/1)
      |> Enum.chunk_by(&(&1 == ""))

    workflows = workflows |> Enum.map(&parse_workflow/1) |> Map.new()
    parts = parts |> Enum.map(&parse_part/1)
    {workflows, parts}
  end

  defp part1_helper(workflow_name, part, workflows) do
    case workflow_name do
      :reject ->
        false

      :accept ->
        true

      _ ->
        workflows[workflow_name]
        |> Enum.find_value(fn action -> action.(part) end)
        |> part1_helper(part, workflows)
    end
  end

  def solve(filename) do
    {workflows, parts} = get_workflows_and_parts(filename)

    parts
    |> Enum.filter(&part1_helper("in", &1, workflows))
    |> Enum.map(&Map.values(&1))
    |> List.flatten()
    |> Enum.sum()
    |> IO.puts()
  end
end

defmodule Day19.Part2 do
  defp get_target_category_limit(action_desc, operator) do
    [guard, target] = String.split(action_desc, ":")
    [category, limit] = String.split(guard, operator)
    {Day19.Part1.canonical_target(target), category, limit}
  end

  defp create_action(action_desc) do
    cond do
      String.contains?(action_desc, "<") ->
        {target, category, limit} = get_target_category_limit(action_desc, "<")

        fn part ->
          limit = Integer.parse(limit) |> elem(0)
          {min, max} = part[category]

          cond do
            max < limit ->
              {{target, part}, nil}

            limit < min ->
              {{:reject, nil}, part}

            true ->
              {
                {target, Map.put(part, category, {min, limit - 1})},
                Map.put(part, category, {limit, max})
              }
          end
        end

      String.contains?(action_desc, ">") ->
        {target, category, limit} = get_target_category_limit(action_desc, ">")

        fn part ->
          limit = Integer.parse(limit) |> elem(0)
          {min, max} = part[category]

          cond do
            max < limit ->
              {{:reject, nil}, part}

            limit < min ->
              {{target, part}, nil}

            true ->
              {
                {target, Map.put(part, category, {limit + 1, max})},
                Map.put(part, category, {min, limit})
              }
          end
        end

      action_desc == "A" ->
        fn part -> {{:accept, part}, {:reject, nil}} end

      action_desc == "R" ->
        fn _ -> {{:reject, nil}, {:reject, nil}} end

      true ->
        fn part -> {{action_desc, part}, {:reject, nil}} end
    end
  end

  defp parse_workflow(workflow_desc) do
    [name, remainder] = String.split(workflow_desc, "{")

    actions =
      remainder
      |> String.replace("}", "")
      |> String.split(",")
      |> Enum.map(&create_action/1)

    workflow = fn part ->
      actions
      |> Enum.reduce_while({[], part}, fn action, {results, part} ->
        if part == nil do
          {:halt, {results, nil}}
        else
          {satisfied_verdict_and_part, unsatisfied_part} = action.(part)
          {:cont, {[satisfied_verdict_and_part | results], unsatisfied_part}}
        end
      end)
      |> elem(0)
    end

    {name, workflow}
  end

  defp get_workflows(filename) do
    File.stream!(filename)
    |> Stream.map(&String.trim_trailing/1)
    |> Enum.chunk_by(&(&1 == ""))
    |> hd
    |> Enum.map(&parse_workflow/1)
    |> Map.new()
  end

  defp process_part(part, workflow_name, workflows) do
    workflows[workflow_name].(part)
    |> Enum.map(fn {verdict, updated_part} ->
      case verdict do
        :accept ->
          updated_part
          |> Map.values()
          |> Enum.map(fn {min, max} ->
            if max - min > 0, do: max - min + 1, else: 0
          end)
          |> Enum.product()

        :reject ->
          0

        next_workflow_name ->
          process_part(updated_part, next_workflow_name, workflows)
      end
    end)
    |> Enum.sum()
  end

  def solve(filename) do
    workflows = get_workflows(filename)

    for(x <- "xmas" |> String.split("") |> Enum.filter(&(&1 != "")), do: [x, {1, 4000}])
    |> Enum.map(&List.to_tuple/1)
    |> Map.new()
    |> process_part("in", workflows)
    |> IO.puts()
  end
end

filename = System.argv() |> List.first()
Day19.Part1.solve(filename)
Day19.Part2.solve(filename)
