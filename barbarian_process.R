library(jsonlite)
library(tibble)
library(tidyverse)

json_string <- "database/build-47954.json"

extract_skill_tree_data <- function(json_string) {
  # Parse the JSON string
  parsed_json <- fromJSON(json_string)

  # Access the 'Skill Tree' data
  skill_tree_data <- parsed_json$Barbarian$`Skill Tree`
  

  # Initialize an empty tibble
  skill_tree_tibble <- tibble(
    id = integer(),
    req_points = integer(),
    reward_hash = numeric(),
    root_node = logical(),
    x_pos = numeric(),
    y_pos = numeric(),
    connections = list(),
    power_name = character(),
    power_tags = list(),
    power_tags_localized = list(),
    skill_desc = character(),
    skill_name = character(),
    skill_rankup_desc = character(),
    reward = list()
  )

  # Iterate over each skill and add to the tibble
  for (skill_id in names(skill_tree_data)) {
    skill <- skill_tree_data[[skill_id]]
    power <- skill$power
    reward <- skill$reward

    skill_tree_tibble <- add_row(
      skill_tree_tibble,
      id = skill$id,
      req_points = skill$req_points,
      reward_hash = skill$reward_hash,
      root_node = skill$root_node,
      x_pos = skill$x_pos,
      y_pos = skill$y_pos,
      connections = list(skill$connections),
      power_name = power$power_name,
      power_tags = list(power$power_tags),
      power_tags_localized = list(power$power_tags_localized),
      skill_desc = power$skill_desc,
      skill_name = power$skill_name,
      skill_rankup_desc = power$skill_rankup_desc,
      reward = list(reward)
    )
  }

  return(skill_tree_tibble)
}


# Example usage:

result <- extract_skill_tree_data(json_string)



extract_paragon_board <- function(json_string) {
  # Parse the JSON string
  parsed_json <- fromJSON(json_string)

  # Access the 'Paragon (Board)' data
  paragon_board_data <- parsed_json$Barbarian$`Paragon (Board)`

  # Initialize an empty tibble
  paragon_board_tibble <- tibble(
    paragon_id = character(),
    name = character(),
    name_localized = list(),
    data = list()
  )

  # Iterate over each paragon board item and add to the tibble
  for (paragon_id in names(paragon_board_data)) {
    paragon_item <- paragon_board_data[[paragon_id]]

    paragon_board_tibble <- add_row(
      paragon_board_tibble,
      paragon_id = paragon_id,
      name = paragon_item$name,
      name_localized = list(paragon_item$name_localized),
      data = list(paragon_item$data)
    )
  }

  return(paragon_board_tibble)
}

extract_paragon_node <- function(json_string) {
  # Parse the JSON string
  parsed_json <- fromJSON(json_string)

  # Access the 'Paragon (Board)' data
  paragon_node_data <- parsed_json$Barbarian$`Paragon (Node)`
  # Initialize an empty tibble
  paragon_node_tibble <- tibble(
    paragon_id = character(),
    name = character(),
    name_localized = list(),
    data = list()
  )

  # Iterate over each paragon board item and add to the tibble
  for (paragon_id in names(paragon_board_data)) {
    paragon_item <- paragon_board_data[[paragon_id]]

    paragon_board_tibble <- add_row(
      paragon_board_tibble,
      paragon_id = paragon_id,
      name = paragon_item$name,
      name_localized = list(paragon_item$name_localized),
      data = list(paragon_item$data)
    )
  }

  return(paragon_board_tibble)
}

# Example usage:
# json_input <- '{"Barbarian": {"Paragon (Board)": {...}}}'
result <-
  extract_paragon_board(json_string) |>
  unnest(data) |>
  select(paragon_id, name, data) |>
  rowid_to_column()

get_columns_paragon <- function(rowid_var) {
  code <-
    tibble(
      columns =
        result |>
          filter(rowid == rowid_var) |>
          select(data) |>
          pull(data) |>
          str_split(pattern = ",") |>
          unlist()
    ) |>
    mutate(columns = ifelse(columns == "", yes = "NA", no = columns)) |>
    mutate(rowid = rowid_var) |>
    mutate(across(everything(), as.character))


  return(code)
}


paragon_board <-
  result |>
  select(rowid, paragon_id, name) |>
  mutate(across(everything(), as.character)) |>
  inner_join(
    map(.x = result$rowid, .f = ~ get_columns_paragon(rowid_var = .x)) |>
      reduce(bind_rows), "rowid"
  )



### Paragon nodes 

# Define the function to extract the Paragon Node
extract_paragon_node_barb <- function(json_string) {
  # Parse the JSON string
  parsed_json <- fromJSON(json_string)
  
  # Access the 'node' data
  node_data <- 
    parsed_json$Barbarian$`Paragon (Node)` |> 
    enframe() |> 
    unnest_auto(value) |> 
    filter(value_id != "desc_localized") |> 
    filter(value_id != "name_localized") |> 
    filter(value_id != "tags_localized") 
  
  # Extract the attributes
  code <- 
    node_data |> 
    select(name) |> 
    left_join(node_data |> filter(value_id == "desc") |> unnest_auto(value) |> rename(desc = value) |> select(-value_id)) |> 
    left_join(node_data |> filter(value_id == "name") |> unnest_auto(value) |> rename(node_name = value)|> select(-value_id)) |> 
    left_join(node_data |> filter(value_id == "tags") |> unnest_auto(value) |> rename(tags = value)|> select(-value_id)) |> 
    left_join(node_data |> filter(value_id == "threshold_bonus") |> unnest_auto(value)) |> 
    left_join(node_data |> filter(value_id == "threshold_requirements") |> unnest_auto(value) |> select(name,Barbarian) |> unnest_auto(Barbarian)) |> 
    left_join(node_data |> filter(value_id == "attributes") |> select(name,value) |>  unnest(value) |>     unnest_wider(value) |>     unnest(formula_data) |>     unnest(formula_data)) |> 
    unique()
  
  return(code)
  }
  
paragon_node_barb <- extract_paragon_node_barb(json_string = json_string)
  

