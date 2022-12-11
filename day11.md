Advent of Code 2022, Day 11
================
Skip Perry
December 2022

``` r
options(dplyr.summarise.inform = FALSE)
library(tidyverse)
```

``` r
df_raw <- 
  read_lines("data/day11a.txt") |> 
  as_tibble()

df <- 
  df_raw |> 
  mutate(monkey_num = floor((row_number() - 1) / 7)) |> 
  group_by(monkey_num) |> 
  summarize(value = paste(value, collapse = " ")) |> 
  separate(value, into = c("a", "b", "c", "d", "e", "f", "g"), sep = "\\:") |> 
  mutate(
    c = str_c("Empty, ", str_remove(c, " Operation")),
    d = str_remove(str_remove(d, "new = old "), " Test"),
    e = str_remove(str_remove(e, "divisible by "), " If true"),
    f = str_remove(str_remove(f, "throw to monkey "), " If false"),
    g = str_remove(g, "throw to monkey ")
  ) |> 
  transmute(
    monkey_num = monkey_num + 1, 
    items = str_split(c, ", "), 
    operation = if_else(str_detect(d, "\\*"), "multiply", "add"),
    add_mult_by = str_trim(str_remove(d, "\\* |\\+")),
    divide_by = as.numeric(e),
    throw_true = as.numeric(f) + 1,
    throw_false = as.numeric(g) + 1
  )

worry <- function(starting_value, operation, add_mult_by, worry_factor) {
  starting_value <- as.numeric(starting_value) 
  
  if (operation == "multiply" & add_mult_by == "old") {
    tmp_val <- starting_value * starting_value
  } else if (operation == "multiply") {
    tmp_val <- starting_value * as.numeric(add_mult_by)
  } else {
    tmp_val <- starting_value + as.numeric(add_mult_by)
  }

    return(floor(tmp_val / worry_factor))
}

monkey_business <- function(df, n_reps, worry_factor) {
  total_inspections <- rep(0, nrow(df))
  cheated_to_get_this_factor <- prod(df$divide_by)
  
  for (n in 1:n_reps) {
    for (i in 1:nrow(df)) {
      if (length(df$items[i][[1]]) == 1) { next }
      
      items_to_inspect <- df$items[i][[1]][-1]
      
      total_inspections[i] <- total_inspections[i] + length(items_to_inspect)
      
      for (j in 1:length(items_to_inspect)) {
        worry_val <- 
          worry(items_to_inspect[j], df$operation[i], df$add_mult_by[i], worry_factor) %% 
          cheated_to_get_this_factor
        worry_modulus <- worry_val %% df$divide_by[i]
        if (worry_modulus == 0) {
          df$items[df$throw_true[i]][[1]] <- c(df$items[df$throw_true[i]][[1]], as.character(worry_val))
        } else {
          df$items[df$throw_false[i]][[1]] <- c(df$items[df$throw_false[i]][[1]], as.character(worry_val))
        }
      }
      
      df$items[i] <- list(df$items[i][[1]][1])
    }  
  }
  return(max(total_inspections) * sort(total_inspections, TRUE)[2])
}
```

``` r
monkey_business(df, n_reps = 20, worry_factor = 3)
```

    ## [1] 100345

``` r
monkey_business(df, n_reps = 10000, worry_factor = 1)
```

    ## [1] 28537348205
