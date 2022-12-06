Advent of Code 2022, Day 6
================
Skip Perry
December 2022

``` r
options(dplyr.summarise.inform = FALSE)
library(tidyverse)
```

``` r
df <- 
  read_lines("data/day6a.txt") |> 
  as_tibble()
```

``` r
day6 <- function(part) {
  cut_val <- ifelse(part == "part1", 4L, 14L)
  
  df_tmp <- 
    str_split(df |> pull(value), pattern = "") |> 
    unlist() |> 
    as_tibble() |> 
    mutate(
      rn = row_number(),
      lag1 = lag(value),
      lag2 = lag(lag1),
      lag3 = lag(lag2),
      lag4 = lag(lag3),
      lag5 = lag(lag4),
      lag6 = lag(lag5),
      lag7 = lag(lag6),
      lag8 = lag(lag7),
      lag9 = lag(lag8),
      lag10 = lag(lag9),
      lag11 = lag(lag10),
      lag12 = lag(lag11),
      lag13 = lag(lag12)
    )
  
  if (part == "part1") {
    df_tmp <- 
      df_tmp |> 
      select(rn, value, lag1, lag2, lag3)
  }
  
  result <- 
    df_tmp |> 
    gather(key = "key", value = "value", -rn) |> 
    filter(!is.na(value)) |> 
    group_by(rn) |> 
    summarize(n_val = n_distinct(value)) |> 
    filter(n_val == cut_val) |> 
    summarize(first = min(rn)) |> 
    pull(first)
  
  return(result)
}

day6("part1")
```

    ## [1] 1300

``` r
day6("part2")
```

    ## [1] 3986
