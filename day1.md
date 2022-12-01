Advent of Code 2022, Day 1
================
Skip Perry
December 2022

``` r
knitr::opts_chunk$set(echo = TRUE)
options(dplyr.summarise.inform = FALSE)
library(tidyverse)
```

``` r
df <- 
  read_lines("day1a.txt") |> 
  as_tibble()

df |> 
  mutate(
    new = if_else(value > 0, FALSE, TRUE),
    elf_num = cumsum(new),
    value = as.integer(value)
  ) |> 
  filter(new == FALSE) |> 
  group_by(elf_num) |> 
  summarize(value = sum(value)) |> 
  arrange(desc(value))
```

    ## # A tibble: 254 × 2
    ##    elf_num value
    ##      <int> <int>
    ##  1     129 65912
    ##  2     226 65610
    ##  3     195 64103
    ##  4     245 64039
    ##  5      99 63879
    ##  6     207 63584
    ##  7     221 63008
    ##  8      80 62886
    ##  9     139 62426
    ## 10      19 62205
    ## # … with 244 more rows
