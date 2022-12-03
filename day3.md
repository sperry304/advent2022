Advent of Code 2022, Day 3
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
  read_lines("data/day3a.txt") |> 
  as_tibble()
```

``` r
score <- tibble(
  intersect = c(letters, LETTERS),
  score = 1:52
)

df2 <- 
  df|> 
  transmute(
    len = str_length(value),
    p1 = str_sub(value, end = len / 2),
    p2 = str_sub(value, start = len / 2 + 1)
  )

df2$intersect <- mapply(function(x, y) paste(intersect(x, y), collapse=", "),
               strsplit(df2$p1, ""), strsplit(df2$p2, ""))

df2 |> 
  left_join(score, by = "intersect") |> 
  summarize(sum(score))
```

    ## # A tibble: 1 × 1
    ##   `sum(score)`
    ##          <int>
    ## 1         8493

``` r
df3 <- 
  df |> 
  mutate(
    grp = rep(1:(nrow(df) / 3), each = 3),
    elf_num = rep(str_c("elf", 1:3), times = nrow(df) / 3)
  ) |> 
  spread(elf_num, value)

df3$intersect_tmp <- 
   mapply(function(x, y) paste(intersect(x, y), collapse=", "),
               strsplit(df3$elf1, ""), strsplit(df3$elf2, ""))

df3$intersect <- 
   mapply(function(x, y) paste(intersect(x, y), collapse=", "),
               strsplit(df3$intersect_tmp, ""), strsplit(df3$elf3, ""))

df3 |> 
  left_join(score, by = "intersect") |> 
  summarize(sum(score))
```

    ## # A tibble: 1 × 1
    ##   `sum(score)`
    ##          <int>
    ## 1         2552
