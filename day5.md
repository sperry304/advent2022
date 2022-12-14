Advent of Code 2022, Day 5
================
Skip Perry
December 2022

``` r
#knitr::opts_chunk$set(echo = TRUE)
options(dplyr.summarise.inform = FALSE)
library(tidyverse)
```

``` r
df <- 
  read_lines("data/day5a.txt") |> 
  as_tibble()

init_pos <- 
  tibble::tribble(
    ~crane, ~pos, ~item,
        1L,   4L,   "G",
        1L,   3L,   "P",
        1L,   2L,   "N",
        1L,   1L,   "R",
        2L,   8L,   "H",
        2L,   7L,   "V",
        2L,   6L,   "S",
        2L,   5L,   "C",
        2L,   4L,   "L",
        2L,   3L,   "B",
        2L,   2L,   "J",
        2L,   1L,   "T",
        3L,   6L,   "L",
        3L,   5L,   "N",
        3L,   4L,   "M",
        3L,   3L,   "B",
        3L,   2L,   "D",
        3L,   1L,   "T",
        4L,   5L,   "B",
        4L,   4L,   "S",
        4L,   3L,   "P",
        4L,   2L,   "V",
        4L,   1L,   "R",
        5L,   8L,   "H",
        5L,   7L,   "V",
        5L,   6L,   "M",
        5L,   5L,   "W",
        5L,   4L,   "S",
        5L,   3L,   "Q",
        5L,   2L,   "C",
        5L,   1L,   "G",
        6L,   7L,   "J",
        6L,   6L,   "B",
        6L,   5L,   "D",
        6L,   4L,   "C",
        6L,   3L,   "S",
        6L,   2L,   "Q",
        6L,   1L,   "W",
        7L,   3L,   "L",
        7L,   2L,   "Q",
        7L,   1L,   "F",
        8L,   8L,   "V",
        8L,   7L,   "F",
        8L,   6L,   "L",
        8L,   5L,   "D",
        8L,   4L,   "T",
        8L,   3L,   "H",
        8L,   2L,   "M",
        8L,   1L,   "W",
        9L,   7L,   "F",
        9L,   6L,   "J",
        9L,   5L,   "M",
        9L,   4L,   "V",
        9L,   3L,   "B",
        9L,   2L,   "P",
        9L,   1L,   "L"
    )
```

``` r
df2 <- 
  df|> 
  separate(value, into = c("num", "from_pos", "to_pos"), sep = "\\s") |> 
  mutate_all(as.numeric)

df2
```

    ## # A tibble: 503 × 3
    ##      num from_pos to_pos
    ##    <dbl>    <dbl>  <dbl>
    ##  1     3        3      7
    ##  2     4        1      9
    ##  3     5        6      3
    ##  4     6        9      8
    ##  5     2        9      5
    ##  6     4        3      7
    ##  7     1        3      6
    ##  8     3        5      7
    ##  9     1        2      1
    ## 10     4        7      8
    ## # … with 493 more rows

``` r
day5 <- function(lift_process) {
  stacks <- list()
  for (i in 1:9) {
    stacks[[i]] <- init_pos |> filter(crane == i) |> arrange(pos) |> pull(item)
  }

  for (i in 1:nrow(df2)) {
    num <- df2$num[i]
    from_pos <- df2$from_pos[i]
    to_pos <- df2$to_pos[i]
  
    if (lift_process == "one by one") {
      stacks[[to_pos]] <- c(stacks[[to_pos]], rev(tail(stacks[[from_pos]], num)))
    } else {
      stacks[[to_pos]] <- c(stacks[[to_pos]], tail(stacks[[from_pos]], num))
    }
    stacks[[from_pos]] <- head(stacks[[from_pos]], length(stacks[[from_pos]]) - num)
  }
  return(paste(lapply(stacks, function(x) tail(x, 1)), collapse = ""))
}
```

``` r
day5(lift_process = "one by one")
```

    ## [1] "HBTMTBSDC"

``` r
day5(lift_process = "all at once")
```

    ## [1] "PQTJRSHWS"
