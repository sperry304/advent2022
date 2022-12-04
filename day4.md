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
  read_lines("data/day4a.txt") |> 
  as_tibble()
```

``` r
df2 <- 
  df |> 
  separate(value, into = c("a", "b"), sep = ",") |> 
  separate(a, into = c("a1", "a2"), sep = "-") |> 
  separate(b, into = c("b1", "b2"), sep = "-") |> 
  mutate_all(as.numeric)

df2
```

    ## # A tibble: 1,000 × 4
    ##       a1    a2    b1    b2
    ##    <dbl> <dbl> <dbl> <dbl>
    ##  1    71    87    70    88
    ##  2     8    92     6    97
    ##  3     7    68     8    69
    ##  4    47    47    47    48
    ##  5    74    94    51    93
    ##  6    48    63    48    49
    ##  7    26    57    57    58
    ##  8    62    63    11    76
    ##  9    16    66    15    47
    ## 10    27    65    27    62
    ## # … with 990 more rows

``` r
df2 |> 
  filter((a1 >= b1 & a2 <= b2) | (b1 >= a1 & b2 <= a2)) |> 
  count()
```

    ## # A tibble: 1 × 1
    ##       n
    ##   <int>
    ## 1   485

``` r
df2 |> 
  mutate(
    overlap = mapply(function(w, x, y, z) 
                     length(intersect(seq(w, x), seq(y, z))),
                     a1, a2, b1, b2)
  ) |> 
  filter(overlap > 0) |> 
  count()
```

    ## # A tibble: 1 × 1
    ##       n
    ##   <int>
    ## 1   857
