Advent of Code 2022, Day 10
================
Skip Perry
December 2022

``` r
options(dplyr.summarise.inform = FALSE)
library(tidyverse)
```

``` r
df <- 
  read_lines("data/day10a.txt") |> 
  as_tibble()
```

``` r
signal <- 1

for (i in 1:nrow(df)) {
  if (df$value[i] == "noop") { signal <- c(signal, last(signal)) }
  else { 
    signal <- c(signal, last(signal), last(signal) + as.numeric(str_remove(df$value[i], "addx ")))  
  }
}

sum(signal[c(20, 60, 100, 140, 180, 220)] * c(20, 60, 100, 140, 180, 220))
```

    ## [1] 14620

``` r
paste(ifelse((abs(signal[1:40] - 0:39) < 2) == TRUE, "#", "."), collapse = "")
paste(ifelse((abs(signal[41:80] - 0:39) < 2) == TRUE, "#", "."), collapse = "")
paste(ifelse((abs(signal[81:120] - 0:39) < 2) == TRUE, "#", "."), collapse = "")
paste(ifelse((abs(signal[121:160] - 0:39) < 2) == TRUE, "#", "."), collapse = "")
paste(ifelse((abs(signal[161:200] - 0:39) < 2) == TRUE, "#", "."), collapse = "")
paste(ifelse((abs(signal[201:240] - 0:39) < 2) == TRUE, "#", "."), collapse = "")
```

    ## [1] "###....##.####.###..#..#.###..####.#..#."

    ## [1] "#..#....#.#....#..#.#..#.#..#.#....#..#."

    ## [1] "###.....#.###..#..#.####.#..#.###..#..#."

    ## [1] "#..#....#.#....###..#..#.###..#....#..#."

    ## [1] "#..#.#..#.#....#.#..#..#.#.#..#....#..#."

    ## [1] "###...##..#....#..#.#..#.#..#.#.....##.."
