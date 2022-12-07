Advent of Code 2022, Day 7
================
Skip Perry
December 2022

``` r
options(dplyr.summarise.inform = FALSE)
library(tidyverse)
```

``` r
df <- 
  read_lines("data/day7a.txt") |> 
  as_tibble() |> 
  mutate(
    value = str_remove(value, "\\$ "),
    cur_dir = NA_character_,
    file_name = NA_character_,
    file_size = NA_integer_
  )

df
```

    ## # A tibble: 1,087 × 4
    ##    value        cur_dir file_name file_size
    ##    <chr>        <chr>   <chr>         <int>
    ##  1 cd home      <NA>    <NA>             NA
    ##  2 ls           <NA>    <NA>             NA
    ##  3 dir fwbjchs  <NA>    <NA>             NA
    ##  4 dir hmnpr    <NA>    <NA>             NA
    ##  5 dir jtrbrcjl <NA>    <NA>             NA
    ##  6 dir lcgv     <NA>    <NA>             NA
    ##  7 dir ldqc     <NA>    <NA>             NA
    ##  8 dir vrvl     <NA>    <NA>             NA
    ##  9 cd fwbjchs   <NA>    <NA>             NA
    ## 10 ls           <NA>    <NA>             NA
    ## # … with 1,077 more rows

``` r
for (i in 1:nrow(df)) {
  if (df$value[i] == "cd home") { df$cur_dir[i] <- "home" }
  else if (df$value[i] == "ls") { df$cur_dir[i] <- df$cur_dir[i-1] }
  else if (str_detect(df$value[i], "^dir ")) { df$cur_dir[i] <- df$cur_dir[i-1] }
  else if (str_detect(df$value[i], "^\\d+ ")) { 
    df$cur_dir[i] <- df$cur_dir[i-1] 
    df$file_name[i] <- str_remove(df$value[i], "\\d+")
    df$file_size[i] <- str_extract(df$value[i], "\\d+")
  }
  else if (str_detect(df$value[i], "^cd \\w+")) { 
    df$cur_dir[i] <- str_c(df$cur_dir[i-1], "/", str_extract(df$value[i], "\\w+$"))
  }
  else if (df$value[i] == "cd ..") { df$cur_dir[i] <- str_remove(df$cur_dir[i-1], "/\\w+$") }
}
```

``` r
dir_sizes_tmp <- 
  df |> 
  mutate(file_size = replace_na(as.numeric(file_size), 0)) |> 
  arrange(cur_dir) |> 
  group_by(cur_dir) |> 
  summarize(dir_size = sum(file_size))
```

``` r
dir_list <- dir_sizes_tmp |> pull(cur_dir)

get_full_dir_size <- function(dir_name) {
  dir_sizes_tmp |> 
    filter(str_detect(cur_dir, dir_name)) |> 
    summarize(dir_size = sum(dir_size)) |> 
    transmute(cur_dir = dir_name, dir_size)
}

dir_sizes <- map_dfr(dir_list, get_full_dir_size)
```

``` r
dir_sizes |> 
  filter(dir_size < 100000) |> 
  summarize(sum(dir_size))
```

    ## # A tibble: 1 × 1
    ##   `sum(dir_size)`
    ##             <dbl>
    ## 1         1611443

``` r
dir_sizes |> 
  filter(dir_size > (30000000 - (70000000 - 42080344))) |> 
  arrange(dir_size)
```

    ## # A tibble: 16 × 2
    ##    cur_dir                                dir_size
    ##    <chr>                                     <dbl>
    ##  1 home/ldqc/jclb/pbb/zmflq/lcgv/mzzpfnr   2086088
    ##  2 home/ldqc/jclb/pbb/wqdlv/wqdlv          2103226
    ##  3 home/ldqc/vhdgcsw                       2224309
    ##  4 home/ldqc/jclb/pbb/zmflq/wqdlv/jrqsqrv  3143244
    ##  5 home/ldqc/jclb/pbb/zmflq/lcgv/rfzrwc    3493129
    ##  6 home/lcgv/vwhf                          3543702
    ##  7 home/ldqc/jclb/pbb/zmflq/wqdlv          4243869
    ##  8 home/ldqc/jclb/vpfggv                   4548392
    ##  9 home/lcgv                               6033578
    ## 10 home/ldqc/jclb/pbb/wqdlv                6180148
    ## 11 home/ldqc/jclb/pbb/zmflq/lcgv           8121467
    ## 12 home/ldqc/jclb/pbb/zmflq               13431333
    ## 13 home/ldqc/jclb/pbb                     19611481
    ## 14 home/ldqc/jclb                         26053496
    ## 15 home/ldqc                              34474241
    ## 16 home                                   42080344
