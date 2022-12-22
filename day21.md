Advent of Code 2022, Day 21
================
Skip Perry
December 2022

``` r
options(dplyr.summarise.inform = FALSE)
library(tidyverse)
library(glue)
```

``` r
df <- 
  read_lines("data/day21a.txt") |> 
  as_tibble() |> 
  separate(value, into = c("a", "b"), sep = ": ")

df_eval <- df

while(nrow(df_eval) > 0) {
  df_numeric <- 
    df_eval |> 
    filter(!str_detect(b, "[a-z]")) |> 
    rowwise() |> 
    mutate(b = eval(parse(text = b)))
  
  df_eval <- 
    df_eval |> 
    anti_join(df_numeric, by = "a")
  
  for (i in 1:nrow(df_numeric)) {
    monkey <- df_numeric$a[i]
    monkey_value <- df_numeric$b[i]
    
    df_eval <- 
      df_eval |> 
      mutate(b = str_replace_all(b, monkey, as.character(monkey_value)))
  }
}

format(df_numeric$b, scientific = FALSE)
```

    ## [1] "124765768589550"

``` r
df <- 
  read_lines("data/day21a.txt") |> 
  as_tibble() |> 
  separate(value, into = c("a", "b"), sep = ": ") |> 
  filter(a != "humn") |> 
  mutate(b = if_else(a == "root", str_replace(b, "\\+", "\\="), b))

df
```

    ## # A tibble: 1,988 × 2
    ##    a     b          
    ##    <chr> <chr>      
    ##  1 qfzv  5          
    ##  2 stgm  2          
    ##  3 nzpf  4          
    ##  4 djpv  psrp * gzbq
    ##  5 zqlp  5          
    ##  6 fpvs  5          
    ##  7 zcft  3          
    ##  8 gcrl  2          
    ##  9 zltl  2          
    ## 10 tjrn  ppwm * blmz
    ## # … with 1,978 more rows

``` r
df_eval <- df
df_numeric_tmp <- 
  df_eval |> 
  filter(!str_detect(b, "[a-z]"), !(a == "humn")) 
df_eval
```

    ## # A tibble: 1,988 × 2
    ##    a     b          
    ##    <chr> <chr>      
    ##  1 qfzv  5          
    ##  2 stgm  2          
    ##  3 nzpf  4          
    ##  4 djpv  psrp * gzbq
    ##  5 zqlp  5          
    ##  6 fpvs  5          
    ##  7 zcft  3          
    ##  8 gcrl  2          
    ##  9 zltl  2          
    ## 10 tjrn  ppwm * blmz
    ## # … with 1,978 more rows

``` r
df_numeric_tmp
```

    ## # A tibble: 994 × 2
    ##    a     b    
    ##    <chr> <chr>
    ##  1 qfzv  5    
    ##  2 stgm  2    
    ##  3 nzpf  4    
    ##  4 zqlp  5    
    ##  5 fpvs  5    
    ##  6 zcft  3    
    ##  7 gcrl  2    
    ##  8 zltl  2    
    ##  9 gdqh  2    
    ## 10 bzgq  3    
    ## # … with 984 more rows

``` r
while (nrow(df_numeric_tmp) > 0L) {
  df_numeric_tmp <- 
    df_eval |> 
    filter(!str_detect(b, "[a-z]"), !(a == "humn"))
  
  if (nrow(df_numeric_tmp) == 0L) { break }
  
  df_numeric <- 
    df_numeric_tmp |> 
    rowwise() |> 
    mutate(b = eval(parse(text = b)))
  
  df_eval <- 
    df_eval |> 
    anti_join(df_numeric, by = "a")
  
  for (i in 1:nrow(df_numeric)) {
    monkey <- df_numeric$a[i]
    monkey_value <- df_numeric$b[i]
    
    df_eval <- 
      df_eval |> 
      mutate(b = str_replace_all(b, monkey, as.character(monkey_value)))
  }
}
df_eval
```

    ## # A tibble: 69 × 2
    ##    a     b         
    ##    <chr> <chr>     
    ##  1 cswp  926 + qlcr
    ##  2 dplf  dcmn + 331
    ##  3 dgjg  5 * lpwp  
    ##  4 rftv  ssqw * 2  
    ##  5 jdnp  rwsm * 2  
    ##  6 ssqw  ffdd - 465
    ##  7 rwsm  nvvt + 26 
    ##  8 crzt  jwnm - 398
    ##  9 bjqp  421 + vljm
    ## 10 jzhq  jcdd - 437
    ## # … with 59 more rows

``` r
for (i in 1:nrow(df_eval)) {
  df_eval <- 
    df_eval |> 
    mutate(b = str_replace(b, df_eval$a[i], glue("({df_eval$b[i]})")))
}
df_eval
```

    ## # A tibble: 69 × 2
    ##    a     b                                                                      
    ##    <chr> <chr>                                                                  
    ##  1 cswp  926 + ((((((((((((((81 + ((192 * (((542 + humn) / 6) - 516)) - 763)) /…
    ##  2 dplf  (((5 * (((((26 + ((((561 + (971 + ((((926 + ((((((((((((((81 + ((192 *…
    ##  3 dgjg  5 * (((((26 + ((((561 + (971 + ((((926 + ((((((((((((((81 + ((192 * ((…
    ##  4 rftv  (((((((((81 + ((192 * (((542 + humn) / 6) - 516)) - 763)) / 11) + 898)…
    ##  5 jdnp  ((((421 + (3 * (((((24 + ((245 + (2 * ((19 * ((((5 * (((((26 + ((((561…
    ##  6 ssqw  ((((((((81 + ((192 * (((542 + humn) / 6) - 516)) - 763)) / 11) + 898) …
    ##  7 rwsm  (((421 + (3 * (((((24 + ((245 + (2 * ((19 * ((((5 * (((((26 + ((((561 …
    ##  8 crzt  ((((24 + ((245 + (2 * ((19 * ((((5 * (((((26 + ((((561 + (971 + ((((92…
    ##  9 bjqp  421 + (3 * (((((24 + ((245 + (2 * ((19 * ((((5 * (((((26 + ((((561 + (…
    ## 10 jzhq  (19 * ((((5 * (((((26 + ((((561 + (971 + ((((926 + ((((((((((((((81 + …
    ## # … with 59 more rows

``` r
df_eval |> filter(a == "root") |> pull(b)
```

    ## [1] "(3 * (476 + ((180354410322238 - (((((194 + ((721 + (((((((((421 + (3 * (((((24 + ((245 + (2 * ((19 * ((((5 * (((((26 + ((((561 + (971 + ((((926 + ((((((((((((((81 + ((192 * (((542 + humn) / 6) - 516)) - 763)) / 11) + 898) * 2) - 262) / 6) + 183) * 18) - 465) * 2) + 388) / 2) - 999) / 2)) * 3) - 387) * 2))) / 2) - 264) / 2)) * 2) - 266) / 12) + 684)) - 148) / 11) + 331)) - 437))) / 5)) * 3) - 965) / 2) - 398))) + 287) / 2) + 26) * 2) - 650) / 7) - 660) / 2)) * 15)) * 2) - 897) + 157) / 2)) / 6))) = 34588563455325"

``` r
3059361893920
```

    ## [1] 3.059362e+12
