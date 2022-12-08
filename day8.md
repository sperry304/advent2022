Advent of Code 2022, Day 8
================
Skip Perry
December 2022

``` r
options(dplyr.summarise.inform = FALSE)
library(tidyverse)

df_raw <- 
  read_lines("data/day8a.txt") |> 
  as_tibble()
```

``` r
mat <- 
  df_raw |> 
  pull(value) |> 
  paste(collapse = "") |> 
  strsplit(split = "") |> 
  unlist() |> 
  as.numeric() |> 
  matrix(nrow = 99, ncol = 99, byrow = TRUE)
```

``` r
num_visible <- 99 + 99 + 97 + 97

for (i in 2:98) {
  for (j in 2:98) {
    tree <- mat[i, j]
    if (max(mat[1:(i-1), j]) < tree) { num_visible <- num_visible + 1; next }
    else if (max(mat[(i+1):99, j]) < tree) { num_visible <- num_visible + 1; next }
    else if (max(mat[i, 1:(j-1)]) < tree) { num_visible <- num_visible + 1; next }
    else if (max(mat[i, (j+1):99]) < tree) { num_visible <- num_visible + 1; next }
  }
}

num_visible
```

    ## [1] 1672

``` r
count_trees_in_view <- function(tree, view) {
  if (length(view) == 1) {
    return(1L)
  } else {
    trees_in_view <- 0L
    for (i in 1:length(view)) {
      trees_in_view <- trees_in_view + 1L
      if (view[i] >= tree) { return(trees_in_view) }
    }
    return(trees_in_view)
  }
}
```

``` r
score <- matrix(0L, nrow = 99, ncol = 99)
for (i in 2:98) {
  for (j in 2:98) {
    tree <- mat[i, j]
    
    above <- rev(mat[1:(i-1), j])
    below <- mat[(i+1):99, j]
    left <- rev(mat[i, 1:(j-1)])
    right <- mat[i, (j+1):99]
    
    a <- count_trees_in_view(tree, above)
    b <- count_trees_in_view(tree, below)
    c <- count_trees_in_view(tree, left)
    d <- count_trees_in_view(tree, right)
    
    score[i,j] <- a * b * c * d
  }
}
max(score)
```

    ## [1] 327180
