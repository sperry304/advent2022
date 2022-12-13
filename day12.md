Advent of Code 2022, Day 12
================
Skip Perry
December 2022

``` r
options(dplyr.summarise.inform = FALSE)
library(tidyverse)
library(igraph)
```

``` r
letter_to_number <- 
  tibble(value = c(letters, "S", "E"), number = c(1:26, 1, 26))

mat <- 
  read_lines("data/day12a.txt") |> 
  as_tibble() |> 
  pull(value) |> 
  paste(collapse = "") |> 
  strsplit(split = "") |> 
  unlist() |> 
  as_tibble() |> 
  left_join(letter_to_number, by = "value") |> 
  pull(number) |> 
  matrix(nrow = 41, byrow = TRUE)

mat[1:5, 1:5]
```

    ##      [,1] [,2] [,3] [,4] [,5]
    ## [1,]    1    2    1    3    3
    ## [2,]    1    2    1    1    3
    ## [3,]    1    2    1    1    3
    ## [4,]    1    2    3    3    3
    ## [5,]    1    2    3    3    3

``` r
node <- function(i, j) { str_c(i,j,sep=",") }
el <- matrix(ncol = 2)

for (i in 1:(nrow(mat))) {
  for (j in 1:(ncol(mat) - 1)) {
    if (mat[i,j+1] - mat[i,j] < 2) { el <- rbind(el, cbind(node(i,j), node(i,j+1))) } 
    if (mat[i,j] - mat[i,j+1] < 2) { el <- rbind(el, cbind(node(i,j+1), node(i,j))) } 
  }
}
for (j in 1:(ncol(mat))) {
  for (i in 1:(nrow(mat) - 1)) {
    if (mat[i+1,j] - mat[i,j] < 2) { el <- rbind(el, cbind(node(i,j), node(i+1,j))) }
    if (mat[i,j] - mat[i+1,j] < 2) { el <- rbind(el, cbind(node(i+1,j), node(i,j))) }
  }
}
```

``` r
g <- graph_from_edgelist(el[-1,], directed = TRUE)

start_node <- "21,1"
end_node <- "21,69"
```

``` r
shortest_paths(g, from = start_node, to = end_node)$vpath[[1]] |> length() - 1
```

    ## [1] 412

``` r
get_shortest_path_from_col1 <- function(i) {
  tibble(
    row = i,
    dist = shortest_paths(g, from = str_c(i, ",1"), to = end_node)$vpath[[1]] |> length() - 1
  )
}

map_dfr(1:41, get_shortest_path_from_col1) |> 
  arrange(dist) |> 
  pull(dist) |> 
  first()
```

    ## [1] 402
