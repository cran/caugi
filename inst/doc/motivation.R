## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(caugi)

## ----pcalg-pag----------------------------------------------------------------
nodes <- c("A", "B", "C", "D", "E")
amat <- matrix(
  c(
    0L, 2L, 0L, 0L, 1L,
    3L, 0L, 2L, 1L, 0L,
    0L, 1L, 0L, 0L, 3L,
    0L, 1L, 0L, 0L, 3L,
    3L, 0L, 3L, 3L, 0L
  ),
  5, 5,
  byrow = TRUE, dimnames = list(nodes, nodes)
)
amat

## ----caugi-pag, eval=FALSE----------------------------------------------------
# caugi(
#   A %-->% B %o->% C %---% E,
#   B %o-o% D %---% E,
#   A %--o% E
# )

