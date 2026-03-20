## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(caugi)
set.seed(42)

## ----first-iteration----------------------------------------------------------
#' @title Correlation implies causation!
#'
#' @param df A `data.frame` with numeric columns
#'
#' @returns A `caugi` representing the causal graph that is totally true!
correlation_implies_causation <- function(df) {
  NULL # not developed yet!
}

## ----df-creation--------------------------------------------------------------
# create correlated data using MASS
df <- MASS::mvrnorm(
  n = 100,
  mu = c(0, 0, 0),
  Sigma = matrix(c(
    1, 0.8, 0.3,
    0.8, 1, 0.4,
    0.3, 0.4, 1
  ), nrow = 3)
) |> as.data.frame()
head(df)

## ----second-iteration---------------------------------------------------------
#' @title Correlation implies causation!
#'
#' @param df A `data.frame` with numeric columns
#'
#' @returns A `caugi` representing the causal graph that is totally true!
correlation_implies_causation <- function(df) {
  cg <- caugi::caugi(nodes = names(df))
  return(NULL)
}

## ----third-iteration----------------------------------------------------------
#' @title Correlation implies causation!
#'
#' @param df A `data.frame` with numeric columns
#'
#' @returns A `caugi` representing the causal graph that is totally true!
correlation_implies_causation <- function(df) {
  cg <- caugi::caugi(nodes = names(df))
  cor_matrix <- cor(df)
  # Add edges for correlations above 0.5
  for (i in seq_len(ncol(cor_matrix))) {
    for (j in 1:i) {
      if (i != j && abs(cor_matrix[i, j]) > 0.5) {
        from <- names(df)[j]
        to <- names(df)[i]
        cg <- caugi::add_edges(cg, from = from, edge = "-->", to = to) # add edge to caugi
      }
    }
  }
  return(cg)
}

## ----try-it-out---------------------------------------------------------------
cg <- correlation_implies_causation(df)
cg

## ----fifth-iteration----------------------------------------------------------
#' @title Correlation implies causation!
#'
#' @param df A `data.frame` with numeric columns
#'
#' @returns A `caugi` representing the causal graph that is totally true!
correlation_implies_causation <- function(df) {
  cg <- caugi::caugi(nodes = names(df))
  cor_matrix <- cor(df)
  # Add edges for correlations above 0.5
  cg <- caugi::caugi(nodes = names(df))
  cor_matrix <- cor(df)
  # Add edges for correlations above 0.5
  for (i in seq_len(ncol(cor_matrix))) {
    for (j in 1:i) {
      if (i != j && abs(cor_matrix[i, j]) > 0.5) {
        from <- names(df)[j]
        to <- names(df)[i]
        cg <- caugi::add_edges(cg, from = from, edge = "-->", to = to) # add edge to caugi
      }
    }
  }
  if (caugi::is_dag(cg)) cg <- caugi::mutate_caugi(cg, class = "DAG")
  return(cg)
}

## ----try-it-out-2-------------------------------------------------------------
cg <- correlation_implies_causation(df)
cg
cg@graph_class

