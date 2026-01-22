## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(caugi)

## -----------------------------------------------------------------------------
cg <- caugi(
  A %-->% B + C,
  B %-->% D,
  C %-->% D,
  class = "DAG"
)

## -----------------------------------------------------------------------------
tmp <- tempfile(fileext = ".caugi.json")
write_caugi(cg, tmp,
  comment = "Example causal graph",
  tags = c("research", "example")
)

## -----------------------------------------------------------------------------
cg_loaded <- read_caugi(tmp)

identical(edges(cg), edges(cg_loaded))

## ----echo=FALSE, comment=""---------------------------------------------------
cat(readLines(tmp), sep = "\n")

## -----------------------------------------------------------------------------
# Serialize to JSON string
json_str <- caugi_serialize(cg)
cat(substr(json_str, 1, 200), "...\n")

# Deserialize from JSON string
cg_from_json <- caugi_deserialize(json_str)

## -----------------------------------------------------------------------------
# Read without building the Rust graph structure
cg_lazy <- read_caugi(tmp, lazy = TRUE)

# Build when needed
cg_lazy <- build(cg_lazy)

## -----------------------------------------------------------------------------
write_caugi(cg, tmp,
  comment = "Mediation model from Study A",
  tags = c("mediation", "study-a", "validated")
)

## -----------------------------------------------------------------------------
# DAG
dag <- caugi(X %-->% Y, Y %-->% Z, class = "DAG")

# PDAG (with undirected edges)
pdag <- caugi(X %-->% Y, Y %---% Z, class = "PDAG")

# ADMG (with bidirected edges)
admg <- caugi(X %-->% Y, Y %<->% Z, class = "ADMG")

# UG (undirected graph)
ug <- caugi(X %---% Y, Y %---% Z, class = "UG")

# Save them all
write_caugi(dag, tempfile(fileext = ".caugi.json"))
write_caugi(pdag, tempfile(fileext = ".caugi.json"))
write_caugi(admg, tempfile(fileext = ".caugi.json"))
write_caugi(ug, tempfile(fileext = ".caugi.json"))

## ----eval=FALSE---------------------------------------------------------------
# tmp_file <- tempfile(fileext = ".caugi.json")
# large_dag <- generate_graph(n = 1000, m = 500, class = "DAG")
# system.time(write_caugi(large_dag, tmp_file))
# system.time(res <- read_caugi(tmp_file))
# unlink(tmp_file)

## ----cleanup, include=FALSE---------------------------------------------------
# Clean up temp files
unlink(tmp)

