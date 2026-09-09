## -----------------------------------------------------------------------------
# suppressPackageStartupMessages({
#   library(caugi)
#   library(bench)
#   library(data.table)
#   library(jsonlite)
#   library(igraph)
#   library(bnlearn)
#   library(dagitty)
#   library(ggm)
#   library(pcalg)
#   library(graph)
# })
# 
# FIXTURES_DIR <- "fixtures"
# RESULTS_DIR <- "results"
# dir.create(RESULTS_DIR, showWarnings = FALSE, recursive = TRUE)
# 
# spec <- jsonlite::fromJSON(
#   file.path(FIXTURES_DIR, "spec.json"),
#   simplifyVector = FALSE
# )
# .skip_table <- if (is.null(spec$skip)) list() else spec$skip

## -----------------------------------------------------------------------------
# load_fixture <- function(fx) {
#   edges_path <- file.path(FIXTURES_DIR, fx$edges_file)
#   edges <- data.table::fread(
#     edges_path,
#     col.names = c("from", "to"),
#     colClasses = "character"
#   )
#   nodes <- paste0("V", seq_len(fx$n))
# 
#   cg <- caugi::add_edges(
#     caugi::caugi(class = "DAG", nodes = nodes),
#     from = edges$from,
#     edge = rep("-->", nrow(edges)),
#     to = edges$to
#   )
#   cg <- caugi::build(cg)
# 
#   ig <- igraph::graph_from_data_frame(
#     as.data.frame(edges),
#     directed = TRUE,
#     vertices = data.frame(name = nodes)
#   )
# 
#   am <- as.matrix(igraph::as_adjacency_matrix(ig))
#   storage.mode(am) <- "integer"
# 
#   bn <- bnlearn::empty.graph(nodes)
#   bnlearn::amat(bn) <- am
# 
#   dg_str <- paste0(
#     "dag {\n",
#     paste(sprintf("%s -> %s", edges$from, edges$to), collapse = "\n"),
#     "\n}"
#   )
#   dg <- dagitty::dagitty(dg_str)
# 
#   # pcalg's searchAM expects the PAG-style 0/1/2/3 amat coding (mark at the
#   # column-end of edge {row, col}): for a -> b, amat[a,b] = 2 (arrowhead at b)
#   # and amat[b,a] = 3 (tail at a).
#   amat_pag <- matrix(0L, nrow(am), ncol(am), dimnames = dimnames(am))
#   amat_pag[am == 1L] <- 2L
#   amat_pag[t(am) == 1L] <- 3L
# 
#   # pcalg::dsep operates on a graphNEL.
#   gNEL <- igraph::as_graphnel(ig)
# 
#   list(
#     cg = cg,
#     ig = ig,
#     bn = bn,
#     dg = dg,
#     am = am,
#     amat_pag = amat_pag,
#     gNEL = gNEL,
#     nodes = nodes
#   )
# }

## -----------------------------------------------------------------------------
# # bench::mark returns a tibble; we convert each row into a long-format record.
# mark_to_rows <- function(bm, operation, fx) {
#   pkg <- as.character(bm$expression)
#   data.table::data.table(
#     language = rep("R", length(pkg)),
#     package = pkg,
#     operation = rep(operation, length(pkg)),
#     fixture_id = rep(fx$id, length(pkg)),
#     n = rep(fx$n, length(pkg)),
#     p = rep(fx$p, length(pkg)),
#     n_edges = rep(fx$n_edges, length(pkg)),
#     median_ns = as.numeric(bm$median) * 1e9,
#     min_ns = as.numeric(bm$min) * 1e9,
#     total_time_ns = as.numeric(bm$total_time) * 1e9,
#     n_iter = as.integer(bm$n_itr),
#     mem_alloc_bytes = as.numeric(bm$mem_alloc)
#   )
# }
# 
# # A skip rule matches when its package + operation match (with "*" as
# # wildcard) and the fixture's n / id fall within optional bounds.
# pkg_skip <- function(pkg, op, fx, skip_table = .skip_table) {
#   if (length(skip_table) == 0L) {
#     return(FALSE)
#   }
#   for (rule in skip_table) {
#     pkg_match <- identical(rule$package, pkg) || identical(rule$package, "*")
#     op_match <- identical(rule$operation, op) || identical(rule$operation, "*")
#     n_min_ok <- is.null(rule$n_min) || fx$n >= rule$n_min
#     n_max_ok <- is.null(rule$n_max) || fx$n <= rule$n_max
#     id_ok <- is.null(rule$fixture_id) || identical(rule$fixture_id, fx$id)
#     if (pkg_match && op_match && n_min_ok && n_max_ok && id_ok) {
#       return(TRUE)
#     }
#   }
#   FALSE
# }
# 
# # Filter the per-package expression list against the skip table, then call
# # bench::mark on the survivors and serialise the result.
# run_bench <- function(calls, operation, fx) {
#   keep <- !vapply(
#     names(calls),
#     pkg_skip,
#     logical(1),
#     op = operation,
#     fx = fx
#   )
#   if (!any(keep)) {
#     return(NULL)
#   }
#   args <- c(
#     calls[keep],
#     list(check = FALSE, min_iterations = 5L, time_unit = "s")
#   )
#   bm <- eval(as.call(c(quote(bench::mark), args)))
#   mark_to_rows(bm, operation, fx)
# }

## -----------------------------------------------------------------------------
# bench_parents <- function(graphs, fx) {
#   v <- fx$test_node
#   v_idx <- match(v, graphs$nodes)
# 
#   calls <- list(
#     caugi = bquote(caugi::parents(graphs$cg, .(v))),
#     igraph = bquote(igraph::neighbors(graphs$ig, .(v), mode = "in")),
#     bnlearn = bquote(bnlearn::parents(graphs$bn, .(v))),
#     dagitty = bquote(dagitty::parents(graphs$dg, .(v))),
#     ggm = bquote(ggm::pa(.(v), graphs$am)),
#     pcalg = bquote(pcalg::searchAM(graphs$amat_pag, .(v_idx), type = "pa"))
#   )
# 
#   run_bench(calls, "parents", fx)
# }

## -----------------------------------------------------------------------------
# bench_children <- function(graphs, fx) {
#   v <- fx$test_node
#   v_idx <- match(v, graphs$nodes)
# 
#   calls <- list(
#     caugi = bquote(caugi::children(graphs$cg, .(v))),
#     igraph = bquote(igraph::neighbors(graphs$ig, .(v), mode = "out")),
#     bnlearn = bquote(bnlearn::children(graphs$bn, .(v))),
#     dagitty = bquote(dagitty::children(graphs$dg, .(v))),
#     ggm = bquote(ggm::ch(.(v), graphs$am)),
#     pcalg = bquote(pcalg::searchAM(graphs$amat_pag, .(v_idx), type = "ch"))
#   )
# 
#   run_bench(calls, "children", fx)
# }

## -----------------------------------------------------------------------------
# bench_ancestors <- function(graphs, fx) {
#   v <- fx$test_node
#   v_idx <- match(v, graphs$nodes)
# 
#   calls <- list(
#     caugi = bquote(caugi::ancestors(graphs$cg, .(v))),
#     igraph = bquote(igraph::subcomponent(graphs$ig, .(v), mode = "in")),
#     bnlearn = bquote(bnlearn::ancestors(graphs$bn, .(v))),
#     dagitty = bquote(dagitty::ancestors(graphs$dg, .(v))),
#     pcalg = bquote(pcalg::searchAM(graphs$amat_pag, .(v_idx), type = "an"))
#   )
# 
#   run_bench(calls, "ancestors", fx)
# }

## -----------------------------------------------------------------------------
# bench_descendants <- function(graphs, fx) {
#   v <- fx$test_node
#   v_idx <- match(v, graphs$nodes)
# 
#   calls <- list(
#     caugi = bquote(caugi::descendants(graphs$cg, .(v))),
#     igraph = bquote(igraph::subcomponent(graphs$ig, .(v), mode = "out")),
#     bnlearn = bquote(bnlearn::descendants(graphs$bn, .(v))),
#     dagitty = bquote(dagitty::descendants(graphs$dg, .(v))),
#     pcalg = bquote(pcalg::searchAM(graphs$amat_pag, .(v_idx), type = "de"))
#   )
# 
#   run_bench(calls, "descendants", fx)
# }

## -----------------------------------------------------------------------------
# bench_markov_blanket <- function(graphs, fx) {
#   v <- fx$test_node
# 
#   calls <- list(
#     caugi = bquote(caugi::markov_blanket(graphs$cg, .(v))),
#     bnlearn = bquote(bnlearn::mb(graphs$bn, .(v))),
#     dagitty = bquote(dagitty::markovBlanket(graphs$dg, .(v)))
#   )
# 
#   run_bench(calls, "markov_blanket", fx)
# }

## -----------------------------------------------------------------------------
# bench_dsep <- function(graphs, fx) {
#   if (is.null(fx$dsep)) {
#     return(NULL)
#   }
# 
#   x <- fx$dsep$x
#   y <- fx$dsep$y
#   z <- unlist(fx$dsep$z)
# 
#   calls <- list(
#     caugi = bquote(caugi::d_separated(graphs$cg, .(x), .(y), .(z))),
#     bnlearn = bquote(bnlearn::dsep(graphs$bn, .(x), .(y), .(z))),
#     dagitty = bquote(dagitty::dseparated(graphs$dg, .(x), .(y), .(z))),
#     pcalg = bquote(pcalg::dsep(.(x), .(y), .(z), graphs$gNEL))
#   )
# 
#   run_bench(calls, "d_separated", fx)
# }

## -----------------------------------------------------------------------------
# bench_subgraph <- function(graphs, fx) {
#   sub <- unlist(fx$subgraph_nodes)
# 
#   calls <- list(
#     caugi = bquote({
#       sg <- caugi::subgraph(graphs$cg, .(sub))
#       caugi::build(sg)
#     }),
#     igraph = bquote(igraph::subgraph(graphs$ig, .(sub))),
#     bnlearn = bquote(bnlearn::subgraph(graphs$bn, .(sub)))
#   )
# 
#   run_bench(calls, "subgraph", fx)
# }

## -----------------------------------------------------------------------------
# results <- list()
# 
# for (fx in spec$fixtures) {
#   message(sprintf("[bench_r] %s (n=%d, edges=%d)", fx$id, fx$n, fx$n_edges))
#   graphs <- load_fixture(fx)
#   results[[length(results) + 1L]] <- bench_parents(graphs, fx)
#   results[[length(results) + 1L]] <- bench_children(graphs, fx)
#   results[[length(results) + 1L]] <- bench_ancestors(graphs, fx)
#   results[[length(results) + 1L]] <- bench_descendants(graphs, fx)
#   results[[length(results) + 1L]] <- bench_markov_blanket(graphs, fx)
#   results[[length(results) + 1L]] <- bench_dsep(graphs, fx)
#   results[[length(results) + 1L]] <- bench_subgraph(graphs, fx)
# }
# 
# results <- results[!vapply(results, is.null, logical(1))]
# out <- data.table::rbindlist(results)
# data.table::fwrite(out, file = file.path(RESULTS_DIR, "r.csv"))
# 
# message(sprintf("[bench_r] wrote %d rows to %s/r.csv", nrow(out), RESULTS_DIR))
# 
# op_levels <- c(
#   "parents",
#   "children",
#   "ancestors",
#   "descendants",
#   "markov_blanket",
#   "d_separated",
#   "subgraph"
# )
# 
# summary_dt <- out[,
#   .(median_ms = median(median_ns) / 1e6),
#   by = .(package, operation)
# ]
# summary_dt[, operation := factor(operation, levels = op_levels)]
# summary_wide <- data.table::dcast(
#   summary_dt,
#   package ~ operation,
#   value.var = "median_ms"
# )
# 
# message(
#   "\n[bench_r] median ms per (package, operation), aggregated over fixtures:"
# )
# 
# print(summary_wide, digits = 3, row.names = FALSE)

