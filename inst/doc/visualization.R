## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.align = "center"
)

## ----setup--------------------------------------------------------------------
library(caugi)

## ----basic-plot---------------------------------------------------------------
# Create a simple DAG
cg <- caugi(
  A %-->% B + C,
  B %-->% D,
  C %-->% D,
  class = "DAG"
)

# Plot with default settings
plot(cg)

## ----sugiyama-layout----------------------------------------------------------
# Create a more complex DAG
dag <- caugi(
  X1 %-->% M1 + M2,
  X2 %-->% M2 + M3,
  M1 %-->% Y,
  M2 %-->% Y,
  M3 %-->% Y,
  class = "DAG"
)

# Use Sugiyama layout explicitly
plot(dag, layout = "sugiyama", main = "Sugiyama")

## ----fruchterman-reingold-----------------------------------------------------
# Create a graph with bidirected edges (ADMG)
admg <- caugi(
  A %-->% C,
  B %-->% C,
  A %<->% B, # Bidirected edge (latent confounder)
  class = "ADMG"
)

# Fruchterman-Reingold handles all edge types
plot(admg, layout = "fruchterman-reingold", main = "Fruchterman-Reingold")

## ----kamada-kawai-------------------------------------------------------------
# Create an undirected graph
ug <- caugi(
  A %---% B,
  B %---% C + D,
  C %---% D,
  class = "UG"
)

plot(ug, layout = "kamada-kawai", main = "Kamada-Kawai")

## ----bipartite-layout---------------------------------------------------------
bipartite_graph <- caugi(
  Treatment_A %-->% Outcome_1 + Outcome_2 + Outcome_3,
  Treatment_B %-->% Outcome_1 + Outcome_2,
  Treatment_C %-->% Outcome_2 + Outcome_3,
  class = "DAG"
)

## ----bipartite-basic----------------------------------------------------------
plot(
  bipartite_graph,
  layout = "bipartite",
  orientation = "rows"
)

## ----bipartite-vertical-------------------------------------------------------
plot(
  bipartite_graph,
  layout = "bipartite",
  orientation = "columns"
)

## ----bipartite-explicit-------------------------------------------------------
partition <- c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)
plot(
  bipartite_graph,
  layout = caugi_layout_bipartite,
  partition = partition,
  orientation = "rows"
)

## ----tiered-basic-------------------------------------------------------------
cg_tiered <- caugi(
  X1 %-->% M1 + M2,
  X2 %-->% M1 + M2,
  M1 %-->% Y,
  M2 %-->% Y
)

## ----tiered-named-list--------------------------------------------------------
tiers <- list(
  exposures = c("X1", "X2"),
  mediators = c("M1", "M2"),
  outcome = "Y"
)

plot(cg_tiered, layout = "tiered", tiers = tiers, orientation = "rows")

## ----tiered-vector------------------------------------------------------------
tiers_vector <- c(X1 = 1, X2 = 1, M1 = 2, M2 = 2, Y = 3)
plot(cg_tiered, layout = "tiered", tiers = tiers_vector, orientation = "columns")

## ----tiered-dataframe---------------------------------------------------------
tiers_df <- data.frame(
  name = c("X1", "X2", "M1", "M2", "Y"),
  tier = c(1, 1, 2, 2, 3)
)

layout_df <- caugi_layout_tiered(cg_tiered, tiers_df, orientation = "rows")

plot(cg_tiered, layout = layout_df)

## ----compare-layouts----------------------------------------------------------
layout_sug <- caugi_layout(dag, method = "sugiyama")
layout_fr <- caugi_layout(dag, method = "fruchterman-reingold")
layout_kk <- caugi_layout(dag, method = "kamada-kawai")

# Examine coordinates
head(layout_sug)

## ----node-styling-global------------------------------------------------------
plot(
  cg,
  node_style = list(
    fill = "lightblue", # Fill color
    col = "darkblue", # Border color
    lwd = 2, # Border width
    padding = 4, # Text padding (mm)
    size = 1.2 # Size multiplier
  )
)

## ----node-styling-locally-----------------------------------------------------
plot(
  cg,
  node_style = list(
    by_node = list(
      A = list(fill = "red", col = "blue", lwd = 2),
      B = list(padding = "2")
    )
  )
)

## ----edge-styling-global------------------------------------------------------
plot(
  dag,
  edge_style = list(
    col = "darkgray", # Edge color
    lwd = 1.5, # Edge width
    arrow_size = 4 # Arrow size (mm)
  )
)

## ----edge-styling-per-type----------------------------------------------------
plot(
  admg,
  layout = "fruchterman-reingold",
  edge_style = list(
    directed = list(col = "blue", lwd = 2),
    bidirected = list(col = "red", lwd = 2, lty = "dashed")
  )
)

## ----edge-styling-per-node----------------------------------------------------
plot(
  admg,
  layout = "fruchterman-reingold",
  edge_style = list(
    by_edge = list(
      A = list(col = "green", lwd = 2)
    )
  )
)

## ----edge-styling-per-specific-edge-------------------------------------------
plot(
  admg,
  layout = "fruchterman-reingold",
  edge_style = list(
    by_edge = list(
      A = list(
        B = list(col = "orange", lwd = 3)
      )
    )
  )
)

## ----edge-styling-combined----------------------------------------------------
plot(
  admg,
  layout = "fruchterman-reingold",
  edge_style = list(
    # Global defaults
    col = "gray80",
    lwd = 1,

    # Per-type styling
    directed = list(col = "blue"),
    bidirected = list(col = "red", lty = "dashed"),

    # All edges from node A
    by_edge = list(
      A = list(
        col = "green",
        lwd = 2,

        # Specific edge A -> B
        B = list(
          col = "orange",
          lwd = 3
        )
      )
    )
  )
)

## ----partial-edges------------------------------------------------------------
g <- caugi(
  A %o->% B,
  B %-->% C,
  C %o-o% D,
  class = "UNKNOWN"
)

plot(
  g,
  edge_style = list(
    partial = list(
      col = "purple",
      lwd = 2,
      circle_size = 2.5 # Larger circles (default is 1.5)
    )
  )
)

## ----label-styling------------------------------------------------------------
plot(
  cg,
  main = "Customized Labels",
  label_style = list(
    col = "white", # Text color
    fontsize = 12, # Font size
    fontface = "bold", # Font face
    fontfamily = "sans" # Font family
  ),
  node_style = list(
    fill = "navy" # Dark background for white text
  )
)

## ----tiered-boxes-basic-------------------------------------------------------
plot(cg_tiered, tiers = tiers)

## ----tiered-boxes-vector------------------------------------------------------
plot(
  cg_tiered,
  tiers = tiers,
  tier_style = list(
    fill = c("lightblue", "lightgreen", "lightyellow"),
    col = "gray50",
    lty = 2,
    alpha = 0.3
  )
)

## ----tiered-boxes-by-tier-----------------------------------------------------
plot(
  cg_tiered,
  tiers = tiers,
  tier_style = list(
    fill = "gray95",
    col = "gray60",
    alpha = 0.2,
    by_tier = list(
      exposures = list(
        fill = "lightblue",
        col = "blue",
        lwd = 2
      ),
      outcome = list(
        fill = "lightyellow",
        col = "orange",
        lwd = 3,
        lty = 1
      )
    )
  )
)

## ----tiered-boxes-labels------------------------------------------------------
plot(
  cg_tiered,
  tiers = tiers, # Named list: exposures, mediators, outcome
  tier_style = list(
    fill = c("lightblue", "lightgreen", "lightyellow"),
    label_style = list(
      fontsize = 11,
      fontface = "bold",
      col = "gray20"
    )
  )
)

## ----tiered-boxes-custom-labels-----------------------------------------------
plot(
  cg_tiered,
  tiers = tiers,
  tier_style = list(
    fill = "gray95",
    labels = c("Exposure Variables", "Mediating Variables", "Outcome Variable")
  )
)

## ----tiered-boxes-none--------------------------------------------------------
plot(
  cg_tiered,
  tiers = tiers,
  tier_style = list(boxes = FALSE, labels = FALSE)
)

## ----pdag-plot----------------------------------------------------------------
pdag <- caugi(
  A %-->% B,
  B %---% C, # Undirected edge
  C %-->% D,
  class = "PDAG"
)

plot(
  pdag,
  edge_style = list(
    directed = list(col = "blue"),
    undirected = list(col = "gray", lwd = 2)
  )
)

## ----admg-plot----------------------------------------------------------------
complex_admg <- caugi(
  X %-->% M1 + M2,
  M1 %-->% Y,
  M2 %-->% Y,
  M1 %<->% M2, # Latent confounder between mediators
  class = "ADMG"
)

plot(
  complex_admg,
  layout = "kamada-kawai",
  node_style = list(fill = "lavender"),
  edge_style = list(
    directed = list(col = "black", lwd = 1.5),
    bidirected = list(col = "red", lwd = 1.5, lty = "dashed", arrow_size = 3)
  )
)

## ----ug-plot------------------------------------------------------------------
markov <- caugi(
  A %---% B + C,
  B %---% D,
  C %---% D + E,
  D %---% E,
  class = "UG"
)

plot(
  markov,
  layout = "fruchterman-reingold",
  node_style = list(
    fill = "lightyellow",
    col = "orange",
    lwd = 2
  ),
  edge_style = list(col = "orange")
)

## ----composition-basic--------------------------------------------------------
# Create two different graphs
g1 <- caugi(
  A %-->% B,
  B %-->% C,
  class = "DAG"
)

g2 <- caugi(
  X %-->% Y,
  Y %-->% Z,
  X %-->% Z,
  class = "DAG"
)

# Create plots
p1 <- plot(g1, main = "Graph 1")
p2 <- plot(g2, main = "Graph 2")

# Horizontal composition (side-by-side)
p1 + p2

## ----composition-pipe---------------------------------------------------------
# Equivalent to p1 + p2
p1 | p2

## ----composition-vertical-----------------------------------------------------
p1 / p2

## ----composition-nested-------------------------------------------------------
g3 <- caugi(
  M1 %-->% M2,
  M2 %-->% M3,
  class = "DAG"
)

p3 <- plot(g3, main = "Graph 3")

# Complex layout: two plots on top, one below
(p1 + p2) / p3

## ----composition-mixed--------------------------------------------------------
(p1 + p2) / (p3 + p1)

## ----composition-spacing------------------------------------------------------
caugi_options(plot = list(spacing = grid::unit(2, "lines")))

p1 + p2

## ----reset-global-options-----------------------------------------------------
caugi_options(caugi_default_options())

## ----global-options-----------------------------------------------------------
# Configure global defaults
caugi_options(plot = list(
  node_style = list(fill = "lightblue", padding = 3),
  edge_style = list(arrow_size = 4, fill = "darkgray"),
  title_style = list(col = "blue", fontsize = 16)
))

# This plot uses the global defaults
plot(cg, main = "Using Global Defaults")

## ----override-options---------------------------------------------------------
# Set global node color
caugi_options(plot = list(
  node_style = list(fill = "lightblue")
))

# Override for this specific plot
plot(cg,
  main = "Custom Colors",
  node_style = list(fill = "pink")
)

# Reset to defauls
caugi_options(caugi_default_options())

## ----query-options------------------------------------------------------------
# View all current options
caugi_options()

# Query specific option
caugi_options("plot")

## ----manual-layout------------------------------------------------------------
coords <- caugi_layout(dag, method = "sugiyama")

# The layout can be used for analysis or custom plotting
print(coords)

# Plot uses the same layout, calling caugi_layout internally
plot(dag, layout = "sugiyama")

## ----grid-integration---------------------------------------------------------
# Create a plot
p <- plot(cg)

# The grob slot is a grid graphics object
class(p@grob)

# You can manipulate it with grid functions
library(grid)

# Draw the plot rotated by 30 degrees
pushViewport(viewport(angle = 30))
grid.draw(p@grob)
popViewport()

