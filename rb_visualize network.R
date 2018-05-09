
library(flexdashboard)
library(DBI)
library(networkD3)
library(visNetwork)
library(igraph)
library(ggplot2)
library(gridExtra)
library(plotly)
library(shiny)

g1 <- graph.adjacency(APavg_betaT10)
edges <- as.data.frame(get.edgelist(g1))
colnames(edges) <- c("from","to")
edges$from <- as.numeric(edges$from)
edges$to <- as.numeric(edges$to)
zeroIndex <- min(edges)


# g1 <- graph_from_edgelist(as.matrix(edges), directed = TRUE) %>%
#   set_vertex_attr("name", value = letters[1:length(nodes)])
nodes <- data.frame(id = vertex_attr(g1, "name"),
                    Degree = degree(g1),
                    Closeness = closeness(g1)
)

cluster <- cluster_walktrap(g1, weights = NULL)

nodes$group <- as.numeric(membership(cluster))


p1 <- plot_ly(nodes,x = ~id, y = ~Degree, type = 'bar') 

p2 <- plot_ly(nodes,x = ~id, y = ~Closeness, type = 'scatter', marker = list(size = 10, color = 'rgba(255, 182, 193, .9)', line = list(color = "black",width = 3)), mode = 'markers+text')

subplot(p1,p2,nrows = 2, titleY = TRUE, titleX = FALSE, margin = 0.05) %>% layout(showlegend = FALSE) 


forceNetwork(Links = edges-zeroIndex, Nodes = nodes, 
             Source = "from", Target = "to",
             NodeID = "id", Group = "group",
             opacity = 0.8)


visNetwork(nodes, edges) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE,
             selectedBy = "group") %>%
  visPhysics(solver = "forceAtlas2Based") %>%
  #visIgraphLayout(layout = "layout.davidson.harel")
  visIgraphLayout(layout = "layout_nicely")
