##plotting using ggraph

matrix[matrix < 1000] <- 0

matrix<- matrix[-c(20), -c(20)]

network_graph <- as_tbl_graph(matrix)


network_graph <- network_graph %>%
  mutate(id = row_number())
network_graph

png(file="network_plot.png",width=8, height=6, units ='in', res=800)
  ggraph(network_graph, layout = 'graphopt') + 
  geom_edge_link(aes(start_cap = label_rect(node1.name), end_cap = label_rect(node2.name), alpha=weight), 
                 arrow = arrow(type = "closed", length = unit(3, 'mm'))) + 
  geom_node_text(aes(label = name)) +
  theme_graph() 
dev.off()


## make igraph object
gr <- graph_from_adjacency_matrix(matrix, mode="directed", weighted=T)

## convert to VisNetwork-list
visn <- toVisNetworkData(gr)

## copy column "weight" to new column "value" in list "edges"
visn$edges$value <- visn$edges$weight


visNetwork(visn$nodes, visn$edges,main="Economic Industry Network",width = "100%") %>%
  visIgraphLayout(layout = "layout_in_circle") %>%
  visEdges(arrows = 'to')  %>%
  visOptions(nodesIdSelection=TRUE) %>%
  visNodes(size = 20,shape="dot", # "shape" variable: customize shape of nodes ("dot", "square", "triangle")
           shadow=TRUE, # "shadow" variable: include/exclude shadow of node
           label=visn$nodes$id, # "label" variable: add labels on nodes
           borderWidth=1, # "borderWidth" variable: set border width of nodes
           color= list(border="grey", highlight=list(border='black',background='yellow')))