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

visNetwork(nodes=nodes, visn$edges,main="Economic Industry Network") %>%
  visIgraphLayout(layout = "layout_in_circle") %>%
  visEdges(arrows = 'to')  %>%
  visOptions(nodesIdSelection=TRUE)

nodes<-visn$nodes %>% mutate(shape="dot", # "shape" variable: customize shape of nodes ("dot", "square", "triangle")
                            shadow=TRUE, # "shadow" variable: include/exclude shadow of node
                            label=visn$name, # "label" variable: add labels on nodes
                            size=20, # "size" variable: set size of nodes
                            borderWidth=1, # "borderWidth" variable: set border width of nodes
                            color.border="grey", # "color.border" variable: set frame color
                            color.highlight.background="yellow", # "color.highlight.background" variable: set color of the selected node
                            color.highlight.border="black") # "color.highlight.border" variable: set frame color of the selected node
visNetwork(nodes=visn$nodes, edges=visn$edges,main="Economic Industry Network") %>% # "main" variable: add a title
  visIgraphLayout()%>%
  visOptions(nodesIdSelection=TRUE) %>%
  visPhysics(stabilization = TRUE)