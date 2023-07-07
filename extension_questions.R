##read in data 
int_con_2018 <- read_excel('data/bb20detailedsutablepublication.xlsx',
                           sheet= 'Table 2 - Int Con 2018',
                           skip=3)


##data wrangling
int_con_2018 <- int_con_2018 %>% 
  drop_na(...1,...2) %>%
  rename(SIC_codes = ...1) %>% 
  rename(SIC_description = ...2) %>%
  mutate_at("SIC_codes", str_replace, "CPA_", "")


int_con_2018 <- int_con_2018 %>%
  rename('C11.01-6 & C12' = 'C1101T1106 & C12') %>%
  rename('C241_3' = 'C241T243') %>%
  rename('F41, F42 & F43' = 'F41, F42  & F43') %>%
  rename('H493_5' = 'H493T495')

is_all_numeric <- function(x) {
  !any(is.na(suppressWarnings(as.numeric(na.omit(x))))) & is.character(x)
} ##a function to convert all columns to numeric except those which produce nas 


int_con_2018 <- int_con_2018  %>%
  select(-c(2,108)) %>%
  mutate_if(is_all_numeric,as.numeric) %>%
  remove_rownames() %>%
  column_to_rownames(var = 'SIC_codes')


matrix <- as.matrix(int_con_2018) #convert to matrix

diag(matrix) <- 0 #remove connections between same industry


full_network<-graph_from_adjacency_matrix(matrix, mode='directed', weighted=TRUE,
                                          add.colnames = TRUE)

plot(full_network)

ggraph(full_network) +
  geom_node_point() +
  geom_edge_link()


#extension questions

col_sums <- colSums(matrix)
max_sum_column <- which.max(col_sums)
max_sum <- max(col_sums)
print(paste("Column with Maximum Sum:", max_sum_column))
print(paste("Maximum Sum:", max_sum))



row_sums <- rowSums(matrix)
max_sum <- max(row_sums)
max_sum_row <- which.max(row_sums)
print(paste("Row with Maximum Sum:", max_sum_row))
print(paste("Maximum Sum:", max_sum))


row_nonzero_counts <- rowSums(matrix != 0)
max_nonzero_row <- which.max(row_nonzero_counts)
max_nonzero_count <- sum(matrix[max_nonzero_row, ] != 0)
print(paste("Row with Greatest Non-Zero Values:", max_nonzero_row))
print(paste("Number of Non-Zero Values in Max Row:", max_nonzero_count))



# Calculate PageRank scores
V(full_network)$name <- colnames(matrix)
pagerank_scores <- page_rank(full_network)$vector
top_indices <- order(pagerank_scores, decreasing = TRUE)[1:5]
top_node_names <- V(full_network)$name[top_indices]
top_node_scores <- pagerank_scores[top_indices]
for (i in 1:length(top_node_names)) {
  print(paste("Node:", top_node_names[i]))
  print(paste("PageRank score:", top_node_scores[i]))
}


# Calculate eigenvector centrality
eigenvector_centralities <- evcent(full_network)$vector

# Get the indices of the top 5 nodes with the highest eigenvector centrality
top_indices <- order(eigenvector_centralities, decreasing = TRUE)[1:5]

# Get the names of the top 5 nodes

top_node_names <- V(full_network)$name[top_indices]

# Get the eigenvector centrality values of the top 5 nodes
top_node_centralities <- eigenvector_centralities[top_indices]

# Display the results
for (i in 1:length(top_node_names)) {
  print(paste("Node:", top_node_names[i]))
  print(paste("Eigenvector Centrality:", top_node_centralities[i]))
}