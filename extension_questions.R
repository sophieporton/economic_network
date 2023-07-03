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

ggraph(full_network) +
  geom_edge_link(aes(start_cap = label_rect(node1.name), end_cap = label_rect(node2.name), alpha=weight), 
                 arrow = arrow(type = "closed", length = unit(3, 'mm'))) +
  geom_node_circle()

