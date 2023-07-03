## read in packages
packages=c('tidyr', 'visNetwork','ggraph','igraph','readxl',
           'stringr','tidyverse','tidygraph','network','ggplot2')
lapply(packages, require, character.only = TRUE)


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


##finding SIC code inconsistencies and replacing them  
list_a <- as.list(int_con_2018$SIC_codes)
list_b <- as.list(colnames(int_con_2018))
list_b <- list_b[-c(1,2,108)]

list_diff <- c(setdiff(list_a, list_b),setdiff(list_b, list_a)) # list of 8

int_con_2018 <- int_con_2018 %>%
                rename('C11.01-6 & C12' = 'C1101T1106 & C12') %>%
                rename('C241_3' = 'C241T243') %>%
                rename('F41, F42 & F43' = 'F41, F42  & F43') %>%
                rename('H493_5' = 'H493T495')

list_a <- as.list(int_con_2018$SIC_codes)
list_b <- as.list(colnames(int_con_2018))
list_b <- list_b[-c(1,2,108)]

list_diff <- c(setdiff(list_a, list_b),setdiff(list_b, list_a)) #list of 0


##formatting matrix
int_con_2018$NEWCOL <- gsub("([A-Z)]+)\\d+.*","\\1", int_con_2018$SIC_codes) #leaving only A,B,C,ect

is_all_numeric <- function(x) {
  !any(is.na(suppressWarnings(as.numeric(na.omit(x))))) & is.character(x)
} ##a function to convert all columns to numeric except those which produce nas 


int_con_2018 <- int_con_2018  %>%
               select(-c(1,2,108)) %>%
               mutate_if(is_all_numeric,as.numeric) %>%
               aggregate(. ~ NEWCOL, sum) %>%
               remove_rownames() %>%
               column_to_rownames(var = 'NEWCOL')


colnames(int_con_2018) <- substr(colnames(int_con_2018), 1, 1) #convert column names to A,B ect.

int_con_2018 <- t(rowsum(t(int_con_2018), group = colnames(int_con_2018), na.rm = TRUE))#sum rows based on column names

matrix <- as.matrix(int_con_2018) #convert to matrix

diag(matrix) <- 0 #remove connections between same industry



##creating tidygraph object and tidygraph EDA

network_graph <- as_tbl_graph(matrix)


network_graph <- network_graph %>%
  mutate(id = row_number())
network_graph

network_graph %>% activate(edges) %>% pull(weight) -> frequencies
hist(frequencies)


degree_out <- network_graph %>%
  activate(nodes) %>%
  mutate(
    centrality_degree(
      weights = NULL,
      mode = "out",
      loops = TRUE,
      normalized = FALSE
    ))   %>%
  data.frame()
  
degree_in <- network_graph %>%
  activate(nodes) %>%
  mutate(
    centrality_degree(
      weights = NULL,
      mode = "in",
      loops = TRUE,
      normalized = FALSE
    )) %>%
  data.frame()

degree_out_weighted <- network_graph %>%
  activate(nodes) %>%
  mutate(
    centrality_degree(
      weights = weight,
      mode = "out",
      loops = TRUE,
      normalized = FALSE
    )) %>%
  data.frame()


degree_in_weighted <- network_graph %>%
  activate(nodes) %>%
  mutate(
    centrality_degree(
      weights = weight,
      mode = "in",
      loops = TRUE,
      normalized = FALSE
    )) %>%
  data.frame()


degree_df <- degree_out %>%
             left_join(degree_in, by= c('name','id')) %>%
             left_join(degree_out_weighted, by= c('name','id')) %>%
             left_join(degree_in_weighted, by= c('name','id')) %>%
             rename(degree_out = centrality_degree......x) %>%
             rename(degree_in = centrality_degree......y) %>%
             rename(degree_out_weighted = centrality_degree......x.x) %>%
             rename(degree_in_weighted = centrality_degree......y.y) %>%
              left_join(ind_struc, by='name')

max(matrix)

