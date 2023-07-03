##read in data 
ind_struc <- read_csv('data/sic2007summaryofstructurtcm6.csv',
                           skip=1)

##data wrangling
ind_struc <- ind_struc %>%
             select(c(1,2)) %>%
             na.omit(ind_struc) %>%
             rename(industry = ...2) %>% 
             rename(name = SECTION)
