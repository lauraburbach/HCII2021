# This file loads the Netlogo Data output and puts it into a usable R Dataframe

library(tidyverse)

#remotes::install_github("ropensci/osfr")
library(osfr)

  

noop <- function(x){x}


limit_size <- 10


input_file <- "BoundedRationality All-table.csv"


raw <- read_csv(input_file, skip_empty_rows = T, skip = 6)

names(raw) <- c("run_number", "agent_count", "epsilon", "backfire", "step", "list_op")

raw <- raw %>% 
  arrange(run_number, step) %>% 
  #head(limit_size) %>% 
  noop()

data <- raw %>% 
  mutate(list_op = str_remove_all(list_op, c("\\[") ) ) %>%     # remove list symbols
  mutate(list_op = str_remove_all(list_op, c("\\]") ) ) %>% 
  rowwise() %>%                                                 # turn list into rows (help index creation)
  mutate(agent = paste(1:agent_count, collapse = " ")) %>% 
  separate_rows(list_op, agent, sep = " ", convert = T) %>% 
  noop()

write_csv(data, "netlogo_ouput.csv")

data %>% filter(agent_count == 100, epsilon == 0.1, backfire == FALSE) %>% pull(run_number) 

data %>% 
#  filter(agent_count == 100) %>% 
#  filter(run_number == 7) %>% 
#  filter(epsilon == 0.2) %>% 
#  filter(agent == 1) %>% 
  ggplot() +
  aes(x = step, 
      y = list_op, 
      group = interaction(run_number, agent),
      color = run_number
      ) + 
  geom_line(alpha = 0.1) + 
  facet_grid(backfire~epsilon)
  
                