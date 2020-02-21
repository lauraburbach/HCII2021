# This file loads the Netlogo Data output and puts it into a usable R Dataframe

library(tidyverse)

# helper terminate function
noop <- function(x){x}

# optional parameter to limit the sorting
limit_size <- 10
# Where to load the data from
input_file <- "data/BoundedRationality All-table.csv"

skip_count <- 7
step_size <- 1000
length_dataset <- dim(read_csv(input_file, skip_empty_rows = T, skip = 6))[1]

data <- data.frame(
  X1 = c(), X2 = c(), X3 = c(), X4 = c(), X5 = c(), X6 = c(), X7 = c()
)

for (i in 0:(length_dataset/step_size)) {
  
  tmp <- read_csv(input_file, skip_empty_rows = T, skip = skip_count + i * step_size, n_max = step_size, col_names = FALSE)
  
  tmp <- tmp %>% 
    arrange("X1", "X5") %>% 
    noop()
  
  tmp %>% 
    mutate("X6" = str_remove_all("X6", c("\\[") ) ) %>%     # remove list symbols
    mutate("X6" = str_remove_all("X6", c("\\]") ) ) %>% 
    rowwise() %>%                                                 # turn list into rows (help index creation)
    mutate("X7" = paste(1:"X2", collapse = " ")) %>% 
    separate_rows("X6", "X7", sep = " ", convert = T) %>% 
    noop() -> tmp
  
  data %>%
    bind_rows(tmp) -> data
}

names(data) <- c("run_number", "agent_count", "epsilon", "backfire", "step", "list_op", "agent")


# ------------- #

raw <- read_csv(input_file, skip_empty_rows = T, skip = 6, n_max = 10000)

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

write_rds(data, "data/netlogo_ouput.rds", compress = "gz")

data %>% filter(agent_count == 100, epsilon == 0.1, backfire == FALSE) %>% pull(run_number) 

data %>% 
  filter(agent_count == 100) %>% 
  filter(run_number == 7) %>% 
  filter(epsilon == 0.2) %>% 
  filter(agent == 1) %>% 
  ggplot() +
  aes(x = step, 
      y = list_op, 
      group = interaction(run_number, agent),
      color = run_number
      ) + 
  geom_line(alpha = 0.1) + 
  facet_grid(backfire~epsilon)
  
                