# This file loads the Netlogo Data output and puts it into a usable R Dataframe

library(tidyverse)

# helper terminate function
noop <- function(x){x}


# Parameters ----

# optional parameter to limit the sorting
limit_size <- 10
# Where to load the data from
input_file <- "data/BoundedRationality All-table.csv"
skip_count <- 7


# Read Netlogo Data ----
raw <- read_csv(input_file, skip_empty_rows = T, skip = 6)
names(raw) <- c("run_number", "agent_count", "epsilon", "backfire", "step", "list_op")


# Read Julia data ----
raw_j <- read_csv("data/results.csv") 
raw_j <- raw_j %>% select(run_number = seed, agent_count, epsilon, backfire, step, list_op = opinion, agent = agent_number)


########

# show single plot ----

plot_run_n <- function(df, run) {
  df %>% filter(run_number %in% run) %>% 
    mutate(list_op = str_remove_all(list_op, c("\\[") ) ) %>%     # remove list symbols
    mutate(list_op = str_remove_all(list_op, c("\\]") ) ) %>% 
    rowwise() %>%                                                 # turn list into rows (help index creation)
    mutate(agent = paste(1:agent_count, collapse = " ")) %>% 
    separate_rows(list_op, agent, sep = " ", convert = T) %>% 
    noop() -> df_temp
  
  
  df_temp <- df_temp %>% mutate(run_number = factor(run_number, labels = c("epsilon = 0.1 & backfire",
                                                                "epsilon = 0.3 & no backfire",
                                                                "epsilon = 0.1 & no backfire",
                                                                "epsilon = 0.2 & no backfire")))
  
  df_temp %>% ggplot() +
    aes(x = step) +
    aes(y = list_op) +
    aes(group = agent) +
    geom_line(alpha = 0.1) +
    scale_x_continuous(limits = c(0,30)) +
    ggplot2::labs(x = "Simulation steps (scale shortend)",
           y = "Opinions of agents", 
           title = "Opinion change of agents over time",
           subtitle = "4 examples from the data set",
           caption = "500 Agents"
    ) + facet_wrap(~run_number)
}

raw %>% filter(run_number == 4151)

raw %>% 
  filter(agent_count == 500) %>% 
  group_by(epsilon, backfire) %>% 
  summarise(id = unique(run_number)[1]) %>% pull(id) -> list_of_ids

plot_run_n(raw, list_of_ids)
plot_run_n(raw, c(4001,4251, 4051, 4151))

ggsave("examples.pdf", width = 5, height = 3.6)

# Filter data to last step ----

raw_f <- raw %>% 
  filter(step == 100) %>% 
  noop()

raw_fj <- raw_j %>% 
  filter(step == 100) %>%
  noop()


# convert netlogo
data <- raw_f %>% 
  mutate(list_op = str_remove_all(list_op, c("\\[") ) ) %>%     # remove list symbols
  mutate(list_op = str_remove_all(list_op, c("\\]") ) ) %>% 
  rowwise() %>%                                                 # turn list into rows (help index creation)
  mutate(agent = paste(1:agent_count, collapse = " ")) %>% 
  separate_rows(list_op, agent, sep = " ", convert = T) %>% 
  mutate(program = "NetLogo") %>% 
  noop()

# adapt julia
data_j <- raw_fj %>% 
  mutate(program = "Julia")

# merge data und limit random noise 
data <- bind_rows(data_j, data) %>% 
  mutate(list_op = round(list_op, 2))

## Create a plot to compare ----

write_rds(data, "data/alldata.rds")

data %>% group_by(run_number, epsilon, backfire, agent_count, program) %>% 
  summarise(opinion_mean = mean(list_op),
            opinion_sd = sd(list_op),
            opinion_count = length(levels(factor(list_op)))) %>% 
  ggplot() +
  aes(x = factor(epsilon)) +
  aes(color = opinion_sd) +
  aes(y = opinion_count) +
  #aes(color = factor(program)) +
  geom_jitter(alpha = 0.5) +
  facet_wrap(backfire~program)

ggsave("opinion_compare.pdf")

## To several t-test for all similar configurations between julia and netlogo
# Depdent variable is opinion sd
# 
# 
library(broom)


data %>% group_by(run_number, epsilon, backfire, agent_count, program) %>% 
  summarise(opinion_mean = mean(list_op),
            opinion_sd = sd(list_op),
            opinion_count = length(levels(factor(list_op)))) %>%
  group_by(epsilon, backfire, agent_count) %>% 
  do(tidy(t.test(opinion_sd ~ program, data = .))) %>% 
  mutate(conf_width = abs(conf.low - conf.high )) %>% 
  arrange(p.value) %>% head(10) %>% 
  select(epsilon, backfire, agent_count, t = statistic, p = p.value, df = parameter ) %>% knitr::kable()
  

  

