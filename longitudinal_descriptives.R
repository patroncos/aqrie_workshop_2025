#-------------------------------------------
# Set up
#-------------------------------------------

library(lavaan)
library(tidyverse)
library(lme4)

data = Demo.growth # this is data from the lavaan package

#-----------------------------------------
# reshape wide to long for MLM
#-----------------------------------------

data <- data %>%
  mutate(id = cur_group_rows())

data_long <- data %>%
  select(id, starts_with("t"), x1, x2) %>%
  pivot_longer(cols = starts_with("t"),
               names_to = "time",
               names_prefix = "t",
               values_to = "outcome") %>%
  left_join(data %>%
              select(id, starts_with("c")) %>%
              pivot_longer(cols = starts_with("c"),
                           names_to = "time",
                           names_prefix = "c",
                           values_to = "covar"), 
            by = c("id", "time")) %>%
  mutate(t = as.numeric(time) - 1)

saveRDS(data_long, "data_long.RDS")

#-----------------------------------------
# summary of descriptives
#-----------------------------------------

summary <- data_long %>%
  group_by(time) %>%
  summarise(mean_outcome=mean(outcome, na.rm=T),
            mean_covar=mean(covar, na.rm=T),
            sd_outcome=sd(outcome, na.rm=T),
            sd_covar=sd(covar, na.rm=T))

#-----------------------------------------
# plot trends over time
#-----------------------------------------

(plot_outcome <- ggplot(data_long, 
                       aes(x=time, y=outcome, group=id)) +
  geom_line() + geom_point())

(plot_covar <- ggplot(data_long, 
                        aes(x=time, y=covar, group=id)) +
    geom_line() + geom_point())


(plot_averages <- ggplot(summary, 
                         aes(x=time, y=mean_outcome, group = 1)) +
  geom_line() + geom_point() +
  geom_line(aes(x=time, y=mean_covar, colour="red", group = 1)) +
  geom_point(aes(x=time, y=mean_covar, colour="red", group = 1)) +
  theme(legend.position = "none"))

