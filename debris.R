library(broom)
library(brms)
library(tidyverse) 

example_poll <- read_csv("outputs/data/example_poll.csv")

model <- brm(supports_ALP ~ gender + age_group, 
             data = example_poll, 
             family = bernoulli()
)


post_stratified_estimates <- 
  model %>% 
  tidybayes::add_predicted_draws(newdata = census_data) %>% 
  rename(alp_predict = .prediction) %>% 
  mutate(alp_predict_prop = alp_predict*cell_prop_of_division_total) %>% 
  group_by(state, .draw) %>% 
  summarise(alp_predict = sum(alp_predict_prop)) %>% 
  group_by(state) %>% 
  summarise(mean = mean(alp_predict), 
            lower = quantile(alp_predict, 0.025), 
            upper = quantile(alp_predict, 0.975))


post_stratified_estimates %>% 
  ggplot(aes(y = mean, x = forcats::fct_inorder(state), color = "MRP estimate")) + 
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0) + 
  ylab("Proportion ALP support") + 
  xlab("State") + 
  geom_point(data = example_poll %>% 
               group_by(state, supports_ALP) %>%
               summarise(n = n()) %>% 
               group_by(state) %>% 
               mutate(prop = n/sum(n)) %>% 
               filter(supports_ALP==1), 
             aes(state, prop, color = "Raw data")) +
  theme_minimal() +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank())



model_Test <- brm(supports_ALP ~ gender + age_group + (1|state), 
             data = example_poll, 
             family = bernoulli()
)
