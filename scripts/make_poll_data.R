example_poll <-
  tibble(
    gender = sample(c("Female", 
                      "Male"), 
                    5000, 
                    replace = TRUE, 
                    prob = c(0.75, 0.25)
    ), # deliberately over-sample females
    age_group = sample(c("ages18to29",
                         "ages30to44",
                         "ages45to59",
                         "ages60plus"), 5000, replace = TRUE)
  )

example_poll <- example_poll %>%
  mutate(
    binomial_param = case_when(
      gender == "Female" & age_group ==  "ages18to29" ~ 0.9,
      gender == "Female" & age_group ==  "ages30to44" ~ 0.7,
      gender == "Female" & age_group ==  "ages45to59" ~ 0.5,
      gender == "Female" & age_group ==  "ages60plus" ~ 0.3,
      gender == "Male" & age_group ==  "ages18to29" ~ 0.7,
      gender == "Male" & age_group ==  "ages30to44" ~ 0.5,
      gender == "Male" & age_group ==  "ages45to59" ~ 0.3,
      gender == "Male" & age_group ==  "ages60plus" ~ 0.1,
      TRUE ~ 0.5
    )
  ) %>%
  rowwise() %>%
  mutate(supports_ALP = 
           rbinom(n = 1, size = 1, prob = binomial_param))

example_poll <- 
  example_poll %>% 
  select(-binomial_param)

example_poll$state  <- 
  sample(c("NSW", "VIC", "QLD", "WA", "SA", "TAS", "ACT", "NT"), 
         5000, 
         replace = TRUE, 
         prob = c(0.32, 0.26, 0.20, 0.10, 0.07, 0.02, 0.02, 0.01)
         )

write_csv(example_poll, "outputs/data/example_poll.csv")

