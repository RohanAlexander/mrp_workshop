library(tidyverse)

females <- read_csv("data/table-female.csv",
                    skip = 8, 
                    col_names = FALSE)
males <- read_csv("data/table-male.csv",
                  skip = 8,
                  col_names = FALSE)
both <- cbind(females, males)
rm(females, males)

# First transpose it - use this rather than t() so that it's still a tibble
both <- as_tibble(cbind(nms = names(both), t(both)), 
                  .name_repair = "minimal")
names(both) <- both[1,]
both <- both[2:nrow(both),]

# Here we get rid of some of the debris that the ABS includes
both <- 
  both %>% 
  select(-X1, 
         -`MB by Commonwealth Electoral Divisions (UR)`,
         -`Data Source: Census of Population and Housing, 2016, TableBuilder`,
         -`INFO`,
         -`Copyright Commonwealth of Australia, 2018, see abs.gov.au/copyright`,
         -`ABS data licensed under Creative Commons, see abs.gov.au/ccby`
  ) %>% 
  rename(age_group = `AGEP Age`,
         heap = `HEAP - 1 Digit Level`,
         gender = `SEXP Sex`)

both <- rbind(both[1:20,], both[23:42,])

# Now we push the age-groups into each cell.
both <- 
  fill(both, age_group)

census_data <- gather(both, 
                      key = "division", 
                      value = "number",
                      Adelaide:Total)

rm(both)


census_data$number <- as.integer(census_data$number)

census_data$age_group[census_data$age_group == "age18to29"] <- "ages18to29"

census_data <- 
  census_data %>% 
  select(division, gender, age_group, heap, number) %>% 
  arrange(division, gender, age_group, heap)

census_data <- 
  census_data %>% 
  filter(age_group != "Total") %>%
  filter(division != "Total")


convert <- read_csv("data/number_to_name_divisions.csv")

convert <- convert %>% 
  select(-number) %>% 
  rename(division = name)

census_data <- 
  census_data %>% 
  left_join(convert, by = "division")

# Get rid of the divisions and education
census_data <- 
  census_data %>% 
  group_by(state, gender, age_group) %>% 
  summarise(number = sum(number)) %>% 
  ungroup()


# Make proportions
census_data <- 
  census_data %>% 
  group_by(state) %>% 
  mutate(total = sum(number)) %>% 
  mutate(cell_prop_of_division_total = number / total) %>% 
  ungroup() %>% 
  select(-total)

write_csv(census_data, "outputs/data/census_data.csv")