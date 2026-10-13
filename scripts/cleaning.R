# cleaning data
# Luke Watson

# load libraries ----------------------------------------------------------

library(tidyverse)

# load raw data -----------------------------------------------------------

raw_data <- read_csv('data/raw_data.csv')

# clean data --------------------------------------------------------------

# impute aka scores to this dataset, not originally included
aka_scores <- c(
  "Argentina" = 56.2,
  "Australia" = 67.7,
  "Austria" = 64.8,
  "Bangladesh" = 29.5,
  "Belgium" = NA,
  "Brazil" = 51.6,
  "Canada" = 68.3,
  "Chile" = 53.7,
  "China" = 49.4,
  "Colombia" = 46.2,
  "Costa Rica" = NA,
  "Croatia" = 51.4,
  "Cyprus" = 43.1,
  "Czech Rep." = 57.4,
  "Denmark" = 74.2,
  "Ecuador" = 43.8,
  "Estonia" = 56.4,
  "Ethiopia" = 35.3,
  "Finland" = 67.8,
  "Germany" = 67.5,
  "Greece" = 47.4,
  "Hungary" = 53.6,
  "Iceland" = 73.5,
  "India" = 38.7,
  "Indonesia" = 33.9,
  "Iran" = 37.2,
  "Israel" = NA,
  "Italy" = 55.9,
  "Japan" = 59.7,
  "Latvia" = 55.2,
  "Lebanon" = 40.2,
  "Lithuania" = 51.3,
  "Malaysia" = 42.1,
  "Malta" = NA,
  "Mexico" = 47.8,
  "Netherlands" = 71.2,
  "New Zealand" = 69.6,
  "Nigeria" = 31.7,
  "Norway" = 73.8,
  "Philippines" = 43.5,
  "Poland" = 50.9,
  "Portugal" = 58.5,
  "Romania" = 44.0,
  "Russia" = 47.0,
  "Serbia" = 47.5,
  "Slovakia" = 51.1,
  "Slovenia" = 58.1,
  "South Africa" = 48.2,
  "South Korea" = 50.9,
  "Spain" = 64.5,
  "Swaziland" = NA,
  "Switzerland" = 68.8,
  "Taiwan" = 55.2,
  "Tanzania" = NA,
  "Turkey" = 37.0,
  "United Kingdom" = 69.4,
  "United States" = 61.9,
  "Zimbabwe" = 35.5
)

# add to raw data
aka_data <- raw_data |> 
  mutate(
    akacoll = aka_scores[nation],
    .after = mincoll
  )

# correct data entry error, add slovenia pelham score to orig data
aka_data$pelcoll[aka_data$nation == "Slovenia"] <- -0.78

# reverse score collectivism measures to aid interpretation
rev_data <- aka_data |> 
  mutate(
    pelcoll = -1 * pelcoll, 
    mincoll = -1 * mincoll, 
    akacoll = 100 - akacoll
  )

# remove those not currently in a relationship
no_singles <- rev_data |> 
  filter(relationship.stat == 'coupled')

message(glue::glue('Removed {nrow(rev_data)-nrow(no_singles)} participants not in a relationship'))

# listwise deletion of participants with no avoidant attachment score or relationship satisfaction score
clean_data <- no_singles |> 
  drop_na(c(secure, dismissive, preoccupied, fearful, prqc))

message(glue::glue('Removed {nrow(no_singles)-nrow(clean_data)} participants with missing attachment and relationship satisfaction scores'))

# verify the number of countries with valid collectivism scores for write up
clean_data |> 
  select(nation, ends_with('coll')) |> 
  group_by(nation) |> 
  summarize(
    across(ends_with('coll'), \(x) sum(!is.na(x)))
  ) |> 
  ungroup() |> 
  summarize(
    across(ends_with('coll'), \(x) n_distinct(nation) - sum(x == 0))
  )

# compute attachment scores
clean_data <- clean_data |> 
  mutate(
    avoid_attachment = -1 * (secure + dismissive - preoccupied - fearful)
  )

# listwise deletion of nations missing any one of the collectivism measures
clean_data_rm_na_nations <- clean_data |> 
  drop_na(ends_with('coll'))

# final sample
message(glue::glue('Removed {nrow(clean_data)-nrow(clean_data_rm_na_nations)} participants from {n_distinct(clean_data$nation)-n_distinct(clean_data_rm_na_nations$nation)} nations'))

# view changes in data ----------------------------------------------------

# do the distributions of main variables change much after dropping na nations?
plot_data <- clean_data |> 
  select(nation, ends_with('coll'), c(avoid_attachment, prqc)) |> 
  mutate(
    dropped_nations = if_else(
      !nation %in% unique(clean_data_rm_na_nations$nation), 
      'dropped', 
      'not_dropped'
    )
  )

# table
plot_data |> 
  group_by(dropped_nations) |> 
  summarize(
    across(-nation, \(x) mean(x, na.rm = T)), 
    n_observations = n(), 
    n_nations = n_distinct(nation)
  )

# dropped nations tend to be a bit more collectivistic

# compare measure distributions
plot_data |> 
  pivot_longer(-contains('nation')) |> 
  ggplot(aes(x = value, fill = dropped_nations)) + 
  geom_histogram(position = 'dodge') + 
  scale_fill_viridis_d(option = 'rocket', end = .8) +
  theme_bw() + 
  facet_wrap(~name, scales = 'free')

# save
# dir.create('fig_output')
ggsave('fig_output/difference_dists.png', device = 'png', width = 8, height = 7, units = 'in')
