# 1. Load data
source("code/load_data.R")
anime_df

# 2. Basic exploration:

#  Plotting
#   i) startDate vs averageScore
#   ii) startDate vs popularity
#   iii) startDate vs tags (filter by...)
#   iv) startDate vs genre (filter by...)
#  ... as well as any other interesting combinations

## Filter the dataset by a given genre
subset(anime_df, grepl("Drama", anime_df$genres))

## Filter the dataset by a given tag
subset(anime_df, grepl("Gore", anime_df$tags))

## Summarise each variable
table(unlist(anime_df$isAdult))
summary(unlist(anime_df$popularity))

## Statistical tests
#  - chisq.test
#  - ks.test / wilcox.test / ANOVA
ks.test(
  unlist(subset(anime_df, grepl("Drama", anime_df$genres))$popularity),
  unlist(subset(anime_df, grepl("Psychological", anime_df$genres))$popularity)
)
## compare mean popularity across genres
t.test(
  unlist(subset(anime_df, grepl("Drama", anime_df$genres))$popularity),
  unlist(subset(anime_df, grepl("Psychological", anime_df$genres))$popularity)
)
t.test(
  unlist(subset(anime_df, unlist(anime_df$isAdult))$popularity),
  unlist(subset(anime_df, !unlist(anime_df$isAdult))$popularity)
)
t.test(
  unlist(subset(anime_df, unlist(anime_df$isAdult))$averageScore),
  unlist(subset(anime_df, !unlist(anime_df$isAdult))$averageScore)
)


# 4. H0: The proportion of 18+ content has increased over time
lm(startDate ~ isAdult)

# 5. H0: There is a threshold of 18+ content in which audience decreases
lm(popularity ~ isAdult)
lm(averageScore ~ isAdult)