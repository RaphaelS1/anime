library(lubridate)
library(rpart)
library(rpart.plot)
library(Hmisc)
library(randomForest)
library(e1071)
library(randomForestSRC)

# 1. Load data
source("code/load_data.R")
head(anime_df)

# 2. Basic exploration:

#  Plotting
#   i) startDate vs averageScore
plot(anime_df$startDate, anime_df$averageScore, xlab = 'Start Date', ylab = 'Average Score')

#   ii) startDate vs popularity
plot(anime_df$startDate, anime_df$popularity)

#   iii) startDate vs tags (filter by...)
## Filter the dataset by a given tag
tag_gore <- subset(anime_df, grepl("Gore", anime_df$tags))
plot(tag_gore$startDate, tag_gore$popularity)
plot(tag_gore$startDate, tag_gore$averageScore)

#   iv) startDate vs genre (filter by...)
## Filter the dataset by a given genre
genre_drama <- subset(anime_df, grepl("Drama", anime_df$genres))
plot(genre_drama$startDate, genre_drama$popularity)
plot(genre_drama$startDate, genre_drama$averageScore)

#  ... as well as any other interesting combinations

plot(anime_df$volumes, anime_df$popularity)
plot(anime_df$volumes, anime_df$averageScore)


## Summarise each variable
table(anime_df$isAdult)
summary(anime_df$popularity)

## Statistical tests

#  - ks.test / wilcox.test / ANOVA
ks.test(
  subset(anime_df, grepl("Drama", anime_df$genres))$popularity,
  subset(anime_df, grepl("Psychological", anime_df$genres))$popularity
)
## compare mean popularity across genres
t.test(
  subset(anime_df, grepl("Drama", anime_df$genres))$popularity,
  subset(anime_df, grepl("Psychological", anime_df$genres))$popularity
)
t.test(
  subset(anime_df, unlist(anime_df$isAdult))$popularity,
  subset(anime_df, !unlist(anime_df$isAdult))$popularity
)
t.test(
  subset(anime_df, unlist(anime_df$isAdult))$averageScore,
  subset(anime_df, !unlist(anime_df$isAdult))$averageScore
)

# 4. H0: The proportion of 18+ content has increased over time
plot(aggregate(anime_df$isAdult ~ year(anime_df$startDate), FUN = mean))

# 5. H0: There is a threshold of 18+ content in which audience decreases
summary(lm(anime_df$popularity ~ anime_df$isAdult))
summary(lm(anime_df$averageScore ~ anime_df$isAdult))
boxplot(anime_df$popularity ~ anime_df$isAdult)
boxplot(anime_df$averageScore ~ anime_df$isAdult)


# Encoding

genres <- unique(unlist(strsplit(anime_df$genres, ',')))
mat_genres <- matrix(0, nrow = nrow(anime_df), ncol = length(genres))
colnames(mat_genres) <- genres
mat_genres

for (i in seq(nrow(anime_df))) {
  mat_genres[i, ] <- as.integer(genres %in% unlist(strsplit(unlist(anime_df[i, 'genres']), ',')))

}

anime_df <- cbind(anime_df, mat_genres)
head(anime_df)
anime_df$genres <- NULL

tags <- unique(unlist(strsplit(anime_df$tags, ',')))
mat_tags <- matrix(0, nrow = nrow(anime_df), ncol = length(tags))
colnames(mat_tags) <- tags
mat_tags

for (i in seq(nrow(anime_df))) {
  mat_tags[i, ] <- as.integer(tags %in% unlist(strsplit(unlist(anime_df[i, 'tags']), ',')))

}

anime_df <- cbind(anime_df, mat_tags)
head(anime_df)
anime_df$tags <- NULL

rm(list = setdiff(ls(), "anime_df"))

rcorr(as.matrix(anime_df[3:8]))
anime_df$chapters <- NULL

lapply(anime_df[1:7], function(x) {
  sum(is.na(x))
})
anime_df$endDate[is.na(anime_df$endDate)] <- Sys.Date()
anime_df$time <- as.numeric(anime_df$endDate - anime_df$startDate)
anime_df$endDate <- NULL
anime_df$startDate <- as.numeric(anime_df$startDate)
anime_df <- anime_df[!is.na(anime_df$startDate), ]

anime_df$volumes[is.na(anime_df$volumes)] <- mean(anime_df$volumes, na.rm = TRUE)

anime_df$averageScore[is.na(anime_df$averageScore)] <- mean(anime_df$averageScore, na.rm = TRUE)

any(anime_df$time < 0)

colnames(anime_df) <- gsub(' |-|\\+', '_', colnames(anime_df))

colnames(anime_df) <- gsub("'", '_', colnames(anime_df), fixed = TRUE)

# Models

tree <- rpart(averageScore ~ ., anime_df)
plot(tree)
text(tree)
tree
rpart.plot(tree, shadow.col = 'grey')

tree1 <- rpart(popularity ~ ., anime_df)
plot(tree1)
text(tree1)
rpart.plot(tree1)


tree2 <- rpart(genres ~ ., anime_df[1:200, ], method = 'class')
p1 <- predict(tree2, anime_df[201:250, ], type = 'class')


model_svm <- svm(averageScore ~., anime_df)
p2 <- predict(model_svm, anime_df)

neuralnet(averageScore ~ ., anime_df)

forest <- rfsrc(averageScore ~ ., data = anime_df)

randomForestSRC::vimp(forest)


# 1) Find bug that causes 0s in genres and tags

which(colSums(anime_df==0) == nrow(anime_df))
all(anime_df$Biographical == 0)

col.rm <- colSums(anime_df_sample==0) == nrow(anime_df_sample)
any(col.rm)

# 2) Subsample data

anime_df_sample <- as.data.frame(anime_df[sample(1:nrow(anime_df), 300),])
anime_df_sample <- anime_df_sample[!col.rm]

# 3) a) Create training and testing data set

set.seed(123)
train_ind <- sample(nrow(anime_df_sample), 200)

training <- anime_df_sample[train_ind, ]
testing <- anime_df_sample[-train_ind, ]
rm(anime_df)

training <- as.data.frame(training)
testing <- as.data.frame(testing)

# b) Train models on training data; make predictions for test data

model_svm <- svm(averageScore ~., training)
predict(model_svm, testing)

forest <- rfsrc(averageScore ~ ., data = training)
forest
randomForestSRC::vimp(forest)

model_rpart <- rpart(averageScore ~ ., training)
predict(model_rpart, testing)

lm <- lm(averageScore ~ ., training)
predict(lm)
plot(lm)
