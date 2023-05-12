library(tidyr)
library(ggplot2)
library(dplyr)

setwd("C:/Users/abidi/Documents/ISAMM/2eme Année Big Data/2eme Semestre/Statistique Descriptive - R/Projet/")
movies_df <- read.csv("movies.csv")
head(movies_df)
str(movies_df)
cat("Shape of data:", dim(movies_df), "\n")
cat("Duplicated rows:", sum(duplicated(movies_df)), "\n")
cat("Missing values count:\n", colSums(is.na(movies_df)), "\n")
df_no_na <- na.omit(movies_df)
cat("If drop all NAs:", dim(df_no_na), "\n")
# Dataset contains 7668 rows and 15 columns.
# 5421 rows out of 7668 don't have NAs.
# Dataset doesn't contain duplicated rows.
# We need 'budget' and 'gross' to calculate the target variable ('success'). Coincidentally, these two columns contain the most amount of NAs.
# 5421 rows create a big enough dataset. Let's just drop NAs.
missing_values <- colSums(is.na(movies_df))
print(missing_values)

for (col in colnames(movies_df)) {
  pct_missing <- mean(is.na(movies_df[[col]]))
  print(paste(col, "-", round(pct_missing*100), "%"))
}

# Drop any rows with missing values
movies_df <- drop_na(movies_df)

# Percentage of missing values for each column
col_missing_pct <- round(colMeans(is.na(movies_df)) * 100, 2)

# Display result for each column
for (col in names(col_missing_pct)) {
  print(paste(col, "-", col_missing_pct[[col]], "%"))
}

# Show data type of each column
str(movies_df)

# Create a boxplot of the variable 'gross'
boxplot(movies_df$gross, main="Boxplot of Gross Earnings", ylab="Gross Earnings")

# Create a boxplot of the 'budget' variable
boxplot(movies_df$budget, main="Boxplot of Budget", ylab="Budget")

# Check for inconsistencies in the data
inconsistent_data <- subset(movies_df, budget > gross)
if (nrow(inconsistent_data) > 0) {
  print('Incohérences dans les données détectées!')
  print(inconsistent_data)
}

str(movies_df)

## Feature engineering 

# drop NAs
data_reduced <- na.omit(movies_df)

#1 if the revenue is higher than the budget
#0 if the revenue is smaller than the budget
data_reduced$success <- ifelse(data_reduced$gross > data_reduced$budget, 1, 0)

table(data_reduced$success)

# The dataset is unbalanced. We have more successful movies (67.8%) than not (32.2%).

# how many values are in the 'released' column, that doesn't have a full date?
sum(sapply(strsplit(data_reduced$released, "\\(")[[1]], function(x) length(strsplit(x, " ")[[1]]) != 3))

# drop these rows
data_reduced <- data_reduced[!(sapply(strsplit(data_reduced$released, "\\("), '[', 1) %>%                             sapply(strsplit, " ") %>%                             sapply(length) != 3), ]

sum(sapply(strsplit(data_reduced$released, "\\("), '[', 1) %>% 
      sapply(strsplit, " ") %>% 
      sapply(length) != 3)

# convert 'released' to datetime type
data_reduced$released <- as.Date(sub("\\(.+", "", data_reduced$released), format = "%d %b %Y")

# get month, day, and day of the week from released
data_reduced$month <- format(data_reduced$released, "%m")
data_reduced$day <- format(data_reduced$released, "%d")
data_reduced$week_day <- weekdays(data_reduced$released)

data_reduced <- data_reduced[c('name', 'rating', 'genre', 'released', 'year', 'month', 'day',                               'week_day', 'score', 'votes', 'director', 'writer', 'star',                               'country', 'budget', 'gross', 'company', 'runtime', 'success')]
head(data_reduced)

summary(data_reduced[,c('rating', 'year', 'month', 'day', 'score', 'votes', 'budget', 'gross', 'runtime', 'success')])

summary(data_reduced[,c('name', 'genre', 'director', 'writer', 'star', 'country', 'company')])

# We have too many unique values in columns: 'director',
# 'writer', 'star', and 'company'. A model won't be able 
# to learn any useful information from them. We need to transform these variables.

# exclude PassengerId from the analysis
data_reduced <- data_reduced[, -1]

# split columns into numerical and categorical
float_cols <- names(data_reduced)[sapply(data_reduced, is.numeric) & sapply(data_reduced, function(x) any(is.finite(x))) & sapply(data_reduced, function(x) !any(is.na(x)))]
int_cols <- names(data_reduced)[sapply(data_reduced, is.integer)]
cat_cols <- names(data_reduced)[!sapply(data_reduced, is.numeric)]
num_cols <- c(float_cols, int_cols)

# further classify numerical variables
binary <- names(data_reduced)[sapply(data_reduced[, num_cols], function(x) length(unique(x)) == 2)]
num_not_binary <- setdiff(num_cols, binary)
float_not_binary <- setdiff(float_cols, binary)
int_not_binary <- setdiff(int_cols, binary)

# print results
cat("Numerical variables: ", num_cols, "\n")
cat("Numerical continuous variables: ", float_not_binary, "\n")
cat("Numerical discrete variables: ", int_not_binary, "\n")
cat("Numerical and not binary variables: ", num_not_binary, "\n")
cat("Binary variables: ", binary, "\n")
cat("Categorical variables: ", cat_cols, "\n")

# create histograms for all numerical variables
par(mfrow=c(3, 3))
for (col in num_not_binary) {
  hist(data_reduced[[col]], main=col, xlab="", ylab="Frequency", breaks=10)
}

# Some of the numeric variables are skewed and need to be normalized.

# compute correlation matrix
cor_matrix <- cor(data_reduced[num_cols], use="pairwise.complete.obs")

# plot heatmap
ggplot(data = reshape2::melt(cor_matrix)) +
  geom_tile(aes(Var2, Var1, fill = value)) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "", y = "", fill = "Correlation")

# Most correlated variables:
  
#  gross - budget: 0.74
# Assumption: more expensive movies make more money.
# gross - votes: 0.61
# Assumption: more popular movies attract more attention.
# votes - score: 0.47
# Assumption: people are more likely to rate movies they like.
# votes - budget: 0.44
# Assumption: more expensive movies attract more attention.
# score - runtime: 0.41
# Assumption: longer movies get higher ratings on average.
# runtime - votes: 0.35
# Assumption: longer movies attract more attention. These two variables might also be spuriously correlated and have a common response variable (score).
# budget - year: 0.33
# Assumption: on average movie budgets have grown over the years.
# success - gross: 0.35
# Assumption: movies with higher revenue are more likely to be successful.
# runtime - budget: 0.32
# Assumption: longer movies are more expensive.
# success - votes: 0.31
# Assumption: movies that attract more attention are more likely to be successful.
# I will take into account the information above to build visualizations later.

# Target variable 'success'

# has almost no correlation with the month, day, and weekday of the release, therefore I will not include these variables in predicting features;
# has the highest correlation with variables 'gross', 'votes', and 'score', but we can't use them as predicting features because we can't know them before the movie release;
# has an observable correlation with variables 'budget' and 'year' that we can use as predicting features.

# create bar plots for categorical data
for (i in c(cat_cols, binary)) {
  ggplot(data_reduced, aes(x = reorder(factor(get(i)), -table(get(i))[get(i)]), fill = factor(get(i)))) +
    geom_bar() +
    ggtitle(i) +
    xlab(i) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
}


library(tidyverse)

data_reduced %>%
  group_by(success, rating) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = rating, values_from = count)

# G (General Audiences) – All ages admitted.
# PG (Parental Guidance Suggested) – Some material may not be suitable for children.
# PG-13 (Parents Strongly Cautioned) – Some material may be inappropriate for children under 13.
# R (Restricted) – Under 17 requires accompanying parent or adult guardian.
# NC-17 (Adults Only) – No one 17 and under admitted.

# Motion Picture Association film rating system Wikipedia page

# From IMBb help:
  
#   'TV-MA' and 'X' is similar to 'NC-17'
# 'Unrated' is the same as 'Not Rated'
#'Approved' is similar to 'PG-13'

library(dplyr)

to_replace <- c('TV-MA' = 'NC-17',
                'X' = 'NC-17',
                'Unrated' = 'Not Rated',
                'Approved' = 'PG-13')

data_reduced$rating <- plyr::mapvalues(data_reduced$rating, from = names(to_replace), to = to_replace)

table(data_reduced$rating)
prop.table(table(data_reduced$rating))

# look closer at the 'genre' column
data_reduced %>%
  group_by(success, genre) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = genre, values_from = count)

count_genre <- table(data_reduced$genre)
count_genre

# create a list of main genres
main_genres <- names(count_genre[count_genre > 10])
main_genres

# put all genres that are not in the list in the class 'other'
data_reduced <- data_reduced %>%
  mutate(genre = ifelse(genre %in% main_genres, genre, "Other"))

prop.table(table(data_reduced$genre))

data_reduced %>%
  group_by(success, genre) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = genre, values_from = count)

# look closer at the 'country' column
count_country <- head(sort(table(data_reduced$country), decreasing = TRUE), 20)
count_country

# create a list of main countries
main_countries <- names(count_country[count_country > 25])
main_countries

# put all countries that are not in the list in the class 'other'
data_reduced$country <- ifelse(data_reduced$country %in% main_countries, data_reduced$country, "Other")
table(data_reduced$success, data_reduced$country)

# movie success might correlate with the movie creators' name recognition;
# creators' name recognition depends on how many movies they have produced before the released date of the current movie.

# create new dataframe before adding new columns for experience
data_clean <- data_reduced %>% 
  ungroup() %>% 
  mutate(row_number = row_number()) %>% 
  select(-c(success, row_number))  # remove 'success' and 'row_number' columns

# create empty columns
data_clean <- data_reduced
data_clean$director_experience <- NA
data_clean$writer_experience <- NA
data_clean$star_experience <- NA
data_clean$company_experience <- NA

# calculate experience and save calculated values in new columns


