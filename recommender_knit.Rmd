---
title: "Recommender system Assignment"
output: html_document
---

It is my second attempt to finish the specialization 
on Recommender systems from [University of Minnesota](https://www.coursera.org/specializations/recommender-systems) but unlike the first time, I am attempting to complete all the assignments not only in Java but also in R - for the practice sake.

### Course #1 - Introduction to Recommender Systems: Non-Personalized and Content-Based

The first assignment in the course ["Introduction to Recommender Systems: Non-Personalized and Content-Based"](https://www.coursera.org/learn/recommender-systems-introduction) will "explore non-personalized and lightly-personalized recommendations" and includes sever deliverables with each deliverable representing a different analysis of the data provided."The first three deliverables represent non-personalized summary statistics; the next two represent product association using two different mechanisms (we aren't using lift here because of how similar the popularity levels are). The last two represent a demographic analysis to explore whether using gender would be wise given this dataset."
```{r }
## Read the dataset uploaded at Google Drive
install.packages('gsheet', repos="http://cran.rstudio.com/")
library('gsheet')
url <- 'https://docs.google.com/spreadsheets/d/1XDBRCYFTxsw27AivxJ5pWxDHN0WA6GqSP46PVe2BCQ4/edit?usp=sharing'
dataset <- gsheet2tbl(url)
```

**Deliverable #1** i.e. calulcation of mean rating.  
**Task**: calculate the mean rating for each movie, order with the highest rating listed first, and submit the top three (along with the mean scores for the top two).
```{r}

dataset_mean_1 <- data.frame(colMeans(dataset, na.rm = TRUE))
install.packages('plyr', repos="https://cran.r-project.org")
library('plyr')
colnames(dataset_mean_1) <- "Score"
ordered_mean_1 <- head(dataset_mean_1[order(-dataset_mean_1$Score),, drop = FALSE], n = 4)
```
**Result**: The ranking of top 3 movies based on average.

| Movie                                          | Score |
|------------------------------------------------|-------|
| 318: Shawshank Redemption, The (1994)          | 3.6   |
| 260: Star Wars: Episode IV - A New Hope (1977) | 3.27  |
| 541: Blade Runner (1982)                       | 3.22  |


**Deliverable #2** i.e. rating count (popularity).  
**Task**: Count the number of ratings for each movie, order with the most number of ratings first, and submit the top three (along with the counts for the top two).
```{r}
dataset_count <- data.frame(colSums(!is.na(dataset)))
colnames(dataset_count) <- "Count"
ordered_count <- head(dataset_count[order(-dataset_count$Count), , drop = FALSE], n = 5)
```
**Result**: The ranking of top 3 movies based on count.

| Movie                                          | Count |
|------------------------------------------------|-------|
| 1: Toy Story (1995)                            | 17    |
| 593: Silence of the Lambs, The (1991)          | 16    |
| 260: Star Wars: Episode IV - A New Hope (1977) | 15    |

**Deliverable #3** i.e. % of ratings 4+ (liking).  
**Task**: Calculate the percentage of ratings for each movie that are 4 or higher. Order with the highest percentage first, and submit the top three (along with the percentage for the top two). Notice that the three different measures of "best" reflect different priorities and give different results; this should help you see why you need to be thoughtful about what metrics you use.
```{r}
install.packages('matrixStats', repos="https://cran.r-project.org")
library('matrixStats')
dataset_positive <- data.frame(colCounts(dataset[1:21,] >= 4, value = TRUE, na.rm = TRUE, drop = FALSE) / colSums(!is.na(dataset)))
colnames(dataset_positive) <- "Perc"
ordered_count <- head(dataset_positive[order(-dataset_positive$Perc), , drop = FALSE], n = 4)
```
**Result**: The ranking of top 3 movies based on "liking".

| Movie                                          | %    |
|------------------------------------------------|------|
| 318: Shawshank,Redemption, The (1994)          | 0.7  |
| 260: Star Wars:,Episode IV - A New Hope (1977) | 0.53 |
| 3578: Gladiator,(2000)                         | 0.5  |

**Deliverable #4** i.e. top movies for someone who has seen Toy Story.  
**Task**: Calculate movies that most often occur with Movie #1: Toy Story, using the (x+y)/x method described in class. In other words, for each movie, calculate the percentage of Toy Story raters who also rated that movie. Order with the highest percentage first, and submit the top 3 (along with the correlations for the top two). Note, you will have ties -- to break the ties, use the lowest-numbered movie as the higher-ranked one. In other words, if Movies 541 and 318 are tied, then 318 gets the higher rank.
```{r}
subset_toy_story <- dataset[!is.na(dataset$X1..Toy.Story..1995.),]
dataset_toy_story <- data.frame(colCounts(!is.na(subset_toy_story))) / NROW(subset(subset_toy_story, select=c("X1..Toy.Story..1995.")))
colnames(dataset_toy_story) <- "Corr"
ordered_dataset_toy_story <- head(dataset_toy_story[order(-dataset_toy_story$Corr),, drop = FALSE], n = 6)
```
**Result**: The ranking of top 3 movies for someone who has seen Toy Story.

| Movie                                          | Corr |
|------------------------------------------------|------|
| 260: Star Wars: Episode IV - A New Hope (1977) | 0.82 |
| 593: Silence of the Lambs, The (1991)          | 0.76 |
| 2916: Total Recall (1990)                      | 0.65 |

**Deliverable #5** i.e. correlation with Toy Story.  
**Task**: Calculate the correlation between the vectors of ratings for Toy Story and each other movie. You can use the built-in CORREL() function. Order by the highest correlation (positive only) and submit the top 3 along with the correlation values for the top 2. Notice the differences between co-occurrence and correlation; these metrics are showing different types of relationships.
```{r}
corr_toy_story <- data.frame(cor(dataset, dataset$X1..Toy.Story..1995., use="pairwise.complete.obs"))
colnames(corr_toy_story) <- "Corr"
ordered_corr_toy_story <- head(corr_toy_story[order(-corr_toy_story$Corr),, drop = FALSE], n = 4)
```
**Result**: The ranking of top 3 movies based on correlation with Toy Story.

| Movie                                 | Corr  |
|---------------------------------------|-------|
| 318: Shawshank Redemption, The (1994) | 0.89  |
| 34: Babe (1995)                       | 0.811 |
| 296: Pulp Fiction (1994)              | 0.71  |

**Deliverable #6** i.e. mean rating difference by gender.  
**Task**: First, recompute the mean rating for each movie separately for males and for females. And calculate the overall mean rating (across all ratings) for males and females. Submit the two movies that have the greatest differences (one where men are most above women, and one where women are most above men) along with the differences in average. Also submit the difference in overall rating averages (female average - male average).
```{r}
pref_by_fem <- dataset[dataset$'Gender..1..F..0.M.'==1,]
dataset_mean_fem <- colMeans(pref_by_fem, na.rm = TRUE)
global_mean_fem <- mean(dataset_mean_fem[-(1:2)])
pref_by_mal <- dataset[dataset$'Gender..1..F..0.M.'==0,]
dataset_mean_mal <- colMeans(pref_by_mal, na.rm = TRUE)
global_mean_mal <- mean(dataset_mean_mal[-(1:2)])
dif_btw_mal_fem <- max((dataset_mean_mal - dataset_mean_fem)[(dataset_mean_mal - dataset_mean_fem)!=max(dataset_mean_mal - dataset_mean_fem)])
dif_btw_fem_mal <- max((dataset_mean_fem  - dataset_mean_mal)[(dataset_mean_fem  - dataset_mean_mal)!=max(dataset_mean_fem  - dataset_mean_mal)])
global_dif_btw_fem_mal <- global_mean_fem - global_mean_mal
```
**Result**: Two movies that have the greatest differences along with the differences in average:
- where men are most above wome: 1198: Raiders of the Lost Ark (1981) - 1.67
- where women are most above men: 34: Babe (1995) - 1.43
The difference in overall rating averages (female average - male average) is equal to 0.12

**Deliverable #7** i.e. the % of ratings 4+ separately for males and females.  
**Task**: You'll be asked to submit two movies as above (largest difference in each direction). And again you'll indicate whether men or women are more likely to rate movies 4 stars or above.
```{r}
positive_fem <- data.frame(colCounts(pref_by_fem >= 4, value = TRUE, na.rm = TRUE, drop = FALSE) / colSums(!is.na(pref_by_fem)))
positive_mal <- data.frame(colCounts(pref_by_mal >= 4, value = TRUE, na.rm = TRUE, drop = FALSE) / colSums(!is.na(pref_by_mal)))
posit_dif_btw_mal_fem <- positive_mal - positive_fem
posit_dif_btw_fem_mal <- positive_fem - positive_mal
```
**Result**: Two movies that have the greatest differences along with the differences in average:
- where men are most above wome: 1198: Raiders of the Lost Ark (1981) - 0.5
- where women are most above men: 2396: Shakespeare in Love (1998) - 0.75
Women tend to rate movies 4 stars or above i.e. the difference is equal to 0.1036436 `r mean(colCounts(pref_by_fem >= 4, value = TRUE, na.rm = TRUE, drop = FALSE) / colSums(!is.na(pref_by_fem))) -mean(
colCounts(pref_by_mal >= 4, value = TRUE, na.rm = TRUE, drop = FALSE) / colSums(!is.na(pref_by_mal)))`
