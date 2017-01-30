#To read the file uploaded at Google Drive
install.packages('gsheet')
library('gsheet')
url <- 'https://docs.google.com/spreadsheets/d/1XDBRCYFTxsw27AivxJ5pWxDHN0WA6GqSP46PVe2BCQ4/edit?usp=sharing'
dataset <- gsheet2tbl(url)
#Mean Rating: Calculate the mean rating for each movie, order with the highest rating listed first, and submit the top three (along with the mean scores for the top two).
dataset_mean_1 <- data.frame(colMeans(dataset, na.rm = TRUE))
#Rename the mean col
install.packages('plyr')
library('plyr')
renamed_mean_1 <- rename(dataset_mean_1,c('colMeans.dataset..na.rm...TRUE.'='Mean'))
#Build a table listing the top three movies
ordered_mean_1 <- head(renamed_mean_1[order(-renamed_mean_1$Mean),,drop=FALSE],n=4)

dataset_count <- data.frame(colSums(!is.na(dataset)))
renamed_ordered_count <- rename(dataset_count, c('colSums..is.na.dataset..'='Mean'))
ordered_count <- head(renamed_ordered_count[order(-renamed_ordered_count$Mean),,drop=FALSE],n=5)

install.packages('matrixStats')
library('matrixStats')
dataset_positive <- data.frame(colCounts(dataset[1:21,]>=4, value=TRUE, na.rm=TRUE, drop = FALSE) / colSums(!is.na(dataset)))
library('plyr')
renamed_positive <- rename(dataset_positive, c('colCounts.dataset.1.21.......4..value...TRUE..na.rm...TRUE..drop...FALSE..colSums..is.na.dataset..'='Mean'))
ordered_count <- head(renamed_positive[order(-renamed_positive$Mean),,drop=FALSE],n=4)

subset_toy_story <- dataset[!is.na(dataset$X1..Toy.Story..1995.),]
dataset_toy_story <- data.frame(colCounts(!is.na(subset_toy_story))) / NROW(subset(subset_toy_story, select=c("X1..Toy.Story..1995.")))
ordered_dataset_toy_story <- head(dataset_toy_story[order(-dataset_toy_story$'colCounts..is.na.subset_toy_story..'),,drop = FALSE], n=8)

corr_toy_story <- data.frame(cor(dataset, dataset$X1..Toy.Story..1995., use="pairwise.complete.obs"))
ordered_corr_toy_story <- head(corr_toy_story[order(-corr_toy_story$'cor.dataset..dataset.X1..Toy.Story..1995...use....pairwise.complete.obs..'),,drop=FALSE], n=4)


pref_by_fem <- dataset[dataset$'Gender..1..F..0.M.'==1,]
dataset_mean_fem <- colMeans(pref_by_fem, na.rm = TRUE)
global_mean_fem <- mean(dataset_mean_fem[-(1:2)])
pref_by_mal <- dataset[dataset$'Gender..1..F..0.M.'==0,]
dataset_mean_mal <- colMeans(pref_by_mal, na.rm = TRUE)
global_mean_mal <- mean(dataset_mean_mal[-(1:2)])
dif_btw_mal_fem <- max((dataset_mean_mal - dataset_mean_fem)[(dataset_mean_mal - dataset_mean_fem)!=max(dataset_mean_mal - dataset_mean_fem)])
dif_btw_fem_mal <- max((dataset_mean_fem  - dataset_mean_mal)[(dataset_mean_fem  - dataset_mean_mal)!=max(dataset_mean_fem  - dataset_mean_mal)])
global_dif_btw_fem_mal <- global_mean_fem - global_mean_mal

library('matrixStats')
positive_fem <- data.frame(colCounts(pref_by_fem >= 4, value = TRUE, na.rm = TRUE, drop = FALSE) / colSums(!is.na(pref_by_fem)))
library('plyr')
positive_mal <- data.frame(colCounts(pref_by_mal >= 4, value = TRUE, na.rm = TRUE, drop = FALSE) / colSums(!is.na(pref_by_mal)))
posit_dif_btw_mal_fem <- positive_mal - positive_fem
posit_dif_btw_fem_mal <- positive_fem - positive_mal
