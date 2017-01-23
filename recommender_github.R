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