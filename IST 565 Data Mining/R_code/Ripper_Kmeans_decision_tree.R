library(stats)
library(factoextra)
library(tidyverse)
library(caret)
library(rpart.plot)
library(arules)
library(rpart)



ripper <- read.csv("ripper_data_set.csv")

###K Means Clustering
ripper_no_labels <- ripper[,-1:-2]

ripper_kmeans <- kmeans(ripper_no_labels, centers = 10)
ripper_kmeans

fviz_cluster(ripper_kmeans, data = ripper_no_labels)

cluster_df <- data.frame(ripper, ripper_kmeans$cluster)
# ggplot(cluster_df) 
cluster_df_tall <- cluster_df %>% 
  gather(key = word, value = value, 3:525)
ggplot(data = cluster_df_tall) + 
  geom_jitter(aes(x = value, y = ripper_kmeans.cluster, color = author))

####Decision tree
ripper[,-2]

removeFactors <- function(x) { if(is.factor(x)) factor(x) else x }

# split dataframe into training and test set
training <- subset(ripper, author != 'Jack the Ripper')
training[] <- lapply(training, removeFactors)
testing <- subset(ripper, author == 'Jack the Ripper')
testing[] <- lapply(testing, removeFactors)



train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 5)
set.seed(333)
dtree_fit <- train(author ~., data = training, method = "rpart",
                   parms = list(split = "information"),
                   trControl=train_control,
                   tuneLength = 20)

dtree_fit

prp(dtree_fit$finalModel, box.palette = "Reds", tweak = 3)
