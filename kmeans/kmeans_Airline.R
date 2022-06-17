library(datasets)
library(MASS)
library(tidyverse)
airline_survey <- read.csv("airline_survey.csv", encoding = "UTF-8")
head(airline_survey)
airline_survey$satis <- as.factor(ifelse(airline_survey$satisfaction == "satisfied", 1, 0))
airline_survey <- airline_survey[,-c(1, 2, 25)]
n <- nrow(airline_survey)
trainI <- sample(1:n,  size = round(0.8*n))
traind <- airline_survey[trainI,]
testd <- airline_survey[-trainI,]

install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

#Decision tree 
dtree <- rpart(formula = satis~., data = traind, method="class")
pred <- predict(dtree, newdata=testd, type="class")
table(Real = testd$satis, Predict = pred)
rpart.plot(dtree)

install.packages("randomForest")
library(randomForest)

#Random Forest
rf <- randomForest(satis ~., data = traind, importance=TRUE, ntree=100, na.action=na.exclude) 
rf
importance(rf)
varImpPlot(rf)
pred=predict(rf, newdata = testd)
table(Real = testd$satis, Predict = pred)



#Q2
airline <- airline_survey[, c(2, 4, 5, 7, 9, 11, 13, 14, 20)]
trainI <- sample(1:n,  size = round(0.01*n))
traind <- airline[trainI,]
#traind$Gender <- ifelse(traind$Gender == "Male", 1, 0)
#traind$Customer.Type <- ifelse(traind$Customer.Type == "Loyal Customer", 1, 0)
#traind$Type.of.Travel <- ifelse(traind$Type.of.Travel == "Business travel", 1, 0)
#traind$Class <- ifelse(traind$Class == "Business", 2, 
  #                     ifelse(traind$Class == "Eco Plus", 1, 0))
k = kmeans(traind[4:9] , centers=3, nstart=25) # k=3, generate 25 initial configurations
str(k)
####
#cluster: a vector of integers (from 1:k) indicating the cluster to which each point is allocated.
#centers: a matrix of cluster centers.
#withinss: vector of within-cluster sum of squares, one component per cluster.
#tot.withinss: total within-cluster sum of squares. That is, sum(withinss).
#size: the number of points in each cluster.
#####
#k$centers
#k$withinss #within class variance
#k$tot.withinss
#table(k$cluster)  
#k$size # cluster size
#k
# plot the results
install.packages("useful")
library(useful)
#library(tidyverse)
#ggplot(traind, aes(x=Type.of.Travel, y=Class)) +
 # geom_point()
plot(k,data=traind[,1:9])


# Elbow Method for K-Means (find the best k by SSE)
fviz_nbclust(traind[4:9], 
             FUNcluster = kmeans,# K-Means
             method = "wss",     # total within sum of square
             k.max = 20          # max number of clusters to consider
) +
  labs(title="Elbow Method for K-Means")+
  geom_vline(xintercept = 3, linetype = 2)


cc = k$cluster
data = cbind(traind,cc)
#ggplot(data, aes(x= Inflight.wifi.service , y=Ease.of.Online.booking,color=as.factor(cc))) +
#  geom_point()
ggplot(data, aes(x=as.factor(cc), y=Inflight.wifi.service)) + 
  geom_boxplot()
ggplot(data, aes(x=as.factor(cc), y=Ease.of.Online.booking)) + 
  geom_boxplot()
ggplot(data, aes(x=as.factor(cc), y=Food.and.drink)) + 
  geom_boxplot()
ggplot(data, aes(x=as.factor(cc), y=Seat.comfort)) + 
  geom_boxplot()
ggplot(data, aes(x=as.factor(cc), y=Inflight.entertainment)) + 
  geom_boxplot()
ggplot(data, aes(x=as.factor(cc), y=Cleanliness)) + 
  geom_boxplot()
library(plyr)

#count(traind, "Class")

tbl <- with(data, table(Class, as.factor(cc)))
barplot(tbl, beside = TRUE, legend = TRUE)

tbl <- with(data, table(Type.of.Travel, as.factor(cc)))
barplot(tbl, beside = TRUE, legend = TRUE)
#ggplot(data = data) +
#  geom_bar( aes( x = Gender)) + 
#  facet_wrap( ~ cc)
# another method is method="silhouette" (Average silhouette Method)
# +geom_vline(xintercept = 3, linetype = 2) # draw a vertical line at k=3

#fviz_nbclust(traind, kmeans, method = "silhouette")

# Compute gap statistic for kmeans
# Recommended value for B is ~500
#library(cluster)
#gap_stat <- clusGap(traind, FUN = kmeans, nstart = 25,
#                    K.max = 10, B = 500)
#fviz_gap_stat(gap_stat)
