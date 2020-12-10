setwd(choose.dir())
credit <- read.csv("German credit.csv")
head(credit)
dim(credit)
str(credit)
for(i in c("Default", "history", "purpose", "foreign"))
  credit[,i] <- as.factor(credit[,i])

library(caTools)
set.seed(7) 
split <- sample.split(Y = credit$Default, SplitRatio = 0.8)
train <- credit[split,]
test <- credit[!split,]

model <- glm(Default ~ ., binomial(link = "logit"), train)
result <- predict(model, test, type = "response")
result <- ifelse(result > 0.5, 1, 0)

mean(result == test$Default)

scaled <- scale(credit[, c("duration", "amount", "installment")])
class(scaled)
scaled <- as.data.frame(scaled)

set.seed(8)
k.max <- 10
wss <- sapply(1:k.max, function(k) { kmeans(credit, k)$tot.withinss } )
wss

plot(1:k.max, wss,
     type = "b", pch = 19, frame = T, 
     xlab = "Number of clusters k",
     ylab= "Total within-clusters sum of squares",
     main = 'Elbow Plot')

set.seed(13)

credit_cluster <- kmeans(scaled, 2) 

library(ggfortify)
autoplot(credit_cluster, scaled, frame = T)

credit <- cbind(credit, cluster = credit_cluster$cluster)

library(class)
pred <- knn.cv(scaled, credit$Default, 5, prob = T)
per <- attributes(.Last.value)
mean(per$prob)







