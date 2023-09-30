hr.data <- read.csv("~/Downloads/hr-data-v2.csv")
set.seed(123)
random.vec <- runif(nrow(hr.data))
training.vec <- ifelse(random.vec < 0.7, 1, 0)
train.data <- hr.data[training.vec==1,]
test.data <- hr.data[training.vec==0,]
logit.model <- glm(attrition ~ ., data = train.data, family = "binomial")
summary(logit.model)
library(stargazer)
stargazer(logit.model, type="text")
test.probs <- predict(logit.model, newdata = test.data, type = "response")
test.pred <- ifelse(test.probs > 0.5, 1, 0)
library(caret)
conf.mat <- confusionMatrix(factor(test.pred, levels = c(0,1)), factor(test.data$attrition, levels = c(0,1)))
accuracy <- conf.mat$overall[1]
sensitivity <- conf.mat$byClass[1]
precision <- conf.mat$byClass[5]
accuracy 
sensitivity
precision
library(ranger)
rf.model <- ranger(attrition ~ ., data = train.data, importance = "permutation", probability = TRUE)
summary(rf.model)
test.probs <- predict(rf.model, data = test.data)$predictions[,2]
test.pred <- ifelse(test.probs > 0.5, 1, 0)
conf.mat <- confusionMatrix(factor(test.pred, levels = c(0,1)), factor(test.data$attrition, levels = c(0,1)))
accuracy <- conf.mat$overall[1]
sensitivity <- conf.mat$byClass[1]
precision <- conf.mat$byClass[5]
accuracy
sensitivity
precision
write.csv(test.data, file = 'hr-predictions.csv', row.names = FALSE)
library(cluster)
library(dplyr)
cat_vars <- select(hr.data, business.travel, department, education.field, gender, job.role)
cat_matrix <- model.matrix(~.-1, data = cat_vars)
hr.data_new <- cbind(hr.data[, !(names(hr.data) %in% c('business.travel', 'department', 'education.field', 'gender', 'job.role'))], cat_matrix)
kmeans.data <- scale(hr.data_new)
wss <- c()
for (i in 1:10) {
  kmeans.model <- kmeans(kmeans.data, centers = i, nstart = 10)
  wss[i] <- kmeans.model$tot.withinss
}
plot(1:10, wss, type = "b", xlab = "Number of clusters (k)", ylab = "Within-cluster sum of squares")
kmeans.model <- kmeans(kmeans.data, centers = 4, nstart = 10)
kmeans.labels <- kmeans.model$cluster
hr.data$cluster <- kmeans.labels
table(kmeans.labels)
library(dplyr)
kmeans.data_df <- as.data.frame(kmeans.data)
kmeans.data_df$cluster <- kmeans.labels
cluster_means <- kmeans.data_df %>%
  group_by(cluster) %>%
  summarize_all(mean)