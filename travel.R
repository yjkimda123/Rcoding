#데이터 불러오기
#dataset
travel = read.csv("travel.csv")




#전처리
#column
names(travel)
names(travel) = c("x","age","employment","graduate",
                  "income","family","disease",
                  "frequentflyer","evertravel","insurance")
travel = travel[,-c(1)]

travel$employment = as.factor(travel$employment)
levels(travel$employment)
levels(travel$employment) = c("goverment","private")

#separate the train-test data
install.packages("ROSE")
library(ROSE)

install.packages("caTools")
library(caTools)

travel$insurance = as.factor(travel$insurance)

#언더샘플링
undersampled_data = ovun.sample(insurance~., data=travel, method = "under" ,N=1420)$data
table(undersampled_data$insurance)

# train과 test 데이터로 분할 (7:3 비율)
set.seed(123)
split = sample.split(undersampled_data$insurance , SplitRatio = 0.7)
train_travel1 = undersampled_data[split, ]
test_travel = undersampled_data[!split, ]

# train 데이터의 y 변수를 5:5로 맞춤
set.seed(123)
train_indices = sample(1:nrow(train_travel1), size = nrow(test_travel))
train_travel = rbind(train_travel1[train_indices, ], train_travel1[-train_indices, ])

# 결과 확인
table(train_travel$insurance)
table(test_travel$insurance)

#save as csv
write.csv(train_travel, "train_travel.csv")
write.csv(test_travel, "test_travel.csv")

#structure
str(train_travel)
train_travel$age = as.numeric(train_travel$age)
train_travel$employment = as.factor(train_travel$employment)
train_travel$graduate = as.factor(train_travel$graduate)
train_travel$income = as.numeric(train_travel$income)
train_travel$family = as.numeric(train_travel$family)
train_travel$disease = as.factor(train_travel$disease)
train_travel$frequentflyer = as.factor(train_travel$frequentflyer)
train_travel$evertravel = as.factor(train_travel$evertravel)
train_travel$insurance = as.factor(train_travel$insurance)
str(train_travel)

str(test_travel)
test_travel$age = as.numeric(test_travel$age)
test_travel$employment = as.factor(test_travel$employment)
test_travel$graduate = as.factor(test_travel$graduate)
test_travel$income = as.numeric(test_travel$income)
test_travel$family = as.numeric(test_travel$family)
test_travel$disease = as.factor(test_travel$disease)
test_travel$frequentflyer = as.factor(test_travel$frequentflyer)
test_travel$evertravel = as.factor(test_travel$evertravel)
test_travel$insurance = as.factor(test_travel$insurance)
str(test_travel)




#데이터 분석
#tree 분석
install.packages("tree")
library(tree)

tree1 = tree(insurance~., data = train_travel, split = c("deviance"), na.action = na.pass,
             control = tree.control(nobs = nrow(train_travel), minsize = 10, mindev = 0.005))
plot(tree1)
text(tree1)
print(tree1)

cv.tree1 = cv.tree(tree1, FUN = prune.misclass, K = 10)
plot(cv.tree1)
tree1 = prune.misclass(tree1, best = 5)
plot(tree1)
text(tree1)
summary(tree1)

tree2 = predict(tree1, test_travel, type = "class")
summary(tree2)

tree3 = cbind(test_travel, tree2)
tree3 = as.data.frame(tree3)
print(tree3)
table(test_travel$insurance, tree2)
confusionMatrix(tree2, test_travel$insurance)

install.packages("caret")
library(caret)


#rpart 분석
install.packages("rpart")
library(rpart)

rpart = rpart(insurance~., data = train_travel,
              method = "class", control = rpart.control(minsplit = 10, maxdepth = 5))
plot(rpart)
text(rpart)
print(rpart)

printcp(rpart)
plotcp(rpart)

rpart_p = prune(rpart, cp = rpart$cptable[which.min(rpart$cptable[,"xerror"]),"CP"])
plot(rpart_p)
text(rpart_p)

rpart_pred <- predict(rpart, test_travel, type = "class")
summary(rpart_pred)

tree3 <- cbind(test_travel, rpart_pred)
tree3 <- as.data.frame(tree3)
print(tree3)

table(test_travel$insurance, rpart_pred)
confusionMatrix(tree2, test_travel$insurance)

install.packages("rattle")
install.packages("rpart.plot")
library(rattle)
library(rpart.plot)
library(RColorBrewer)
rpart.plot(rpart)
fancyRpartPlot(rpart)


#C4.5 분석
install.packages("RWeka",dependencies = TRUE)
library(RWeka)
#install.packages("caret")
#library(caret)
cf = createFolds(train_travel$insurance, k = 10)

c45fit = train(insurance~., data = train_travel, trControl = trainControl(method = "cv", indexOut = cf))
c45fit
plot(c45fit)
c45fit$finalModel
c45_pred = predict(object = c45fit, newdata = test_travel, type = "raw")
table(test_travel$insurance, c45_pred)
confusionMatrix(c45_pred,test_travel$insurance)


#C5.0 분석
install.packages("C50")
library(C50)
install.packages("printr")
library(printr)
c50 = C5.0(insurance ~., data = train_travel)
plot(c50)
summary(c50)
c50_pred = predict(object = c50, newdata = test_travel, type = "class")
table(test_travel$insurance, c50_pred)
confusionMatrix(c50_pred, test_travel$insurance)


#Boosting C5.0 분석
c50_boost = C5.0(insurance ~., data = train_travel, trials = 10)
plot(c50_boost)
summary(c50_boost)
results_boost = predict(object = c50_boost, newdata = test_travel, type = "class")
table(test_travel$insurance, results_boost)
confusionMatrix(results_boost, test_travel$insurance)


#Bagging
install.packages("ipred")
library(ipred)
library(rpart)
bagg_train = bagging(insurance ~., data = train_travel, nbag = 1000,
                     control = rpart.control(minsplit = 10), coob = T)
bagg_train
library(caret)
bagg_predict = predict(bagg_train, test_travel, type = "class")
confusionMatrix(bagg_predict, test_travel$insurance)


#Random Forest
install.packages("randomForest")
require(randomForest)
rf_train = randomForest(insurance ~., data = train_travel, importance = TRUE,
                        ntree = 1000, , mtry = 2)
rf_train

rf_pred = predict(rf_train, test_travel, type = "class")
summary(rf_pred)
table(test_travel$insurance, rf_pred)
confusionMatrix(rf_pred, test_travel$insurance)

importance(rf_train)
importance(rf_train,type=1)

varImpPlot(rf_train,type=1)
varImpPlot(rf_train,type=2)


#Boosting_gbm 분석
install.packages("gbm")
library(gbm)
boost_train = gbm(insurance ~., data = train_travel,
                  distribution = "multinomial", n.trees = 1000,
                  shrinkage = 0.01, interaction.depth = 4)
boost_train
boost_pred = predict.gbm(object = boost_train, newdata = test_travel,
                         n.trees = 1000, type = "response")
value = apply(boost_pred, 1, which.max)
value
boost_result = data.frame(test_travel$insurance, value)
with(boost_result, table(test_travel$insurance, value))
confusionMatrix(boost_pred, test_travel$insurance)


#logistic regression 분석
install.packages("MASS")
library(MASS)
install.packages("nnet")
library(nnet)

nra = multinom(insurance ~., data = train_travel, maxit = 1000)
pxnra = predict(nra, type = "class")
round(coef(nra), 2)
table(pxnra, train_travel$insurance)
pxnratest = predict(nra, newdata = test_travel, type = "class")
summary(pxnratest)
table(pxnratest, test_travel$insurance)
confusionMatrix(pxnratest, test_travel$insurance)



#numeric인 애들은 수치를 단순화함 / 나머지 이산형은 ifelse 사용
str(train_travel)
#View(train_travel)
train_travel$age = 2*(train_travel$age - mean(train_travel$age))/
  (max(train_travel$age)-min(train_travel$age))
train_travel$income = 2*(train_travel$income - mean(train_travel$income))/
  (max(train_travel$income) - min(train_travel$income))
train_travel$family = 2*(train_travel$family - mean(train_travel$family))/
  (max(train_travel$family) - min(train_travel$family))
train_travel[,2] = ifelse(train_travel[,2] == "goverment", -1, 1);
train_travel[,3] = ifelse(train_travel[,3] == "0", -1, 1)
train_travel[,6] = ifelse(train_travel[,6] == "0", -1, 1)
train_travel[,7] = ifelse(train_travel[,7] == "0", -1, 1)
train_travel[,8] = ifelse(train_travel[,8] == "0", -1, 1)
train_travel[,9] = ifelse(train_travel[,9] == "0", -1, 1)
str(train_travel)
#View(train_travel)

str(test_travel)
test_travel$age = 2*(test_travel$age - mean(test_travel$age))/
  (max(test_travel$age)-min(test_travel$age))
test_travel$income = 2*(test_travel$income - mean(test_travel$income))/
  (max(test_travel$income) - min(test_travel$income))
test_travel$family = 2*(test_travel$family - mean(test_travel$family))/
  (max(test_travel$family) - min(test_travel$family))
test_travel[,2] = ifelse(test_travel[,2] == "goverment", -1, 1);
test_travel[,3] = ifelse(test_travel[,3] == "0", -1, 1)
test_travel[,6] = ifelse(test_travel[,6] == "0", -1, 1)
test_travel[,7] = ifelse(test_travel[,7] == "0", -1, 1)
test_travel[,8] = ifelse(test_travel[,8] == "0", -1, 1)
test_travel[,9] = ifelse(test_travel[,9] == "0", -1, 1)
str(test_travel)
#View(test_travel)


#neuralnet 분석
install.packages("neuralnet")
library(neuralnet)

nn = neuralnet(insurance~., data = train_travel,
               algorithm = "rprop+", act.fct = 'logistic',
               linear.output = TRUE, hidden = 3)
plot(nn)

nn_pred = compute(nn, test_travel[,-9])
print(nn_pred)
plot(test_travel$insurance ~ nn_pred$net.result)
table(test_travel$insurance, ifelse(nn_pred$net.result >=0, 1, -1))
confusionMatrix(nn_pred, test_travel$insurance)




#모형별 결과 비교
#ROC curve
install.packages("caTools")
library(caTools)
colAUC(cbind(tree2, rpart_pred, c45_pred, c50_pred, results_boost, bagg_predict, rf_pred,
             pxnratest), test_travel$insurance, plotROC = T)

#auc 값 구하기
install.packages("ROCR")
library(ROCR)
install.packages("pROC")
library(pROC)

performance()