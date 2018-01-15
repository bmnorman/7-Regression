library(caret)
library(kernlab)
library(Hmisc)

# Basic caret tutorial
data(spam)
set.seed(32343)
inTrain <- createDataPartition(y=spam$type, p=0.8, list=FALSE)  # best to do 0.6
training <- spam[inTrain, ]
testing <- spam[-inTrain, ]
#dim(training)
modelFit <- train(type ~., data=training, method="glm")
#modelFit$finalModel # use this to see the values for the specific model
predictions <- predict(modelFit, newdata=testing)
confusionMatrix(predictions, testing$type)

# data slicing
folds <- createFolds(y=spam$type, k=10, list=TRUE, returnTrain=TRUE)
#folds <- createFolds(y=spam$type, k=10, list=TRUE, returnTrain=FALSE) # Use this to k-fold the test dataset
#sapply(folds, length) # check the length of each fold

# training options
# I'm should be able to generat this using args(train.default), but that does not appear to be working
function (x, y, method = "rf", preProcess = NULL, ..., weights = NULL,
          metric = ifelse(is.factor(y), "Accuracy", "RMSE"), maximize = ifelse(
            metric == "RMSE", FALSE, TRUE), trControl = trainControl(), tuneGrid = NULL,
          tuneLength = 3) # RSquared could be used instead of RMSE for linear models
NULL

args(trainControl)

# plotting predictors
featurePlot(x = training[, c("age","eduction","jobclass")],
            y = training$wage,
            plot="pairs")
qplot(age, wage, data=training)
qplot(age, wage, colour=jobclass, data=training)

qplot(age, wage, colour=eduction, data=training)
qq + geom_smooth(method="lm", formula=y~x)

cutWage <- cut2(training$wage, g=3)
p1 <- qplot(cutWage, age, data=training, fill=cutWage, geom=c("boxplot"))
p2 <- qplot(cutWage, age, data=training, fill=cutWage, geom=c("boxplot","jitter"))
grid.arrange(p1,p2, ncol=2)

t1 <- table(cutWage, training$jobclass)
prop.table(t1,1)

qplot(wage, colour=eduction, data=training, geom="density")

# basic preprocessing
# to see distributions of variables:
hist(training$capitalAve, main="", xlab="ave. capital run length")
# to standardize:
traincapAve <- training$capitalAve
trainCapAveS <- (trainCapAve - mean(trainCapAve))/(sd(trainCapAve))
# note - the above must also be done to the test data set - use the mean and sd of the training set average
# the 2 lines below perform the same process
preObj <- preProcess(training[,-58], method=c("center","scale"))
trainCapAveS <- predict(preObj, training[.-58])$capitalAve
testCapAveS <- predict(preObj, testing[,-58])$capitalAve
# preprocess can also be passed as an argument to train directly
# if some data is missing, use the preprocess function with a method of "knnImpute"
preObj <- preProcess(training[,58], method="knnImpute")
capAve <- predict(preObj, training[,58])$capAve

# covariate creation
# to remove zero, or near zero covariates:
nsv <- nearZeroVar(training, saveMetrics=TRUE)
# to convert a covariate to an n-degree polynomial function
bsBasis <- bs(training$age, df=3) # creates a cubic polynomial function in age
predict(bsBasis, age=testing$age)

# Preprocessing with PCA
# to check for correlations:
M <- abs(cor(training[, 58]))
diag(M)
which(M > 0.8, arr.ind=T)
# isolate correlated dataset and perform PCA
smallSpam <- spam[ ,c(32,34)]
prComp <- prcomp(smallSpam)
plot(prComp$x[,1], prComp$x[,2])
prComp$rotation # to see what rotation has occured
# to do PCA on the entire dataset:
prComp <- prcomp(log10(spam[, -58]+1)) # the log is to make the data look more normal
# in caret:
preProc <- preProcess(log10(spam[,-58]+1), method="pca", pcaComp=2)
spamPC <- predict(preProc, log10(spam[,-58]+1))
# to do this on the training dataset (i.e. using the principal component as the data):
trainPC <- predict(preProc, log10(training[,-58]+1))
modelFit <- train(training$type ~ ., method="glm", data=trainPC)
# this can be done in one go:
modelFit <- train(training$type ~ ., method="glm", preProcess="pca", data=training)
confusionMatrix(testing$type, predict(modelFit, testing))


# predicting with regression
lines(trainFaith$waiting, lm1$fitted, lwd=3)
newData <- data.frame(waiting=80)
predict(lm1, newData) # should be able to just apply test dataset here - newData=testData
# to get the errors:
sqrt(mean((lm1$fitted-trainFaith$eruptions)^2)) # RMSE
sqrt(mean((predict(lm1, newdata=testFaith) - testFaith$eruptions)^2)) # good estimate of the valid RMSE
 # prediction intervals
pred1 <- predict(lm1, newdata=testFaith, interval="prediction")
ord <- order(testFaith$waiting)
plot(testFaith$waiting, testFaith$eruptions, pch=19, col="blue")
matlines(testFaith$waiting[ord],pred1[ord], type="l", col=c(1,2,2), lty=c(1,1,1), lwd=3)
# in caret:
modFit <- train(eruptions ~ waiting, data=trainFaith, method="lm")
summary(modelFit$finalModel)

# predicting with regression - multiple covariates
# to plot predicted vs actual values:
pred <- predict(modFit, testing)
qplot(wage, pred, colour=year, data=testing)

