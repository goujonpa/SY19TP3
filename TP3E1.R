# Paul GOUJON
# UTC - SY19 - TP3

# Ex 1 : CLASSIFICATION : Spam data analysis

rm(list=ls())
library(MASS)
library(caTools)
library(pROC)

# >>> First read spambase.names
# >>> Load data
X = read.table("./data/spambase.dat", header=F)
colnames(X) = as.vector(as.matrix(read.table("./data/spambase_name.names", header=F)))

# >>> Splitting into train / text samples
splitting = sample.split(X[,58], SplitRatio=2/3)
# length(splitting[which(splitting)]) / length(splitting) 
# (if u want to check this is 2/3)
Xtrain = X[splitting, ]
Xtst = X[!splitting, ]

# >>> Linear Discriminant Analysis
lda.fit = lda(spam ~., data=Xtrain)
sink("./plots/lda_fit_output1.txt")
lda.fit
sink()

# >>> Logistic regression
glm.fit = glm(spam ~., data=Xtrain)
sink("./plots/logreg_fit_output1.txt")
glm.fit
sink()

# >>> Confusion matrix

confusion <- function(data, model, modelType) {
    if (modelType == "glm") {
        prediction <- ifelse(predict(model, data, type='response') > 0.5, TRUE, FALSE)
    } else if (modelType == "lda") {
        prediction <- ifelse(as.vector(predict(model, data, type="response")$class) == "1", TRUE, FALSE)
    }
    confusion  <- table(prediction, as.logical(model$y))
    confusion  <- cbind(confusion, c(1 - confusion[1,1]/(confusion[1,1]+confusion[2,1]), 1 - confusion[2,2]/(confusion[2,2]+confusion[1,2])))
    confusion  <- as.data.frame(confusion)
    names(confusion) <- c('FALSE', 'TRUE', 'class.error')
    confusion
}

glm.conf = confusion(Xtrain, glm.fit, "glm")

lda.fit$y <- Xtrain$spam
lda.conf = confusion(Xtrain, lda.fit, "lda")

# >>> COR curves

lda.roc = roc(Xtrain$spam, as.vector(predict(lda.fit, Xtrain)$x))
plot(lda.roc)

glm.roc = roc(Xtrain$spam, predict(glm.fit, Xtrain, type="link"))
plot(glm.roc, add=T, col="Red")


