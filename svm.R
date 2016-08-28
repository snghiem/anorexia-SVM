library(MASS)
library(e1071)

?anorexia
# The anorexia data frame has 72 rows and 3 columns. 
# Weight change data for young female anorexia patients.

set.seed(1)
summary(anorexia)

train = sample(c(TRUE, FALSE), 50, rep=T) # for 50 obsevations
svm.fit=svm(Treat~., data=anorexia[train,], kernel="linear", cost=10, scale=FALSE)
summary(svm.fit)

# 31 support vectors for this 50 observations are too much.
# I need to choose cost wisely by cross-validation

set.seed(2)
cv.outcome = tune(svm, Treat~., data=anorexia[train,], kernel="polynomial",
                  ranges = list(cost=c(.001, .01, .1, 1, 10, 100,1000), degree=c(1,2,3,4)))
summary(cv.outcome)
best.svm = cv.outcome$best.model
# so here our best.model is cost=100 and degree=3
ypred = predict(best.svm, anorexia[!train,])
table(predict = ypred, truth=anorexia$Treat[!train])
# error rate
mean(ypred!=anorexia$Treat[!train])
# well, not good at all

# non-linear SVM
# let's try something like radial kernel
set.seed(3)
cv.outcome = tune(svm, Treat~., data=anorexia[train,], kernel="radial",
                  ranges = list(cost=c(.001, .01, .1, 1, 10, 100,1000)))
summary(cv.outcome)
# well the best cost is 100
ypred = predict(cv.outcome$best.model, anorexia[!train,])
table(predict=ypred, truth=anorexia$Treat[!train])
mean(ypred!=anorexia$Treat[!train])
# improve a bit but not much

contrasts(anorexia$Treat)
color = ifelse(anorexia$Treat=="CBT", "red", ifelse(anorexia$Treat=="Cont", "yellow", "purple"))
plot(anorexia[,-1], col=color, pch=19)
summary(svm.fit)
# Now I need to fit the whole dataset
svm.fit = svm(Treat~., data=anorexia, cost=10, kernel="radial", scale=FALSE)
plot(svm.fit, anorexia)
# well we can see that they are everywhere, not clearly separated

