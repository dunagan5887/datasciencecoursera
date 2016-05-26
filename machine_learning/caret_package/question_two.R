library(AppliedPredictiveModeling)
data(concrete)
library(caret)
library(Hmisc)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

modfit <- train(CompressiveStrength ~ FlyAsh, method='lm', data=concrete)
finMod <- modfit$finalModel
plot(finMod$residuals,pch=19)

featurePlot(x=training[,c("Superplasticizer","CoarseAggregate")],
            y = training$CompressiveStrength,
            plot="pairs")


featurePlot(x=training[,c("FineAggregate","Age")],
            y = training$CompressiveStrength,
            plot="pairs")


featurePlot(x=training[,c("Cement","BlastFurnaceSlag")],
            y = training$CompressiveStrength,
            plot="pairs")

featurePlot(x=training[,c("FlyAsh","Water")],
            y = training$CompressiveStrength,
            plot="pairs")

featurePlot(x=training[,c("FlyAsh","Age")],
            y = training$CompressiveStrength,
            plot="pairs")


featurePlot(x=seq,
            y = training$CompressiveStrength,
            plot="pairs")

qplot(CompressiveStrength, Age, colour=FlyAsh, data=training)

qplot(CompressiveStrength, FlyAsh, colour=Age, data=training)


training_rows = nrow(training)
seq = 1:training_rows

cutCompressiveStrength <- Hmisc::cut2(training$CompressiveStrength,g=6)
table(cutCompressiveStrength)

cutAge <- Hmisc::cut2(training$Age,g=7)
table(cutAge)

p1 <- qplot(cutCompressiveStrength,cutAge, data=training,fill=cutCompressiveStrength,
            geom=c("boxplot"))


p2 <- qplot(cutCompressiveStrength,cutAge, data=training,fill=cutCompressiveStrength,
            geom=c("boxplot","jitter"))

grid.arrange(p1,p2,ncol=2)

cutFlyAsh <- Hmisc::cut2(training$FlyAsh, g=9)
table(cutFlyAsh)

table(cutCompressiveStrength, cutAge)
table(cutCompressiveStrength, cutFlyAsh)


qplot(seq, CompressiveStrength, data=training)

qplot(seq, CompressiveStrength, colour=Age, data=training)
qplot(seq, cutCompressiveStrength, colour=cutFlyAsh, data=training)


"There is a non-random pattern in the plot of the outcome versus index that does not appear to be 
perfectly explained by any predictor suggesting a variable may be missing."
