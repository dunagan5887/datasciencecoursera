library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

IL_column_indexes <- grep('^IL', names(training))

IL_columns <- training[,IL_column_indexes]

prComp <- prcomp(IL_columns)


preProc <- preProcess(IL_columns,method="pca",thresh=.9)
spamPC <- predict(preProc,log10(spam[,-58]+1))