# Read Desharnais77 public dataset from promise uottawa repository
desharnais <- read.table("DesharnaisPromise.csv",
                                             sep = ",", header = TRUE)

desha <- subset(desharnais,Project!=38)
# continue to remove 38 , 44, 66, 75 because not enough data in these projects
desha <- subset(desha,Project!=44)
desha <- subset(desha,Project!=66)
desha <- subset(desha,Project!=75)
### because ? in some fields under these columns
#   has to specify as numeric
desha$TeamExp<-as.numeric(desha$TeamExp)
desha$ManagerExp<-as.numeric(desha$ManagerExp)
# Keep only Project+Effor + 8 independent var, remove project, yearEnd, PointsNnAdjust
desha <- desha[,c(1,2,3,5:10,12)]
### randomly keep 6 project rows , remaining 71 rows, keep as Training set
deshaTrain <- subset(desha,Project!=69)
deshaTrain <- subset(deshaTrain,Project!=72)
deshaTrain <- subset(deshaTrain,Project!=73)
deshaTrain <- subset(deshaTrain,Project!=74)
deshaTrain <- subset(deshaTrain,Project!=70)
deshaTrain <- subset(deshaTrain,Project!=71)
### keep the 6 rows in Test dataset
deshaTest <- subset(desha,Project>68 & Project <75)
### remove first column Project from both Train and Test sets
deshaTrain <- deshaTrain[,c(2:10)]
deshaTest <- deshaTest[,c(2:10)]
deshaTrainY = deshaTrain$Effort
deshaTestY = deshaTest$Effort
#remove Effort from X set
deshaTrainX = deshaTrain[,c(1:3,5:9)]
deshaTestX = deshaTest[,c(1:3,5:9)]
solTestY = deshaTestY
solTrainY = deshaTrainY
solTrainX = deshaTrainX
solTestX = deshaTestX
solTrainXtrans = deshaTrainX
solTestXtrans = deshaTestX


#try Kuhn ch6 Linear Regression and cousins
library(AppliedPredictiveModeling)

library(lattice)
### Some initial plots of the data

xyplot(solTrainY ~ solTrainX$Entities, type = c("p", "g"),
                  ylab = "Effort",
                  main = "(a)",
                   xlab = "Entities")


xyplot(solTrainY ~ solTrainX$Transactions, type = c("p", "g"),
                 ylab = "Effort",
                xlab = "Trans")

library(corrplot)


corrplot::corrplot(cor(solTrainXtrans),
                                     order = "hclust",
                                    tl.cex = .8)
### Section 6.2 Linear Regression
set.seed(100)
indx <- createFolds(solTrainY, returnTrain = TRUE)
ctrl <- trainControl(method = "cv", index = indx)
set.seed(100)
lmTune0 <- train(x = solTrainXtrans, y = solTrainY,
                                   method = "lm",
                                    trControl = ctrl)


tooHigh <- findCorrelation(cor(solTrainXtrans), .8)
trainXfiltered <- solTrainXtrans[, -tooHigh]
testXfiltered  <-  solTestXtrans[, -tooHigh]

set.seed(100)
lmTune <- train(x = trainXfiltered, y = solTrainY,
                  +                 method = "lm",
                  +                 trControl = ctrl)

lmTune

### Save the test set results in a data frame                 
testResults <- data.frame(obs = solTestY,
                            +                           Linear_Regression = predict(lmTune, testXfiltered))

### Section 6.4 Penalized Models
 ridgeGrid <- expand.grid(lambda = seq(0, .1, length = 15))

set.seed(100)
ridgeTune <- train(x = solTrainXtrans, y = solTrainY,
                                       method = "ridge",
                                        tuneGrid = ridgeGrid,
                                        trControl = ctrl,
                                        preProc = c("center", "scale"))
ridgeTune

print(update(plot(ridgeTune), xlab = "Penalty"))

enetGrid <- expand.grid(lambda = c(0, 0.01, .1),
                            +                         fraction = seq(.05, 1, length = 20))
set.seed(100)
enetTune <- train(x = solTrainXtrans, y = solTrainY,
                                      method = "enet",
                                      tuneGrid = enetGrid,
                                      trControl = ctrl,
                                     preProc = c("center", "scale"))
enetTune

plot(enetTune)
testResults$Enet <- predict(enetTune, solTestXtrans)
 ### Section 6.3 Partial Least Squares

## Run PLS and PCR on solubility data and compare results
 # removed tune grid ncomp =... to use default , max number of components
set.seed(100)
 plsTune <- train(x = solTrainXtrans, y = solTrainY,
                                     method = "pls",
                                    trControl = ctrl)
 plsTune

 testResults$PLS <- predict(plsTune, solTestXtrans)
 
 set.seed(100)
pcrTune <- train(x = solTrainXtrans, y = solTrainY,
                    +                  method = "pcr",
                    +                  trControl = ctrl)
 pcrTune   
 

 plsResamples <- plsTune$results
plsResamples$Model <- "PLS"
pcrResamples <- pcrTune$results
pcrResamples$Model <- "PCR"
plsPlotData <- rbind(plsResamples, pcrResamples)
 xyplot(RMSE ~ ncomp,
                 data = plsPlotData,
                 #aspect = 1,
                    xlab = "# Components",
                  ylab = "RMSE (Cross-Validation)",
                 auto.key = list(columns = 2),
                 groups = Model,
                  type = c("o", "g"))
plsImp <- varImp(plsTune, scale = FALSE)
plot(plsImp, top = 25, scales = list(y = list(cex = .95)))
 
 
 
 

#读取数据
desharnais=read.csv("desharnais77.csv")
desharnais


#基础分析
summary(desharnais)

#柱状图
hist(desharnais$Effort)

#散点图
plot(desharnais$Effort)
plot(desharnais$Transactions)
plot(desharnais$Effort,desharnais$Transactions)

# 添加回归线
abline(lm(desharnais$Transactions ~ desharnais$Effort), col = "red")

#pearson连续

cor.test(desharnais$Transactions, desharnais$Effort, method=c("pearson"))

#回归分析
relation <- lm(desharnais$Transactions~desharnais$Effort)

print(relation)

relation <- lm(desharnais$Transactions~desharnais$Effort+desharnais$Entities)

print(relation)