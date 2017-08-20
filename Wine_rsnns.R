library(RSNNS)
set.seed(600)
#yeast_data = read.csv("F:/KE_CA_CI1/CA/norm_yeast.csv",header = TRUE)

wine_data = read.csv("F:/KE_CA_CI1/CA/Normalized_wine2.csv", header=TRUE)

#wine_data = read.csv("C:/Users/e0013515/Downloads/Normalized_wine2.csv",header=TRUE)
#yeast_data$quality <- as.factor(wine_data$quality)
wine <- wine_data[sample(1:nrow(wine_data),length(1:nrow(wine_data))),1:ncol(wine_data)]

wineValues <- wine[,1:11]
wineTargets <- wine[,12]
wineDecTargets <- decodeClassLabels(wineTargets)
wine <- splitForTrainingAndTest(wineValues,wineDecTargets,ratio = 0.25)
wine <- normTrainingAndTestSet(wine)

#index <- sample(1:nrow(wine_data),round(0.75*nrow(wine_data)))
#train_mlp <- wine_data[index,]
#test_mlp <- wine_data[-index,]

model <- mlp(wine$inputsTrain,wine$targetsTrain,size=7,learnFuncParams=0.8,maxit=60,learnFunc="Std_Backpropagation",inputsTest=wine$inputsTest,targetsTest=wine$targetsTest)
summary(model)

predictions <- predict(model,wine$inputsTest)

confusionMatrix(wine$targetsTest,predictions)

conf <- data.frame(confusionMatrix(wine$targetsTest,predictions))
#conf1 <- as.numeric(conf)
#conf$targets <- as.numeric(conf$targets)
#conf$predictions <- as.numeric(conf$predictions)
#conf$Freq <- as.numeric(conf$Freq)

len <- nrow(wine$targetsTest)
res=0
#for (i in 1:len)
#  {
  #if( conf$targets[i] == conf$predictions[i])
  
   #res = res + conf$Freq[i])
#}

for(i in 1:nrow(conf))
{
  if(as.numeric(conf[i,1]) == conf[i,2])
    res = res + as.numeric(conf[i,3])
  
}

cat("Accuracy of MLP : " ,res/len * 100,"\n")

acc_mlp=res/len * 100

#---------------------------------------------------------------------------------------
model2 <- rbf(wine$inputsTrain,wine$targetsTrain,size=30,maxit=1000,initFuncParams=c(0,1,0,0.01,0.01),learnFuncParams=c(1e-8, 0, 1e-8, 0.1, 0.8),linOut=TRUE)
#model2 <- rbfDDA(wine$inputsTrain,wine$targetsTrain,maxit = 1, initFunc = "Randomize_Weights",initFuncParams = c(-0.3, 0.3), learnFunc = "RBF-DDA",learnFuncParams = c(0.4, 0.2, 5), updateFunc = "Topological_Order",updateFuncParams = c(0), shufflePatterns = TRUE)
summary(model2)

predictions2 <- predict(model2,wine$inputsTest)
confusionMatrix(wine$targetsTest,predictions2)

conf2 <- data.frame(confusionMatrix(wine$targetsTest,predictions2))
#conf1 <- as.numeric(conf)
#conf$targets <- as.numeric(conf$targets)
#conf$predictions <- as.numeric(conf$predictions)
#conf$Freq <- as.numeric(conf$Freq)

len2 <- nrow(wine$targetsTest)
res2=0
#for (i in 1:len)
#  {
#if( conf$targets[i] == conf$predictions[i])

#res = res + conf$Freq[i])
#}

for(i in 1:nrow(conf2))
{
  if(as.numeric(conf2[i,1]) == conf2[i,2])
    res2 = res2 + as.numeric(conf2[i,3])
  
}

cat("Accuracy of RBF : " ,res2/len2 * 100,"\n")

acc_rbf=res2/len2 * 100

#--------------------------------------------------------------------------------------

model3 <- jordan(wine$inputsTrain,wine$targetsTrain,size=8,maxit=1000,initFunc = "JE_Weights",initFuncParams=c(1,-1,0.3,1,0.5),learnFunc = "JE_BP",learnFuncParams=c(0.1),updateFunc = "JE_Order",linOut=TRUE,outContext = FALSE)
#model2 <- rbfDDA(wine$inputsTrain,wine$targetsTrain,maxit = 1, initFunc = "Randomize_Weights",initFuncParams = c(-0.3, 0.3), learnFunc = "RBF-DDA",learnFuncParams = c(0.4, 0.2, 5), updateFunc = "Topological_Order",updateFuncParams = c(0), shufflePatterns = TRUE)
summary(model3)

predictions3 <- predict(model3,wine$inputsTest)
confusionMatrix(wine$targetsTest,predictions3)

conf3 <- data.frame(confusionMatrix(wine$targetsTest,predictions3))
#conf1 <- as.numeric(conf)
#conf$targets <- as.numeric(conf$targets)
#conf$predictions <- as.numeric(conf$predictions)
#conf$Freq <- as.numeric(conf$Freq)

len3 <- nrow(wine$targetsTest)
res3=0
#for (i in 1:len)
#  {
#if( conf$targets[i] == conf$predictions[i])

#res = res + conf$Freq[i])
#}

for(i in 1:nrow(conf3))
{
  if(as.numeric(conf3[i,1]) == conf3[i,2])
    res3 = res3 + as.numeric(conf3[i,3])
  
}

cat("Accuracy of Jordan Recurrent N/W : " ,res3/len3 * 100,"\n")

acc_jordan=res3/len3 * 100

#----------------------------------------------------------------------------------
fin_pred <- matrix(nrow = nrow(wine$targetsTest),ncol=ncol(wine$targetsTest))
for (j in 1:nrow(wine$targetsTest)) {
  for (k in 1:ncol(wine$targetsTest)) {
    fin_pred[j,k]=(predictions[j,k]+predictions2[j,k]+predictions3[j,k])/3
    
  }
}

confusionMatrix(wine$targetsTest,fin_pred)

conff <- data.frame(confusionMatrix(wine$targetsTest,fin_pred))
#conf1 <- as.numeric(conf)
#conf$targets <- as.numeric(conf$targets)
#conf$predictions <- as.numeric(conf$predictions)
#conf$Freq <- as.numeric(conf$Freq)

lenf <- nrow(wine$targetsTest)
resf=0
#for (i in 1:len)
#  {
#if( conf$targets[i] == conf$predictions[i])

#res = res + conf$Freq[i])
#}

for(i in 1:nrow(conff))
{
  if(as.numeric(conff[i,1]) == conff[i,2])
    resf = resf + as.numeric(conff[i,3])
  
}

cat("Accuracy of Hybrid N/W : " ,resf/lenf * 100,"\n")

acc_hybrid=resf/lenf * 100

#--------------------------------------------------------------------------------

fin_pred2 <- matrix(nrow = nrow(wine$targetsTest),ncol=ncol(wine$targetsTest))
for (j in 1:nrow(wine$targetsTest)) {
  for (k in 1:ncol(wine$targetsTest)) {
    fin_pred2[j,k]=(predictions[j,k]+predictions2[j,k])/2
    
  }
}

confusionMatrix(wine$targetsTest,fin_pred2)

conff2 <- data.frame(confusionMatrix(wine$targetsTest,fin_pred2))
#conf1 <- as.numeric(conf)
#conf$targets <- as.numeric(conf$targets)
#conf$predictions <- as.numeric(conf$predictions)
#conf$Freq <- as.numeric(conf$Freq)

lenf2 <- nrow(wine$targetsTest)
resf2=0
#for (i in 1:len)
#  {
#if( conf$targets[i] == conf$predictions[i])

#res = res + conf$Freq[i])
#}

for(i in 1:nrow(conff2))
{
  if(as.numeric(conff2[i,1]) == conff2[i,2])
    resf2 = resf2 + as.numeric(conff2[i,3])
  
}

cat("Accuracy of Hybrid N/W 2 : " ,resf2/lenf2 * 100,"\n")

acc_hybrid2=resf2/lenf2 * 100

#--------------------------------------------------------------------------------
cat("Accuracy of MLP N/W : " ,acc_mlp,"\n")
cat("Accuracy of RBF N/W : " ,acc_rbf,"\n")
cat("Accuracy of Jordan N/W : " ,acc_jordan,"\n")
cat("Accuracy of Hybrid N/W : " ,acc_hybrid,"\n")
cat("Accuracy of Hybrid N/W  2: " ,acc_hybrid2,"\n")

