library(neuralnet)
library(MASS)
#set.seed(500)
set.seed(500)
wine_data = read.csv("F:/KE_CA_CI1/CA/Normalized_wine2.csv",header = TRUE)
str(wine_data)
normalize <- function(x){return((x-min(x))/(max(x)-min(x)))}
wine_norm <- as.data.frame(lapply(wine_data, normalize))
summary(wine_norm$quality)
index <- sample(1:nrow(wine_norm),round(0.75*nrow(wine_norm)))
train <- wine_norm[index,]
test <- wine_norm[-index,]

result=wine_norm[-index,]
length=ncol(wine_norm)
result$actual=result[,length]

n <- names(train)
f <- as.formula(paste("quality ~",paste(n[!n %in% "quality"],collapse = "+")))
#wine_model <- neuralnet(f,data = train,hidden = 5,threshold=0.3,learningrate = 0.2,algorithm = "rprop+",linear.output = TRUE)
wine_model <- neuralnet(f,data = train,hidden = 5,algorithm="rprop+")#,stepmax = 1e+05,rep = 500)
#wine_model <- neuralnet(f,data = train,hidden = c(5,3))
plot(wine_model)
model_results <- compute(wine_model,test[1:11])
predicted_quality <- model_results$net.result
cor(predicted_quality,test$quality)
model_results2 <- model_results$net.result*(max(wine_data$quality)-min(wine_data$quality))+min(wine_data$quality)
test.r <- (test$quality)*(max(wine_data$quality)-min(wine_data$quality))+min(wine_data$quality)
MSE.nn <- sum((test.r - model_results2)^2)/nrow(test)
print(MSE.nn)
library(Metrics)
mse(test.r,model_results2)

result$predict=round(model_results$net.result)
result.size=nrow(result)

#result.size = nrow(model_results$net.result)
result.correct = nrow(result[result$predict == result$actual,])

z <- c(test$quality)
a <- c(model_results$net.result)
b <- z-a

res = 0
for (i in 1:nrow(test))
{ 
  if(abs(b[i]) < 0.083)
    res= res + 1
}

cat("No of test cases = ",result.size,"\n")
cat("Correct predictions = ", res ,"\n")
cat("Accuracy = ", res / result.size * 100 ,"\n")