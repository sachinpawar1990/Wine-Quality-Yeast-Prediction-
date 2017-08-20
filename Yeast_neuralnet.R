library(neuralnet)
library(MASS)
#set.seed(500)
set.seed(500)
yeast_data = read.csv("F:/KE_CA_CI1/CA/norm_yeast.csv",header = TRUE)
str(yeast_data)
#normalize <- function(x){return((x-min(x))/(max(x)-min(x)))}
#wine_norm <- as.data.frame(lapply(wine_data, normalize))
#summary(wine_norm$quality)
index <- sample(1:nrow(yeast_data),round(0.75*nrow(yeast_data)))
train <- yeast_data[index,]
test <- yeast_data[-index,]

result=yeast_data[-index,]
length=ncol(yeast_data)
result$actual=result[,length]

n <- names(train)
f <- as.formula(paste("loc_site ~",paste(n[!n %in% "loc_site"],collapse = "+")))
#wine_model <- neuralnet(f,data = train,hidden = 5,threshold=0.3,learningrate = 0.2,algorithm = "rprop+",linear.output = TRUE)
yeast_model <- neuralnet(f,data = train,hidden = 5,algorithm="rprop+")#,stepmax = 1e+05,rep = 500)
#wine_model <- neuralnet(f,data = train,hidden = c(5,3))
plot(yeast_model)
model_results <- compute(yeast_model,test[1:11])
predicted_site <- model_results$net.result
cor(predicted_site,test$loc_site)
model_results2 <- model_results$net.result*(max(yeast_data$loc_site)-min(yeast_data$loc_site))+min(yeast_data$loc_site)
test.r <- (test$loc_site)*(max(yeast_data$loc_site)-min(yeast_data$loc_site))+min(yeast_data$loc_site)
MSE.nn <- sum((test.r - model_results2)^2)/nrow(test)
print(MSE.nn)
library(Metrics)
mse(test.r,model_results2)

result$predict=round(model_results$net.result)
result.size=nrow(result)

#result.size = nrow(model_results$net.result)
result.correct = nrow(result[result$predict == result$actual,])

z <- c(test$loc_site)
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