library(nnet)
set.seed(500)
wine_data = read.csv("F:/KE_CA_CI1/CA/Normalized_wine2.csv",header = TRUE)

wine_data$quality <- as.factor(wine_data$quality)
index <- sample(1:nrow(wine_data),round(0.75*nrow(wine_data)))
train <- wine_data[index,]
test <- wine_data[-index,]

model_nnet <- nnet(quality ~ ., data=train, size=6, maxit=600)
pred<- predict(model_nnet, test, type="class")
table(true=test$quality, predicted=pred)

library(caret)
confusionMatrix(pred,test$quality)
result<- cbind(test$quality, data.frame(pred))
#a <- c(as.numeric(test$quality))
a <- test$quality
b <- c(as.numeric(pred))
res=0
for (i in 1:nrow(result))
{ if(a[i]==b[i])
  res = res + 1
#pred_mlff <- nrow(result[a[i] == b[i],])/nrow(result)
}
result.size=nrow(result)

#pred_mlff <- pred_mlff*100

cat("Accuracy_Neural_Net = ",res/result.size * 100,"\n")

acc_mlff = res/result.size * 100
# -------------------------------------------------------------------------
#install.packages("grnn")
library(grnn)

wine_data = read.csv("F:/KE_CA_CI1/CA/winequality-white.csv", header=TRUE)
#wine_data = read.csv("F:/KE_CA_CI1/CA/Normalized_wine2.csv", header=TRUE)

size=nrow(wine_data)

length=ncol(wine_data)

#index <- 1:size

#positions <- sample(index, trunc(size * 0.75))

train_grnn <- wine_data[index,]
test_grnn <- wine_data[-index,1:length-1]
result_grnn = wine_data[-index,]
result_grnn$actual = result_grnn[,length]
result_grnn$predict = -1

grnn <- learn(train_grnn,variable.column=length)
grnn <- smooth(grnn, sigma = 1)


for(i in 1:nrow(test_grnn))
{	
  vec <- as.matrix(test_grnn[i,])
  resg <- guess(grnn, vec)
  
  if(is.nan(resg))
  {
    cat("Entry ",i," Generated NaN result!\n")
  }
  else
  {
    result_grnn$predict[i] <- resg
  }
}



result_grnn.size = nrow(result_grnn)
result_grnn.correct = nrow(result_grnn[round(result_grnn$predict) == result_grnn$actual,])
cat("No of test cases = ",result_grnn.size,"\n")
cat("Correct predictions = ", result_grnn.correct ,"\n")
cat("Accuracy = ", result_grnn.correct / result_grnn.size * 100 ,"\n")

acc_grnn = result_grnn.correct / result_grnn.size * 100
#--------------------------------------------------------------------------------------

#install.packages("pnn")
library(pnn)

wine_data = read.csv("F:/KE_CA_CI1/CA/winequality-white.csv", header=TRUE)
#wine_data = read.csv("F:/KE_CA_CI1/CA/Normalized_wine2.csv", header=TRUE)
wine_data$quality <- as.factor(wine_data$quality)
#p <- rep('q',nrow(wine_data))
#wine_data$quality <- paste(p,wine_data$quality,sep = "")
size=nrow(wine_data)

length=ncol(wine_data)

#index <- 1:size

#positions <- sample(index, trunc(size * 0.75))

train_pnn <- wine_data[index,]
test_pnn <- wine_data[-index,1:length-1]
result_pnn = wine_data[-index,]
result_pnn$actual = result_pnn[,length]
result_pnn$predict = -1

pnn <- learn(train_pnn, category.column=length)
#nn <- learn(train)
pnn <- smooth(pnn, sigma = 0.6)


for(i in 1:nrow(test_pnn))
{	
  vec <- as.matrix(test_pnn[i,])
  resp <- guess(pnn, vec)
  
  #if(is.nan(res))
  #{
  # cat("Entry ",i," Generated NaN result!\n")
  #}
  #else
  #{
  result_pnn$predict[i] <- getElement(resp,1)
  #result_pnn$predict[i] <- res$category
  #}
}



result_pnn.size = nrow(result_pnn)
result_pnn.correct = nrow(result_pnn[result_pnn$predict == result_pnn$actual,])
cat("No of test cases = ",result_pnn.size,"\n")
cat("Correct predictions = ", result_pnn.correct ,"\n")
cat("Accuracy = ", result_pnn.correct / result_pnn.size * 100 ,"\n")

acc_pnn = result_pnn.correct / result_pnn.size * 100

# ---------------------------------------------------------------

comb_result <- cbind(result,res_grnn=round(result_grnn$predict),res_pnn=result_pnn$predict)
output <- matrix()

require(functional)
#output$comb <- apply(comb_result[,-1,drop=FALSE], 1, Compose(table,
 #                   function(i) i==max(i),
  #                  which,
  #                  names,
  #                  function(i) paste0(i, collapse='/')
#)
#)

#output$actual <- apply(comb_result$`test$quality`,1,function(x) print(x))


output$comb1 <- apply(comb_result[,-1,drop=FALSE], 1, Compose(table,
                                                             function(i) i==max(i),
                                                             which,
                                                             names,
                                                             function(i) paste0(if (length(i) >1) i[2] else i[1])
)
)

rest=0
for (j in 1:nrow(comb_result)) {
  
  #comb_result$final[j]=names(which.max(table(comb_result[j,])))

#if(sort(table(comb_result[j,]),decreasing = T)[1] > 1) { comb_result$final[j] = names(which.max(table(comb_result[j,])))} 
  
  #else { comb_result$final[j] = comb_result[j,2]}
  
  if (output$comb1[j] == comb_result$`test$quality`[j])
    rest = rest + 1
}

final.size = nrow(comb_result)
final.correct = nrow(comb_result[comb_result$final == comb_result$`test$quality`,])
#cat("No of test cases = ",result_pnn.size,"\n")
cat("Correct predictions = ", rest ,"\n")
cat("Final Accuracy = ", rest / final.size * 100 ,"\n")

acc_final = rest / final.size * 100
#-----------------------------------------------------------------

cat("Accuracy of MLFF",acc_mlff,"\n")
cat("Accuracy of GRNN",acc_grnn,"\n")
cat("Accuracy of PNN",acc_pnn,"\n")
cat("Accuracy of Hybrid Model",acc_final,"\n")
