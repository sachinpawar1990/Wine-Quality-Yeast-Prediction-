library(nnet)
set.seed(500)
yeast_data = read.csv("F:/KE_CA_CI1/CA/norm_yeast.csv",header = TRUE)
#yeast_data$quality <- as.factor(wine_data$quality)
index <- sample(1:nrow(yeast_data),round(0.75*nrow(yeast_data)))
train <- yeast_data[index,]
test <- yeast_data[-index,]

model_nnet <- nnet(loc_site ~ ., data=train, size=11, maxit=1000)
pred<- predict(model_nnet, test, type="class")
table(true=test$loc_site, predicted=pred)

library(caret)
confusionMatrix(pred,test$loc_site)
result<- cbind(test$loc_site, data.frame(pred))
#a <- c(as.numeric(test$quality))
a <- test$loc_site
b <- c(pred)
res=0
for (i in 1:nrow(result))
{ if(a[i]==b[i])
  res = res + 1
#pred_mlff <- nrow(result[a[i] == b[i],])/nrow(result)
}
result.size=nrow(result)

#pred_mlff <- pred_mlff*100

cat("Accuracy_Neural_Net = ",res/result.size * 100,"\n")

acc_mlff <- res/result.size * 100

#library(Metrics)
#res_mse <- mse(a,b)
#cat("Mean Squared Error",res_mse,"\n")
#res = 0
#for(i in 1:nrow(result))
#{ if (as.numeric(result$pred) == result$test.quality)
#  res = res + 1
#}

#print(res)



# --------------------------------------------------------------------------------------------

library(grnn)

yeast_data = read.csv("F:/KE_CA_CI1/CA/yeast_2_grnn.csv", header=TRUE)
#wine_data = read.csv("F:/KE_CA_CI1/CA/Normalized_wine2.csv", header=TRUE)

size=nrow(yeast_data)

length=ncol(yeast_data)

#index <- 1:size

#positions <- sample(index, trunc(size * 0.75))

train_grnn <- yeast_data[index,]
test_grnn <- yeast_data[-index,1:length-1]
result_grnn = yeast_data[-index,]
result_grnn$actual = result_grnn[,length]
result_grnn$predict = -1

grnn <- learn(train_grnn,variable.column = length)
grnn <- smooth(grnn, sigma = 0.1)


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

acc_grnn <- result_grnn.correct / result_grnn.size * 100

#result_grnnchar <- as.character(round(result_grnn$predict))
#replace(result_grnnchar,c("0","1","2","3","4","5","6","7","8","9"),c("CYT","NUC","MIT","ME3","ME2","ME1","EXC","VAC","POX","ERL"))
# -----------------------------------------------------------------------------------------

library(pnn)

yeast_data = read.csv("F:/KE_CA_CI1/CA/yeast_2.csv", header=TRUE)
#wine_data = read.csv("F:/KE_CA_CI1/CA/Normalized_wine2.csv", header=TRUE)

size=nrow(yeast_data)

length=ncol(yeast_data)

#index <- 1:size

#positions <- sample(index, trunc(size * 0.75))

train_pnn <- yeast_data[index,]
test_pnn <- yeast_data[-index,1:length-1]
result_pnn = yeast_data[-index,]
result_pnn$actual = result_pnn[,length]
result_pnn$predict = -1

pnn <- learn(train_pnn, category.column=length)
#nn <- learn(train)
pnn <- smooth(pnn, sigma = 0.1)


for(i in 1:nrow(test_pnn))
{	
  vec <- as.matrix(test_pnn[i,])
  resp <- guess(pnn, vec)
  
  #if(is.nan(res))
  #{
  #cat("Entry ",i," Generated NaN result!\n")
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

acc_pnn <- result_pnn.correct / result_pnn.size * 100

#------------------------------------------------------------------------------------

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
                                                              function(i) paste0(if (length(i) >1) i[1] else i[1])
)
)

rest=0
for (j in 1:nrow(comb_result)) {
  
  #comb_result$final[j]=names(which.max(table(comb_result[j,])))
  
  #if(sort(table(comb_result[j,]),decreasing = T)[1] > 1) { comb_result$final[j] = names(which.max(table(comb_result[j,])))} 
  
  #else { comb_result$final[j] = comb_result[j,2]}
  
  if (output$comb1[j] == comb_result$`test$loc_site`[j])
    rest = rest + 1
}

final.size = nrow(comb_result)
final.correct = nrow(comb_result[comb_result$final == comb_result$`test$loc_site`,])
#cat("No of test cases = ",result_pnn.size,"\n")
cat("Correct predictions = ", rest ,"\n")
cat("Final Accuracy = ", rest / final.size * 100 ,"\n")

acc_final = rest / final.size * 100
#-----------------------------------------------------------------


comb_result2 <- cbind(result,res_pnn=result_pnn$predict)
output2 <- matrix()

require(functional)



output2$comb1 <- apply(comb_result2[,-1,drop=FALSE], 1, Compose(table,
                                                              function(i) i==max(i),
                                                              which,
                                                              names,
                                                              function(i) paste0(if (length(i) >1) i[1] else i[1])
)
)

rest2=0
for (j in 1:nrow(comb_result2)) {
  
  if (output2$comb1[j] == comb_result2$`test$loc_site`[j])
    rest2 = rest2 + 1
}

final2.size = nrow(comb_result2)
final2.correct = nrow(comb_result2[comb_result2$final == comb_result2$`test$loc_site`,])
#cat("No of test cases = ",result_pnn.size,"\n")
cat("Correct predictions = ", rest2 ,"\n")
cat("Final Accuracy = ", rest2 / final2.size * 100 ,"\n")

acc_final2 = rest2 / final2.size * 100


#----------------------------------------------------------------


#-----------------------------------------------------------------

model_nnet2 <- nnet(loc_site ~ ., data=train, size=6, maxit=600)
pred2<- predict(model_nnet2, test, type="class")
table(true=test$loc_site, predicted=pred2)

library(caret)
confusionMatrix(pred2,test$loc_site)
result2<- cbind(test$loc_site, data.frame(pred2))
#a <- c(as.numeric(test$quality))
a <- test$loc_site
b <- c(pred2)
res2=0
for (i in 1:nrow(result2))
{ if(a[i]==b[i])
  res2 = res2 + 1
#pred_mlff <- nrow(result[a[i] == b[i],])/nrow(result)
}
result2.size=nrow(result2)

#pred_mlff <- pred_mlff*100

cat("Accuracy_MLFF 2 = ",res2/result2.size * 100,"\n")

acc_mlff2 <- res2/result2.size * 100
#------------------------------------------------------------------


comb_result3 <- cbind(result,res_mlff2=result2$pred2,res_pnn=result_pnn$predict)
output3 <- matrix()

require(functional)



output3$comb1 <- apply(comb_result3[,-1,drop=FALSE], 1, Compose(table,
                                                                function(i) i==max(i),
                                                                which,
                                                                names,
                                                                function(i) paste0(if (length(i) >1) i[2] else i[1])
)
)

rest3=0
for (j in 1:nrow(comb_result3)) {
  
  if (output3$comb1[j] == comb_result3$`test$loc_site`[j])
    rest3 = rest3 + 1
}

final3.size = nrow(comb_result3)
final3.correct = nrow(comb_result3[comb_result3$final == comb_result3$`test$loc_site`,])
#cat("No of test cases = ",result_pnn.size,"\n")
cat("Correct predictions = ", rest3 ,"\n")
cat("Final Accuracy = ", rest3 / final3.size * 100 ,"\n")

acc_final3 = rest3 / final3.size * 100
#-----------------------------------------------------------------

cat("Accuracy of MLFF",acc_mlff,"\n")
cat("Accuracy of GRNN",acc_grnn,"\n")
cat("Accuracy of PNN",acc_pnn,"\n")
cat("Accuracy of MLFF 2",acc_mlff2,"\n")
cat("Accuracy of Hybrid Model 1",acc_final,"\n")
cat("Accuracy of Hybrid Model 2",acc_final2,"\n")
cat("Accuracy of Hybrid Model 3",acc_final3,"\n")
