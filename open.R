library(caret)
library(mlbench)
require(xlsx)

transform = function(x){
  if(class(x) == "character"){
    values = sort(unique(x))
    for(i in 1:length(x)){
      x[i] = which(values == x[i])  
    }
  }
  return(x)
}

data = read.csv("limpios.csv",
                encoding = "UTF-8", 
                stringsAsFactors = F)

nombres = names(data)

names(data) = paste("v",as.character(1:100), sep = "")
data = data[complete.cases(data), ]
# write.csv(x = data, file = "semi_minada.csv")
data = na.omit(data)

data = as.data.frame(apply(data,2, transform))

for(x in 1:ncol(data)){
  data[[x]] = as.numeric(data[[x]])
}


data[[58]] = factor(data[[58]])

# control = trainControl(method = "reapetedcv", number=10, repeats=3)
data$Ha.Comprado.o.No

data$Ha.Comprado.o.No = factor(data$Ha.Comprado.o.No)
model = train(data$Ha.Comprado.o.No~ ., data = data, method="lvq")



importance <- varImp(model, scale=FALSE)
print(importance)
plot(importance)

data$email = NULL

class(data$Clasica)

for(x in 1:28){
  data[[x]] = as.numeric(data[[x]])
}

# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(data[,1:28], data[,29], sizes=c(1:28), rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))
