library(MASS)
library(lattice)
library(randomForest)
library(xlsx)

dir = "C:/Users/Huran/Desktop/Data_Project/HousePrice_Competition_16"
setwd(dir)

train = read.csv("Data/train.csv",header=TRUE, sep = ",", na.strings = c("NA", " ", ""))
test = read.csv ("Data/test.csv", header=TRUE, sep=",", na.strings = c("NA", " ", ""))

data = list(train = train, test = test)

lapply(data, function(x) any(is.na(x)))

sapply(train, function(x) sum(is.na(x)))

for(i in seq(data)){

  for(j in 1:nrow(data[[i]])){
    bsmt = data[[i]][,grepl("Bsmt", names(data[[i]]))]

    if (any(is.na(bsmt[j,]))) {
      data[[i]]$BSMT[[j]] = 0
    }
    else {
      data[[i]]$BSMT[[j]] = 1
      }
  }
}

for(i in seq(data)){

  for(j in 1:nrow(data[[i]])){
    grg = data[[i]][,grepl("Garage", names(data[[i]]))]

    if (any(is.na(grg[j,]))) {
      data[[i]]$GRG[[j]] = 0
    }
    else {
      data[[i]]$GRG[[j]] = 1
      }
  }
}

for(i in seq(data)){

data[[i]] = data[[i]][,!names(data[[i]]) %in% names(bsmt)]
data[[i]] = data[[i]][,!names(data[[i]]) %in% names(grg)]
}

sapply(data, function(x) sum(is.na(x)))

for(i in 1:2){
 print(sapply(data[[i]],function(x) sum(is.na(x))))

}

train = data[[1]]
test = data[[2]]

train = na.roughfix(train)
test = na.roughfix(test)

rfm = randomForest(SalePrice~., data = train, ntree=1000, do.trace=TRUE)

for (i in names(test)){
  levels(train[, i]) = unique(c(levels(train[,i]), levels(test[,i])))
  levels(test[,i]) = levels(train[,i])
}


identical(colnames(subset(train, select=-SalePrice)), colnames(test))

sale = predict(rfm, test)
print(sale)

sub = data.frame(Id = test$Id, SalePrice = sale)

write.csv(sub, file = "sub.csv", row.names = FALSE)

