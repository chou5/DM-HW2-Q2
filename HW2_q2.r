#Below are all for Data Mining HW2 question 2
#q2-1
wine = read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data", 
                  sep=",",
                  header = FALSE)
names(wine) = c("Class","Alcohol","Malic.acid","Ash","Alcalinity.of.ash","Magnesium","Total.phenols","Flavanoids","Nonflavanoid.phenols","Proanthocyanins","Color.intensity","Hue","OD280/OD315.of.diluted.wines","Proline")
wine.features = wine
wine.features$Class <- NULL

most_correlated <- function(input_data,num)
{
  cormatrix <- cor(input_data)
  diag(cormatrix) <- 0
  cormatrix[lower.tri(cormatrix)] <- 0
  tmp <- as.data.frame(as.table(cormatrix))
  names(tmp) <- c("First_Wine", "Second_Wine","Correlation")
  result = head(tmp[order(abs(tmp$Correlation),decreasing=T),],n=num)
  return (result)
}


least_correlated <- function(input_data,num)
{
  cormatrix <- cor(input_data)
  diag(cormatrix) <- 0
  cormatrix[lower.tri(cormatrix)] <- 0
  tmp <- as.data.frame(as.table(cormatrix))
  names(tmp) <- c("First_Wine", "Second_Wine","Correlation")
  tmp2 <- subset(tmp,Correlation!=0)
  result = head(tmp2[order(abs(tmp2$Correlation),decreasing=F),],n=num)
  return (result)
}

result = most_correlated(wine,4)
four_most_correlated <- list()
result

for(i in c(1:4)){
  four_most_correlated <- append(four_most_correlated, result$First_Wine[i])
  four_most_correlated <- append(four_most_correlated, result$Second_Wine[i])
}

get_column <- unique(four_most_correlated)
dta <- wine[as.integer(get_column[1])]
for(i in c(2:4)){
  dta <- cbind(dta, wine[as.integer(get_column[i])])
}

most1 = wine[,c("Total.phenols","Flavanoids")]
most2 = wine[,c("Class","Flavanoids")]
most3 = wine[,c("Class","OD280/OD315.of.diluted.wines")]
most4 = wine[,c("Flavanoids","OD280/OD315.of.diluted.wines")]
par(mfrow=c(2,2))
plot(most1, xlab = "Total.phenols", ylab = "Flavanoids", main = "The Most Correlated Pairs of Features")
plot(most2, xlab = "Class", ylab = "Flavanoids", main = "The 2nd Most Correlated Pairs of Features")
plot(most3, xlab = "Class", ylab = "OD280/OD315.of.diluted.wines", main = "The 3rd Most Correlated Pairs of Features")
plot(most4, xlab = "Flavanoids", ylab = "OD280/OD315.of.diluted.wines", main = "The 4th Most Correlated Pairs of Features")


result2 = least_correlated(wine,4)
four_least_correlated <- list()
result2


for(i in c(1:4)){
  four_least_correlated <- append(four_least_correlated, result2$First_Wine[i])
  four_least_correlated <- append(four_least_correlated, result2$Second_Wine[i])
}

get_column2 <- unique(four_least_correlated)
dta1 <- wine[as.integer(get_column2[1])]
for(i in c(2:5)){
  dta1 <- cbind(dta1, wine[as.integer(get_column2[i])])
}

least1 = wine[,c("Proanthocyanins","Color.intensity")]
least2 = wine[,c("Alcalinity.of.ash","Color.intensity")]
least3 = wine[,c("Ash","Proanthocyanins")]
least4 = wine[,c("Ash","OD280/OD315.of.diluted.wines")]
par(mfrow=c(2,2))
plot(least1, xlab = "Proanthocyanins", ylab = "Color.intensity", main = "The Least Correlated Pairs of Features")
plot(least2, xlab = "Alcalimity.of.ash", ylab = "Color.intensity", main = "The 2nd Least Correlated Pairs of Features")
plot(least3, xlab = "Ash", ylab = "Proanthocyanins", main = "The 3rd Least Correlated Pairs of Features")
plot(least4, xlab = "Ash", ylab = "OD280/OD315.of.diluted.wines", main = "The 4th Least Correlated Pairs of Features")



#q2-2
#Following can get the Euclidean distance between every item and every other item
get_eucli_matrix <- function(input_data){
  result<-as.matrix(dist(input_data,method = "euclidean",upper = TRUE))
  return (result)
}


wine.eucli_matrix <- get_eucli_matrix(wine.features)

count1 = 0
for(i in c(1:178)){
  temp <- cbind(wine.eucli_matrix[i,],1:178)
  check <- temp[order(temp[,1]),][2,2]
  if(wine[check,][1] == wine[i,][1]){
    count1 = count1+1    
  }
}

count1/178

count2 = 0
count3 = 0
count4 = 0

for(i in 1:178){
  temp <- cbind(wine.eucli_matrix[i,],1:178)
  check <- temp[order(temp[,1]),][2,2]
  
  if(wine[i,][1] == 1){
    if (wine[i,][1] == wine[check,][1]) count2=count2+1
  }
  if(wine[i,][1] == 2){
    if (wine[i,][1] == wine[check,][1]) count3=count3+1
  }
  if(wine[i,][1] == 3){
    if (wine[i,][1] == wine[check,][1]) count4=count4+1
  }
}

count2/59
count3/71
count4/48

#q2-3
#0-1 Normalization
Normalized<-function(x)
{
  return ((x - min(x))/(max(x) - min(x)))
}
#z-score Normalization
Z_score<-function(x)
{
  return((x-mean(x))/sd(x))
}

after_normalized_01 <- Normalized(wine.features[,1])
after_normalized_z <- Z_score(wine.features[,1])

for (i in 2:13){
  m <- Normalized(wine.features[,i])
  after_normalized_01 <- cbind(after_normalized_01,m)
}

for (i in 2:13){
  m <- Z_score(wine.features[,i])
  after_normalized_z <- cbind(after_normalized_z,m)
}

after_normalized_01_eicli_matrix <- get_eucli_matrix(after_normalized_01)
after_normalized_z_eicli_matrix <- get_eucli_matrix(after_normalized_z)

count5_01 = 0
count5_z = 0
for(i in c(1:178)){
  temp <- cbind(after_normalized_01_eicli_matrix[i,],1:178)
  check <- temp[order(temp[,1]),][2,2]
  if(wine[check,][1] == wine[i,][1]){
    count5_01 = count5_01+1    
  }
}

for(i in c(1:178)){
  temp <- cbind(after_normalized_z_eicli_matrix[i,],1:178)
  check <- temp[order(temp[,1]),][2,2]
  if(wine[check,][1] == wine[i,][1]){
    count5_z = count5_z+1    
  }
}

count5_01/178
count5_z/178

count6_01 = 0
count6_z = 0
count7_01 = 0
count7_z = 0
count8_01 = 0
count8_z = 0

for(i in 1:178){
  temp <- cbind(after_normalized_01_eicli_matrix[i,],1:178)
  check <- temp[order(temp[,1]),][2,2]
  
  if(wine[i,][1] == 1){
    if (wine[i,][1] == wine[check,][1]) count6_01=count6_01+1
  }
  if(wine[i,][1] == 2){
    if (wine[i,][1] == wine[check,][1]) count7_01=count7_01+1
  }
  if(wine[i,][1] == 3){
    if (wine[i,][1] == wine[check,][1]) count8_01=count8_01+1
  }
}

for(i in 1:178){
  temp <- cbind(after_normalized_z_eicli_matrix[i,],1:178)
  check <- temp[order(temp[,1]),][2,2]
  
  if(wine[i,][1] == 1){
    if (wine[i,][1] == wine[check,][1]) count6_z=count6_z+1
  }
  if(wine[i,][1] == 2){
    if (wine[i,][1] == wine[check,][1]) count7_z=count7_z+1
  }
  if(wine[i,][1] == 3){
    if (wine[i,][1] == wine[check,][1]) count8_z=count8_z+1
  }
}
count6_01/59
count6_z/59
count7_01/71
count7_z/71
count8_01/48
count8_z/48


