url <- "https://cran.r-project.org/src/contrib/Archive/ascrda/ascrda_1.15.tar.gz"
pkgFile <- "ascrda_1.15.tar.gz"
download.file(url = url, destfile = pkgFile)
# Install package

install.packages(c("rda", "sfsmisc", "e1071", "pamr"))
install.packages(pkgs=pkgFile, type="source", repos=NULL)

# http://www.inside-r.org/packages/cran/ascrda/docs/FitRda
install.packages("ascrda")
require(ascrda)

df_vowel_train <- read.table("../vowel.train.csv", sep=",", header=1)
df_vowel_test  <- read.table("../vowel.test.csv", sep=",", header=1)

df_vowel_train$row.names<- NULL
df_vowel_test$row.names<- NULL
y <- df_vowel_train$y 
y_test <- df_vowel_test$y 
X <- df_vowel_train[ ,c(F,T,T,T,T,T,T,T,T,T,T)]
X_test <- df_vowel_test[ ,c(F,T,T,T,T,T,T,T,T,T,T)]

a <- rep(0, 100)
res_train <- rep(0, 100)
res_test  <- rep(0, 100)

for(i in 1:101){
  a[i] <- 0.01*(i-1)
  print (a[i])
  startTime <- proc.time()[3]
  ans <- FitRda(X, y, X_test, y_test, alpha=a)
  endTime <- proc.time()[3]
  print(endTime-startTime)
  res_train[i] <- ans[1]
  res_test[i]  <- ans[2]
}
df_result <- data.frame(train=res_train, test=res_test)
write.table(df_result, file="df_result.csv", sep=",")


