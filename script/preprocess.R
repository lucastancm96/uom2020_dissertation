library(mice)
library(ggplot2)
library("gridExtra")
source("http://peterhaschke.com/Code/multiplot.R")

# Import data set
train_ch <- read.csv('csv_data/train_ch.csv')
test_ch <- read.csv('csv_data/test_ch.csv')
train_noch <- read.csv('csv_data/train_noch.csv')
test_noch <- read.csv('csv_data/test_noch.csv')
# edudf <- read.csv('csv_data/edudf.csv')
# summary(train)
summary(train_ch)
summary(test_ch)
summary(train_noch)
summary(test_noch)

#********** Imputing for train_ch **********
# Maxit is the cycle of imputation (times of iterations)
# M is the number of dataset you created
# If set m=1, maxit=50, you'll impute for 50 iterations but only generate 1 imputed dataset
# The purpose of Maxit is to stabilize the parameters (to converge), such that
# the order ot the variable does not affect the imputed values
# If set m=5, maxit=1, you'll impute 1 time only but generate 5 imputed dataset

# Create df for train_ch to impute
train_chdf <- train_ch[, c('EDU', 'LIV_AR', 'SUBJ_HHI', 'EMP_STAT')]
imptrain_ch <- mice(train_chdf, method='cart', seed=42)
comptrain_ch <- complete(imptrain_ch, 5)

# Create final imputed train_ch df
dummytrain_ch <- train_ch[]
train_chcl <- list(colnames(comptrain_ch))
for (col in train_chcl){
  dummytrain_ch[col] <- comptrain_ch[col]
}

#********** Imputing for test_ch **********
# Create df for test_ch to impute
test_chdf <- test_ch[, c('AGE', 'EDU', 'LIV_AR', 'SUBJ_HHI')]
imptest_ch <- mice(test_chdf, method='cart', seed=42)
comptest_ch <- complete(imptest_ch, 5)

# Create final imputed test_ch df
dummytest_ch <- test_ch[]
test_chcl <- list(colnames(comptest_ch))
for (col in test_chcl){
  dummytest_ch[col] <- comptest_ch[col]
}

#********** Imputing for train_noch **********
# Create df for train_ch to impute
train_nochdf <- train_noch[, c('EDU', 'LIV_AR', 'SUBJ_HHI', 'EMP_STAT')]
imptrain_noch <- mice(train_nochdf, method='cart', seed=42)
comptrain_noch <- complete(imptrain_noch, 5)

# Create final imputed train_noch df
dummytrain_noch <- train_noch[]
train_nochcl <- list(colnames(comptrain_noch))
for (col in train_nochcl){
  dummytrain_noch[col] <- comptrain_noch[col]
}

#********** Imputing for test_noch **********
# Create df for test_ch to impute
test_nochdf <- test_noch[, c('EDU', 'LIV_AR', 'SUBJ_HHI')]
imptest_noch <- mice(test_nochdf, method='cart', seed=42)
comptest_noch <- complete(imptest_noch, 5)

# Create final imputed test_noch df
dummytest_noch <- test_noch[]
test_nochcl <- list(colnames(comptest_noch))
for (col in test_nochcl){
  dummytest_noch[col] <- comptest_noch[col]
}

#********** Write imputed df into csv **********
write.csv(dummytrain_ch,"csv_data/imptrain_ch.csv", row.names = FALSE)
write.csv(dummytest_ch,"csv_data/imptest_ch.csv", row.names = FALSE)
write.csv(dummytrain_noch,"csv_data/imptrain_noch.csv", row.names = FALSE)
write.csv(dummytest_noch,"csv_data/imptest_noch.csv", row.names = FALSE)

#********** Plot imputed values to see plausible values **********
train_chplot <- densityplot(imptrain_ch, data = ~ EDU+ LIV_AR + SUBJ_HHI, main='Train set with child')
test_chplot <- densityplot(imptest_ch, data = ~ EDU + LIV_AR + SUBJ_HHI, main='Test set with child')
train_nochplot <- densityplot(imptrain_noch, data = ~ EDU+ LIV_AR + SUBJ_HHI, main='Train set without child')
test_nochplot <- densityplot(imptest_noch, data = ~ EDU + LIV_AR + SUBJ_HHI, main='Test set without child')

par(mfrow=c(2, 3))
print(train_chplot, split=c(1,1,1,2), more=TRUE)
print(test_chplot, split=c(1,2,1,2), more=TRUE)

par(mfrow=c(2, 3))
print(train_nochplot, split=c(1,1,1,2), more=TRUE)
print(test_nochplot, split=c(1,2,1,2), more=TRUE)

# ?print.trellis

#********** Test code **********
# Impute dummy df 
md.pattern(edudf)
summary(edudf)
imp1 <- mice(edudf, method='cart', seed=42)
imp1$imp$SS
imp1$imp$UNI

#********** Sample steps to replace data with column from another df **********
a_val1 <- c(1,2,3,4,5)
a_val2 <- c(6,7,8,9,10)
a_val3 <- c(11,12,13,14,15)
a <- data.frame(a_val1, a_val2, a_val3)

a_val1 <- c(100,200,300,400,500)
a_val3 <- c(30,40,50,60,70)
b <- data.frame(a_val1, a_val3)

col_list1 <- list(colnames(b))

for (col in col_list1){
  a[col] <- b[col]
}

#********** Useful Code **********
# Check the rest of the null columns
colSums(is.na(dummytrain))

#********** Save Project **********
save.image('r_data/preprocess.RData')
