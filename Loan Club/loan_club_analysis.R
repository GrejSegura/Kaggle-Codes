#LOAN CLUB ANALYSIS FOR KAGGLE
loan <- read.csv(file.choose(), header = T , sep = ",")

####THIS PART IS THE CONVERSION OF CATEGORICAL TO BINOMIAL VARIABLES
#convert loan_status to categorical --- Fully Paid is 1 , 0 otherwise
loanstat <- ifelse(loan$loan_status == "Fully Paid", 1, 0) ###this is the predicted variable

#convert term 36 months = 1 and 60 months = 0
loanterm <- ifelse(loan$term == " 36 months", 1, 0)

#convert verification_status, not verified = 1
loanver <- ifelse(loan$verification_status == "Not Verified", 1, 0)

#convert home ownership, OWN = 1
loanown <- ifelse(loan$home_ownership == "OWN", 1, 0)

#convert emp_years
loanemp <- ifelse(loan$emp_length == "10+ years", 1, 0)

#create new data frame
loan3 <- as.data.frame(cbind(loan, loanstat, loanterm, loanver, loanown, loanemp))

#cleaup the dataframe - remove unused columns
#this is a sample
loan3 <- loan3[,-c(8:74)]
loan3 <- loan3[,-c(1:2,4:6)]
View(loan3)

#Scale the loan_amnt and int_rate to centered values
scaleloan <- scale(cbind(loan3$loan_amnt, loan3$int_rate), center =T, scale = T)
loan3 <- as.data.frame(cbind(scaleloan, loan3[,-(1:2)]))
View(loan3)

#change the column names back to original
names(loan3)[1] <- "loan_amnt"
names(loan3)[2] <- "int_rate"
View(loan3)


##create data frames for train and test
index <- sample(nrow(loan3), round(nrow(loan3)*.7))

trainloan <- loan3[index,]
trainloan <- as.data.frame(cbind(trainloan[,-3],trainloan$loanstat))
names(trainloan)[7] <- "loanstat"

testloan <- loan3[-index,-3]
View(trainloan)
View(testloan)

###WE WILL USE MULTIPLE METHODS TO ANALYZE THE EFFECTS OF THE INDEPENDENT VARIABLES

### 1 FIRST WE USE LOGISTIC REGRESSION
set.seed(1)

logitmodel <- glm(loanstat~., family = binomial(link = "logit"), data = trainloan)

#TEST USING THE TESTLOAN DATA -- type = "response" means results
# should be in probabilities, if type ="link" then log odds

predtest <- predict.glm(logitmodel, newdata = testloan, type = "response")

#convert predtest to 0 and 1, 1 if probability is > 0.5
predicted <- ifelse(predtest >0.5, 1, 0)

##check the result
##this is a sample
predicted[1:12]

### 2 USING RANDOM FOREST METHOD
library(rpart)

forestmodel <- rpart(loanstat~loan_amnt+int_rate+loanterm+loanver+loanown
                     +loanemp,data=trainloan)
plot(forestmodel)
text(forestmodel, use.n = TRUE)
print(forestmodel)

### 3 USE NEURALNET with 2 hidden layers with 4 and 3 vertices
library(neuralnet)

nnmodel <- neuralnet(loanstat~loan_amnt+int_rate+loanterm+loanver+loanown
                     +loanemp,data=trainloan, hidden = c(4,3,2))

print(nnmodel)

plot(nnmodel)

prednn <- predict(nnmodel, testloan)

### 4 USE SUPPORT VECTOR MACHINE
library(e1071)

svmodel <- svm(loanstat~., data=trainloan)
