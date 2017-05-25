##summary description of loan club data
##and visualization using ggplot2


summary(loan3$loan_amnt)

library(ggplot2)

#plot comparison between fully paid and non-paid loans
g <- ggplot(loan3, aes(as.factor(loanstat))) + geom_bar()
g1 <- g + labs(title = "Loan Status Comparison", x = "Status", y = "Number of Loans")
# this is not functioning --- g1 <- scale_x_manual(labels = "Non-Paid", "Fully Paid")
g1

l <- ggplot(loan3,aes(loan_amnt)) + geom_density()
l
