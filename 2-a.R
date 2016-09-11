training = read.csv("training.csv")[, 2:39]
test = read.csv("test.csv")[, 2:35]
samples_training = read.table("samples.txt", header = TRUE)[1:38, ]
samples_test = read.table("samples.txt", header = TRUE)[39:72, ]

n1 = sum(samples_training$Class == "ALL")
n2 = sum(samples_training$Class == "AML")
s1 = apply(training[which(samples_training$Class == "ALL")], 1, var)
s2 = apply(training[which(samples_training$Class == "AML")], 1, var)

mean_class1 = apply(training[which(samples_training$Class == "ALL")], 1, mean)
mean_class2 = apply(training[which(samples_training$Class == "AML")], 1, mean)
sample_sd = (((n1-1) * s1 + (n2-1) * s2) / (n1+n2-2))^0.5

# sol1
#result_class1 = 0
#for(j in 1:p) {
#  result_class1 = result_class1 + dnorm(as.numeric(test[j, ]), m1[j], sample_sd[j], log = TRUE)
#}

#result_class2 = 0
#for(j in 1:p) {
#  result_class2 = result_class2 + dnorm(as.numeric(test[j, ]), m2[j], sample_sd[j], log = TRUE)  
#}

# sol2 
#result_class1 = 0
#for(j in 1:34) {
#  result_class1[j] = sum(dnorm(as.numeric(test[, j]), mean_class1, sample_sd, log = TRUE))
#}
#[1] -81963.25

#result_class2 = 0
#for(j in 1:34) {
#  result_class2[j] = sum(dnorm(as.numeric(test[, j]), mean_class2, sample_sd, log = TRUE))
#}

# test - f2
norm_class1 = (apply(test, 2, dnorm, m = mean_class1, sd = sample_sd, log = TRUE))
result_class1 = apply(norm_class1, 2, sum)
norm_class2 = (apply(test, 2, dnorm, m = mean_class2, sd = sample_sd, log = TRUE))
result_class2 = apply(norm_class2, 2, sum)

validcase_class1 = sum(samples_test$Class[which(result_class1 > result_class2)] == "ALL")
validcase_class2 = sum(samples_test$Class[which(result_class1 <= result_class2)] == "AML")

# predicted class labels
predicted_test_class = ifelse(result_class1 > result_class2, "ALL", "AML")
#table(predicted_test_class)

# evaluate
evaluation = ifelse(predicted_test_class == samples_test[, 2], "Correct", "Wrong")
table(evaluation)