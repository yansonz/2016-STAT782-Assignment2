training = read.csv("training.csv")[, 2:39]
test = read.csv("test.csv")[, 2:35]
test_withID = read.csv("test.csv")
samples_training = read.table("samples.txt", header = TRUE)[1:38, ]
samples_test = read.table("samples.txt", header = TRUE)[39:72, ]

n1 = sum(samples_training$Class == "ALL")
n2 = sum(samples_training$Class == "AML")
s1 = apply(training[which(samples_training$Class == "ALL")], 1, var)
s2 = apply(training[which(samples_training$Class == "AML")], 1, var)

mean_class1 = apply(training[which(samples_training$Class == "ALL")], 1, mean)
mean_class2 = apply(training[which(samples_training$Class == "AML")], 1, mean)
sample_sd = (((n1-1) * s1 + (n2-1) * s2) / (n1+n2-2))^0.5
total_mean = apply(training, 1, mean)

m_class1 = (1/n1 - 1/(n1+n2))^0.5
m_class2 = (1/n2 - 1/(n1+n2))^0.5

d_class1 = (mean_class1 - total_mean) / (m_class1 * sample_sd)
d_class2 = (mean_class2 - total_mean) / (m_class2 * sample_sd)

lamda = 6
x = abs(d_class1) - lamda
shrunken_d_class1 = sign(d_class1) *  ifelse(x > 0, x, 0) 
x = abs(d_class2) - lamda
shrunken_d_class2 = sign(d_class2) *  ifelse(x > 0, x, 0) 

dummy_class1 = which(shrunken_d_class1 == 0)
dummy_class2 = which(shrunken_d_class2 == 0)
nopower_index = intersect(dummy_class1, dummy_class2)
power_index = setdiff(1:7129, nopower_index)

shrunken_m_class1 = total_mean + m_class1 * sample_sd * shrunken_d_class1
shrunken_m_class2 = total_mean + m_class2 * sample_sd * shrunken_d_class2

#p = nrow(test) #7129
# classifier#A
#result_class1 = 0
#for(j in 1:p) {
#  if(j %in% nopower_index) next
#  result_class1 = result_class1 + dnorm(as.numeric(test[j, ]), shrunken_m_class1[j], sample_sd[j], log = TRUE)
#}
norm_class1 = apply(test[power_index, ], 2, dnorm, m = shrunken_m_class1[power_index], sd = sample_sd[power_index], log = TRUE)
result_class1 = apply(norm_class1, 2, sum)

#norm_class2 = (apply(test, 2, dnorm, m = mean_class2, sd = sample_sd, log = TRUE))
#result_class2 = apply(norm_class2, 2, sum)

# classifier#B
#result_class2 = 0
#for(j in 1:p) {
#  if(j %in% nopower_index) next
#  result_class2 = result_class2 + dnorm(as.numeric(test[j, ]), shrunken_m_class2[j], sample_sd[j], log = TRUE)  
#}
norm_class2 = apply(test[power_index, ], 2, dnorm, m = shrunken_m_class2[power_index], sd = sample_sd[power_index], log = TRUE)
result_class2 = apply(norm_class2, 2, sum)

#validcase_class1 = sum(samples_test$Class[which(result_class1 > result_class2)] == "ALL")
#validcase_class2 = sum(samples_test$Class[which(result_class1 <= result_class2)] == "AML")

# predict class labels
predicted_test_class = ifelse(result_class1 > result_class2, "ALL", "AML")
test_result = as.data.frame(matrix(c(names(test), predicted_test_class), ncol = 2))
colnames(test_result) = c("Sample", "Predicted Result")
test_result

# evaluate
evaluation = ifelse(predicted_test_class == samples_test[, 2], "Correct", "Wrong")
table(evaluation)

# Genes having Predictive power
as.data.frame(test_withID$Gene.Accession.Number[power_index])