

Categories = c("Brown","Yellow","Orange","Green","Coffee","Total")
observed = c(224,119,130,48,59)
observed
assumed = round((observed/sum(observed)),1)
assumed
expected = c(sum(observed)*assumed)
expected
#expected = c(580*assumed)
#expected

Q4=sum((observed-expected)^2/expected)
Q4


Critical_val = qchisq(.05,4,lower.tail = F)
Critical_val

Critical_val = qchisq(1-.05,4)
Critical_val


