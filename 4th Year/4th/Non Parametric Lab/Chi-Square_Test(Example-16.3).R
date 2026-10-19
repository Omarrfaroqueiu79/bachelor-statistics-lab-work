Categories = c("Brown","Yellow","Orange","Green","Coffee","Total")
observed = c(224,119,130,48,59)
observed
assumed = round((observed/sum(observed)),1)
assumed
expected = c(sum(observed)*assumed)
expected
#expected = c(580*assumed)
#expected

f_assumed=c(assumed,sum(assumed))
f_assumed
f_expected=c(expected,sum(expected))
f_expected
f_observed=c(observed,sum(observed))
f_observed


Table=data.frame(
           Categories= Categories,
           Observed=f_observed,
           Assumed=f_assumed,
           Expected=f_expected)
Table

Q4=sum((observed-expected)^2/expected)
Q4


Critical_val = qchisq(.05,4,lower.tail = F)
Critical_val

Critical_val = qchisq(1-.05,4)
Critical_val


