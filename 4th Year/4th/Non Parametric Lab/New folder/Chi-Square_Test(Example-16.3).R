

Categories = c("Brown","Yellow","Orange","Green","Coffee","Total")
observed = c(224,119,130,48,59)
assumed = round((observed/sum(observed)),1)
expected = c(sum(observed)*assumed)
expected
#expected = c(580*assumed)
#expected

assumed=c(assumed,sum(assumed))
assumed
expected=c(expected,sum(expected))
expected
observed=c(observed,sum(observed))
observed


Table=data.frame(
           Categories= Categories,
           Observed=observed,
           Assumed=assumed,
           Expected=expected)
Table


Critical_val = qchisq(.05,4,lower.tail = F)
Critical_val

Critical_val = qchisq(1-.05,4)
Critical_val


