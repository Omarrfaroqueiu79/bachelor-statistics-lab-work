##__________________16_3____________________
observed=c(224,119,130,48,59)
Assumed_Pi=c(.4,.2,.2,.1,.1)
expected=Assumed_Pi*sum(observed)
expected

data.frame(observed,Assumed_Pi,expected)

chi_sqr=sum((observed-expected)^2/expected)
chi_sqr

qchisq(.05,4, lower.tail = FALSE)


