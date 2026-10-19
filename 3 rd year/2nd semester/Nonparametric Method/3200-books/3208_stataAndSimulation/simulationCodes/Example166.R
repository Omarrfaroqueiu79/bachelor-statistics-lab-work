# Example 16.3: Suppose a baseball player hits a home run once in every 10 times at bat on average. Suppose he gets exactly two "at bats" in every game. Using simulation , estimate the likelihood that the player 
# will hit 2 home runs in a single game.

set.seed(125)
Num <- rep(0,1000)
for(i in 1:1000){
  x <- sample(0:9,1)
  y <- sample(0:9,1)
  out <- 10*y+x
  if(out==2){
    Num[i]<-1
  }else{
    Num[i]<-0
  }
}

Estimate <- sum(Num)/1000
Estimate

# Or
set.seed(125)
Num <-rep(0,1000)
for(i in 1:1000){
  x <- sample(0:9,1)
  y <- sample(0:9,1)
  Num[i]<- 10*y+x
}
Estimate <- sum(Num==2)/1000
Estimate
