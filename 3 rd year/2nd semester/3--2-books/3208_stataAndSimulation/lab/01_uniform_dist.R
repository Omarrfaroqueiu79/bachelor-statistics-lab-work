# 
# 
# # x = seq(1,100,0.5)
# # x
# # y = dunif(x,0,1)
# # y
# # dunif(1.5,0,1)
# 
# 
# x = 0:4
# dunif(x,min = 1,max = 3)
# 
# 
# # ploting uniform distribution
# 
# plotunif <- function(x, min = 0, max = 1, lwd = 1, col = 1, ...) {
#   # Grid of X-axis values
#   if (missing(x)) {
#     x <- seq(min - 0.5, max + 0.5, 0.01)
#   }
#   if(max < min) {
#     stop("'min' must be lower than 'max'")
#   }
#   plot(x, dunif(x, min = min, max = max),
#        xlim = c(min - 0.25, max + 0.25), type = "l",
#        lty = 0, ylab = "f(x)", ...)
#   segments(min, 1/(max - min), max, 1/(max - min), col = col, lwd = lwd)
#   segments(min - 2, 0, min, 0, lwd = lwd, col = col)
#   segments(max, 0, max + 2, 0, lwd = lwd, col = col)
#   points(min, 1/(max - min), pch = 19, col = col)
#   points(max, 1/(max - min), pch = 19, col = col)
#   segments(min, 0, min, 1/(max - min), lty = 2, col = col, lwd = lwd)
#   segments(max, 0, max, 1/(max - min), lty = 2, col = col, lwd = lwd)
#   points(0, min, pch = 21, col = col, bg = "white")
#   points(max, min, pch = 21, col = col, bg = "white")
# }
# 
# plotunif(x)
x = runif(100,0,1)
missing(x)
plotunif <- function(x,min = 0,max = 1,lwd = 1,col = 1,...){
  if(missing(x)){
    x <- seq(min-.5,max+.5,0.01)
  }
  if(max <min){
    stop("min must be lower than max")
  }
  plot(x,dunif(x,min = min,max=max),xlim=c(min-.5,max+.5),type="l",lty=0,ylab="f(x)",...)
  segments(min,1/(max-min),max,1/(max-min),col=col,lwd=lwd)
  segments(min-2,0,min,0,lwd=lwd,col=col)
  segments(max,0,max+2,0,lwd=lwd,col=col)
  points(max,1/(max-min),pch=19,col=col)
  points(max,1/(max-min),pch=19,col=col)
  segments(min,0,min,1/(max-min),lty=2,col=col,lwd=lwd)
  segments(max,0,max,1/(max-min),lty=2,col=col,lwd=lwd)
  points(0,min,pch=21,col=col,bg="white")
  points(max,min,pch =21,col=col,bg="white")
}
plotunif(min=0,max=1,lwd=2,col=4,main="Uniform PDF")
## uniform area
unif_area <- function(min=0,max=1,lb,ub,col=1,acholor="lightgray",...){
  x <- seq(min-.25*max,max+.25*max,0.001)
  if(missing(lb)){
    lb <- min(x)
  }
  if(missing(ub)){
    ub <- max(x)
  }
  if(max < min){
    stop("Min must be lower than max")
  }
  x2 <- seq(lb,ub,length = 1000)
  plot(x,dunif(x,min,max),xlim=c(min-.25*max,max+.25*max),type="l",ylab="f(x)",lty=0,...)
  y <- dunif(x2,min,max)
  polygon(c(lb,x2,ub),c(0,y,0),col=acolor,lty=0)
  segments(min, 1/(max - min), max, 1/(max - min), lwd = 2, col = col)
  segments(min - 2 * max, 0, min, 0, lwd = 2, col = col)
  segments(max, 0, max + 2 * max, 0, lwd = 2, col = col)
  points(min, 1/(max - min), pch = 19, col = col)
  points(max, 1/(max - min), pch = 19, col = col)
  segments(min, 0, min, 1/(max - min), lty = 2, col = col, lwd = 2)
  segments(max, 0, max, 1/(max - min), lty = 2, col = col, lwd = 2)
  points(0, min, pch = 21, col = col, bg = "white")
  points(max, min, pch = 21, col = col, bg = "white")
}

unif_area(min = 0, max = 1, lb = 0, ub = 0.5,
          main = "punif(0.5)", acolor = "white")

unif_area <- function(min = 0, max = 1, lb, ub, col = 1,
                      acolor = "lightgray", ...) {
  x <- seq(min - 0.25 * max, max + 0.25 * max, 0.001)
  if (missing(lb)) {
    lb <- min(x)
  }
  if (missing(ub)) {
    ub <- max(x)
  }
  if(max < min) {
    stop("'min' must be lower than 'max'")
  }
  x2 <- seq(lb, ub, length = 1000)
  plot(x, dunif(x, min = min, max = max),
       xlim = c(min - 0.25 * max, max + 0.25 * max), type = "l",
       ylab = "f(x)", lty = 0, ...)
  y <- dunif(x2, min = min, max = max)
  polygon(c(lb, x2, ub), c(0, y, 0), col = acolor, lty = 0)
  segments(min, 1/(max - min), max, 1/(max - min), lwd = 2, col = col)
  segments(min - 2 * max, 0, min, 0, lwd = 2, col = col)
  segments(max, 0, max + 2 * max, 0, lwd = 2, col = col)
  points(min, 1/(max - min), pch = 19, col = col)
  points(max, 1/(max - min), pch = 19, col = col)
  segments(min, 0, min, 1/(max - min), lty = 2, col = col, lwd = 2)
  segments(max, 0, max, 1/(max - min), lty = 2, col = col, lwd = 2)
  points(0, min, pch = 21, col = col, bg = "white")
  points(max, min, pch = 21, col = col, bg = "white")
}
unif_area(min = 0, max = 1, lb = 0, ub = 0.5,
          main = "punif(0.5)", acolor = "white")

















