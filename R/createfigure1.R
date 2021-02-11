#' createfigure1 function
#' Title:createfigure1
#' This function allows you to convert the numeric dataframe to cumulative frequency figure.
#' @param l2 Enter the dataframe which is numeric class. And the information of group is listed last.
#' @param l3 ENter the data  is the group information which is characteritic class.
#' @param main, xlab and ylab were characteristic type, which were use to create the title and axis names.
#' @keywords  createfigure1
#' @export
#' @examples
#' x1 <- rnorm(20,4,6);x2 <- rep(c("DCB","DES"),each=10);x1 <- data.frame(x1,x2);createfigure1(x1,x2, main="Late loss distribution",xlab="Late loss (mm)",ylab = "cumulative frequency (%)")
#' @author Yongfeng Shi
#' @title createfigure1
#' Author:Yongfeng Shi
#' Version: .0.0.9000
#' Maintainer: yongfeng shi <shiyf@jlu.edu.cn>
#' @encoding UTF-8
#'

createfigure1 <- function(l2,l3,main="main",xlab="x",ylab="y"){
  r <- colnames(l2)
  ncol <- length(r)-1
  x <- data.frame(l2)
  y <- as.character(l3)
  z <- data.frame(table(y))
  rownames(z) <- z[,1]
  legend1 <- rownames(z)
  e <- length(legend1)
  max <- round(max(x[(1:ncol)]))+1
  min <- round(min(x[(1:ncol)]))-1
  i <- 1
  m <- c("gray","red","blue","purple","brown","black","yellow")
  w <- matrix(1:length(legend1)*ncol,ncol =length(legend1),nrow = ncol,byrow =F)
  w <- t(w)
  for (i in (1:length(legend1))) {
    j <- 1
    for (j in (1:ncol)) {
      w[i,j] <- paste(legend1[i],r[j])

    }


  }
  a <- data.frame()
  opar <- par(no.readonly = T)
  par(lwd=4,cex=1,font.lab=2)
  p <- nrow(z)
  i <- 1
  for (i in 1:p)
  {a <-x[y==rownames(z)[i],]
  n <- 1
  q <- ncol(a)
  for (n in 1:(q-1))
  {
    q1 <- a[,n]
    count <- table(cut(q1,breaks = seq((min(q1)),max,by=0.001)))

    pcount <- prop.table(count)*100
    cumsum <- cumsum(pcount)
    x1 <- seq((min(q1)),max,by=0.001)
    x1 <- x1[c(1:length(cumsum))]

    if (i==1&n==1){
      plot(x1,cumsum,type = "l",lty=i,col="gray",xlab=xlab,ylab=ylab,main=main,bty="n",
           xaxt="n",yaxt="n")
      axis(side = 1,at=c(seq(from=min,to=max,by=0.5)),labels = c(seq(from=min,to=max,by=0.5)))
      axis(side = 2,at=c(seq(0,100,by=10)),labels =c(seq(0,100,by=10)))
    }

    else {
      points(x1,cumsum,type="l",lty=i,col=m[n])}

  }

  }

  legend("topleft",legend=w,lty = c(1:e),col =rep( m[1:ncol],each=e),cex = 1,bty = "n")

}








