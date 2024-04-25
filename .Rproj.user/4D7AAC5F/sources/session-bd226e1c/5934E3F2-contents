#' @param x1,x2 data vectors
#' @param B number of bootstrap samples
#' @param type can be "two-sided", "less" (for H0: sigma_1 < sigma_2), or "greater".
bootstrap_equal_var <- function(x1, x2, B=10000, type="two-sided"){
  C1 <- C2 <- 0
  n1 <- length(x1)
  n2 <- length(x2)
  for(i in 1:B){
    ind1 <- sample(n1,n1,TRUE)
    xx1 <- x1[ind1]
    S_boot1 <- sd(xx1)

    ind2 <- sample(n2,n2,TRUE)
    xx2 <- x2[ind2]
    S_boot2 <- sd(xx2)

    if(S_boot1 < S_boot2){
      C1 <- C1 + 1
    }else{
      C2 <- C2 + 1
    }
  }
  if(type=="two-sided"){
    p_val <- 2*min(C1, C2)/B
  }
  if(type=="less"){
    p_val <- C2/B
  }
  if(type=="greater"){
    p_val <- C1/B
  }
  return(p_val)
}
