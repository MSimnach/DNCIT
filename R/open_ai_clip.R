r_open_ai_clip <- function(X,Y,Z){
  res <- list()
  res$p <- fcit$fcit$test(X, Y, Z)
  res$Stat <- -1
  return(res)
}
