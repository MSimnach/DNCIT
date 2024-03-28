## KPC graph knn
kpc_graph <- function(X, Y, Z, k, Knn, model.formula.YZ, alpha = 0.05, M = 30, nstep = 50){
  Y.CPT <- cpt_gam(Y,Z, model.formula.YZ, alpha = alpha, M = M, nstep = nstep)
  T.graph <- KPC::KPCgraph(Y=X,Z=Y, X=Z, k=k, Knn=Knn)
  T.graph.vec <- rep(100000, M)
  for (j in 1:M){
    T.graph.vec[j] <- KPC::KPCgraph(Y=X, Z=Y.CPT[,j], X=Z, k=k, Knn=Knn)
  }
  res <- (1 + sum(T.graph.vec >= T.graph)) / (1+M)
  return(res)
}
