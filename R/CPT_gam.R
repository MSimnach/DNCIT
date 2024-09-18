#Conditional permutation test based on
# Berrett, T. B., Wang, Y., Barber, R. F., & Samworth, R. J. (2020).
# The conditional permutation test for independence while controlling for confounders.
# Journal of the Royal Statistical Society Series B: Statistical Methodology, 82(1), 175-197.
generate_X_CPT_gaussian = function(nstep,M,X0,mu,sig2){
  # Runs the conditional permutation test using the distribution X | Z=Z[i] ~ N(mu[i],sig2[i])
  log_lik_mat = -(X0^2)%*%t(1/2/sig2) + X0%*%t(mu/sig2)
  # log_lik_mat[i,j] = density at X=X0[i] when Z=Z[j]
  Pi_mat = generate_X_CPT(nstep,M,log_lik_mat)
  X_mat = X0[Pi_mat]
  dim(X_mat) = c(M,length(X0))
  return(t(X_mat))
}

generate_X_CPT = function(nstep,M,log_lik_mat,Pi_init=NULL){
  # log_lik_mat is the n-by-n matrix with entries log(q(X_i|Z_j))
  # this function produces M exchangeable permutations, initialized with permutation Pi_init
  n = dim(log_lik_mat)[1]
  if(length(Pi_init)==0){
    Pi_init = 1:n
  }
  Pi_ = generate_X_CPT_MC(nstep,log_lik_mat,Pi_init)
  Pi_mat = matrix(0,M,n)
  for(m in 1:M){
    Pi_mat[m,] = generate_X_CPT_MC(nstep,log_lik_mat,Pi_)
  }
  return(Pi_mat)
}

generate_X_CPT_MC = function(nstep,log_lik_mat,Pi_){
  # log_lik_mat is the n-by-n matrix with entries log(q(X_i|Z_j))
  # this function runs the MC sampler, initialized with permutation Pi_
  n = length(Pi_)
  npair = floor(n/2)
  for(istep in 1:nstep){
    perm = sample(n)
    inds_i = perm[1:npair]
    inds_j = perm[(npair+1):(2*npair)]
    # for each k=1,...,npair, deciding whether to swap Pi_[inds_i[k]] with Pi_[inds_j[k]]
    log_odds = (log_lik_mat[cbind(Pi_[inds_i],inds_j)] + log_lik_mat[cbind(Pi_[inds_j],inds_i)]
                - log_lik_mat[cbind(Pi_[inds_i],inds_i)] - log_lik_mat[cbind(Pi_[inds_j],inds_j)])
    swaps = stats::rbinom(npair,1,1/(1+exp(-pmax(-500,log_odds))))
    Pi_[c(inds_i,inds_j)] = Pi_[c(inds_i,inds_j)] + swaps*(Pi_[c(inds_j,inds_i)]-Pi_[c(inds_i,inds_j)])
  }
  return(Pi_)
}


cpt_gam <- function(Y, Z, model.formula.YZ, M, nstep){
  YZ <- cbind(Y,Z)
  #conditional mean and variance via gam for y_i|z_i
  colnames(YZ) <- paste('V', 1:ncol(YZ), sep='')
  gam_formula <- stats::formula(model.formula.YZ)
  gam.XYZ <- mgcv::gam(gam_formula, data = as.data.frame(YZ), method='REML')
  gam.mean <- gam.XYZ$fit
  gam.var <- mgcv::predict.gam(gam.XYZ, se.fit=TRUE)$se.fit
  #CPT Y
  Y.CPT <- generate_X_CPT_gaussian(nstep, M, Y, gam.mean, gam.var)
}
