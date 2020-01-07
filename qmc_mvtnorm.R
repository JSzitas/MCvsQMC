# quasi random multivariate normal distribution

rmvnorm_qmc <- function( n,
                         mean = rep(0, nrow(sigma)),
                         sigma = diag(length(mean)),
                         method = "eigen", # other options include chol, svd
                         time_rng = FALSE,
                         reinit = FALSE )
{
  
  checkmate::assert_true(isSymmetric(sigma, tol = sqrt(.Machine$double.eps),
                                     check.attributes = FALSE))
  checkmate::assert_set_equal(length(mean), nrow(sigma))
  

  if(method == "eigen"){
  res <- eigen(sigma, symmetric = TRUE)
    if (!all(res$values >= -sqrt(.Machine$double.eps) * abs(res$values[1]))){
      warning("sigma is numerically not positive semidefinite")
       }
  res <- t(res$vectors %*% (t(res$vectors) * sqrt(pmax(res$values, 0))))
  }
  else if(method == "svd"){
    res <- svd(sigma)
    if (!all(res$d >= -sqrt(.Machine$double.eps) * abs(res$d[1]))){
      warning("sigma is numerically not positive semidefinite")
    }
    res <- t(res$v %*% (t(res$u) * sqrt(pmax(res$d, 0))))
  }
  else if(method == "chol"){
    res <- chol(sigma, pivot = TRUE)
    res[, order(attr(res, "pivot"))]
  }
  
  res <- matrix(randtoolbox::halton( n, dim = ncol(sigma), normal = TRUE,
                                     usetime = time_rng, init = reinit),
                   nrow = n ) %*%  res
  res <- sweep(res, 2, mean, "+")
  colnames(res) <- names(mean)
  return(res)
}