# how to generate random gamma using QMC

  
rgamma_qmc <- function( shape, scale ){  
  alpha <- shape
  theta <- scale
  if(alpha > 1){
    d<- alpha - 1/3
    c <- 1/sqrt(9*d)
    flag <- 1
    while(flag == 1){
      Z <- randtoolbox::halton( n = 1, dim = 1, usetime = TRUE, normal = TRUE)
      if( Z > (-1/c) ){
        V <- (1 + c * Z)^3
        U <- randtoolbox::halton( n = 1, dim = 1, usetime = TRUE )
        flag <- log(U) > (((0.5*Z)^2) +d -(d*V) +(d*log(V))) 
      }
    }
    x <- d * (V/theta)
  }
  else{
    x <-  rgamma_qmc(alpha+1,theta)
    x <- x*randtoolbox::halton( n = 1, dim = 1, usetime = TRUE)^(1/alpha)
  }
  return(x)
}


rgamma_qmc_2 <- function(n = 1, shape, scale , reinit = FALSE, time_rng = FALSE){
  res_vec <- rep(NA, n)
  Z_vec <- randtoolbox::halton( n, dim = 1, normal = TRUE,
                                usetime = time_rng, init = reinit)
  U_vec <- randtoolbox::halton( n, dim = 1,
                                usetime = time_rng, init = reinit )
  final_x_mult <- randtoolbox::halton( n, dim = 1,
                                       usetime = time_rng,
                                       init = reinit )
  alpha <- shape
  theta <- scale
  for(i in 1:n){
    if(alpha > 1)
      {
      d<- alpha - 1/3
      c <- 1/sqrt(9*d)
      flag <- 1
      iter_counter <- 0
        while(flag == 1)
        {
          Z <- Z_vec[i]
          iter_counter <- iter_counter + 1
            if( Z > (-1/c) )
            {
            V <- (1 + c * Z)^3
            U <- U_vec[i]
            flag <- log(U) > (((0.5*Z)^2) +d -(d*V) +(d*log(V))) 
            }
              if(iter_counter > 10000)
              {
                Z <- randtoolbox::halton( n, dim = 1, normal = TRUE)
              }
              if(iter_counter > 100000)
              {
                break;
              }
        }
        x <- d * (V/theta)
      }
      else
      {
        x <-  rgamma_qmc(alpha+1,theta)
        x <- x*final_x_mult[i]^(1/alpha)
      }
    res_vec[i] <- x
  }  
return(res_vec)
}


  

rgamma_2 <- function( n = 1, shape, scale ){
  res_vec <- rep(NA, n)
  for(i in 1:n){
    alpha <- shape
    theta <- scale
    if(alpha > 1){
      d<- alpha - 1/3
      c <- 1/sqrt(9*d)
      flag <- 1
      while(flag == 1){
        Z <- rnorm(1)
        if( Z > (-1/c) ){
          V <- (1 + c * Z)^3
          U <- runif(1)
          flag <- log(U) > (((0.5*Z)^2) +d -(d*V) +(d*log(V))) 
        }
      }
      x <- d * (V/theta)
    }
    else{
      x <-  rgamma_qmc(alpha+1,theta)
      x <- x*runif(1)^(1/alpha)
    }
    res_vec[i] <- x
  }  
  return(res_vec) 
}

  