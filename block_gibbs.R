#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'

gibbs_sampler <- function( Y,
                           X,
                           iterations = 200000,
                           burn_in = 20000,
                           trim_samples = 50,
                           init_val = 1,
                           phi_pars = c(0.7,1000))
{
  # set initial values for the sampler and create initial objects
  X_set <- solve( t(X) %*% X )
  phi <- rep( 0, iterations )
  B <- matrix( nrow = iterations, ncol = ncol(X))
  # initial phi value to start sampler
  phi[1] <- init_val
  # gibbs sampling
  for(i in 2:iterations ){

    B[i,] <- mvtnorm::rmvnorm( n = 1,
                               mean = c( (X_set %*% t(X)) %*% Y ),
                               sigma = phi[ i - 1 ] * X_set )


    phi[i] <- invgamma::rinvgamma( n = 1,
                                   shape = ( nrow(X)/2 + phi_pars[1] ),
                                   rate =
                                     .5*( t(Y - X %*% B[i,]) %*%
                                            (Y - X %*% B[i,])) + phi_pars[2] )

  }


  # burn_in, trim
  keep_draws <- seq( burn_in , iterations, trim_samples )
  phi <- phi[ keep_draws ]
  B <- B[ keep_draws, ]



  joint_post <- data.frame( B = B, phi = phi,
                            kept_draws = keep_draws )
  colnames(joint_post) <- c(paste(colnames(X)),"phi", "keep_draws")
return(joint_post)
}



QMC_gibbs_sampler <- function( Y,
                           X,
                           iterations = 200000,
                           burn_in = 20000,
                           trim_samples = 50,
                           init_val = 1,
                           phi_pars = c(0.7,1000))
{
  # set initial values for the sampler and create initial objects
  X_set <- solve( t(X) %*% X )
  phi <- rep( 0, iterations )
  B <- matrix( nrow = iterations, ncol = ncol(X))
  dist_mean <- c( (X_set %*% t(X)) %*% Y )
  # initial phi value to start sampler
  phi[1] <- init_val
  # gibbs sampling
  rng_flag <- TRUE
  init_flag <- TRUE
for(i in 2:iterations ){
    current_sigma <- phi[ i - 1 ] * X_set

    
    B[i,] <- rmvnorm_qmc( 1,
                          mean = dist_mean,
                          sigma = current_sigma, 
                          time_rng = rng_flag,
                          reinit = init_flag)

    phi[i] <- 1/( rgamma_qmc_2( 1,
                                shape = ( nrow(X)/2 + phi_pars[1] ),
                                scale = (.5*( t(Y - X %*% B[i,]) %*%
                                (Y - X %*% B[i,])) + phi_pars[2]),
                  time_rng = rng_flag, 
                  reinit = init_flag))

  rng_flag <- FALSE
  init_flag <- FALSE
  }
  # burn_in, trim
  keep_draws <- seq( burn_in , iterations, trim_samples )
  phi <- phi[ keep_draws ]
  B <- B[ keep_draws, ]


  # joint_post <- data.frame( B = B, phi = phi,
  #                           kept_draws = keep_draws)

  joint_post <- data.frame( B = B, phi = phi,
                            kept_draws = keep_draws)
  colnames(joint_post) <- c(paste(colnames(X)),"phi", "keep_draws")
  return(joint_post)
}

cpu <- data.table::fread( paste( getwd(),
                                 "/machine.data",
                                 sep = ""))

colnames(cpu) <- c( "vendor", "model", "myct", "mmim", 
                    "mmax", "cache","chmin","chmax",
                    "prp", "erp")
cpu_original <- cpu[,10]
cpu <- cpu[,1:9]
cpu_mm <- model.matrix(prp~., cpu)


# baseline using glmnet (because lm will willingly throw an error, and thanks to
# model matrix we are suddenly in a p>n situation)
try <- glmnet::cv.glmnet(x = cpu_mm, y = unlist(cpu[,9]))

predict(try, cpu_mm)


# lets just not use the model matrix and restrict ourselves to things unrelated to 
# producer or model
# MC integration - vanilla gibbs sampling
gibbs <- gibbs_sampler( Y = unlist(cpu[,9]),
                        X = as.matrix( cpu[,-c(1,2,9)], ncol = 6),
                        burn_in = 5000,
                        iterations = 50000 )
# QMC integration - not so vanilla gibbs sampling
gibbs_qmc <- QMC_gibbs_sampler( Y = unlist(cpu[,9]),
                          X = as.matrix( cpu[,-c(1,2,9)], ncol = 6),
                          burn_in = 5000,
                          iterations = 50000 )

# generate all means of resulting chains after some burnin 
unlist(lapply(gibbs[,1:6], mean))
unlist(lapply(gibbs_qmc[,1:6], mean))
# find the difference between vanilla and QMC
unlist(lapply(gibbs[,1:6], mean)) - unlist(lapply(gibbs_qmc[,1:6], mean))
# and ratios of variance
unlist(lapply(gibbs[,1:6], var))
unlist(lapply(gibbs_qmc[,1:6], var))
unlist(lapply(gibbs[,1:6], var))/unlist(lapply(gibbs_qmc[,1:6], var)) 


plottable <- rbind(unlist(lapply(gibbs[,1:6], mean)),
                   unlist(lapply(gibbs_qmc[,1:6], mean)),
                   (unlist(lapply(gibbs[,1:6], mean)) -
                      unlist(lapply(gibbs_qmc[,1:6], mean))))
rownames(plottable) <- c("MC", "QMC", "Rozdiel")
colnames(plottable) <- paste("Koef. ", colnames(plottable), sep = "")
xtable::xtable(plottable)

plottable <- rbind(unlist(lapply(gibbs[,1:6], var)),
                   unlist(lapply(gibbs_qmc[,1:6], var)),
                   (unlist(lapply(gibbs[,1:6], var))/
                      unlist(lapply(gibbs_qmc[,1:6], var))))
rownames(plottable) <- c("MC", "QMC", "N�sobok")
colnames(plottable) <- paste("Var. ", colnames(plottable), sep = "")
xtable::xtable(plottable)



