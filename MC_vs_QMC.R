#' Simulátor vısledkov zápasov Real Madrid a Barcelona
#'
#' @description Jednoduchı nástroj na vıpoèet pravdepodobnosti vısledku, v ktorom kadı tím skóroval
#'
#' @param simulations Celkovı poèet simulácii.
#' @param max.goals Maximálny oèakávanı poèet gólov, Default = 10.
#' @param prob_B Pravdepodobnos, e vyhrá Barcelona. Defaults to 0.5.
#' @param prob_RM Pravdepodobnos, e vyhrá Real Madrid. Defaults to 0.4.
#' @export
#'
#' @examples
#'
#' \dontrun{
#' goal_match_simulator( simulations = 100000,
#'                       max.goals = 15 )
#'
#'
#'
#' }
#'
#'
#'
#'








goal_match_simulator <- function( simulations = 10000,
                                  max.goals = 10,
                                  prob_B = 0.4,
                                  prob_RM = 0.35 )
{
  # vygeneruj vısledky zápasu - najprv generuj pravdepodobnos, e vyhrá jeden z tímov
  # vıber Barcelony ako prvej je úplne irelevantnı - nemá vplyv na vısledok
  Result <- rbinom(simulations, size = 1, prob = prob_B)
  # prekóduj vıhru Barcelony
  Result[Result == 1] <- "W_B"
  # zo zvysku vyber pozorovania ktore sa rovnaju 0 a vygeneruj pocet vyhier RM 
  # ktory sa rovná frakcii z nich, teda 
  Result[Result == 0] <-  rbinom(length(Result[Result == 0]), size = 1,
                                 prob = 1/prob_B*prob_RM)
  # nahrad 1 vyhrami RM
  Result[Result == 1] <- "W_RM"
  
  # dalej vies ze vyhry su generovane z rozlozenia ktore je podla zadania z prvej casti
  W_1 <- floor(runif(n = simulations, 0, 2.999)) 
  # a z druhej casti 
  W_2 <- ceiling(runif(n = simulations, 0, max.goals - 2)) + 2
  # a hodis si mincou aby si zistil v ktorej casti vlastne si
  W <- rbinom(n = simulations, size = 1, prob = 0.5)
  W <- W * W_1 + (1-W) * W_2
  
  # tzv je dane ze s pravdepodobnostou 50% to bude medzi 0-2
  # co robi prave floor runif(n,0,2,2.999)
  # a s pravdepodobnostou 50%
  # to bude viac az do maximalneho poctu golov
  # aby sme nepresiahli urcenu hranicu tak nastavime ze sa bude pripocitavat s pravdepodobnostou 
  # 50% max pocet golov - 2 (lebo 2 tam uz su)
  
  # uz len 2 problemy - situacie ked nepadol gol a niekto vyhral
  # a situacie kde bola remiza ale padol neparny pocet golov
  # tych sa musime zbavit - idealne tak ze tam padne gol, lebo to nam potom
  # nezmaze situacie kde skutocne nastala remiza
  while(sum(W == 0 & Result != 0) > 0){
    # vyberame iba tie ktore nevyhovuju cez index, je to pohodlnejsie
    selection_index <- which(W == 0 & Result != 0)
    
    W[selection_index] <- round(runif(n = length(selection_index), min = 0, max = 2)) + 
      rbinom(n = length(selection_index), size = 1, prob = 0.5) * 
      round(runif(n = length(selection_index), min = 0, max = max.goals-2)) 
  }  # tento loop bude bezat a premienat tie hodnoty az kym nebudu vsetky nevyhovujuce 
  # zmenene na novu vzorku
  
  
  # neparny pocet golov + remiza
  while(sum(W %% 2 != 0 & Result == 0) > 0){
    # vyberame iba tie ktore nevyhovuju cez index, je to pohodlnejsie
    selection_index <- which(W %% 2 != 0 & Result == 0)
    
    W[selection_index] <- round(runif(n = length(selection_index), min = 0, max = 2)) + 
      rbinom(n = length(selection_index), size = 1, prob = 0.5) * 
      round(runif(n = length(selection_index), min = 0, max = max.goals-2)) 
  }
  
  
  # potom priradis kazdemu timu goly podla toho ci vyhral 
  # najprv v remizach
  goals_B <- rep(1:simulations)
  goals_RM <- rep(1:simulations)
  
  goals_B[which(Result == "0")] <- W[which(Result == "0")]/2
  goals_RM[which(Result == "0")] <- goals_B[which(Result == "0")]
  
  # potom ked vyhrala Barcelona tak vyhra
  # urcite 1 bod + o nieco viac ako polku (zaokruhlene dole cez floor)
  goals_B[which(Result == "W_B")] <- 1 + floor(W[which(Result == "W_B")]*(runif(1)/2+0.5))
  # ie vyherny tim vyhral urcite polovicu bodov a potom z nich este aspon 1 + nieco
  goals_RM[which(Result == "W_B")] <- W[which(Result == "W_B")] - 
    goals_B[which(Result == "W_B")]
  # tim ktory ten dany zapas prehral potom dostal to co zostalo z bodov
  # analogicky pre RM
  goals_RM[which(Result == "W_RM")] <-  1 + floor(W[which(Result == "W_RM")]*(runif(1)/2+0.5))
  
  goals_B[which(Result == "W_RM")] <- W[which(Result == "W_RM")] - 
    goals_RM[which(Result == "W_RM")] 
  
  
  prob <- 1 - (( sum(goals_B == 0) + sum( goals_RM == 0 ) - 
                   sum( (goals_B == 0)*(goals_RM == 0)))/simulations)
  
  return( prob )
}




goal_match_simulator_QMC <- function( simulations = 10000,
                                      max.goals = 10,
                                      prob_B = 0.4,
                                      prob_RM = 0.35,
                                      skip = 2000,
                                      leap = 100 )
{
  # vygeneruj vısledky zápasu - najprv generuj pravdepodobnos, e vyhrá jeden z tímov
  # vıber Barcelony ako prvej je úplne irelevantnı - nemá vplyv na vısledok
  Result <- rbinom(simulations, size = 1, prob = prob_B)
  # prekóduj vıhru Barcelony
  Result[Result == 1] <- "W_B"
  # zo zvysku vyber pozorovania ktore sa rovnaju 0 a vygeneruj pocet vyhier RM 
  # ktory sa rovná frakcii z nich, teda 
  Result[Result == 0] <-  rbinom(length(Result[Result == 0]), size = 1,
                                 prob = 1/prob_B*prob_RM)
  # nahrad 1 vyhrami RM
  Result[Result == 1] <- "W_RM"
  
  # dalej vies ze vyhry su generovane z rozlozenia ktore je podla zadania z prvej casti
  p <- randtoolbox::halton( n = simulations*leap + skip )
  
  points <- p[(skip + 1):length(p)]
  points <- points[seq(1,length(points), by = leap)]
  
  W_1 <- floor(2.99999 * points ) 
  # a z druhej casti 
  p <- randtoolbox::halton( n = simulations*leap + skip )
  
  points <- p[(skip + 1):length(p)]
  points <- points[seq(1,length(points), by = leap)]
  W_2 <- ceiling((max.goals - 2) * points) + 2
  # a hodis si mincou aby si zistil v ktorej casti vlastne si
  W <- rbinom(n = simulations, size = 1, prob = 0.5)
  W <- W * W_1 + (1-W) * W_2
  
  # tzv je dane ze s pravdepodobnostou 50% to bude medzi 0-2
  # co robi prave floor runif(n,0,2,2.999)
  # a s pravdepodobnostou 50%
  # to bude viac az do maximalneho poctu golov
  # aby sme nepresiahli urcenu hranicu tak nastavime ze sa bude pripocitavat s pravdepodobnostou 
  # 50% max pocet golov - 2 (lebo 2 tam uz su)
  
  # uz len 2 problemy - situacie ked nepadol gol a niekto vyhral
  # a situacie kde bola remiza ale padol neparny pocet golov
  # tych sa musime zbavit - idealne tak ze tam padne gol, lebo to nam potom
  # nezmaze situacie kde skutocne nastala remiza
  while(sum(W == 0 & Result != 0) > 0){
    # vyberame iba tie ktore nevyhovuju cez index, je to pohodlnejsie
    selection_index <- which(W == 0 & Result != 0)
    p <- randtoolbox::halton( n = length(selection_index)*leap + skip )
    
    points <- p[(skip + 1):length(p)]
    points_1 <- points[seq(1,length(points), by = leap)]
    
    p <- randtoolbox::halton( n = length(selection_index)*leap + skip )
    
    points <- p[(skip + 1):length(p)]
    points_2 <- points[seq(1,length(points), by = leap)]
    
    W[selection_index] <- round(2*points_1) + 
      rbinom(n = length(selection_index), size = 1, prob = 0.5) * 
      round( (max.goals-2) * points_2 ) 
  }  # tento loop bude bezat a premienat tie hodnoty az kym nebudu vsetky nevyhovujuce 
  # zmenene na novu vzorku
  
  
  # neparny pocet golov + remiza
  while(sum(W %% 2 != 0 & Result == 0) > 0){
    # vyberame iba tie ktore nevyhovuju cez index, je to pohodlnejsie
    selection_index <- which(W %% 2 != 0 & Result == 0)
    
    p <- randtoolbox::halton( n = length(selection_index)*leap + skip )
    
    points <- p[(skip + 1):length(p)]
    points_1 <- points[seq(1,length(points), by = leap)]
    
    p <- randtoolbox::halton( n = length(selection_index)*leap + skip )
    
    points <- p[(skip + 1):length(p)]
    points_2 <- points[seq(1,length(points), by = leap)]
    
    W[selection_index] <- round(2* points_1 ) + 
      rbinom(n = length(selection_index), size = 1, prob = 0.5) * 
      round((max.goals-2)* points_2) 
  }
  
  
  # potom priradis kazdemu timu goly podla toho ci vyhral 
  # najprv v remizach
  goals_B <- rep(1:simulations)
  goals_RM <- rep(1:simulations)
  
  goals_B[which(Result == "0")] <- W[which(Result == "0")]/2
  goals_RM[which(Result == "0")] <- goals_B[which(Result == "0")]
  
  # potom ked vyhrala Barcelona tak vyhra
  # urcite 1 bod + o nieco viac ako polku (zaokruhlene dole cez floor)
  goals_B[which(Result == "W_B")] <- 1 + floor(W[which(Result == "W_B")]*(runif(1)/2+0.5))
  # ie vyherny tim vyhral urcite polovicu bodov a potom z nich este aspon 1 + nieco
  goals_RM[which(Result == "W_B")] <- W[which(Result == "W_B")] - 
    goals_B[which(Result == "W_B")]
  # tim ktory ten dany zapas prehral potom dostal to co zostalo z bodov
  # analogicky pre RM
  goals_RM[which(Result == "W_RM")] <-  1 + floor(W[which(Result == "W_RM")]*(runif(1)/2+0.5))
  
  goals_B[which(Result == "W_RM")] <- W[which(Result == "W_RM")] - 
    goals_RM[which(Result == "W_RM")] 
  
  
  prob <- 1 - (( sum(goals_B == 0) + sum( goals_RM == 0 ) - 
                   sum( (goals_B == 0)*(goals_RM == 0)))/simulations)
  
  return( prob )
}

res_MC <- list()
res_QMC <- list()


library(doFuture)

registerDoFuture()
plan(multiprocess)

res_QMC <- list()
res_QMC <- foreach( i = 41:100) %dopar% {
  k = 1000*i
  
  unlist(replicate(100, goal_match_simulator_QMC( simulations = k,
                                                  max.goals = 8,
                                                  prob_B = 0.4,
                                                  prob_RM = 0.35,
                                                  skip = 2000,
                                                  leap = 100)))
  
}

res_QMC <- c(res_QMC, new_res)

res_mean <- cbind( unlist(lapply(res_MC, mean)),
                   unlist(lapply(res_QMC, mean)))
res_mean <- data.frame(res_mean)
colnames(res_mean) <- c("MC","QMC")
xtable::xtable(res_mean)


res_sd <- cbind( unlist(lapply(res_MC, sd)),
                 unlist(lapply(res_QMC, sd)))


index <- seq(1:100)
rnd <- runif(1:100)
haltn <- randtoolbox::halton( n = 100 )


data_rnd <- data.frame(cbind(index, rnd))
colnames(data_rnd) <- c("Index","Hodnota")
data_hltn <- data.frame(cbind(index, haltn))
colnames(data_hltn) <- c("Index","Hodnota")


ggplot2::ggplot(data = data_rnd, ggplot2::aes(x = Index, y = Hodnota)) +
  ggplot2::geom_point(color = "Red") 

ggplot2::ggplot(data = data_hltn, ggplot2::aes(x = Index, y = Hodnota)) +
  ggplot2::geom_point(color = "blue") 


rnd <- runif(1:500)
haltn <- randtoolbox::halton( n = 500 )
ggplot2::ggplot(data = data.frame(rnd), ggplot2::aes( rnd)) +
  ggplot2::geom_density( color = "Red") 

ggplot2::ggplot(data = data.frame(haltn), ggplot2::aes(haltn)) +
  ggplot2::geom_density(color = "blue") 


haltn_rnd <- unlist(replicate(100, randtoolbox::halton( n = 1, usetime = TRUE )))
ggplot2::ggplot(data = data.frame(haltn_rnd), ggplot2::aes(haltn_rnd)) +
  ggplot2::geom_density(color = "black") 
