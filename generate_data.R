## Generates some total junk items
#
# generate.dichotomous.data = function(N, J, J_star){
#
#   good_J = ceiling(J*J_star)
#   rest_J = J - good_J
#
#   theta = rnorm(N)
#
#   alphas = rep(1, good_J)
#
#   betas = as.list(qnorm(seq(0.05, 0.95, length = good_J)))
#
#   itemscores <- matrix(c(0), nrow=N, ncol=good_J)
#
#   P = list()
#
#   for(m in 1:good_J){
#     bt = betas[[m]]
#     bt = c(0, bt)
#
#     m_j = length(bt)
#
#     inside <- matrix(c(0), nrow=N, ncol=m_j)
#     for(i in 1:m_j){
#       inside[,i] <- as.numeric(alphas[m])*(theta-bt[i])
#     }
#
#     cumul <- matrix(c(0), nrow=N, ncol=m_j)
#     for(i in 1:length(theta)){
#       cumul[i,] <- cumsum(inside[i,])
#     }
#
#     nums <- exp(cumul)
#     P[[m]] <- nums/rowSums(nums)
#
#     for(k in 1:N)
#       itemscores[k,m] <-sample(1:m_j, 1, replace=T, prob=P[[m]][k,])
#   }
#
#   rest_scores = as.data.frame(matrix(rbinom(rest_J*N, 1, p = 0.5), nrow = N))
#   validation = as.data.frame(as.numeric(theta > 1))
#   good_data = as.data.frame(itemscores) - 1
#
#   my_data = cbind(good_data, rest_scores, validation)
#   names(my_data) = c(paste("V", 1:J, sep = ""), "outcome")
#
#   return(list(data = my_data[,1:J], outcome = my_data$outcome))
# }
#
# N = 1000
# J = 20
# J_star = 0.3
#
# temp = generate.dichotomous.data(N, J, J_star)
#
#
CalculateSensitivity = function(est, true_test){
  true_pos = sum(est == 1 & true_test == 1)
  false_neg = sum(est == 0 & true_test == 1)
  sensitivity = true_pos/(true_pos + false_neg)
  sensitivity
}

CalculateSpecificity = function(est, true_test){
  true_neg = sum(est == 0 & true_test == 0)
  false_pos = sum(est == 1 & true_test == 0)
  specificity = true_neg/(true_neg + false_pos)
  specificity
}
#
# ss = rowSums(temp[,1:ceiling(J*J_star)])
# threshold = floor(pnorm(1)*J*J_star)
# estimated_classifications = as.numeric(ss >= threshold)
#
# CalculateSensitivity(estimated_classifications, temp$outcome)
# CalculateSpecificity(estimated_classifications, temp$outcome)

## Creates items with low alpha for a bunch of them
create.dichotomous.items2 = function(theta, betas, alphas){
  J =  length(alphas)
  K = length(theta)

  itemscores <- matrix(c(0), nrow=K, ncol=J)
  P = list()

  for(m in 1:J){
    bt = betas[[m]]
    bt = c(0, bt)

    m_j = length(bt)

    inside <- matrix(c(0), nrow=K, ncol=m_j)
    for(i in 1:m_j){
      inside[,i] <- as.numeric(alphas[m])*(theta-bt[i])
    }

    cumul <- matrix(c(0), nrow=K, ncol=m_j)
    for(i in 1:length(theta)){
      cumul[i,] <- cumsum(inside[i,])
    }

    nums <- exp(cumul)
    P[[m]] <- nums/rowSums(nums)

    for(k in 1:K)
      itemscores[k,m] <-sample(1:m_j, 1, replace=T, prob=P[[m]][k,])
  }

  data <- as.data.frame(itemscores)

  list(data.frame = data, P=P)
}

N = 1000
J = 20
J_star = 0.3
theta = rnorm(N)
betas = sample(qnorm(seq(0.05, 0.95, length = J)))
list_betas = as.list(betas)
alphas = c(rep(1,ceiling(J*J_star)), rep(0.2, J-ceiling(J*J_star)))
data = create.dichotomous.items2(theta, list_betas, alphas)$data.frame - 1

ss = rowSums(data[,1:ceiling(J*J_star)])
threshold = floor(pnorm(1)*J*J_star)
estimated_classifications = as.numeric(ss >= threshold)
outcome = theta > 0
#
# CalculateSensitivity(estimated_classifications, outcome)
# CalculateSpecificity(estimated_classifications, outcome)
#
#
# ss2 = rowSums(data)
# estimated_classifications2 = as.numeric(ss2 >= floor(pnorm(0)*J))
#
# CalculateSensitivity(estimated_classifications2, outcome)
# CalculateSpecificity(estimated_classifications2, outcome)

temp_data = cbind(data, as.numeric(outcome))
names(temp_data)[21] = "outcome"

write.csv(temp_data, file = "test_data.csv", row.names = FALSE)



GenerateSimulationData <- function(N, J, J_star, name_stub, path){
  theta = rnorm(N)
  betas = sample(qnorm(seq(0.05, 0.95, length = J)))
  list_betas = as.list(betas)
  alphas = c(rep(1,ceiling(J*J_star)), rep(0.2, J-ceiling(J*J_star)))
  data = create.dichotomous.items2(theta, list_betas, alphas)$data.frame - 1

  ss = rowSums(data[,1:ceiling(J*J_star)])
  threshold = floor(pnorm(1)*J*J_star)
  estimated_classifications = as.numeric(ss >= threshold)
  outcome = theta > 0

  temp_data = cbind(data, as.numeric(outcome))
  temp_full = list(theta=theta, betas=betas, alphas=alphas, data=data)
  names(temp_data)[21] = "outcome"

  name_stub <- paste0(name_stub, '_',
                      as.character(N), '_',
                      as.character(J), '_',
                      as.character(round(J_star,2)))

  full_path <- paste0(path, 'params/', name_stub, '_full.rds')
  data_path <- paste0(path, 'data/', name_stub, '.csv')
  saveRDS(temp_full, file=full_path)
  write.csv(temp_data, file = data_path, row.names = FALSE)
}

path <- '~/projects/mcmc-short-form/'

N <- 1000
J <- 20
J_stars <- c(1/3,1/2,2/3)
reps <- 100

set.seed(8675309)

for (J_star in J_stars){
  for (rep in 1:reps){
    stub <- paste0('simulation-A_', as.character(rep))
    GenerateSimulationData(N, J, J_star, stub, path)
  }
}




