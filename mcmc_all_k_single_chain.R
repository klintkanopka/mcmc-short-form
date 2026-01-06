path <- '~/Dropbox/ShortForming\ MCMC/data/simulation-A_7_1000_20_0.5.csv'
test <- read.csv(path)

y <- test$outcome
num_items <- ncol(test)-1
resp <- test[,1:num_items]



best_cut_score <- function(sf_score, y, k, q) {
  best_cut <- 0
  best_bw_accuracy <- 0

  for (cut_score in 1:k) {
    y_hat <- sf_score >= cut_score
    bw_accuracy <- CalculateBalancedAccuracy(y_hat, y, q)

    if (bw_accuracy > best_bw_accuracy) {
      best_bw_accuracy <- bw_accuracy
      best_cut <- cut_score
    }
  }

  accuracy <- CalculateAccuracy(sf_score >= best_cut, y)
  sensitivity <- CalculateSensitivity(sf_score >= best_cut, y)
  specificity <- CalculateSpecificity(sf_score >= best_cut, y)

  return(list(cut_score = best_cut,
              bw_accuracy = best_bw_accuracy,
              accuracy = accuracy,
              sensitivity = sensitivity,
              specificity = specificity))
}

CalculateSensitivity <-  function(y_hat, y){
  true_pos <-  sum(y_hat == 1 & y == 1)
  false_neg <-  sum(y_hat == 0 & y == 1)
  sensitivity <-  true_pos/(true_pos + false_neg)
  return(sensitivity)
}

CalculateSpecificity <-  function(y_hat, y){
  true_neg <-  sum(y_hat == 0 & y == 0)
  false_pos <-  sum(y_hat == 1 & y == 0)
  specificity <-  true_neg/(true_neg + false_pos)
  return(specificity)
}

CalculateAccuracy <- function(y_hat, y){
  accuracy <- mean(y_hat == y)
  return(accuracy)
}

CalculateBalancedAccuracy <- function(y_hat, y, q){
  bw_acc <- q*CalculateSensitivity(y_hat, y) + (1-q)*CalculateSpecificity(y_hat, y)
  return(bw_acc)
}


ProposeSwap <- function(sf){

  # find 1s and 0s indices
  ones <- which(sf == 1)
  zeros <- which(sf == 0)

  # sample two indices for swapping
  swap_idx <- c(sample(zeros, 1), sample(ones, 1))

  # swap the letters at selected indices
  sf[swap_idx] <- sf[c(swap_idx[2], swap_idx[1])]

  return(sf)
}


ProposeSwapFloatK <- function(sf){
  idx <- sample(1:length(sf),1)
  if (sf[idx]) sf[idx] <- 0 else sf[idx] <- 1
  return(sf)
}


evaluate_short_form <- function(y, resp, short_form, q){

  k <- sum(short_form)
  sf_score <- colSums(short_form * t(resp))

  find_cut <- best_cut_score(sf_score, y, k, q)

  cut_score <- find_cut$cut_score
  bw_accuracy <- find_cut$bw_accuracy
  accuracy <- find_cut$accuracy
  sensitivity <- find_cut$sensitivity
  specificity <- find_cut$specificity

  return(list(short_form = short_form,
              cut_score = cut_score,
              bw_accuracy = bw_accuracy,
              accuracy = accuracy,
              sensitivity = sensitivity,
              specificity = specificity,
              name = 1:length(short_form)))

}

mcmc <- function(iter, y, resp, q){

  num_items <- ncol(resp)
  current_short_form <- sample(c(0,1), num_items, replace=TRUE)
  current_evals <- evaluate_short_form(y, resp, current_short_form, q)
  current_bw_accuracy <- current_evals$bw_accuracy

  best_evals <- current_evals
  best_bw_accuracy <- current_bw_accuracy

  bw_accuracies <- vector('numeric', length=iter)
  k_seq <- vector('numeric', length=iter)

  step <- 0

  while (step<iter) {

    # swap
    proposed_short_form <- ProposeSwapFloatK(current_short_form)

    # evaluate proposed short form
    proposed_evals <- evaluate_short_form(y, resp, proposed_short_form, q)
    proposed_bw_accuracy <- proposed_evals$bw_accuracy

    ratio <- exp(log(proposed_bw_accuracy) - log(current_bw_accuracy))
    lambda <- exp(-9*step/iter)

    if (ratio>1 | runif(1) < ratio*lambda) {
      step <- step + 1
      current_short_form <- proposed_short_form
      current_bw_accuracy <- proposed_bw_accuracy
      current_evals <- proposed_evals

      if (proposed_bw_accuracy > best_bw_accuracy) {
        best_bw_accuracy <- proposed_bw_accuracy
        best_evals <- proposed_evals
      }

      bw_accuracies[step] <- current_bw_accuracy
      k_seq[step] <- sum(current_short_form)
    }

  }

  out_stats <- as.data.frame(best_evals) |>
    pivot_wider(id_cols = c(cut_score, bw_accuracy, accuracy, sensitivity, specificity),
                values_from = short_form,
                names_prefix = 'item_',
                names_from = name)

  out <- list(wba=bw_accuracies, k_seq=k_seq, out_stats=out_stats, best_evals=best_evals)
  return(out)

}

iter <- 5e2

system.time(out_1 <- mcmc(iter, y, resp, 0.5))
system.time(out_2 <- mcmc(iter, y, resp, 0.5))
system.time(out_3 <- mcmc(iter, y, resp, 0.5))
system.time(out_4 <- mcmc(iter, y, resp, 0.5))
system.time(out_5 <- mcmc(iter, y, resp, 0.5))

results <- data.frame(iter = rep(1:iter, times=5),
                      chain = rep(1:5, each=iter),
                      k = c(out_1$k_seq,
                            out_2$k_seq,
                            out_3$k_seq,
                            out_4$k_seq,
                            out_5$k_seq),
                      wba = c(out_1$wba,
                              out_2$wba,
                              out_3$wba,
                              out_4$wba,
                              out_5$wba))


ggplot(results, aes(x=iter, y=k, color=as.factor(chain))) +
  geom_line() +
  facet_grid(chain~.) +
  theme_minimal()

ggplot(results, aes(x=iter, y=wba, color=as.factor(chain))) +
  geom_line() +
  theme_minimal()

ggplot(results, aes(x=iter, y=wba, color=as.factor(k))) +
  geom_point() +
  theme_minimal()

ggplot(results, aes(x=as.factor(k), y=wba, fill=as.factor(chain))) +
  geom_boxplot() +
  theme_minimal()

bind_rows(out_1$out_stats,
          out_2$out_stats,
          out_3$out_stats,
          out_4$out_stats,
          out_5$out_stats)

