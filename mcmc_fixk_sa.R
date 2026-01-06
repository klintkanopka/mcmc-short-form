
#setwd('/scratch/projects/kanopkalab/mcmc-short-form')

files <- list.files("data/")

library(tidyverse)
library(parallel)
library(purrr)

# iterate through data files
for(f in files){

  path <- paste0("data/", f)
  test <- read.csv(path)

  y <- test$outcome
  num_items <- ncol(test)-1
  resp <- test[,1:num_items]

  # calculate sensitivity
  CalculateSensitivity <-  function(y_hat, y){
    true_pos <-  sum(y_hat == 1 & y == 1)
    false_neg <-  sum(y_hat == 0 & y == 1)
    sensitivity <-  true_pos/(true_pos + false_neg)
    return(sensitivity)
  }

  # calculate specificity
  CalculateSpecificity <-  function(y_hat, y){
    true_neg <-  sum(y_hat == 0 & y == 0)
    false_pos <-  sum(y_hat == 1 & y == 0)
    specificity <-  true_neg/(true_neg + false_pos)
    return(specificity)
  }

  # calculate accuracy
  CalculateAccuracy <- function(y_hat, y){
    accuracy <- mean(y_hat == y)
    return(accuracy)
  }

  # calculate balanced weighted accuracy
  CalculateBalancedAccuracy <- function(y_hat, y, q){
    bw_acc <- q*CalculateSensitivity(y_hat, y) + (1-q)*CalculateSpecificity(y_hat, y)
    return(bw_acc)
  }

  # finding cut score that corresponds to the best bw accuracy
  # returns best cut score, best bw accuracy, and other metrics
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

  # wrapper function to evaluate a short form
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

  # propose a new short form: randomly swap an item
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

  # parallelized mcmc with simulated annealing
  mcmc_fixed_k <- function(iter, y, resp, k, q = 0.5, nchains = 3) {

    require(parallel)

    mcmc <- function(iter, y, resp, k, q){

      num_items <- ncol(resp)
      current_short_form <- sample(c(rep(1, k), rep(0, num_items - k)))
      current_evals <- evaluate_short_form(y, resp, current_short_form, q)
      current_bw_accuracy <- current_evals$bw_accuracy

      best_evals <- current_evals
      best_bw_accuracy <- current_bw_accuracy

      bw_accuracies <- vector('numeric', length=iter)

      for (i in 1:iter) {

        # swap
        proposed_short_form <- ProposeSwap(current_short_form)

        # evaluate proposed short form
        proposed_evals <- evaluate_short_form(y, resp, proposed_short_form, q)
        proposed_bw_accuracy <- proposed_evals$bw_accuracy

        ratio <- exp(log(proposed_bw_accuracy) - log(current_bw_accuracy))

        # determine whether to accept proposed short form
        if (ratio > 1 | runif(1) < ratio*0.001) {

          current_short_form <- proposed_short_form
          current_bw_accuracy <- proposed_bw_accuracy
          current_evals <- proposed_evals

          if (proposed_bw_accuracy > best_bw_accuracy) {
            best_bw_accuracy <- proposed_bw_accuracy
            best_evals <- proposed_evals
          }

        }

        bw_accuracies[i] <- current_bw_accuracy

      }

      out <- as.data.frame(best_evals) |>
        pivot_wider(id_cols = c(cut_score, bw_accuracy, accuracy, sensitivity, specificity),
                    values_from = short_form,
                    names_prefix = 'item_',
                    names_from = name)
      return(out)

    }

    cl <- makeCluster(nchains)
    clusterExport(cl, c("CalculateSpecificity", "CalculateSensitivity",
                        "CalculateAccuracy", "best_cut_score","ProposeSwap", "pivot_wider",
                        "evaluate_short_form", "CalculateBalancedAccuracy"))

    control <- rep(iter, nchains)


    results <- parLapply(cl, control,fun = mcmc,
                         y = y, resp = resp, k = k, q = q)

    stopCluster(cl)

    results <- list_rbind(results)
    results <- results |>
      mutate(prop_agree = mean(bw_accuracy == max(bw_accuracy))) |>
      arrange(-bw_accuracy) |>
      head(1) |>
      mutate(k = k, q = q)


    return(results)

  }

  iter <-  1e3

  # format output file path
  format_outpath <- function(file, output){

    t <- strsplit(substr(file, 1, nchar(file)-4), split = "_")[[1]]

    out <- paste(c(t[1:4], as.character(output$k),
                    t[5],as.character(round(output$q,2))), collapse = "_")

    outpath <- paste0("output/", out, ".RDS")

    return(outpath)

  }

  # running mcmc with different q values
  output1 <- mcmc_fixed_k(y, resp, k = 3, iter = iter, q=0.5, nchains=5)
  saveRDS(output1, file = format_outpath(f, output1))

  output2 <- mcmc_fixed_k(y, resp, k = 3, iter = iter, q=1/3, nchains=5)
  saveRDS(output2, file = format_outpath(f, output2))

  output3 <- mcmc_fixed_k(y, resp, k = 3, iter = iter, q=2/3, nchains=5)
  saveRDS(output3, file = format_outpath(f, output3))

}





