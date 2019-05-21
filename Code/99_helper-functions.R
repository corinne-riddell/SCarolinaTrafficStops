#' Round all columns with numerical values in a data frame
#' 
#' A function to round all columns with numerical values in a data frame
#' 
#' @details All numbers are rounded in decimal (e.g., 0.003), not scientific, format
#' 
#' @param df a data frame with some numeric columns. Required
#' @param digits the number of digits in decimal part. Defauld: 3
#'
#' @return the data frame with rounded numbers
#' @export
#' @examples
#' \dontrun{
#' mtx <- data.frame(a = c("a", "b", "c"), b = c(0.123456, 1.2345678, 2.34), c = c(5.678e-3, 3.4567890, 6.789^3))
#' mtx
#' round_df(mtx)
#' }
#' @note Source: \url{https://stackoverflow.com/questions/9063889/how-to-round-a-data-frame-in-r-that-contains-some-character-variables}
##
round_df <- function(df, digits = 3) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
  
  df[,nums] <- round(df[,nums], digits = digits)
  
  (df)
}

#' Calculate the marginal number of stops, and marginal RRs
#' 
#' A function to be called by boot() to calculate bootstrapped estimates and CIs
#' 
#' @param formula a formula for the model you would like to run
#' @param data a data frame on which you will run the model
#' @param indices the indices chosen by the bootstrap resample
#' 
#' @return a vector with 11 terms. The first three are the marginal number of stops before the policy change to White, Black, and Hispanic drivers (in that order), the next three terms are the marginal number of stops after the policy change to these drivers (same order), followed by the RR associated with the policy change in White, Black, and Hispanic drivers, and the additional change to the risk for Black vs. White drivers, and for Hispanic vs. White drivers.
##
bs <- function(formula, data, indices){
  d <- data[indices,] # allows boot to select sample 
  fit <- glm(formula, data = d, family = "quasipoisson")
  #return(coef(fit)) 
  d$preds <- predict(fit, type = "response", newdata = d %>% mutate(race = "White", post_policy = 0) )
  mean_bs_white0 <- mean(d$preds)
  
  # predict number of stops after SET race == "Hispanic" and SET post_policy == 0:
  d$preds <- predict(fit, type = "response", newdata = d %>% mutate(race = "Hispanic", post_policy = 0) )
  mean_bs_hisp0 <- mean(d$preds)
  
  # predict number of stops after SET race == "Black" and SET post_policy == 0:
  d$preds <- predict(fit, type = "response", newdata = d %>% mutate(race = "Black", post_policy = 0) )
  mean_bs_black0 <- mean(d$preds)
  
  # predict number of stops after SET race == "White" and SET post_policy == 1:
  d$preds <- predict(fit, type = "response", newdata = d %>% mutate(race = "White", post_policy = 1) )
  mean_bs_white1 <- mean(d$preds)
  
  # predict number of stops after SET race == "Hispanic" and SET post_policy == 1:
  d$preds <- predict(fit, type = "response", newdata = d %>% mutate(race = "Hispanic", post_policy = 1) )
  mean_bs_hisp1 <- mean(d$preds)
  
  # predict number of stops after SET race == "Black" and SET post_policy == 1:
  d$preds <- predict(fit, type = "response", newdata = d %>% mutate(race = "Black", post_policy = 1) )
  mean_bs_black1 <- mean(d$preds)
  
  #relative increase in the risk of policy change in black vs. white drivers
  rr_white <- mean_bs_white1/mean_bs_white0
  rr_black <- mean_bs_black1/mean_bs_black0
  add_increase_black <- rr_black/rr_white
  
  rr_hisp <- mean_bs_hisp1/mean_bs_hisp0
  add_increase_hisp <- rr_hisp/rr_white
  
  return(c(mean_bs_white0, mean_bs_black0, mean_bs_hisp0,
           mean_bs_white1, mean_bs_black1, mean_bs_hisp1,
           rr_white, rr_black, rr_hisp, add_increase_black, add_increase_hisp)) 
}

bs_logistic <- function(formula, data, indices){
  d <- data[indices,] # allows boot to select sample 
  fit <- glm(formula, data = d, family = "binomial")
  #return(coef(fit)) 
  d$preds <- predict(fit, type = "response", newdata = d %>% mutate(race = "White", post_policy = 0) )
  mean_bs_white0 <- mean(d$preds)
  
  # predict number of stops after SET race == "Hispanic" and SET post_policy == 0:
  d$preds <- predict(fit, type = "response", newdata = d %>% mutate(race = "Hispanic", post_policy = 0) )
  mean_bs_hisp0 <- mean(d$preds)
  
  # predict number of stops after SET race == "Black" and SET post_policy == 0:
  d$preds <- predict(fit, type = "response", newdata = d %>% mutate(race = "Black", post_policy = 0) )
  mean_bs_black0 <- mean(d$preds)
  
  # predict number of stops after SET race == "White" and SET post_policy == 1:
  d$preds <- predict(fit, type = "response", newdata = d %>% mutate(race = "White", post_policy = 1) )
  mean_bs_white1 <- mean(d$preds)
  
  # predict number of stops after SET race == "Hispanic" and SET post_policy == 1:
  d$preds <- predict(fit, type = "response", newdata = d %>% mutate(race = "Hispanic", post_policy = 1) )
  mean_bs_hisp1 <- mean(d$preds)
  
  # predict number of stops after SET race == "Black" and SET post_policy == 1:
  d$preds <- predict(fit, type = "response", newdata = d %>% mutate(race = "Black", post_policy = 1) )
  mean_bs_black1 <- mean(d$preds)
  
  #relative increase in the risk of policy change in black vs. white drivers
  rr_white <- mean_bs_white1/mean_bs_white0
  rr_black <- mean_bs_black1/mean_bs_black0
  add_increase_black <- rr_black/rr_white
  
  rr_hisp <- mean_bs_hisp1/mean_bs_hisp0
  add_increase_hisp <- rr_hisp/rr_white
  
  return(c(mean_bs_white0, mean_bs_black0, mean_bs_hisp0,
           mean_bs_white1, mean_bs_black1, mean_bs_hisp1,
           rr_white, rr_black, rr_hisp, add_increase_black, add_increase_hisp)) 
}
