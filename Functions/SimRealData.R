# continuous outcome
simData.realData <- function(n.tree, n.test, seed) {
  
  set.seed(seed)
  n.samples <- n.tree + n.test
  
  # length of baseline covariates: 15
  data.realData <- data.frame(W_1_0 = rbinom(n.samples, 1, 0.5))
  
  # multivariate normal, mean 0, variance 1, covariance/off-diagonal 0.2
  lth.W.0   <- 15-1
  sigma.W.0 <- diag(lth.W.0)
  sigma.W.0 <- sigma.W.0 + 0.2
  sigma.W.0 <- sigma.W.0 - 0.2 * diag(lth.W.0)
  W.0 <- rmvnorm(n.samples, mean = rep(0, lth.W.0), sigma = sigma.W.0)
  W.0 <- data.frame(W.0)
  colnames(W.0) <- paste0("W_", 2:(2+lth.W.0-1), "_0")
  
  data.realData <- cbind(data.realData, W.0)
  colnames(data.realData)[1:2] <- c("W_2_0", "W_1_0") 
  data.realData <- data.realData %>%
    select(W_1_0, W_2_0, W_3_0:W_15_0)
  
  # A(0)
  data.realData <- data.realData %>%
    mutate(prob_A_0 = 1 / (1 + exp(-(-0.5 + 0.2 * W_1_0 + 0.2 * W_2_0 + 0.4 * W_3_0 + 0.5 * W_4_0)))) %>%
    mutate(A_0      = rbinom(n.samples, size = 1, prob = prob_A_0))
  
  # table(data.realData$A_0)
  
  # probability of event at time 1
  data.realData <- data.realData %>%
    mutate(prob_C_a1_0 = 1 / (1 + exp(-(-4 + 0.8 * 1 + 0.3 * W_1_0 - 0.3 * W_2_0 - 0.3 * W_3_0 + 0.1 * W_4_0))),
           prob_C_a0_0 = 1 / (1 + exp(-(-4 + 0.8 * 0 + 0.3 * W_1_0 - 0.3 * W_2_0 - 0.3 * W_3_0 + 0.1 * W_4_0)))) %>%
    mutate(C_a1_0 = rbinom(n.samples, size = 1, prob = prob_C_a1_0),
           C_a0_0 = rbinom(n.samples, size = 1, prob = prob_C_a0_0)) %>%
    mutate(C_0 = ifelse(A_0 == 1, C_a1_0, C_a0_0)) %>%
    mutate(C_0 = BinaryToCensoring(is.censored = C_0)) %>%
    mutate(Y_a1c0_1 = -3 + 0.1 * 1 + 0.3 * W_1_0 - 2 * 1 * (W_2_0 > 0.5) + 2 * W_4_0 + 2 * W_5_0 + rnorm(n.samples, mean = 0, sd = 1),
           Y_a0c0_1 = -3 + 0.1 * 0 + 0.3 * W_1_0 - 2 * 0 * (W_2_0 > 0.5) + 2 * W_4_0 + 2 * W_5_0 + rnorm(n.samples, mean = 0, sd = 1)) %>%
    mutate(Y_1 = ifelse(A_0 == 1, Y_a1c0_1, Y_a0c0_1)) %>%
    mutate(Y_1 = ifelse(C_0 == "censored", NA, Y_1))
  
  latent.data.realData <- data.realData %>%
    select(prob_A_0, prob_C_a1_0, prob_C_a0_0, C_a1_0, C_a0_0, Y_a1c0_1, Y_a0c0_1)
  data.realData <- data.realData %>%
    select(-prob_A_0, -prob_C_a1_0, -prob_C_a0_0, -C_a1_0, -C_a0_0, -Y_a1c0_1, -Y_a0c0_1)
  
  # table(data.realData$A_0, data.realData$C_0)
  
  #################
  #### time 2 #####
  #################
  data.realData <- data.realData %>%
    mutate(W_4_a1c0_1 = 0.2 * 1 + 0.5 * W_1_0 - 0.4 * W_2_0 - 0.4 * W_3_0 + 0.5 * W_4_0 - 0.5 * W_5_0 + rnorm(n.samples, mean = 0, sd = 0.4),
           W_4_a0c0_1 = 0.2 * 0 + 0.5 * W_1_0 - 0.4 * W_2_0 - 0.4 * W_3_0 + 0.5 * W_4_0 - 0.5 * W_5_0 + rnorm(n.samples, mean = 0, sd = 0.4)) %>%
    mutate(W_5_a1c0_1 = 0.1 * 1 + 0.1 * W_1_0 + 0.1 * W_2_0 - 0.4 * W_3_0 + 0.5 * W_4_a1c0_1 - 0.5 * W_5_0 + rnorm(n.samples, mean = 0, sd = 0.4),
           W_5_a0c0_1 = 0.1 * 0 + 0.1 * W_1_0 + 0.1 * W_2_0 - 0.4 * W_3_0 + 0.5 * W_4_a0c0_1 - 0.5 * W_5_0 + rnorm(n.samples, mean = 0, sd = 0.4)) %>%
    mutate(W_4_1 = ifelse(A_0 == 1, W_4_a1c0_1, W_4_a0c0_1),
           W_5_1 = ifelse(A_0 == 1, W_5_a1c0_1, W_5_a0c0_1))
  
  lth.W.1   <- 8 - 2
  sigma.W.1 <- diag(lth.W.1)
  sigma.W.1 <- sigma.W.1 + 0.2
  sigma.W.1 <- sigma.W.1 - 0.2 * diag(lth.W.1)
  W.1 <- rmvnorm(n.samples, mean = rep(0, lth.W.1), sigma = sigma.W.1)
  W.1 <- data.frame(W.1)
  colnames(W.1) <- paste0("W_", 6:(6+8-2-1), "_1")
  
  data.realData <- cbind(data.realData, W.1)
  
  data.realData <- data.realData %>%
    mutate(prob_A_a1c0_1 = 1 / (1 + exp(-(-1 + 0.1 * W_1_0 + 0.1 * W_2_0 + 0.2 * W_3_0 + 0.2 * W_4_0 - 1 * W_4_a1c0_1 - 0.5 * W_5_a1c0_1))),
           prob_A_a0c0_1 = 1 / (1 + exp(-(-1 + 0.1 * W_1_0 + 0.1 * W_2_0 + 0.2 * W_3_0 + 0.2 * W_4_0 - 1 * W_4_a0c0_1 - 0.5 * W_5_a0c0_1)))) %>%
    mutate(A_a1c0_1      = rbinom(n.samples, size = 1, prob = prob_A_a1c0_1),
           A_a0c0_1      = rbinom(n.samples, size = 1, prob = prob_A_a0c0_1)) %>% 
    mutate(A_1 = ifelse(A_0 == 1, A_a1c0_1, A_a0c0_1))
  
  data.realData <- data.realData %>%
    mutate(prob_C_a11c0_1 = 1 / (1 + exp(-(-4 + 0.3 * 1 + 0.5 * 1 + 0.3 * W_1_0 - 0.3 * W_2_0 - 0.3 * W_3_0 + 0.1 * W_4_a1c0_1 + 0.1 * W_5_0))),
           prob_C_a10c0_1 = 1 / (1 + exp(-(-4 + 0.3 * 1 + 0.5 * 0 + 0.3 * W_1_0 - 0.3 * W_2_0 - 0.3 * W_3_0 + 0.1 * W_4_a1c0_1 + 0.1 * W_5_0))),
           prob_C_a01c0_1 = 1 / (1 + exp(-(-4 + 0.3 * 0 + 0.5 * 1 + 0.3 * W_1_0 - 0.3 * W_2_0 - 0.3 * W_3_0 + 0.1 * W_4_a0c0_1 + 0.1 * W_5_0))),
           prob_C_a00c0_1 = 1 / (1 + exp(-(-4 + 0.3 * 0 + 0.5 * 0 + 0.3 * W_1_0 - 0.3 * W_2_0 - 0.3 * W_3_0 + 0.1 * W_4_a0c0_1 + 0.1 * W_5_0)))) %>%
    mutate(C_a11c0_1 = rbinom(n.samples, size = 1, prob = prob_C_a11c0_1),
           C_a10c0_1 = rbinom(n.samples, size = 1, prob = prob_C_a10c0_1),
           C_a01c0_1 = rbinom(n.samples, size = 1, prob = prob_C_a01c0_1),
           C_a00c0_1 = rbinom(n.samples, size = 1, prob = prob_C_a00c0_1)) %>%   
    mutate(C_1 = ifelse((A_0 == 1) & (A_1 == 1), C_a11c0_1, NA)) %>%
    mutate(C_1 = ifelse((A_0 == 1) & (A_1 == 0), C_a10c0_1, C_1)) %>%
    mutate(C_1 = ifelse((A_0 == 0) & (A_1 == 1), C_a01c0_1, C_1)) %>%
    mutate(C_1 = ifelse((A_0 == 0) & (A_1 == 0), C_a00c0_1, C_1)) %>%
    mutate(Y_a11c0_2 = -2 + 0.1 * 1 + 0.1 * 1 + 0.3 * W_1_0 - 2 * 1 * (W_2_0 > 0.5) - 2 * 1 * (W_2_0 > 0.5) - 0.3 * W_3_0 + 2 * W_4_a1c0_1 + 2 * W_5_a1c0_1 + rnorm(n.samples, mean = 0, sd = 1),
           Y_a10c0_2 = -2 + 0.1 * 1 + 0.1 * 0 + 0.3 * W_1_0 - 2 * 1 * (W_2_0 > 0.5) - 2 * 0 * (W_2_0 > 0.5) - 0.3 * W_3_0 + 2 * W_4_a1c0_1 + 2 * W_5_a1c0_1 + rnorm(n.samples, mean = 0, sd = 1),
           Y_a01c0_2 = -2 + 0.1 * 0 + 0.1 * 1 + 0.3 * W_1_0 - 2 * 0 * (W_2_0 > 0.5) - 2 * 1 * (W_2_0 > 0.5) - 0.3 * W_3_0 + 2 * W_4_a0c0_1 + 2 * W_5_a0c0_1 + rnorm(n.samples, mean = 0, sd = 1),
           Y_a00c0_2 = -2 + 0.1 * 0 + 0.1 * 0 + 0.3 * W_1_0 - 2 * 0 * (W_2_0 > 0.5) - 2 * 0 * (W_2_0 > 0.5) - 0.3 * W_3_0 + 2 * W_4_a0c0_1 + 2 * W_5_a0c0_1 + rnorm(n.samples, mean = 0, sd = 1)) %>%
    mutate(Y_2 = ifelse((A_0 == 1) & (A_1 == 1), Y_a11c0_2, NA)) %>%
    mutate(Y_2 = ifelse((A_0 == 1) & (A_1 == 0), Y_a10c0_2, Y_2)) %>%
    mutate(Y_2 = ifelse((A_0 == 0) & (A_1 == 1), Y_a01c0_2, Y_2)) %>%
    mutate(Y_2 = ifelse((A_0 == 0) & (A_1 == 0), Y_a00c0_2, Y_2)) 
  
  # hist(-2 + 0.1 * 1 + 0.1 * 1 + 0.3 * data.realData$W_1_0 - 2 * 1 * (data.realData$W_2_0 > 0.5) - 
  #        2 * 1 * (data.realData$W_2_0 > 0.5) - 0.3 * data.realData$W_3_0 + 2 * data.realData$W_4_a1c0_1 + 2 * data.realData$W_5_a1c0_1)
  # hist(-2 + 0.1 * 0 + 0.1 * 0 + 0.3 * data.realData$W_1_0 - 2 * 0 * (data.realData$W_2_0 > 0.5) - 
  #        2 * 0 * (data.realData$W_2_0 > 0.5) - 0.3 * data.realData$W_3_0 + 2 * data.realData$W_4_a0c0_1 + 2 * data.realData$W_5_a0c0_1)
  
  # censoring
  data.realData <- data.realData %>%
    mutate(W_4_1 = ifelse(C_0 == "censored", NA, W_4_1),
           W_5_1 = ifelse(C_0 == "censored", NA, W_5_1),
           W_6_1 = ifelse(C_0 == "censored", NA, W_6_1),
           W_7_1 = ifelse(C_0 == "censored", NA, W_7_1),
           W_8_1 = ifelse(C_0 == "censored", NA, W_8_1),
           W_9_1 = ifelse(C_0 == "censored", NA, W_9_1),
           W_10_1 = ifelse(C_0 == "censored", NA, W_10_1),
           W_11_1 = ifelse(C_0 == "censored", NA, W_11_1),
           A_1   = ifelse(C_0 == "censored", NA, A_1)) %>%
    mutate(C_1 = ifelse(C_0 == "censored", NA, C_1)) %>%
    mutate(C_1 = BinaryToCensoring(is.censored = C_1)) %>%
    mutate(Y_2 = ifelse(C_1 == "censored", NA, Y_2))
  
  latent.data.realData <- latent.data.realData %>%
    mutate(W_4_a1c0_1 = data.realData$W_4_a1c0_1,
           W_4_a0c0_1 = data.realData$W_4_a0c0_1,
           W_5_a1c0_1 = data.realData$W_5_a1c0_1,
           W_5_a0c0_1 = data.realData$W_5_a0c0_1,
           prob_A_a1c0_1 = data.realData$prob_A_a1c0_1,
           prob_A_a0c0_1 = data.realData$prob_A_a0c0_1,
           A_a1c0_1      = data.realData$A_a1c0_1,
           A_a0c0_1      = data.realData$A_a0c0_1,
           prob_C_a11c0_1 = data.realData$prob_C_a11c0_1,
           prob_C_a10c0_1 = data.realData$prob_C_a10c0_1,
           prob_C_a01c0_1 = data.realData$prob_C_a01c0_1,
           prob_C_a00c0_1 = data.realData$prob_C_a00c0_1,
           C_a11c0_1 = data.realData$C_a11c0_1,
           C_a10c0_1 = data.realData$C_a10c0_1,
           C_a01c0_1 = data.realData$C_a01c0_1,
           C_a00c0_1 = data.realData$C_a00c0_1,
           # prob_Y_a11c0_2 = data.realData$prob_Y_a11c0_2,
           # prob_Y_a10c0_2 = data.realData$prob_Y_a10c0_2,
           # prob_Y_a01c0_2 = data.realData$prob_Y_a01c0_2,
           # prob_Y_a00c0_2 = data.realData$prob_Y_a00c0_2,
           Y_a11c0_2 = data.realData$Y_a11c0_2,
           Y_a10c0_2 = data.realData$Y_a10c0_2,
           Y_a01c0_2 = data.realData$Y_a01c0_2,
           Y_a00c0_2 = data.realData$Y_a00c0_2)
  data.realData <- data.realData %>%
    select(-W_4_a1c0_1, -W_4_a0c0_1, -W_5_a1c0_1, -W_5_a0c0_1,
           -prob_A_a1c0_1, -prob_A_a0c0_1, -A_a1c0_1, -A_a0c0_1,
           -prob_C_a11c0_1, -prob_C_a10c0_1, -prob_C_a01c0_1, -prob_C_a00c0_1, 
           -C_a11c0_1, -C_a10c0_1, -C_a01c0_1, -C_a00c0_1,
           -Y_a11c0_2,  -Y_a10c0_2, -Y_a01c0_2, -Y_a00c0_2)
  
  # table(data.realData$A_1)
  # table(data.realData$A_0[(data.realData$C_0 == "uncensored") & (data.realData$C_1 == "uncensored")],
  #       data.realData$A_1[(data.realData$C_0 == "uncensored") & (data.realData$C_1 == "uncensored")])
  # table(data.realData$C_0, data.realData$C_1)
  
  tree.ind  <- sample.int(n = n.samples, size = n.tree, replace = F)
  test.ind  <- (1:n.samples)[!((1:n.samples) %in% tree.ind)]
  tree.data        <- data.realData[tree.ind, ]
  latent.tree.data <- latent.data.realData[tree.ind, ]
  test.data        <- data.realData[test.ind, ]
  latent.test.data <- latent.data.realData[test.ind, ]
  
  # table(data.realData$A_1, data.realData$C_1)
  # table(data.realData$A_0[!is.na(data.realData$Y_2)], data.realData$A_1[!is.na(data.realData$Y_2)])
  # sum((data.realData$C_1 == "uncensored") & (data.realData$A_0 == 1) & (data.realData$A_1 == 1), na.rm = T)
  
  # Truth (need to repeat to verify)
  #################
  #### time 1 #####
  #################
  tree.truth.realData.a1.w2gt05.t1 <- mean(latent.tree.data$Y_a1c0_1[tree.data$W_2_0 > 0.5])
  tree.truth.realData.a0.w2gt05.t1 <- mean(latent.tree.data$Y_a0c0_1[tree.data$W_2_0 > 0.5])
  tree.truth.realData.a1.w2lteq05.t1 <- mean(latent.tree.data$Y_a1c0_1[tree.data$W_2_0 <= 0.5])
  tree.truth.realData.a0.w2lteq05.t1 <- mean(latent.tree.data$Y_a0c0_1[tree.data$W_2_0 <= 0.5])
  tree.truth.t1 <- (tree.truth.realData.a1.w2lteq05.t1 - tree.truth.realData.a0.w2lteq05.t1) -
    (tree.truth.realData.a1.w2gt05.t1 - tree.truth.realData.a0.w2gt05.t1)
  
  test.truth.realData.a1.w2gt05.t1 <- mean(latent.test.data$Y_a1c0_1[test.data$W_2_0 > 0.5])
  test.truth.realData.a0.w2gt05.t1 <- mean(latent.test.data$Y_a0c0_1[test.data$W_2_0 > 0.5])
  test.truth.realData.a1.w2lteq05.t1 <- mean(latent.test.data$Y_a1c0_1[test.data$W_2_0 <= 0.5])
  test.truth.realData.a0.w2lteq05.t1 <- mean(latent.test.data$Y_a0c0_1[test.data$W_2_0 <= 0.5])
  test.truth.t1 <- (test.truth.realData.a1.w2lteq05.t1 - test.truth.realData.a0.w2lteq05.t1) -
    (test.truth.realData.a1.w2gt05.t1 - test.truth.realData.a0.w2gt05.t1)
  
  # colnames.tree.data <- colnames(tree.data %>% select(W_1_0:Y_1))
  # result.tmle.t1.QF.gF <- ltmle(tree.data %>%
  #                                 select(W_1_0:Y_1) %>%
  #                                 filter(W_2_0 > 0.5), 
  #                               Anodes = grep("A_", colnames.tree.data), 
  #                               Cnodes = grep("C_", colnames.tree.data),
  #                               Ynodes = grep("Y_", colnames.tree.data), 
  #                               abar = list(treatment = c(1), control = c(0)), 
  #                               gbounds = c(0.025, 0.975),
  #                               estimate.time = F)
  # summ.t1.QF.gF.w2gt05 <- summary(result.tmle.t1.QF.gF)
  # 
  # result.tmle.t1.QF.gF <- ltmle(tree.data %>%
  #                                 select(W_1_0:Y_1) %>%
  #                                 filter(W_2_0 <= 0.5), 
  #                               Anodes = grep("A_", colnames.tree.data), 
  #                               Cnodes = grep("C_", colnames.tree.data),
  #                               Ynodes = grep("Y_", colnames.tree.data), 
  #                               abar = list(treatment = c(1), control = c(0)), 
  #                               gbounds = c(0.025, 0.975),
  #                               estimate.time = F)
  # summ.t1.QF.gF.w2lteq05 <- summary(result.tmle.t1.QF.gF)
  
  #################
  #### time 2 #####
  #################
  tree.truth.realData.a1.w2gt05.t2 <- mean(latent.tree.data$Y_a11c0_2[tree.data$W_2_0 > 0.5])
  tree.truth.realData.a0.w2gt05.t2 <- mean(latent.tree.data$Y_a00c0_2[tree.data$W_2_0 > 0.5])
  tree.truth.realData.a1.w2lteq05.t2 <- mean(latent.tree.data$Y_a11c0_2[tree.data$W_2_0 <= 0.5])
  tree.truth.realData.a0.w2lteq05.t2 <- mean(latent.tree.data$Y_a00c0_2[tree.data$W_2_0 <= 0.5])
  tree.truth.t2 <- (tree.truth.realData.a1.w2lteq05.t2 - tree.truth.realData.a0.w2lteq05.t2) -
    (tree.truth.realData.a1.w2gt05.t2 - tree.truth.realData.a0.w2gt05.t2)
  
  test.truth.realData.a1.w2gt05.t2 <- mean(latent.test.data$Y_a11c0_2[test.data$W_2_0 > 0.5])
  test.truth.realData.a0.w2gt05.t2 <- mean(latent.test.data$Y_a00c0_2[test.data$W_2_0 > 0.5])
  test.truth.realData.a1.w2lteq05.t2 <- mean(latent.test.data$Y_a11c0_2[test.data$W_2_0 <= 0.5])
  test.truth.realData.a0.w2lteq05.t2 <- mean(latent.test.data$Y_a00c0_2[test.data$W_2_0 <= 0.5])
  test.truth.t2 <- (test.truth.realData.a1.w2lteq05.t2 - test.truth.realData.a0.w2lteq05.t2) -
    (test.truth.realData.a1.w2gt05.t2 - test.truth.realData.a0.w2gt05.t2)
  
  latent.tree.data <- latent.tree.data %>%
    mutate(truth.trt.eff.t1 = ifelse(tree.data$W_2_0 > 0.5, 
                                     tree.truth.realData.a1.w2gt05.t1 - tree.truth.realData.a0.w2gt05.t1,
                                     tree.truth.realData.a1.w2lteq05.t1 - tree.truth.realData.a0.w2lteq05.t1),
           truth.trt.eff.t2 = ifelse(tree.data$W_2_0 > 0.5, 
                                     tree.truth.realData.a1.w2gt05.t2 - tree.truth.realData.a0.w2gt05.t2,
                                     tree.truth.realData.a1.w2lteq05.t2 - tree.truth.realData.a0.w2lteq05.t2))
  
  latent.test.data <- latent.test.data %>%
    mutate(truth.trt.eff.t1 = ifelse(test.data$W_2_0 > 0.5, 
                                     test.truth.realData.a1.w2gt05.t1 - test.truth.realData.a0.w2gt05.t1,
                                     test.truth.realData.a1.w2lteq05.t1 - test.truth.realData.a0.w2lteq05.t1),
           truth.trt.eff.t2 = ifelse(test.data$W_2_0 > 0.5, 
                                     test.truth.realData.a1.w2gt05.t2 - test.truth.realData.a0.w2gt05.t2,
                                     test.truth.realData.a1.w2lteq05.t2 - test.truth.realData.a0.w2lteq05.t2))
  
  # colnames.tree.data <- colnames(tree.data)
  # result.tmle.t2.QF.gF <- ltmle(tree.data[tree.data$W_2_0 > 0.5, ], 
  #                                                Anodes = grep("A_", colnames.tree.data), 
  #                                                Cnodes = grep("C_", colnames.tree.data),
  #                                                Lnodes = grep("W_[4-5]_1", colnames.tree.data), 
  #                                                Ynodes = grep("Y_", colnames.tree.data), 
  #                                                abar = list(treatment = c(1, 1), control = c(0, 0)), 
  #                                                gbounds = c(0.025, 0.975),
  #                                                estimate.time = F)
  # summ.t2.QF.gF.w2gt05 <- summary(result.tmle.t2.QF.gF)
  # 
  # result.tmle.t2.QF.gF <- ltmle(tree.data[tree.data$W_2_0 <= 0.5, ], 
  #                               Anodes = grep("A_", colnames.tree.data), 
  #                               Cnodes = grep("C_", colnames.tree.data),
  #                               Lnodes = grep("W_[4-5]_1", colnames.tree.data), 
  #                               Ynodes = grep("Y_", colnames.tree.data), 
  #                               abar = list(treatment = c(1, 1), control = c(0, 0)), 
  #                               gbounds = c(0.025, 0.975),
  #                               estimate.time = F)
  # summ.t2.QF.gF.w2lteq05 <- summary(result.tmle.t2.QF.gF)
  
  return(list(tree.data = tree.data,
              test.data = test.data,
              latent.tree.data = latent.tree.data,
              latent.test.data = latent.test.data,
              tree.truth       = c(tree.truth.t1, tree.truth.t2),
              test.truth       = c(test.truth.t1, test.truth.t2),
              corr.split       = list(c("W_2_0"), c("W_2_0")),
              where.split      = list(c(1), c(1)),                   # number of row in rpart.object$frame
              dir.split        = list(c(1), c(-1)),                  # direction of making splits, ncat column in rpart.object$splits
              noise.var        = c("W_1_0", paste0("W_", 3:15, "_0"))))
}
