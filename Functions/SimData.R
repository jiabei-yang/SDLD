simData.trtDep.csrDep.svvl <- function(n.tree, n.test, seed) {
  
  set.seed(seed)
  n.samples <- n.tree + n.test
  
  # length of baseline covariates: 5
  # multivariate normal, mean 0, variance 1, covariance/off-diagonal 0.2
  lth.W.0   <- 5
  sigma.W.0 <- diag(lth.W.0)
  sigma.W.0 <- sigma.W.0 + 0.2
  sigma.W.0 <- sigma.W.0 - 0.2 * diag(lth.W.0)
  W.0 <- rmvnorm(n.samples, mean = rep(0, lth.W.0), sigma = sigma.W.0)
  
  data.trtDep.csrDep.Ysvvl <- data.frame(W.0)
  colnames(data.trtDep.csrDep.Ysvvl) <- paste0("W_", 1:lth.W.0, "_0")
  
  # A(0)
  data.trtDep.csrDep.Ysvvl <- data.trtDep.csrDep.Ysvvl %>%
    mutate(prob_A_0 = 1 / (1 + exp(-(-0.5 + 0.2 * W_1_0 + 0.2 * W_2_0 + 0.4 * W_3_0 + 0.5 * W_4_0)))) %>%
    mutate(A_0      = rbinom(n.samples, size = 1, prob = prob_A_0))
  
  # table(data.trtDep.csrDep.Ysvvl$A_0)
  
  # probability of event at time 1
  data.trtDep.csrDep.Ysvvl <- data.trtDep.csrDep.Ysvvl %>%
    mutate(prob_C_a1_0 = 1 / (1 + exp(-(-4 + 0.8 * 1 + 0.3 * W_1_0 - 0.3 * W_2_0 - 0.3 * W_3_0 + 0.1 * W_4_0))),
           prob_C_a0_0 = 1 / (1 + exp(-(-4 + 0.8 * 0 + 0.3 * W_1_0 - 0.3 * W_2_0 - 0.3 * W_3_0 + 0.1 * W_4_0)))) %>%
    mutate(C_a1_0 = rbinom(n.samples, size = 1, prob = prob_C_a1_0),
           C_a0_0 = rbinom(n.samples, size = 1, prob = prob_C_a0_0)) %>%
    mutate(C_0 = ifelse(A_0 == 1, C_a1_0, C_a0_0)) %>%
    mutate(C_0 = BinaryToCensoring(is.censored = C_0)) %>%
    mutate(prob_Y_a1c0_1 = 1 / (1 + exp(-(-3 + 0.1 * 1 + 0.3 * W_1_0 - 2 * 1 * (W_2_0 > 0.5) + 2 * W_4_0 + 2 * W_5_0))),
           prob_Y_a0c0_1 = 1 / (1 + exp(-(-3 + 0.1 * 0 + 0.3 * W_1_0 - 2 * 0 * (W_2_0 > 0.5) + 2 * W_4_0 + 2 * W_5_0)))) %>%
    mutate(Y_a1c0_1 = rbinom(n.samples, size = 1, prob = prob_Y_a1c0_1),
           Y_a0c0_1 = rbinom(n.samples, size = 1, prob = prob_Y_a0c0_1)) %>%
    mutate(Y_1 = ifelse(A_0 == 1, Y_a1c0_1, Y_a0c0_1)) %>%
    mutate(Y_1 = ifelse(C_0 == "censored", NA, Y_1))
  
  latent.data.trtDep.csrDep.Ysvvl <- data.trtDep.csrDep.Ysvvl %>%
    select(prob_A_0, prob_C_a1_0, prob_C_a0_0, C_a1_0, C_a0_0, prob_Y_a1c0_1, prob_Y_a0c0_1, Y_a1c0_1, Y_a0c0_1)
  data.trtDep.csrDep.Ysvvl <- data.trtDep.csrDep.Ysvvl %>%
    select(-prob_A_0, -prob_C_a1_0, -prob_C_a0_0, -C_a1_0, -C_a0_0, -prob_Y_a1c0_1, -prob_Y_a0c0_1, -Y_a1c0_1, -Y_a0c0_1)
  
  #################
  #### time 2 #####
  #################
  data.trtDep.csrDep.Ysvvl <- data.trtDep.csrDep.Ysvvl %>%
    mutate(W_4_a1c0_1 = 0.2 * 1 + 0.5 * W_1_0 - 0.4 * W_2_0 - 0.4 * W_3_0 + 0.5 * W_4_0 - 0.5 * W_5_0 + rnorm(n.samples, mean = 0, sd = 0.4),
           W_4_a0c0_1 = 0.2 * 0 + 0.5 * W_1_0 - 0.4 * W_2_0 - 0.4 * W_3_0 + 0.5 * W_4_0 - 0.5 * W_5_0 + rnorm(n.samples, mean = 0, sd = 0.4)) %>%
    mutate(W_5_a1c0_1 = 0.1 * 1 + 0.1 * W_1_0 + 0.1 * W_2_0 - 0.4 * W_3_0 + 0.5 * W_4_a1c0_1 - 0.5 * W_5_0 + rnorm(n.samples, mean = 0, sd = 0.4),
           W_5_a0c0_1 = 0.1 * 0 + 0.1 * W_1_0 + 0.1 * W_2_0 - 0.4 * W_3_0 + 0.5 * W_4_a0c0_1 - 0.5 * W_5_0 + rnorm(n.samples, mean = 0, sd = 0.4)) %>%
    mutate(W_4_1 = ifelse(A_0 == 1, W_4_a1c0_1, W_4_a0c0_1),
           W_5_1 = ifelse(A_0 == 1, W_5_a1c0_1, W_5_a0c0_1))
  
  data.trtDep.csrDep.Ysvvl <- data.trtDep.csrDep.Ysvvl %>%
    mutate(prob_A_a1c0_1 = 1 / (1 + exp(-(-1 + 0.1 * W_1_0 + 0.1 * W_2_0 + 0.2 * W_3_0 + 0.2 * W_4_0 - 1 * W_4_a1c0_1 - 0.5 * W_5_a1c0_1))),
           prob_A_a0c0_1 = 1 / (1 + exp(-(-1 + 0.1 * W_1_0 + 0.1 * W_2_0 + 0.2 * W_3_0 + 0.2 * W_4_0 - 1 * W_4_a0c0_1 - 0.5 * W_5_a0c0_1)))) %>%
    mutate(A_a1c0_1      = rbinom(n.samples, size = 1, prob = prob_A_a1c0_1),
           A_a0c0_1      = rbinom(n.samples, size = 1, prob = prob_A_a0c0_1)) %>% 
    mutate(A_1 = ifelse(A_0 == 1, A_a1c0_1, A_a0c0_1)) 
  
  data.trtDep.csrDep.Ysvvl <- data.trtDep.csrDep.Ysvvl %>%
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
    mutate(prob_Y_a11c0_2 = 1 / (1 + exp(-(-2 + 0.1 * 1 + 0.1 * 1 + 0.3 * W_1_0 - 2 * 1 * (W_2_0 > 0.5) - 2 * 1 * (W_2_0 > 0.5) - 0.3 * W_3_0 + 2 * W_4_a1c0_1 + 2 * W_5_a1c0_1))),
           prob_Y_a10c0_2 = 1 / (1 + exp(-(-2 + 0.1 * 1 + 0.1 * 0 + 0.3 * W_1_0 - 2 * 1 * (W_2_0 > 0.5) - 2 * 0 * (W_2_0 > 0.5) - 0.3 * W_3_0 + 2 * W_4_a1c0_1 + 2 * W_5_a1c0_1))),
           prob_Y_a01c0_2 = 1 / (1 + exp(-(-2 + 0.1 * 0 + 0.1 * 1 + 0.3 * W_1_0 - 2 * 0 * (W_2_0 > 0.5) - 2 * 1 * (W_2_0 > 0.5) - 0.3 * W_3_0 + 2 * W_4_a0c0_1 + 2 * W_5_a0c0_1))),
           prob_Y_a00c0_2 = 1 / (1 + exp(-(-2 + 0.1 * 0 + 0.1 * 0 + 0.3 * W_1_0 - 2 * 0 * (W_2_0 > 0.5) - 2 * 0 * (W_2_0 > 0.5) - 0.3 * W_3_0 + 2 * W_4_a0c0_1 + 2 * W_5_a0c0_1)))) %>%
    mutate(Y_a11c0_2 = rbinom(n.samples, size = 1, prob = prob_Y_a11c0_2),
           Y_a10c0_2 = rbinom(n.samples, size = 1, prob = prob_Y_a10c0_2),
           Y_a01c0_2 = rbinom(n.samples, size = 1, prob = prob_Y_a01c0_2),
           Y_a00c0_2 = rbinom(n.samples, size = 1, prob = prob_Y_a00c0_2)) %>%
    mutate(Y_2 = ifelse((A_0 == 1) & (A_1 == 1), Y_a11c0_2, NA)) %>%
    mutate(Y_2 = ifelse((A_0 == 1) & (A_1 == 0), Y_a10c0_2, Y_2)) %>%
    mutate(Y_2 = ifelse((A_0 == 0) & (A_1 == 1), Y_a01c0_2, Y_2)) %>%
    mutate(Y_2 = ifelse((A_0 == 0) & (A_1 == 0), Y_a00c0_2, Y_2)) %>%
    mutate(Y_2 = ifelse(Y_1 == 1, 1, Y_2))
  
  # hist(-2 + 0.1 * 1 + 0.1 * 1 + 0.3 * data.trtDep.csrDep.Ysvvl$W_1_0 - 2 * 1 * (data.trtDep.csrDep.Ysvvl$W_2_0 > 0.5) - 
  #        2 * 1 * (data.trtDep.csrDep.Ysvvl$W_2_0 > 0.5) - 0.3 * data.trtDep.csrDep.Ysvvl$W_3_0 + 2 * data.trtDep.csrDep.Ysvvl$W_4_a1c0_1 + 2 * data.trtDep.csrDep.Ysvvl$W_5_a1c0_1)
  # hist(-2 + 0.1 * 0 + 0.1 * 0 + 0.3 * data.trtDep.csrDep.Ysvvl$W_1_0 - 2 * 0 * (data.trtDep.csrDep.Ysvvl$W_2_0 > 0.5) - 
  #        2 * 0 * (data.trtDep.csrDep.Ysvvl$W_2_0 > 0.5) - 0.3 * data.trtDep.csrDep.Ysvvl$W_3_0 + 2 * data.trtDep.csrDep.Ysvvl$W_4_a0c0_1 + 2 * data.trtDep.csrDep.Ysvvl$W_5_a0c0_1)
  
  # remove covariates and event probabilities after event at time 1
  data.trtDep.csrDep.Ysvvl <- data.trtDep.csrDep.Ysvvl %>%
    mutate(W_4_1 = ifelse(Y_1 == 1, NA, W_4_1),
           W_5_1 = ifelse(Y_1 == 1, NA, W_5_1),
           A_1   = ifelse(Y_1 == 1, NA, A_1),
           C_1   = ifelse(Y_1 == 1, NA, C_1))
  
  # censoring
  data.trtDep.csrDep.Ysvvl <- data.trtDep.csrDep.Ysvvl %>%
    mutate(W_4_1 = ifelse(C_0 == "censored", NA, W_4_1),
           W_5_1 = ifelse(C_0 == "censored", NA, W_5_1),
           A_1   = ifelse(C_0 == "censored", NA, A_1)) %>%
    mutate(C_1 = ifelse(C_0 == "censored", NA, C_1)) %>%
    mutate(C_1 = BinaryToCensoring(is.censored = C_1)) %>%
    # only set Y_2 to NA if previous Y not 1 and censored at this point
    mutate(Y_2 = ifelse((C_1 == "censored") & (Y_1 == 0), NA, Y_2))
  
  latent.data.trtDep.csrDep.Ysvvl <- latent.data.trtDep.csrDep.Ysvvl %>%
    mutate(W_4_a1c0_1 = data.trtDep.csrDep.Ysvvl$W_4_a1c0_1,
           W_4_a0c0_1 = data.trtDep.csrDep.Ysvvl$W_4_a0c0_1,
           W_5_a1c0_1 = data.trtDep.csrDep.Ysvvl$W_5_a1c0_1,
           W_5_a0c0_1 = data.trtDep.csrDep.Ysvvl$W_5_a0c0_1,
           prob_A_a1c0_1 = data.trtDep.csrDep.Ysvvl$prob_A_a1c0_1,
           prob_A_a0c0_1 = data.trtDep.csrDep.Ysvvl$prob_A_a0c0_1,
           A_a1c0_1      = data.trtDep.csrDep.Ysvvl$A_a1c0_1,
           A_a0c0_1      = data.trtDep.csrDep.Ysvvl$A_a0c0_1,
           prob_C_a11c0_1 = data.trtDep.csrDep.Ysvvl$prob_C_a11c0_1,
           prob_C_a10c0_1 = data.trtDep.csrDep.Ysvvl$prob_C_a10c0_1,
           prob_C_a01c0_1 = data.trtDep.csrDep.Ysvvl$prob_C_a01c0_1,
           prob_C_a00c0_1 = data.trtDep.csrDep.Ysvvl$prob_C_a00c0_1,
           C_a11c0_1 = data.trtDep.csrDep.Ysvvl$C_a11c0_1,
           C_a10c0_1 = data.trtDep.csrDep.Ysvvl$C_a10c0_1,
           C_a01c0_1 = data.trtDep.csrDep.Ysvvl$C_a01c0_1,
           C_a00c0_1 = data.trtDep.csrDep.Ysvvl$C_a00c0_1,
           prob_Y_a11c0_2 = data.trtDep.csrDep.Ysvvl$prob_Y_a11c0_2,
           prob_Y_a10c0_2 = data.trtDep.csrDep.Ysvvl$prob_Y_a10c0_2,
           prob_Y_a01c0_2 = data.trtDep.csrDep.Ysvvl$prob_Y_a01c0_2,
           prob_Y_a00c0_2 = data.trtDep.csrDep.Ysvvl$prob_Y_a00c0_2,
           Y_a11c0_2 = data.trtDep.csrDep.Ysvvl$Y_a11c0_2,
           Y_a10c0_2 = data.trtDep.csrDep.Ysvvl$Y_a10c0_2,
           Y_a01c0_2 = data.trtDep.csrDep.Ysvvl$Y_a01c0_2,
           Y_a00c0_2 = data.trtDep.csrDep.Ysvvl$Y_a00c0_2)
  data.trtDep.csrDep.Ysvvl <- data.trtDep.csrDep.Ysvvl %>%
    select(-W_4_a1c0_1, -W_4_a0c0_1, -W_5_a1c0_1, -W_5_a0c0_1,
           -prob_A_a1c0_1, -prob_A_a0c0_1, -A_a1c0_1, -A_a0c0_1,
           -prob_C_a11c0_1, -prob_C_a10c0_1, -prob_C_a01c0_1, -prob_C_a00c0_1, 
           -C_a11c0_1, -C_a10c0_1, -C_a01c0_1, -C_a00c0_1,
           -prob_Y_a11c0_2, -prob_Y_a10c0_2, -prob_Y_a01c0_2, -prob_Y_a00c0_2, 
           -Y_a11c0_2,  -Y_a10c0_2, -Y_a01c0_2, -Y_a00c0_2)
  
  # table(data.trtDep.csrDep.Ysvvl$A_1)
  # table(data.trtDep.csrDep.Ysvvl$A_0[(data.trtDep.csrDep.Ysvvl$C_0 == "uncensored") & (data.trtDep.csrDep.Ysvvl$C_1 == "uncensored")], 
  #       data.trtDep.csrDep.Ysvvl$A_1[(data.trtDep.csrDep.Ysvvl$C_0 == "uncensored") & (data.trtDep.csrDep.Ysvvl$C_1 == "uncensored")])
  # table(data.trtDep.csrDep.Ysvvl$C_0, data.trtDep.csrDep.Ysvvl$C_1)
  
  tree.ind  <- sample.int(n = n.samples, size = n.tree, replace = F)
  test.ind  <- (1:n.samples)[!((1:n.samples) %in% tree.ind)]
  tree.data        <- data.trtDep.csrDep.Ysvvl[tree.ind, ]
  latent.tree.data <- latent.data.trtDep.csrDep.Ysvvl[tree.ind, ]
  test.data        <- data.trtDep.csrDep.Ysvvl[test.ind, ]
  latent.test.data <- latent.data.trtDep.csrDep.Ysvvl[test.ind, ]
  
  # Truth (need to repeat to verify)
  #################
  #### time 1 #####
  #################
  tree.truth.trtDep.csrDep.Ysvvl.a1.w2gt05.t1 <- mean(latent.tree.data$prob_Y_a1c0_1[tree.data$W_2_0 > 0.5])
  tree.truth.trtDep.csrDep.Ysvvl.a0.w2gt05.t1 <- mean(latent.tree.data$prob_Y_a0c0_1[tree.data$W_2_0 > 0.5])
  tree.truth.trtDep.csrDep.Ysvvl.a1.w2lteq05.t1 <- mean(latent.tree.data$prob_Y_a1c0_1[tree.data$W_2_0 <= 0.5])
  tree.truth.trtDep.csrDep.Ysvvl.a0.w2lteq05.t1 <- mean(latent.tree.data$prob_Y_a0c0_1[tree.data$W_2_0 <= 0.5])
  tree.truth.t1 <- (tree.truth.trtDep.csrDep.Ysvvl.a1.w2lteq05.t1 - tree.truth.trtDep.csrDep.Ysvvl.a0.w2lteq05.t1) -
    (tree.truth.trtDep.csrDep.Ysvvl.a1.w2gt05.t1 - tree.truth.trtDep.csrDep.Ysvvl.a0.w2gt05.t1)
  
  test.truth.trtDep.csrDep.Ysvvl.a1.w2gt05.t1 <- mean(latent.test.data$prob_Y_a1c0_1[test.data$W_2_0 > 0.5])
  test.truth.trtDep.csrDep.Ysvvl.a0.w2gt05.t1 <- mean(latent.test.data$prob_Y_a0c0_1[test.data$W_2_0 > 0.5])
  test.truth.trtDep.csrDep.Ysvvl.a1.w2lteq05.t1 <- mean(latent.test.data$prob_Y_a1c0_1[test.data$W_2_0 <= 0.5])
  test.truth.trtDep.csrDep.Ysvvl.a0.w2lteq05.t1 <- mean(latent.test.data$prob_Y_a0c0_1[test.data$W_2_0 <= 0.5])
  test.truth.t1 <- (test.truth.trtDep.csrDep.Ysvvl.a1.w2lteq05.t1 - test.truth.trtDep.csrDep.Ysvvl.a0.w2lteq05.t1) -
    (test.truth.trtDep.csrDep.Ysvvl.a1.w2gt05.t1 - test.truth.trtDep.csrDep.Ysvvl.a0.w2gt05.t1)
  
  #################
  #### time 2 #####
  #################
  tree.truth.trtDep.csrDep.Ysvvl.a1.w2gt05.t2 <- mean(latent.tree.data$prob_Y_a1c0_1[tree.data$W_2_0 > 0.5] +
                                                   (1 - latent.tree.data$prob_Y_a1c0_1[tree.data$W_2_0 > 0.5]) *
                                                   latent.tree.data$prob_Y_a11c0_2[tree.data$W_2_0 > 0.5])
  tree.truth.trtDep.csrDep.Ysvvl.a0.w2gt05.t2 <- mean(latent.tree.data$prob_Y_a0c0_1[tree.data$W_2_0 > 0.5] +
                                                   (1 - latent.tree.data$prob_Y_a0c0_1[tree.data$W_2_0 > 0.5]) *
                                                   latent.tree.data$prob_Y_a00c0_2[tree.data$W_2_0 > 0.5])
  tree.truth.trtDep.csrDep.Ysvvl.a1.w2lteq05.t2 <- mean(latent.tree.data$prob_Y_a1c0_1[tree.data$W_2_0 <= 0.5] +
                                                     (1 - latent.tree.data$prob_Y_a1c0_1[tree.data$W_2_0 <= 0.5]) *
                                                     latent.tree.data$prob_Y_a11c0_2[tree.data$W_2_0 <= 0.5])
  tree.truth.trtDep.csrDep.Ysvvl.a0.w2lteq05.t2 <- mean(latent.tree.data$prob_Y_a0c0_1[tree.data$W_2_0 <= 0.5] +
                                                     (1 - latent.tree.data$prob_Y_a0c0_1[tree.data$W_2_0 <= 0.5]) *
                                                     latent.tree.data$prob_Y_a00c0_2[tree.data$W_2_0 <= 0.5])
  tree.truth.t2 <- (tree.truth.trtDep.csrDep.Ysvvl.a1.w2lteq05.t2 - tree.truth.trtDep.csrDep.Ysvvl.a0.w2lteq05.t2) -
    (tree.truth.trtDep.csrDep.Ysvvl.a1.w2gt05.t2 - tree.truth.trtDep.csrDep.Ysvvl.a0.w2gt05.t2)
  
  test.truth.trtDep.csrDep.Ysvvl.a1.w2gt05.t2 <- mean(latent.test.data$prob_Y_a1c0_1[test.data$W_2_0 > 0.5] +
                                                        (1 - latent.test.data$prob_Y_a1c0_1[test.data$W_2_0 > 0.5]) *
                                                        latent.test.data$prob_Y_a11c0_2[test.data$W_2_0 > 0.5])
  test.truth.trtDep.csrDep.Ysvvl.a0.w2gt05.t2 <- mean(latent.test.data$prob_Y_a0c0_1[test.data$W_2_0 > 0.5] +
                                                        (1 - latent.test.data$prob_Y_a0c0_1[test.data$W_2_0 > 0.5]) *
                                                        latent.test.data$prob_Y_a00c0_2[test.data$W_2_0 > 0.5])
  test.truth.trtDep.csrDep.Ysvvl.a1.w2lteq05.t2 <- mean(latent.test.data$prob_Y_a1c0_1[test.data$W_2_0 <= 0.5] +
                                                          (1 - latent.test.data$prob_Y_a1c0_1[test.data$W_2_0 <= 0.5]) *
                                                          latent.test.data$prob_Y_a11c0_2[test.data$W_2_0 <= 0.5])
  test.truth.trtDep.csrDep.Ysvvl.a0.w2lteq05.t2 <- mean(latent.test.data$prob_Y_a0c0_1[test.data$W_2_0 <= 0.5] +
                                                          (1 - latent.test.data$prob_Y_a0c0_1[test.data$W_2_0 <= 0.5]) *
                                                          latent.test.data$prob_Y_a00c0_2[test.data$W_2_0 <= 0.5])
  test.truth.t2 <- (test.truth.trtDep.csrDep.Ysvvl.a1.w2lteq05.t2 - test.truth.trtDep.csrDep.Ysvvl.a0.w2lteq05.t2) -
    (test.truth.trtDep.csrDep.Ysvvl.a1.w2gt05.t2 - test.truth.trtDep.csrDep.Ysvvl.a0.w2gt05.t2)
  
  latent.tree.data <- latent.tree.data %>%
    mutate(truth.trt.eff.t1 = ifelse(tree.data$W_2_0 > 0.5, 
                                     tree.truth.trtDep.csrDep.Ysvvl.a1.w2gt05.t1 - tree.truth.trtDep.csrDep.Ysvvl.a0.w2gt05.t1,
                                     tree.truth.trtDep.csrDep.Ysvvl.a1.w2lteq05.t1 - tree.truth.trtDep.csrDep.Ysvvl.a0.w2lteq05.t1),
           truth.trt.eff.t2 = ifelse(tree.data$W_2_0 > 0.5, 
                                     tree.truth.trtDep.csrDep.Ysvvl.a1.w2gt05.t2 - tree.truth.trtDep.csrDep.Ysvvl.a0.w2gt05.t2,
                                     tree.truth.trtDep.csrDep.Ysvvl.a1.w2lteq05.t2 - tree.truth.trtDep.csrDep.Ysvvl.a0.w2lteq05.t2))
  
  latent.test.data <- latent.test.data %>%
    mutate(truth.trt.eff.t1 = ifelse(test.data$W_2_0 > 0.5, 
                                     test.truth.trtDep.csrDep.Ysvvl.a1.w2gt05.t1 - test.truth.trtDep.csrDep.Ysvvl.a0.w2gt05.t1,
                                     test.truth.trtDep.csrDep.Ysvvl.a1.w2lteq05.t1 - test.truth.trtDep.csrDep.Ysvvl.a0.w2lteq05.t1),
           truth.trt.eff.t2 = ifelse(test.data$W_2_0 > 0.5, 
                                     test.truth.trtDep.csrDep.Ysvvl.a1.w2gt05.t2 - test.truth.trtDep.csrDep.Ysvvl.a0.w2gt05.t2,
                                     test.truth.trtDep.csrDep.Ysvvl.a1.w2lteq05.t2 - test.truth.trtDep.csrDep.Ysvvl.a0.w2lteq05.t2))
  
  return(list(tree.data = tree.data,
              test.data = test.data,
              latent.tree.data = latent.tree.data,
              latent.test.data = latent.test.data,
              tree.truth       = c(tree.truth.t1, tree.truth.t2),
              test.truth       = c(test.truth.t1, test.truth.t2),
              corr.split       = list(c("W_2_0"), c("W_2_0")),
              where.split      = list(c(1), c(1)),                   # number of row in rpart.object$frame
              dir.split        = list(c(1), c(-1)),                  # direction of making splits, ncat column in rpart.object$splits
              noise.var        = c("W_1_0", "W_3_0", "W_4_0", "W_5_0")))
}



# continuous outcome
simData.trtDep.csrDep.yCont <- function(n.tree, n.test, seed) {
  
  set.seed(seed)
  n.samples <- n.tree + n.test
  
  # length of baseline covariates: 5
  # multivariate normal, mean 0, variance 1, covariance/off-diagonal 0.2
  lth.W.0   <- 5
  sigma.W.0 <- diag(lth.W.0)
  sigma.W.0 <- sigma.W.0 + 0.2
  sigma.W.0 <- sigma.W.0 - 0.2 * diag(lth.W.0)
  W.0 <- rmvnorm(n.samples, mean = rep(0, lth.W.0), sigma = sigma.W.0)
  
  data.trtDep.csrDep.YCont <- data.frame(W.0)
  colnames(data.trtDep.csrDep.YCont) <- paste0("W_", 1:lth.W.0, "_0")
  
  # A(0)
  data.trtDep.csrDep.YCont <- data.trtDep.csrDep.YCont %>%
    mutate(prob_A_0 = 1 / (1 + exp(-(-0.5 + 0.2 * W_1_0 + 0.2 * W_2_0 + 0.4 * W_3_0 + 0.5 * W_4_0)))) %>%
    mutate(A_0      = rbinom(n.samples, size = 1, prob = prob_A_0))
  
  # table(data.trtDep.csrDep.YCont$A_0)
  
  # probability of event at time 1
  data.trtDep.csrDep.YCont <- data.trtDep.csrDep.YCont %>%
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
  
  latent.data.trtDep.csrDep.YCont <- data.trtDep.csrDep.YCont %>%
    select(prob_A_0, prob_C_a1_0, prob_C_a0_0, C_a1_0, C_a0_0, Y_a1c0_1, Y_a0c0_1)
  data.trtDep.csrDep.YCont <- data.trtDep.csrDep.YCont %>%
    select(-prob_A_0, -prob_C_a1_0, -prob_C_a0_0, -C_a1_0, -C_a0_0, -Y_a1c0_1, -Y_a0c0_1)
 
  # table(data.trtDep.csrDep.YCont$A_0, data.trtDep.csrDep.YCont$C_0)
  
  #################
  #### time 2 #####
  #################
  data.trtDep.csrDep.YCont <- data.trtDep.csrDep.YCont %>%
    mutate(W_4_a1c0_1 = 0.2 * 1 + 0.5 * W_1_0 - 0.4 * W_2_0 - 0.4 * W_3_0 + 0.5 * W_4_0 - 0.5 * W_5_0 + rnorm(n.samples, mean = 0, sd = 0.4),
           W_4_a0c0_1 = 0.2 * 0 + 0.5 * W_1_0 - 0.4 * W_2_0 - 0.4 * W_3_0 + 0.5 * W_4_0 - 0.5 * W_5_0 + rnorm(n.samples, mean = 0, sd = 0.4)) %>%
    mutate(W_5_a1c0_1 = 0.1 * 1 + 0.1 * W_1_0 + 0.1 * W_2_0 - 0.4 * W_3_0 + 0.5 * W_4_a1c0_1 - 0.5 * W_5_0 + rnorm(n.samples, mean = 0, sd = 0.4),
           W_5_a0c0_1 = 0.1 * 0 + 0.1 * W_1_0 + 0.1 * W_2_0 - 0.4 * W_3_0 + 0.5 * W_4_a0c0_1 - 0.5 * W_5_0 + rnorm(n.samples, mean = 0, sd = 0.4)) %>%
    mutate(W_4_1 = ifelse(A_0 == 1, W_4_a1c0_1, W_4_a0c0_1),
           W_5_1 = ifelse(A_0 == 1, W_5_a1c0_1, W_5_a0c0_1))
  
  data.trtDep.csrDep.YCont <- data.trtDep.csrDep.YCont %>%
    mutate(prob_A_a1c0_1 = 1 / (1 + exp(-(-1 + 0.1 * W_1_0 + 0.1 * W_2_0 + 0.2 * W_3_0 + 0.2 * W_4_0 - 1 * W_4_a1c0_1 - 0.5 * W_5_a1c0_1))),
           prob_A_a0c0_1 = 1 / (1 + exp(-(-1 + 0.1 * W_1_0 + 0.1 * W_2_0 + 0.2 * W_3_0 + 0.2 * W_4_0 - 1 * W_4_a0c0_1 - 0.5 * W_5_a0c0_1)))) %>%
    mutate(A_a1c0_1      = rbinom(n.samples, size = 1, prob = prob_A_a1c0_1),
           A_a0c0_1      = rbinom(n.samples, size = 1, prob = prob_A_a0c0_1)) %>% 
    mutate(A_1 = ifelse(A_0 == 1, A_a1c0_1, A_a0c0_1))
  
  data.trtDep.csrDep.YCont <- data.trtDep.csrDep.YCont %>%
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
  
  # hist(-2 + 0.1 * 1 + 0.1 * 1 + 0.3 * data.trtDep.csrDep.YCont$W_1_0 - 2 * 1 * (data.trtDep.csrDep.YCont$W_2_0 > 0.5) - 
  #        2 * 1 * (data.trtDep.csrDep.YCont$W_2_0 > 0.5) - 0.3 * data.trtDep.csrDep.YCont$W_3_0 + 2 * data.trtDep.csrDep.YCont$W_4_a1c0_1 + 2 * data.trtDep.csrDep.YCont$W_5_a1c0_1)
  # hist(-2 + 0.1 * 0 + 0.1 * 0 + 0.3 * data.trtDep.csrDep.YCont$W_1_0 - 2 * 0 * (data.trtDep.csrDep.YCont$W_2_0 > 0.5) - 
  #        2 * 0 * (data.trtDep.csrDep.YCont$W_2_0 > 0.5) - 0.3 * data.trtDep.csrDep.YCont$W_3_0 + 2 * data.trtDep.csrDep.YCont$W_4_a0c0_1 + 2 * data.trtDep.csrDep.YCont$W_5_a0c0_1)
  
  # censoring
  data.trtDep.csrDep.YCont <- data.trtDep.csrDep.YCont %>%
    mutate(W_4_1 = ifelse(C_0 == "censored", NA, W_4_1),
           W_5_1 = ifelse(C_0 == "censored", NA, W_5_1),
           A_1   = ifelse(C_0 == "censored", NA, A_1)) %>%
    mutate(C_1 = ifelse(C_0 == "censored", NA, C_1)) %>%
    mutate(C_1 = BinaryToCensoring(is.censored = C_1)) %>%
    mutate(Y_2 = ifelse(C_1 == "censored", NA, Y_2))
  
  latent.data.trtDep.csrDep.YCont <- latent.data.trtDep.csrDep.YCont %>%
    mutate(W_4_a1c0_1 = data.trtDep.csrDep.YCont$W_4_a1c0_1,
           W_4_a0c0_1 = data.trtDep.csrDep.YCont$W_4_a0c0_1,
           W_5_a1c0_1 = data.trtDep.csrDep.YCont$W_5_a1c0_1,
           W_5_a0c0_1 = data.trtDep.csrDep.YCont$W_5_a0c0_1,
           prob_A_a1c0_1 = data.trtDep.csrDep.YCont$prob_A_a1c0_1,
           prob_A_a0c0_1 = data.trtDep.csrDep.YCont$prob_A_a0c0_1,
           A_a1c0_1      = data.trtDep.csrDep.YCont$A_a1c0_1,
           A_a0c0_1      = data.trtDep.csrDep.YCont$A_a0c0_1,
           prob_C_a11c0_1 = data.trtDep.csrDep.YCont$prob_C_a11c0_1,
           prob_C_a10c0_1 = data.trtDep.csrDep.YCont$prob_C_a10c0_1,
           prob_C_a01c0_1 = data.trtDep.csrDep.YCont$prob_C_a01c0_1,
           prob_C_a00c0_1 = data.trtDep.csrDep.YCont$prob_C_a00c0_1,
           C_a11c0_1 = data.trtDep.csrDep.YCont$C_a11c0_1,
           C_a10c0_1 = data.trtDep.csrDep.YCont$C_a10c0_1,
           C_a01c0_1 = data.trtDep.csrDep.YCont$C_a01c0_1,
           C_a00c0_1 = data.trtDep.csrDep.YCont$C_a00c0_1,
           # prob_Y_a11c0_2 = data.trtDep.csrDep.YCont$prob_Y_a11c0_2,
           # prob_Y_a10c0_2 = data.trtDep.csrDep.YCont$prob_Y_a10c0_2,
           # prob_Y_a01c0_2 = data.trtDep.csrDep.YCont$prob_Y_a01c0_2,
           # prob_Y_a00c0_2 = data.trtDep.csrDep.YCont$prob_Y_a00c0_2,
           Y_a11c0_2 = data.trtDep.csrDep.YCont$Y_a11c0_2,
           Y_a10c0_2 = data.trtDep.csrDep.YCont$Y_a10c0_2,
           Y_a01c0_2 = data.trtDep.csrDep.YCont$Y_a01c0_2,
           Y_a00c0_2 = data.trtDep.csrDep.YCont$Y_a00c0_2)
  data.trtDep.csrDep.YCont <- data.trtDep.csrDep.YCont %>%
    select(-W_4_a1c0_1, -W_4_a0c0_1, -W_5_a1c0_1, -W_5_a0c0_1,
           -prob_A_a1c0_1, -prob_A_a0c0_1, -A_a1c0_1, -A_a0c0_1,
           -prob_C_a11c0_1, -prob_C_a10c0_1, -prob_C_a01c0_1, -prob_C_a00c0_1, 
           -C_a11c0_1, -C_a10c0_1, -C_a01c0_1, -C_a00c0_1,
           -Y_a11c0_2,  -Y_a10c0_2, -Y_a01c0_2, -Y_a00c0_2)
  
  # table(data.trtDep.csrDep.YCont$A_1)
  # table(data.trtDep.csrDep.YCont$A_0[(data.trtDep.csrDep.YCont$C_0 == "uncensored") & (data.trtDep.csrDep.YCont$C_1 == "uncensored")], 
  #       data.trtDep.csrDep.YCont$A_1[(data.trtDep.csrDep.YCont$C_0 == "uncensored") & (data.trtDep.csrDep.YCont$C_1 == "uncensored")])
  # table(data.trtDep.csrDep.YCont$C_0, data.trtDep.csrDep.YCont$C_1)
  
  tree.ind  <- sample.int(n = n.samples, size = n.tree, replace = F)
  test.ind  <- (1:n.samples)[!((1:n.samples) %in% tree.ind)]
  tree.data        <- data.trtDep.csrDep.YCont[tree.ind, ]
  latent.tree.data <- latent.data.trtDep.csrDep.YCont[tree.ind, ]
  test.data        <- data.trtDep.csrDep.YCont[test.ind, ]
  latent.test.data <- latent.data.trtDep.csrDep.YCont[test.ind, ]
  
  # table(data.trtDep.csrDep.YCont$A_1, data.trtDep.csrDep.YCont$C_1)
  # table(data.trtDep.csrDep.YCont$A_0[!is.na(data.trtDep.csrDep.YCont$Y_2)], data.trtDep.csrDep.YCont$A_1[!is.na(data.trtDep.csrDep.YCont$Y_2)])
  # sum((data.trtDep.csrDep.YCont$C_1 == "uncensored") & (data.trtDep.csrDep.YCont$A_0 == 1) & (data.trtDep.csrDep.YCont$A_1 == 1), na.rm = T)
  
  # Truth (need to repeat to verify)
  #################
  #### time 1 #####
  #################
  tree.truth.trtDep.csrDep.YCont.a1.w2gt05.t1 <- mean(latent.tree.data$Y_a1c0_1[tree.data$W_2_0 > 0.5])
  tree.truth.trtDep.csrDep.YCont.a0.w2gt05.t1 <- mean(latent.tree.data$Y_a0c0_1[tree.data$W_2_0 > 0.5])
  tree.truth.trtDep.csrDep.YCont.a1.w2lteq05.t1 <- mean(latent.tree.data$Y_a1c0_1[tree.data$W_2_0 <= 0.5])
  tree.truth.trtDep.csrDep.YCont.a0.w2lteq05.t1 <- mean(latent.tree.data$Y_a0c0_1[tree.data$W_2_0 <= 0.5])
  tree.truth.t1 <- (tree.truth.trtDep.csrDep.YCont.a1.w2lteq05.t1 - tree.truth.trtDep.csrDep.YCont.a0.w2lteq05.t1) -
    (tree.truth.trtDep.csrDep.YCont.a1.w2gt05.t1 - tree.truth.trtDep.csrDep.YCont.a0.w2gt05.t1)
  
  test.truth.trtDep.csrDep.YCont.a1.w2gt05.t1 <- mean(latent.test.data$Y_a1c0_1[test.data$W_2_0 > 0.5])
  test.truth.trtDep.csrDep.YCont.a0.w2gt05.t1 <- mean(latent.test.data$Y_a0c0_1[test.data$W_2_0 > 0.5])
  test.truth.trtDep.csrDep.YCont.a1.w2lteq05.t1 <- mean(latent.test.data$Y_a1c0_1[test.data$W_2_0 <= 0.5])
  test.truth.trtDep.csrDep.YCont.a0.w2lteq05.t1 <- mean(latent.test.data$Y_a0c0_1[test.data$W_2_0 <= 0.5])
  test.truth.t1 <- (test.truth.trtDep.csrDep.YCont.a1.w2lteq05.t1 - test.truth.trtDep.csrDep.YCont.a0.w2lteq05.t1) -
    (test.truth.trtDep.csrDep.YCont.a1.w2gt05.t1 - test.truth.trtDep.csrDep.YCont.a0.w2gt05.t1)
  
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
  tree.truth.trtDep.csrDep.YCont.a1.w2gt05.t2 <- mean(latent.tree.data$Y_a11c0_2[tree.data$W_2_0 > 0.5])
  tree.truth.trtDep.csrDep.YCont.a0.w2gt05.t2 <- mean(latent.tree.data$Y_a00c0_2[tree.data$W_2_0 > 0.5])
  tree.truth.trtDep.csrDep.YCont.a1.w2lteq05.t2 <- mean(latent.tree.data$Y_a11c0_2[tree.data$W_2_0 <= 0.5])
  tree.truth.trtDep.csrDep.YCont.a0.w2lteq05.t2 <- mean(latent.tree.data$Y_a00c0_2[tree.data$W_2_0 <= 0.5])
  tree.truth.t2 <- (tree.truth.trtDep.csrDep.YCont.a1.w2lteq05.t2 - tree.truth.trtDep.csrDep.YCont.a0.w2lteq05.t2) -
    (tree.truth.trtDep.csrDep.YCont.a1.w2gt05.t2 - tree.truth.trtDep.csrDep.YCont.a0.w2gt05.t2)
  
  test.truth.trtDep.csrDep.YCont.a1.w2gt05.t2 <- mean(latent.test.data$Y_a11c0_2[test.data$W_2_0 > 0.5])
  test.truth.trtDep.csrDep.YCont.a0.w2gt05.t2 <- mean(latent.test.data$Y_a00c0_2[test.data$W_2_0 > 0.5])
  test.truth.trtDep.csrDep.YCont.a1.w2lteq05.t2 <- mean(latent.test.data$Y_a11c0_2[test.data$W_2_0 <= 0.5])
  test.truth.trtDep.csrDep.YCont.a0.w2lteq05.t2 <- mean(latent.test.data$Y_a00c0_2[test.data$W_2_0 <= 0.5])
  test.truth.t2 <- (test.truth.trtDep.csrDep.YCont.a1.w2lteq05.t2 - test.truth.trtDep.csrDep.YCont.a0.w2lteq05.t2) -
    (test.truth.trtDep.csrDep.YCont.a1.w2gt05.t2 - test.truth.trtDep.csrDep.YCont.a0.w2gt05.t2)
  
  latent.tree.data <- latent.tree.data %>%
    mutate(truth.trt.eff.t1 = ifelse(tree.data$W_2_0 > 0.5, 
                                     tree.truth.trtDep.csrDep.YCont.a1.w2gt05.t1 - tree.truth.trtDep.csrDep.YCont.a0.w2gt05.t1,
                                     tree.truth.trtDep.csrDep.YCont.a1.w2lteq05.t1 - tree.truth.trtDep.csrDep.YCont.a0.w2lteq05.t1),
           truth.trt.eff.t2 = ifelse(tree.data$W_2_0 > 0.5, 
                                     tree.truth.trtDep.csrDep.YCont.a1.w2gt05.t2 - tree.truth.trtDep.csrDep.YCont.a0.w2gt05.t2,
                                     tree.truth.trtDep.csrDep.YCont.a1.w2lteq05.t2 - tree.truth.trtDep.csrDep.YCont.a0.w2lteq05.t2))
  
  latent.test.data <- latent.test.data %>%
    mutate(truth.trt.eff.t1 = ifelse(test.data$W_2_0 > 0.5, 
                                     test.truth.trtDep.csrDep.YCont.a1.w2gt05.t1 - test.truth.trtDep.csrDep.YCont.a0.w2gt05.t1,
                                     test.truth.trtDep.csrDep.YCont.a1.w2lteq05.t1 - test.truth.trtDep.csrDep.YCont.a0.w2lteq05.t1),
           truth.trt.eff.t2 = ifelse(test.data$W_2_0 > 0.5, 
                                     test.truth.trtDep.csrDep.YCont.a1.w2gt05.t2 - test.truth.trtDep.csrDep.YCont.a0.w2gt05.t2,
                                     test.truth.trtDep.csrDep.YCont.a1.w2lteq05.t2 - test.truth.trtDep.csrDep.YCont.a0.w2lteq05.t2))
  
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
              noise.var        = c("W_1_0", "W_3_0", "W_4_0", "W_5_0")))
}
