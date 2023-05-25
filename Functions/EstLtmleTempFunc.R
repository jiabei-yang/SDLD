gen.flRk.df <- function(data.to.flRk, ind.A, ind.C, ind.Y, ind.L){
  
  colInd.flRk <- (sapply(data.to.flRk, function(col) length(unique(col[!is.na(col)]))) > 1)
  data.flRk   <- NULL
  max.t       <- length(ind.A) 
  lth.L       <- length(ind.L)
  
  updt.ind.A <- ind.A
  updt.ind.C <- ind.C
  updt.ind.Y <- ind.Y
  updt.ind.L <- ind.L
  
  ind.A.i <- 1
  col.i   <- 1
  ind.rm.L <- NULL  
  
  while (ind.A.i <= max.t) {
    # while(col.i <= 15){
    
    ind.C.i <- ind.A.i
    ind.Y.i <- ind.A.i
    
    if (col.i < ind.A[ind.A.i]) {
      
      # whether we have a full rank column
      if (colInd.flRk[col.i]) { 
        data.flRk <- cbind(data.flRk, data.to.flRk[, col.i])
        colnames(data.flRk)[ncol(data.flRk)] <- colnames(data.to.flRk)[col.i]
        
      } else { 
        updt.ind.A[ind.A.i:max.t] <- updt.ind.A[ind.A.i:max.t] - 1
        updt.ind.C[ind.C.i:max.t] <- updt.ind.C[ind.C.i:max.t] - 1
        updt.ind.Y[ind.Y.i:max.t] <- updt.ind.Y[ind.Y.i:max.t] - 1
        
        # for Lnodes, 
        # need to see if L node is baseline covariates, need to subtract 1 from all L nodes
        # need to subtract 1 from the later nodes index (only if there are later Lnodes)
        if (col.i %in% ind.L) {
          tmp.ind <- which(ind.L == col.i)
          if (tmp.ind < lth.L) {
            updt.ind.L[(tmp.ind+1):lth.L] <- updt.ind.L[(tmp.ind+1):lth.L] - 1
          }
          ind.rm.L <- c(ind.rm.L, tmp.ind)
          
        } else {
          updt.ind.L <- updt.ind.L - 1
        }
        
      }
      
      # look at next column
      col.i <- col.i + 1
      
      # when column becomes treatment
      # 1) go to the next treatment
      # 2) go to the next L node column
    } else { # else if col.i < ind.A[ind.A.i]
      ind.A.i <- ind.A.i + 1
      data.flRk <- cbind(data.flRk, data.to.flRk[, col.i:(col.i+2)])
      colnames(data.flRk)[(ncol(data.flRk)-2):ncol(data.flRk)] <- colnames(data.to.flRk)[col.i:(col.i+2)]
      
      col.i <- col.i + 3
    }
    
  } # while 
  
  # remove the L columns as needed
  if (!is.null(ind.rm.L)) {
    updt.ind.L <- updt.ind.L[-ind.rm.L]
  }
  
  return(list(data.flRk  = data.flRk,
              updt.ind.A = updt.ind.A,
              updt.ind.C = updt.ind.C,
              updt.ind.Y = updt.ind.Y,
              updt.ind.L = updt.ind.L))
  
}

stemp.ltmle <- function(y, wt, x, parms, continuous){
  
  # parms: num.truc.obs, number of observations to cut on both side
  #        num.optimal, number of iterations to calculate goodness and find maximum
  #        num.goodness.iter, number of iterations to look at in each round
  #        parallel, whether calculate in parallel
  
  # print(x[1])
  n <- length(y)
  
  sub.ind  <- y
  sub.data <- parms$whole.data[sub.ind, ]
  
  max.time <- length(parms$ind.A)
  
  # make sure enough number of observations in both treated and controled arm having outcome observed
  # return directly if there are not enough observations
  if (min(sum((apply(sub.data[, parms$ind.A], 1, function(x) sum(x == parms$abar$treatment, na.rm = T)) == max.time) & 
              (!is.na(sub.data[, parms$ind.Y[max.time]]))),
          sum((apply(sub.data[, parms$ind.A], 1, function(x) sum(x == parms$abar$control, na.rm = T)) == max.time) & 
              (!is.na(sub.data[, parms$ind.Y[max.time]])))) < parms$min.obsY.trt) {
    cat("no goodness here!\n")
    return(list(goodness  = rep(0, n - 1),
                direction = rep(1, n - 1)))
  }
  
  if (continuous) {
    
    # print("continuous")
    goodness  <- rep(0, n - 1)
    direction <- rep(1, n - 1)
    
    # for categorical variables converted to numeric values
    # only need to calculate at different categories
    ux     <- unique(x)
    lth.ux <- length(ux)
    fixed.calc.ind <- (lth.ux <= parms$max.lvls)
    
    if (fixed.calc.ind) {
      
      # if only one level, no goodness will be calculated 
      if (lth.ux == 1) {
        calc.ind <- NULL
      } else {
        tmp <- rle(x)
        calc.ind <- cumsum(tmp$lengths)
        calc.ind <- calc.ind[-lth.ux]
      }
      
    } else {
      if ((n - 2 * parms$num.truc.obs) < parms$num.goodness.iter) {
        calc.ind <- parms$num.truc.obs:(n - parms$num.truc.obs)
      } else {
        calc.ind <- unique(round(seq(parms$num.truc.obs, n - parms$num.truc.obs, length.out = parms$num.goodness.iter)))
      }
    }
    
    while (!is.null(calc.ind)) {
      
      # Ensure the model is at least fitted on num.truc.obs obs
      # for (i in (parms$num.truc.obs:(n-parms$num.truc.obs))) { 
      
      if (parms$parallel) {
        
        tmp.res <- foreach(calc.ind.i = 1:length(calc.ind),
                           .packages  = "ltmle",
                           .combine   = rbind) %dopar% {
                             
                             # if (calc.ind.i %% 100 == 0) {
                             #   print(calc.ind.i)
                             # }
                             
                             i <- calc.ind[calc.ind.i]
                             sub.data.l <- sub.data[1:i, ]
                             sub.data.r <- sub.data[(i+1):n, ]
                             wt.l       <- wt[1:i]
                             wt.r       <- wt[(i+1):n]
                             
                             colnames.sub.data <- colnames(sub.data)
                             
                             # make sure enough number of observations in both treated and controled arm having outcome observed
                             if (min(sum((apply(sub.data.l[, parms$ind.A], 1, function(x) sum(x == parms$abar$treatment, na.rm = T)) == max.time) & 
                                         (!is.na(sub.data.l[, parms$ind.Y[max.time]]))),
                                     sum((apply(sub.data.r[, parms$ind.A], 1, function(x) sum(x == parms$abar$treatment, na.rm = T)) == max.time) &
                                         (!is.na(sub.data.r[, parms$ind.Y[max.time]]))),
                                     sum((apply(sub.data.l[, parms$ind.A], 1, function(x) sum(x == parms$abar$control, na.rm = T)) == max.time) &
                                         (!is.na(sub.data.l[, parms$ind.Y[max.time]]))),
                                     sum((apply(sub.data.r[, parms$ind.A], 1, function(x) sum(x == parms$abar$control, na.rm = T)) == max.time) & 
                                         (!is.na(sub.data.r[, parms$ind.Y[max.time]])))) < parms$min.obsY.trt) {
                               # cat("here!\n")
                               return(c(0, 1))
                             }
                             
                             # get sub.data.i into full rank matrix
                             # need to find how many columns are removed after making the matrix full rank                             
                             tmp.l <- gen.flRk.df(data.to.flRk = sub.data.l,
                                                  ind.A        = parms$ind.A,
                                                  ind.C        = parms$ind.C,
                                                  ind.Y        = parms$ind.Y,
                                                  ind.L        = parms$ind.L)
                             tmp.r <- gen.flRk.df(data.to.flRk = sub.data.r,
                                                  ind.A        = parms$ind.A,
                                                  ind.C        = parms$ind.C,
                                                  ind.Y        = parms$ind.Y,
                                                  ind.L        = parms$ind.L)
                             
                             # t0 <- Sys.time()
                             result.tmle.l <- withWarnings(try(suppressMessages(ltmle(tmp.l$data.flRk, 
                                                                                      Anodes = tmp.l$updt.ind.A, 
                                                                                      Cnodes = tmp.l$updt.ind.C,
                                                                                      Lnodes = tmp.l$updt.ind.L,
                                                                                      Ynodes = tmp.l$updt.ind.Y, 
                                                                                      survivalOutcome = parms$survivalOutcome, 
                                                                                      abar            = parms$abar, 
                                                                                      gbounds         = parms$gbounds,
                                                                                      estimate.time   = F, 
                                                                                      observation.weights = wt.l))))
                             # t1 <- Sys.time() 
                             # lth.t1 <- t1-t0
                             
                             # tmp.data  <- tmp.l$data.flRk
                             # tmp.ind.A <- tmp.l$updt.ind.A
                             # tmp.ind.C <- tmp.l$updt.ind.C
                             # tmp.ind.L <- tmp.l$updt.ind.L
                             # tmp.ind.Y <- tmp.l$updt.ind.Y
                             # t0 <- Sys.time()
                             # result.tmle.l <- withWarnings(try(suppressMessages(ltmle(tmp.data, 
                             #                                                          Anodes = tmp.ind.A, 
                             #                                                          Cnodes = tmp.ind.C,
                             #                                                          Lnodes = tmp.ind.L,
                             #                                                          Ynodes = tmp.ind.Y, 
                             #                                                          survivalOutcome = parms$survivalOutcome, 
                             #                                                          abar            = parms$abar, 
                             #                                                          gbounds         = parms$gbounds,
                             #                                                          estimate.time   = F, 
                             #                                                          observation.weights = wt.l))))
                             # t1 <- Sys.time() 
                             # lth.t2 <- t1-t0
                             
                             result.tmle.r <- withWarnings(try(suppressMessages(ltmle(tmp.r$data.flRk, 
                                                                                      Anodes = tmp.r$updt.ind.A, 
                                                                                      Cnodes = tmp.r$updt.ind.C,
                                                                                      Lnodes = tmp.r$updt.ind.L,
                                                                                      Ynodes = tmp.r$updt.ind.Y, 
                                                                                      survivalOutcome = parms$survivalOutcome, 
                                                                                      abar            = parms$abar, 
                                                                                      gbounds         = parms$gbounds,
                                                                                      estimate.time   = F, 
                                                                                      observation.weights = wt.r))))
                             
                             # if error message "Error in FitAndPredict() : \n  Estimation failed because there are fewer than 2 observations to fit\n"
                             # use original goodness and direction value
                             # proceed separately for left and right split
                             if (class(result.tmle.l$value) == "try-error") {
                               
                               return(c(0, 1))
                               # if (result.tmle.l == "Error in FitAndPredict() : \n  Estimation failed because there are fewer than 2 observations to fit\n") {
                               # c(0, 1)
                               # } else {
                               #   # print(c(x[1], calc.ind.i))
                               #   stop("Different error in ltmle")
                               # }
                               
                             } else if (class(result.tmle.r$value) == "try-error") {
                               return(c(0, 1))
                               # if (result.tmle.r == "Error in FitAndPredict() : \n  Estimation failed because there are fewer than 2 observations to fit\n") {
                               #   c(0, 1)
                               # } else {
                               #   # print(c(x[1], calc.ind.i))
                               #   stop("Different error in ltmle")
                               # }
                               
                             } else {
                               summ.l <- summary(result.tmle.l$value)
                               summ.r <- summary(result.tmle.r$value)
                               
                               # summ.l$effect.measures$ATE
                               # summ.r$effect.measures$ATE
                               tmp.goodness <- (summ.l$effect.measures$ATE$estimate - summ.r$effect.measures$ATE$estimate)^2 /
                                 ((summ.l$effect.measures$ATE$std.dev)^2 + (summ.r$effect.measures$ATE$std.dev)^2)
                               tmp.direction <- sign(summ.l$effect.measures$ATE$estimate - summ.r$effect.measures$ATE$estimate)
                               
                               return(c(tmp.goodness, tmp.direction))
                             } # else class() == "try-error"
                             
                           } # tmp.res
        
        if (length(calc.ind) == 1) {
          goodness[calc.ind] <- tmp.res[1]
          direction[calc.ind] <- tmp.res[2]
        } else {
          goodness[calc.ind] <- tmp.res[, 1]
          direction[calc.ind] <- tmp.res[, 2]
        }
        
      } else { # if else parallel
        
        for (calc.ind.i in 1:length(calc.ind)) {
          
          # if (calc.ind.i %% 100 == 0) {
          #   print(calc.ind.i)
          # }
          
          i <- calc.ind[calc.ind.i]
          sub.data.l <- sub.data[1:i, ]
          sub.data.r <- sub.data[(i+1):n, ]
          wt.l       <- wt[1:i]
          wt.r       <- wt[(i+1):n]
          
          colnames.sub.data <- colnames(sub.data)
          
          # make sure enough number of observations in both treated and controled arm having outcome observed
          if (min(sum((apply(sub.data.l[, parms$ind.A], 1, function(x) sum(x == parms$abar$treatment, na.rm = T)) == max.time) & 
                      (!is.na(sub.data.l[, parms$ind.Y[max.time]]))),
                  sum((apply(sub.data.r[, parms$ind.A], 1, function(x) sum(x == parms$abar$treatment, na.rm = T)) == max.time) &
                      (!is.na(sub.data.r[, parms$ind.Y[max.time]]))),
                  sum((apply(sub.data.l[, parms$ind.A], 1, function(x) sum(x == parms$abar$control, na.rm = T)) == max.time) &
                      (!is.na(sub.data.l[, parms$ind.Y[max.time]]))),
                  sum((apply(sub.data.r[, parms$ind.A], 1, function(x) sum(x == parms$abar$control, na.rm = T)) == max.time) & 
                      (!is.na(sub.data.r[, parms$ind.Y[max.time]])))) < parms$min.obsY.trt) {
            next
          }
          
          # get sub.data.i into full rank matrix
          # need to find how many columns are removed after making the matrix full rank
          tmp.l <- gen.flRk.df(data.to.flRk = sub.data.l,
                               ind.A        = parms$ind.A,
                               ind.C        = parms$ind.C,
                               ind.Y        = parms$ind.Y,
                               ind.L        = parms$ind.L)
          tmp.r <- gen.flRk.df(data.to.flRk = sub.data.r,
                               ind.A        = parms$ind.A,
                               ind.C        = parms$ind.C,
                               ind.Y        = parms$ind.Y,
                               ind.L        = parms$ind.L)
          
          # tmp <- sub.data.l.flRk %>%
          #   mutate(nhif_3 = ifelse(nhif_3 == "no", 0 , nhif_3))
          # head(tmp.l$data.flRk)
          # tmp.l$updt.ind.A
          # tmp.l$updt.ind.C
          # tmp.l$updt.ind.L
          # tmp.l$updt.ind.Y
          
          result.tmle.l <- withWarnings(try(suppressMessages(ltmle(tmp.l$data.flRk, 
                                                                   Anodes = tmp.l$updt.ind.A, 
                                                                   Cnodes = tmp.l$updt.ind.C,
                                                                   Lnodes = tmp.l$updt.ind.L,
                                                                   Ynodes = tmp.l$updt.ind.Y, 
                                                                   survivalOutcome = parms$survivalOutcome, 
                                                                   abar            = parms$abar, 
                                                                   gbounds         = parms$gbounds,
                                                                   estimate.time   = F, 
                                                                   observation.weights = wt.l))))
          
          result.tmle.r <- withWarnings(try(suppressMessages(ltmle(tmp.r$data.flRk, 
                                                                   Anodes = tmp.r$updt.ind.A, 
                                                                   Cnodes = tmp.r$updt.ind.C,
                                                                   Lnodes = tmp.r$updt.ind.L,
                                                                   Ynodes = tmp.r$updt.ind.Y, 
                                                                   survivalOutcome = parms$survivalOutcome, 
                                                                   abar            = parms$abar, 
                                                                   gbounds         = parms$gbounds,
                                                                   estimate.time   = F, 
                                                                   observation.weights = wt.r))))
          
          # if error message "Error in FitAndPredict() : \n  Estimation failed because there are fewer than 2 observations to fit\n"
          # use original goodness and direction value
          # proceed separately for left and right split
          if (class(result.tmle.l$value) == "try-error") {
            
            next
            # if (result.tmle.l == "Error in FitAndPredict() : \n  Estimation failed because there are fewer than 2 observations to fit\n") {
            #   next
            # } else {
            #   print(c(x[1], calc.ind.i))
            #   stop("Different error in ltmle")
            # }
            
          } else if (class(result.tmle.r$value) == "try-error") {
            
            next
            # if (result.tmle.r == "Error in FitAndPredict() : \n  Estimation failed because there are fewer than 2 observations to fit\n") {
            #   next
            # } else {
            #   print(c(x[1], calc.ind.i))
            #   stop("Different error in ltmle")
            # }
            
          } else {
            
            summ.l <- summary(result.tmle.l$value)
            summ.r <- summary(result.tmle.r$value)
            
            # summ.l$effect.measures$ATE
            # summ.r$effect.measures$ATE
            goodness[i] <- (summ.l$effect.measures$ATE$estimate - summ.r$effect.measures$ATE$estimate)^2 /
              ((summ.l$effect.measures$ATE$std.dev)^2 + (summ.r$effect.measures$ATE$std.dev)^2)
            direction[i] <- sign(summ.l$effect.measures$ATE$estimate - summ.r$effect.measures$ATE$estimate)
          }
        } # for calc.ind.i loop
        
      } # else parallel
      
      # goodness[calc.ind]  
      # plot(goodness[calc.ind])
      # sum(sub.data$W_2_0<=0.5)
      # calc.ind[which.max(goodness[calc.ind])]
      # ate.l
      # ate.r
      # ate.l <- summ.l$effect.measures$ATE
      # ate.r <- summ.r$effect.measures$ATE
      
      if (length(unique(goodness)) == 1) {
        break
        
      } else {
        
        # there might be more than 1 step values
        # only NA if 1 step value and equal to 1
        if (((length(unique(diff(calc.ind))) == 1) & (1 %in% unique(diff(calc.ind)))) | fixed.calc.ind) {
          calc.ind <- NULL
          
        } else {
          
          length.calc.ind <- length(calc.ind)
          avg.goodness <- rep(0, length.calc.ind)
          # number of indexes to skip at the beginning or end to take average
          skip.ind <- floor(parms$num.optimal / 2)
          
          # treat differently when odds and even number of num.optimal
          if ((parms$num.optimal %% 2) == 1) {
            
            for (calc.ind.i in (1+skip.ind):(length.calc.ind-skip.ind)) {
              avg.goodness[calc.ind.i] <- sum(goodness[calc.ind][(calc.ind.i-skip.ind):(calc.ind.i+skip.ind)]) / parms$num.optimal
            }
            
            max.goodness   <- which.max(avg.goodness)
            start.calc.ind <- max.goodness - skip.ind
            end.calc.ind   <- max.goodness + skip.ind
            
            
          } else { # ((parms$num.optimal %% 2) == 1)
            for (calc.ind.i in (1+skip.ind):(length.calc.ind-skip.ind+1)) {
              avg.goodness[calc.ind.i] <- sum(goodness[calc.ind][(calc.ind.i-skip.ind):(calc.ind.i+skip.ind-1)]) / parms$num.optimal
            }
            
            max.goodness   <- which.max(avg.goodness)
            start.calc.ind <- max.goodness - skip.ind
            end.calc.ind   <- max.goodness + skip.ind - 1
            
          } # if ((parms$num.optimal %% 2) == 1) {
          
          # create new indexes based on length of intervals
          if ((calc.ind[end.calc.ind] - calc.ind[start.calc.ind] + 1) <= parms$num.goodness.iter) {
            calc.ind <- calc.ind[start.calc.ind]:calc.ind[end.calc.ind]
            
          } else {
            calc.ind <- round(seq(calc.ind[start.calc.ind], calc.ind[end.calc.ind], length.out = parms$num.goodness.iter))
          }
          
        } # if ((length(unique(diff(calc.ind))) == 1) & (1 %in% unique(diff(calc.ind)))) { calc.ind == NULL
        
      } # goodness not all 0s
      
    } # while
    
    # plot(goodness)
  } else { # if continuous
    
    # print("not continuous")
    ux <- sort(unique(x))
    lth.ux <- length(ux)
    
    trt.eff.ux <- NULL
    
    # order treatment effect
    for (ux.i in 1:lth.ux) {
      
      ind.ux.i <- (x == ux[ux.i])
      sub.data.i <- sub.data[ind.ux.i, ]
      
      # get sub.data.i into full rank matrix
      # need to find how many columns are removed after making the matrix full rank      
      tmp <- gen.flRk.df(data.to.flRk = sub.data.i,
                         ind.A        = parms$ind.A,
                         ind.C        = parms$ind.C,
                         ind.Y        = parms$ind.Y,
                         ind.L        = parms$ind.L)
      
      result.tmle.i <- withWarnings(try(suppressMessages(ltmle(tmp$data.flRk, 
                                                               Anodes = tmp$updt.ind.A, 
                                                               Cnodes = tmp$updt.ind.C,
                                                               Lnodes = tmp$updt.ind.L,
                                                               Ynodes = tmp$updt.ind.Y, 
                                                               survivalOutcome = parms$survivalOutcome, 
                                                               abar            = parms$abar, 
                                                               gbounds         = parms$gbounds,
                                                               estimate.time   = F, 
                                                               observation.weights = wt[ind.ux.i]))))
      
      if (class(result.tmle.i$value) == "try-error") {
        stop("Error in ordinal split")
      } else {
        summ.i <- summary(result.tmle.i$value)
      }
      trt.eff.ux <- c(trt.eff.ux, summ.i$effect.measures$ATE$estimate)
      
    } # for ux.i loop
    
    ord.ux <- order(trt.eff.ux)
    goodness  <- rep(0, lth.ux - 1)
    direction <- ux[ord.ux] 
    
    for (ux.i in 1:(lth.ux - 1)) {
      
      ind.l <- (x %in% ux[ord.ux[1:ux.i]])
      ind.r <- (x %in% ux[ord.ux[(ux.i + 1):lth.ux]])
      
      sub.data.l <- sub.data[ind.l, ]
      sub.data.r <- sub.data[ind.r, ]
      wt.l       <- wt[ind.l]
      wt.r       <- wt[ind.r]
      
      tmp.l <- gen.flRk.df(data.to.flRk = sub.data.l,
                           ind.A        = parms$ind.A,
                           ind.C        = parms$ind.C,
                           ind.Y        = parms$ind.Y,
                           ind.L        = parms$ind.L)
      tmp.r <- gen.flRk.df(data.to.flRk = sub.data.r,
                           ind.A        = parms$ind.A,
                           ind.C        = parms$ind.C,
                           ind.Y        = parms$ind.Y,
                           ind.L        = parms$ind.L)
      
      result.tmle.l <- withWarnings(try(suppressMessages(ltmle(tmp.l$data.flRk, 
                                                               Anodes = tmp.l$updt.ind.A, 
                                                               Cnodes = tmp.l$updt.ind.C,
                                                               Lnodes = tmp.l$updt.ind.L,
                                                               Ynodes = tmp.l$updt.ind.Y, 
                                                               survivalOutcome = parms$survivalOutcome, 
                                                               abar            = parms$abar, 
                                                               gbounds         = parms$gbounds,
                                                               estimate.time   = F, 
                                                               observation.weights = wt.l))))
      
      result.tmle.r <- withWarnings(try(suppressMessages(ltmle(tmp.r$data.flRk, 
                                                               Anodes = tmp.r$updt.ind.A, 
                                                               Cnodes = tmp.r$updt.ind.C,
                                                               Lnodes = tmp.r$updt.ind.L,
                                                               Ynodes = tmp.r$updt.ind.Y, 
                                                               survivalOutcome = parms$survivalOutcome, 
                                                               abar            = parms$abar, 
                                                               gbounds         = parms$gbounds,
                                                               estimate.time   = F, 
                                                               observation.weights = wt.r))))
      
      if ((class(result.tmle.l) == "try-error") | (class(result.tmle.r) == "try-error")) {
        next
        # stop("Error in ordinal split")
      } else {
        summ.l <- summary(result.tmle.l$value)
        summ.r <- summary(result.tmle.r$value)
      }
      
      goodness[ux.i] <- (summ.l$effect.measures$ATE$estimate - summ.r$effect.measures$ATE$estimate)^2 /
        ((summ.l$effect.measures$ATE$std.dev)^2 + (summ.r$effect.measures$ATE$std.dev)^2)
      
    } # for ux.i loop
    
  } # else ordinal split 
  
  cat(c(x[1], max(goodness), which.max(goodness), direction[which.max(goodness)], "\n"))
  list(goodness  = goodness,
       direction = direction)
  
}

etemp.ltmle <- function(y, wt, parms) {
  
  n <- length(y)
  
  sub.ind  <- y
  sub.data <- parms$whole.data[sub.ind, ]
  
  wmean <- sum(sub.data[, parms$ind.Y[length(parms$ind.Y)]]*wt, na.rm = T)/sum(wt)
  rss <- sum(wt*(sub.data[, parms$ind.Y[length(parms$ind.Y)]]-wmean)^2, na.rm = T)
  
  # get sub.data.i into full rank matrix
  # need to find how many columns are removed after making the matrix full rank
  tmp <- gen.flRk.df(data.to.flRk = sub.data,
                     ind.A        = parms$ind.A,
                     ind.C        = parms$ind.C,
                     ind.Y        = parms$ind.Y,
                     ind.L        = parms$ind.L)
  
  result.tmle <- withWarnings(try(suppressMessages(ltmle(tmp$data.flRk, 
                                                         Anodes = tmp$updt.ind.A, 
                                                         Cnodes = tmp$updt.ind.C,
                                                         Lnodes = tmp$updt.ind.L,
                                                         Ynodes = tmp$updt.ind.Y, 
                                                         survivalOutcome = parms$survivalOutcome, 
                                                         abar            = parms$abar, 
                                                         gbounds         = parms$gbounds,
                                                         estimate.time   = F, 
                                                         observation.weights = wt))))
  
  # if error message "Error in FitAndPredict() : \n  Estimation failed because there are fewer than 2 observations to fit\n"
  # use original goodness and direction value
  # proceed separately for left and right split
  if (class(result.tmle$value) == "try-error") {
    
    avg.trt.effct <- 0
    # if (result.tmle == "Error in FitAndPredict() : \n  Estimation failed because there are fewer than 2 observations to fit\n") {
    #   avg.trt.effct <- NA
    # } else {
    #   print("eval")
    #   stop("Different error in ltmle")
    # }
  } else {
    summ <- summary(result.tmle$value)
    avg.trt.effct <- summ$effect.measures$ATE$estimate
  }
  
  cat(c(n, avg.trt.effct, range(y), "\n"))
  list(label = avg.trt.effct, deviance = rss)
  
}


