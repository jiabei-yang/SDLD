EstLtmle.CvMethod1.MltLamda = function(tree.data, tree.list, lambda.used, test.data, 
                                       ind.A, ind.C, ind.L, ind.Y, survivalOutcome,
                                       abar, gbounds, min.obsY.trt) {
  
  # min.obs.mod: minimum number of observations for the model to be fitted
  # models when there are only treated/untreated unit in the subset
  
  colnames.test.data <- colnames(test.data)
  
  # Storage space for the complexity value associated with each candidate tree
  complex.val = matrix(NA, ncol = length(tree.list), nrow = length(lambda.used))
  
  max.time <- length(ind.A)
  
  # Looping through the candidate trees
  for (m in 1:ncol(complex.val)) {
    # for (m in c(1)){
    # Finding the tree being evaluated
    # print(m)
    tree.used = tree.list[[m]] 
    
    # If only root node there is no internal node
    if (nrow(tree.used$frame) == 1) {
      goodness.test = 0
      numb.int = 0
      
    } else {            # If at least one split
      is.leaf <- (tree.used$frame$var == "<leaf>")
      goodness.test = 0
      # tmp.goodness <- NULL
      # Calculate the goodness of the tree using the test
      # Finding the test data falling in each terminal node
      numb.int = sum(!is.leaf)
      # Finding all kids on terminal node 
      #kids.i <- order(as.numeric(rownames(tree.used$frame)))
      
      # Calculating split complexity for each internal node of branch
      right.ind  <- 1
      last.right <- NULL
      # w.last.right <- NULL
      
      for (h in 1:dim(tree.used$frame)[1]) {
        # for (h in 3:109){
        # h <- h + 1
        
        # Finding observations in validation sample falling in that node
        if (tree.used$frame$var[h] == "<leaf>") {
          next
          
        } else if (tree.used$frame$n[h] == dim(tree.data)[1]) {
          test.data.used <- test.data
          # val.w.used      <- val.w
          
        } else {
          if (!is.null(last.left)) {
            test.data.used <- last.left[[1]]
            # val.w.used      <- w.last.left[[1]]
          } else {
            test.data.used <- as.data.frame(last.right[[right.ind - 1]])
            # val.w.used      <- as.data.frame(w.last.right[[right.ind - 1]])
            if (length(last.right) > 2) {
              last.right   <- last.right[1:(right.ind - 2)]
              # w.last.right <- w.last.right[1:(right.ind - 2)]
            } else if (length(last.right) == 2){
              last.right   <- list(last.right[[1]])
              # w.last.right <- list(w.last.right[[1]])
            } else {
              last.right <- NULL
              # w.last.right <- NULL
            }
            right.ind <- right.ind - 1
          }
        }
        
        row.ind <- sum((tree.used$frame$var[1:h]) != "<leaf>")
        split.used <- tree.used$splits[row.ind, 4]
        var.used <- tree.used$frame$var[h]
        col.ind <- which(colnames(test.data.used) == var.used)
        
        # Calculate goodness corresponding to split
        # Finding observations falling in right and left node
        # Need to figure out observations going to the left/right node
        
        # NEED TO UPDATE CATEGORICAL SPLITS LATER IF USED
        # if ((split.used %% 1) == 0){   
        # if (class(tree.data[, as.character(var.used)]) == "factor") {   
        #   # Categorical covariate split
        #   lvls <- levels(test.data[, col.ind])
        #   test.data.left  <- test.data.used[test.data.used[, col.ind] %in% lvls[tree.used$csplit[split.used, ] == 1], ]
        #   test.data.right <- test.data.used[test.data.used[, col.ind] %in% lvls[tree.used$csplit[split.used,] == 3], ]
        #   # val.w.used.left  <- val.w.used[test.data.used[, col.ind] %in% lvls[tree.used$csplit[split.used, ] == 1], ]
        #   # val.w.used.right <- val.w.used[test.data.used[, col.ind] %in% lvls[tree.used$csplit[split.used,] == 3], ]
        
        # } else {
        # Continuous covariate split
        # Need to take care of left or right
        if (tree.used$splits[row.ind, 2] > 0) {
          test.data.left  <- test.data.used[test.data.used[,  col.ind] >= split.used, ]
          test.data.right <- test.data.used[test.data.used[,  col.ind] < split.used, ]
          # val.w.used.left  <- val.w.used[test.data.used[,  col.ind] >= split.used, ]
          # val.w.used.right <- val.w.used[test.data.used[,  col.ind] < split.used, ]
          
        } else {
          test.data.left <- test.data.used[test.data.used[,  col.ind] < split.used, ]
          test.data.right <- test.data.used[test.data.used[,  col.ind] >= split.used, ]
          # val.w.used.left  <- val.w.used[test.data.used[,  col.ind] < split.used, ]
          # val.w.used.right <- val.w.used[test.data.used[,  col.ind] >= split.used, ]
        }
        
        # }
        
        if (tree.used$frame$var[h+1] != "<leaf>") {
          last.left   <- list(test.data.left)
          # w.last.left <- list(val.w.used.left)
        } else{
          last.left   <- NULL
          # w.last.left <- NULL
        }
        
        which.right <- as.numeric(rownames(tree.used$frame)[h+1]) + 1
        if (tree.used$frame$var[as.numeric(rownames(tree.used$frame)) == which.right] != "<leaf>") {
          last.right[[right.ind]]   <- test.data.right
          # w.last.right[[right.ind]] <- val.w.used.right
          right.ind <- right.ind + 1
        } 
        
        # Skip if there is no treated/untreated in the left/right subgroup
        if (min(sum((apply(test.data.left[, ind.A], 1, function(x) sum(x == abar$treatment, na.rm = T)) == max.time) & 
                    (!is.na(test.data.left[, ind.Y[max.time]]))),
                sum((apply(test.data.right[, ind.A], 1, function(x) sum(x == abar$treatment, na.rm = T)) == max.time) &
                    (!is.na(test.data.right[, ind.Y[max.time]]))),
                sum((apply(test.data.left[, ind.A], 1, function(x) sum(x == abar$control, na.rm = T)) == max.time) &
                    (!is.na(test.data.left[, ind.Y[max.time]]))),
                sum((apply(test.data.right[, ind.A], 1, function(x) sum(x == abar$control, na.rm = T)) == max.time) & 
                    (!is.na(test.data.right[, ind.Y[max.time]])))) < min.obsY.trt) {
          # if (min(sum(rowSums(test.data.left[, ind.A] == abar$treatment) == length(ind.A), na.rm = T),
          #         sum(rowSums(test.data.left[, ind.A] == abar$control) == length(ind.A), na.rm = T),
          #         sum(rowSums(test.data.right[, ind.A] == abar$treatment) == length(ind.A), na.rm = T),
          #         sum(rowSums(test.data.right[, ind.A] == abar$control) == length(ind.A), na.rm = T)) == 0) {
          next
        }
        
        # get test.data.left, test.data.right into full rank matrix
        # need to find how many columns are removed after making the matrix full rank                             
        tmp.l <- gen.flRk.df(data.to.flRk = test.data.left,
                             ind.A        = ind.A,
                             ind.C        = ind.C,
                             ind.Y        = ind.Y,
                             ind.L        = ind.L)
        tmp.r <- gen.flRk.df(data.to.flRk = test.data.right,
                             ind.A        = ind.A,
                             ind.C        = ind.C,
                             ind.Y        = ind.Y,
                             ind.L        = ind.L)
        
        result.tmle.l <- withWarnings(try(suppressMessages(ltmle(tmp.l$data.flRk, 
                                                                 Anodes = tmp.l$updt.ind.A, 
                                                                 Cnodes = tmp.l$updt.ind.C,
                                                                 Lnodes = tmp.l$updt.ind.L,
                                                                 Ynodes = tmp.l$updt.ind.Y, 
                                                                 survivalOutcome = survivalOutcome, 
                                                                 abar            = abar, 
                                                                 gbounds         = gbounds,
                                                                 estimate.time   = F))))
        
        result.tmle.r <- withWarnings(try(suppressMessages(ltmle(tmp.r$data.flRk, 
                                                                 Anodes = tmp.r$updt.ind.A, 
                                                                 Cnodes = tmp.r$updt.ind.C,
                                                                 Lnodes = tmp.r$updt.ind.L,
                                                                 Ynodes = tmp.r$updt.ind.Y, 
                                                                 survivalOutcome = survivalOutcome, 
                                                                 abar            = abar, 
                                                                 gbounds         = gbounds,
                                                                 estimate.time   = F))))
        
        # if error message "Error in FitAndPredict() : \n  Estimation failed because there are fewer than 2 observations to fit\n"
        # use original goodness and direction value
        # proceed separately for left and right split
        if (class(result.tmle.l$value) == "try-error") {
          
          # if (result.tmle.l == "Error in FitAndPredict() : \n  Estimation failed because there are fewer than 2 observations to fit\n") {
          next
          # } else {
          #   stop("Different error in ltmle")
          # }
          
        } else if (class(result.tmle.r$value) == "try-error") {
          
          # if (result.tmle.r == "Error in FitAndPredict() : \n  Estimation failed because there are fewer than 2 observations to fit\n") {
          next
          # } else {
          #   stop("Different error in ltmle")
          # }
          
        } else {
          summ.l <- summary(result.tmle.l$value)
          summ.r <- summary(result.tmle.r$value)
          
          # summ.l$effect.measures$ATE
          # summ.r$effect.measures$ATE
          goodness.test <- goodness.test + (summ.l$effect.measures$ATE$estimate - summ.r$effect.measures$ATE$estimate)^2 /
            ((summ.l$effect.measures$ATE$std.dev)^2 + (summ.r$effect.measures$ATE$std.dev)^2)
        }
        
        # print(var)
      } # End h loop
    } # End if (nrow(tree.used$frame) == 1)
    # Calculating complexity value
    
    for (lambda.i in 1:length(lambda.used)){
      complex.val[lambda.i, m] = goodness.test - lambda.used[lambda.i] * numb.int
    }
    
  } # End m loop
  
  tree.final <- list()
  # Averaging over cross validation sets
  for (lambda.i in 1:length(lambda.used)){
    tree.final[[lambda.i]] = tree.list[[which.max(complex.val[lambda.i, ])]]
  }
  
  return(list(tree.final, complex.val))
}
