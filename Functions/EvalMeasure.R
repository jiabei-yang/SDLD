eval.measures.eff <- function(final.tree, test.data, latent.test.data, noise.var, 
                              corr.split, where.split, dir.split) {
  
  # Calculate the size of tree 
  size.tree <- sum(final.tree$frame$var == "<leaf>")
  
  # Calcualte the number of times the tree splits on noise variables 
  numb.noise <- sum(final.tree$frame$var %in% noise.var)
  
  # find if the tree is exactly the correct tree
  if (is.null(corr.split)) {
    exact.corr <- sum(size.tree == 1)
    
  } else if (size.tree != (length(corr.split[[1]]) + 1)) {
    exact.corr <- 0
    
  } else {
    
    for (list.i in 1:length(corr.split)) {
      cond <- T
      
      for (split.i in 1:length(corr.split[[list.i]])) {
        cond <- cond & (final.tree$frame$var[where.split[[list.i]][split.i]] == corr.split[[list.i]][split.i])
        cond <- cond & (final.tree$splits[split.i, "ncat"] == dir.split[[list.i]][split.i])
        
      }
      
      if (cond) {
        break
      } 
      
    }
    
    exact.corr <- sum(cond)
    # there should not be splits made on noise variables bc all trees larger than the wanted size are filtered out in the last condition
    
  }
  
  # MSE
  # when there is only 1 split or root node, r session aborted
  if (nrow(final.tree$frame) > 3) {
    pred.tree = predict(final.tree, newdata = test.data)
    
  } else if (nrow(final.tree$frame) == 3) {
    # If there is one or zero splits there is a weird memory error so need to do manually
    pred.tree = rep(NA, nrow(test.data))
    split.used = final.tree$splits[, 4]
    var.used = final.tree$frame$var[1]
    col.ind <- which(colnames(test.data) == var.used)
    
    if (length(split.used) > 1) {
      split.used <- split.used[1]
      direction <- final.tree$splits[1, 2]
    } else {
      direction <- final.tree$splits[2]
    }
    
    # Need to figure out observations going to the left/right node
    if ((split.used %% 1) == 0) {   
      
      # Categorical covariate split
      lvls <- levels(test.data[, col.ind])
      pred.tree[test.data[, col.ind] %in% lvls[final.tree$csplit[split.used,] == 1]] <- final.tree$frame$yval[2]
      pred.tree[test.data[, col.ind] %in% lvls[final.tree$csplit[split.used,] == 3]] <- final.tree$frame$yval[3]
      
    } else{
      # Continuous covariate split
      # Need to take care of left or right
      if (direction > 0) {
        pred.tree[test.data[,  col.ind] >= split.used] <- final.tree$frame$yval[2]
        pred.tree[test.data[,  col.ind] < split.used]  <- final.tree$frame$yval[3]
      } else {
        pred.tree[test.data[,  col.ind] < split.used]  <- final.tree$frame$yval[2]
        pred.tree[test.data[,  col.ind] >= split.used] <- final.tree$frame$yval[3]
      }
      
    }
    
  } else { # root only tree
    pred.tree = rep(final.tree$frame$yval, nrow(test.data))
  }
  # mse will be NA if there is only 1 observation in the terminal node
  mse  <- mean((pred.tree - latent.test.data$truth.trt.eff.t2)^2)
  
  # PPS
  pps    <- 1
  n.comb <- choose(nrow(test.data), 2)

  for (i in 1:(nrow(test.data)-1)) {

    for (j in (i+1):nrow(test.data)) {

      a=b=0
      if (latent.test.data$truth.trt.eff.t2[i] == latent.test.data$truth.trt.eff.t2[j]) {
        a = 1
      }

      # Treat observations in the 1-observation terminal node be in the same node
      # if(is.na(pred.tree[i]) | is.na(pred.tree[j])) {
      #   b = 1
      # } else
      if (pred.tree[i] == pred.tree[j]) {
        b = 1
      }
      pps = pps - abs(a-b) / n.comb
    } # for j loop
  } # for i loop
  
  return(list(mse                 = mse, 
              exact.corr          = exact.corr, 
              size.tree           = size.tree,
              # num.splt            = num.splt,
              numb.noise          = numb.noise,
              pps                 = pps))
  
}



# NEED TO WORK ON CATEGORICAL FIRST SPLITS
eval.corr.frst.splts <- function(large.tree, corr.split) {

  cond <- (large.tree$frame$var[1] == corr.split[[1]][1])
  return(cond)
  
}
