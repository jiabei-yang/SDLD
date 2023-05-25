create.sequence <- function(large.tree) {
  
  if (dim(large.tree$frame)[1] == 1) { # Deal with the root only tree
    
    tree.list = list(large.tree)
    lambda.list = list(Inf)
    
  } else {
    
    # Finding which variables are leaf nodes
    is.leaf <- (large.tree$frame$var == "<leaf>")
    
    # A function adapted from the partykit package that identifies the rows of the frame 
    # which correspond to child nodes of row i in frame matrix
    rpart.kids <- function(node, is.leaf) {
      if (is.leaf[node]) return(NULL)
      else return(c(node + 1L, 
                    which((cumsum(!is.leaf[-(1L:node)]) + 1L) == cumsum(is.leaf[-(1L:node)]))[1L] + 1L + node))
    }
    
    # Finding goodness of the split
    large.tree$frame$split.stat = 0
    large.tree$frame$split.stat[!is.leaf] = large.tree$splits[, 3]
    
    # Calculating the g(h) parameter for each non-terminal node
    g.h = rep(0, nrow(large.tree$frame))
    
    for (i in 1:nrow(large.tree$frame)) {
      
      if (is.leaf[i]) {g.h[i] = Inf} else{
        # Find all kids of node i
        kids.i = i
        stop.loop = FALSE
        while (stop.loop == FALSE) {
          
          kids.old = kids.i
          for(j in 1:length(kids.i)){
            kids.i = unique(c(kids.i, rpart.kids(kids.i[j], is.leaf)))
          }
          if (length(kids.i) == length(kids.old)) {stop.loop = TRUE}          
          # Calculating g.h for node i
          g.h[i] = sum(large.tree$frame$split.stat[kids.i])/sum(large.tree$frame$split.stat[kids.i] != 0)
        }
      }
    }
    
    # Adding g.h to frame
    large.tree$frame$g.h = g.h
    
    # Start pruning
    # First tree is the large tree
    tree.list = list(large.tree)
    lambda.list = list(0)
    stop.prune = FALSE
    k = 1
    
    while (stop.prune == FALSE) {
      
      # STOPPED HERE
      tree.used = tree.list[[k]]
      # Calculating the g(h) parameter for each non-terminal node
      tree.used$frame$g.h = rep(0, nrow(tree.used$frame))
      is.leaf.prune <- (tree.used$frame$var == "<leaf>")
      # Setting splitting statistics for new terminal nodes to 0
      tree.used$frame$split.stat[is.leaf.prune] = 0
      
      # Calculating the g(h) function for each non-terminal node
      for(i in 1:nrow(tree.used$frame)){
        if (is.leaf.prune[i]) {tree.used$frame$g.h[i] = Inf} else{
          # Find all kids of node i
          kids.i = i
          stop.loop = FALSE
          while (stop.loop == FALSE) {
            kids.old = kids.i
            for (j in 1:length(kids.i)) {
              kids.i = unique(c(kids.i, rpart.kids(kids.i[j], is.leaf.prune)))
            }
            if (length(kids.i) == length(kids.old)) {stop.loop = TRUE}          
            tree.used$frame$g.h[i] = sum(tree.used$frame$split.stat[kids.i])/sum(tree.used$frame$split.stat[kids.i] != 0)
          }
        }
      }
      
      # Finding the value which minimizes g(h) (among internal nodes)
      to.prune = which.min(tree.used$frame$g.h)
      # Finding the minimum g.h value
      g.h.min = min(tree.used$frame$g.h)
      
      # Find all kids of node to.prune
      kids.i = to.prune
      stop.loop = FALSE
      while (stop.loop == FALSE) {
        kids.old = kids.i
        for(j in 1:length(kids.i)){
          kids.i = unique(c(kids.i, rpart.kids(kids.i[j], is.leaf.prune)))
        }
        if (length(kids.i) == length(kids.old)) {stop.loop = TRUE}          
      }
      
      # Finding number of splits to prune
      split.to.prune = length(kids.i[which(!is.leaf.prune[kids.i])])
      
      # Creating the new splits and frames for new tree
      splits.new = tree.used$splits[-c(sum(!is.leaf.prune[1:to.prune]):(sum(!is.leaf.prune[1:to.prune]) + split.to.prune - 1)), ]
      frame.new = tree.used$frame[-setdiff(kids.i, to.prune), ]
      
      # Changing all nodes that were internal nodes and are now terminal node to terminal nodes
      frame.new$var[to.prune] =  "<leaf>"
      
      tree.new = tree.used
      tree.new$frame = frame.new
      if (class(splits.new)[1] == "matrix") {
        tree.new$splits = splits.new
      } else if (class(splits.new) == "numeric") {
        tree.new$splits = matrix(splits.new, nrow = 1)
        colnames(tree.new$splits) = colnames(tree.used$splits)
      }
      
      # Changing the terminal node for $where in rpart object
      tree.new$where = tree.used$where
      tree.new$where[tree.new$where %in% kids.i] = to.prune
      tree.new$where[tree.new$where > max(kids.i)] = tree.new$where[tree.new$where > max(kids.i)] - length(kids.i) + 1
      tree.new$where = as.integer(tree.new$where)
      
      k = k+1
      # Add tree and lambda to the list
      tree.list[[k]] <- tree.new
      lambda.list[[k]] <- g.h.min
      if (sum(tree.new$frame$var == "<leaf>") == 1) {stop.prune = TRUE}  
    }
    
  }
  
  return(list(tree.list = tree.list, lambda.list = lambda.list))
  
}
