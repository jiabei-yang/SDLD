# Treatment only randomized at time 0
MaintainA <- function(data, current.node, nodes) {
  Anodes <- nodes$A
  if (!(current.node %in% Anodes)) 
    return(NULL)
  if (!(any(Anodes < current.node))) 
    return(NULL)
  prev.a.node <- max(Anodes[Anodes < current.node])
  is.deterministic <- rep(T, nrow(data))
  prob1 <- ifelse(data[, prev.a.node] == 0, 0, 1)
  return(list(is.deterministic = is.deterministic, prob1 = prob1))
}