load("../../Data/Simulation/FindLambdaTrtDepCsrDepYCont20220714_1_28.RData")
performance.trtDep.csrDep.yCont.1to28 <- performance.trtDep.csrDep.yCont
load("../../Data/Simulation/FindLambdaTrtDepCsrDepYCont20220714_29_400.RData")
performance.trtDep.csrDep.yCont.29to400 <- performance.trtDep.csrDep.yCont
load("../../Data/Simulation/FindLambdaTrtDepCsrDepYCont20220714_401_700.RData")
performance.trtDep.csrDep.yCont.401to700 <- performance.trtDep.csrDep.yCont
load("../../Data/Simulation/FindLambdaTrtDepCsrDepYCont20220714_701_1000.RData")
performance.trtDep.csrDep.yCont.701to1000 <- performance.trtDep.csrDep.yCont

performance.trtDep.csrDep.yCont <- c(performance.trtDep.csrDep.yCont.1to28,
                                     performance.trtDep.csrDep.yCont.29to400,
                                     performance.trtDep.csrDep.yCont.401to700,
                                     performance.trtDep.csrDep.yCont.701to1000)
 
length(performance.trtDep.csrDep.yCont[[112]])
dim(performance.trtDep.csrDep.yCont[[2]]$complex.test)

save(performance.trtDep.csrDep.yCont, 
     file = "../../Data/Simulation/PerformanceFindLambdaTrtDepCsrDepYCont20220714.RData")
load("../../Data/Simulation/PerformanceFindLambdaTrtDepCsrDepYCont20220714.RData")

complex.test <- list()
mse          <- NULL
exact.corr   <- NULL
size.tree    <- NULL
numb.noise   <- NULL
pps          <- NULL
corr.frst.splt <- NULL

length.lambda <- length(c(1:3, qchisq(0.95, 1), 4:40))

for (i in 1:1000) {
  
  tmp.mse          <- NULL
  tmp.exact.corr   <- NULL
  tmp.size.tree    <- NULL
  tmp.numb.noise   <- NULL
  tmp.pps          <- NULL
  
  for (j in 1:length.lambda) {
    tmp.mse <- c(tmp.mse,
                 performance.trtDep.csrDep.yCont[[i]][[j]]$mse)
    tmp.exact.corr <- c(tmp.exact.corr,
                        performance.trtDep.csrDep.yCont[[i]][[j]]$exact.corr)
    tmp.size.tree <- c(tmp.size.tree,
                       performance.trtDep.csrDep.yCont[[i]][[j]]$size.tree)
    tmp.numb.noise <- c(tmp.numb.noise,
                        performance.trtDep.csrDep.yCont[[i]][[j]]$numb.noise)
    tmp.pps <- c(tmp.pps,
                 performance.trtDep.csrDep.yCont[[i]][[j]]$pps)
  }
  
  mse        <- rbind(mse, tmp.mse)
  exact.corr <- rbind(exact.corr, tmp.exact.corr)
  size.tree  <- rbind(size.tree, tmp.size.tree)
  numb.noise <- rbind(numb.noise, tmp.numb.noise)
  pps        <- rbind(pps, tmp.pps)
  
  complex.test[[i]] <- performance.trtDep.csrDep.yCont[[i]]$complex.test
  corr.frst.splt    <- c(corr.frst.splt, performance.trtDep.csrDep.yCont[[i]]$corr.frst.splt)
  
}

lambda.list <- c(1:3, qchisq(0.95, 1), 4:40)

exact.corr.lambda <- apply(exact.corr, 2, mean)
plot(exact.corr.lambda)
lambda.list[which.max(exact.corr.lambda)]

mse.lambda <- apply(mse, 2, mean)
plot(mse.lambda)
lambda.list[which.min(mse.lambda)]

size.tree.lambda <- apply(size.tree, 2, mean)
plot(size.tree.lambda)

numb.noise.lambda <- apply(numb.noise, 2, mean)
plot(numb.noise.lambda)
lambda.list[which.min(numb.noise.lambda)]

pps.lambda <- apply(pps, 2, mean)
plot(pps.lambda)
lambda.list[which.max(pps.lambda)]

mean(corr.frst.splt)



# mse for tree of size 2
table(size.tree[, 40])
table(size.tree[, 41])
# tree from lambda 39 and 40 all are of size 2
identical(mse[, 40], mse[, 41])

hist(mse[, 41])
boxplot(mse[, 41])
summary(mse[, 41])

library(ggplot2)

mse <- data.frame(mse)

pdf("../../Report/Manuscript20230402/MseDepthTwo20230518.pdf", width = 5, height = 3)
ggplot(data = mse) +
  geom_boxplot(aes(X41)) +
  xlab("MSE") +
  ylab("Trees of depth 2") +
  ylim(-1, 1) +
  # coord_flip() +
  scale_x_continuous(breaks = seq(0, 0.25, 0.05)) +
  theme_bw() +
  theme(axis.ticks.y = element_blank(),
        axis.text.y  = element_blank())
dev.off()
