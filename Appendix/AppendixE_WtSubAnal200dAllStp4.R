#!/usr/bin/env Rscript
#####################
# read in functions #
#####################
# load functions
setwd("../")
folder <- paste(getwd(), "/Functions/", sep="")
functions <- list.files(folder)
functions.dir <- paste(folder, functions, sep = "")
for (functions.i in functions.dir){
  source(functions.i)
}

# sbatch -J SimDataRpart20210507 RunMa20201224.sh SimDataRpart20200422.R 1 100

setwd("DataAnalysis/")

registerDoParallel(cores=28)

# load("../../Data/Data20210512/Processed/Summ2AllWt20220329.RData")
load("../Simulation/seed1000.rda")
load("WtSubAnal200dAllStp3_20220512.RData")

n.iters.i <- ind.tree.size3.genderFemaleDob[33]
load(paste0("../../Data/Data20210512/Processed/TreeRes20220512/WtSubAnal200dAllStp1_seed", n.iters.i, "_windowlength4_maxdepth10_minsplit2000_minbucket1000_minobsytrain50_20220512.RData"))
load(paste0("../../Data/Data20210512/Processed/TreeRes20220512/WtSubAnal200dAllStp2_seed", n.iters.i, "_windowlength4_maxdepth10_minsplit2000_minbucket1000_minobsytrain50_minobsytest10_20220512.RData"))

final.tree.wt200d <- final.tree.list.wt200d[[1]][4][[1]]

# dimension
tree.data.wt200d <- finalData.noMiss.t0Jul2016.switchDTG.wt200d.scaled[tree.ind.wt200d, ]
dim(tree.data.wt200d)

mean(tree.data.wt200d$male)
1-mean(tree.data.wt200d$male)

mean((tree.data.wt200d$male == 0) & (tree.data.wt200d$time0_dob < final.tree.wt200d$splits[2, "index"]))
mean((tree.data.wt200d$male == 0) & (tree.data.wt200d$time0_dob >= final.tree.wt200d$splits[2, "index"]))
mean((tree.data.wt200d$male == 0) & (tree.data.wt200d$time0_dob < final.tree.wt200d$splits[2, "index"])) +
  mean((tree.data.wt200d$male == 0) & (tree.data.wt200d$time0_dob >= final.tree.wt200d$splits[2, "index"]))

mean(finalData.noMiss.t0Jul2016.switchDTG.wt200d.scaled$male)
1-mean(finalData.noMiss.t0Jul2016.switchDTG.wt200d.scaled$male)

mean((finalData.noMiss.t0Jul2016.switchDTG.wt200d.scaled$male == 0) & (finalData.noMiss.t0Jul2016.switchDTG.wt200d.scaled$time0_dob < final.tree.wt200d$splits[2, "index"]))
mean((finalData.noMiss.t0Jul2016.switchDTG.wt200d.scaled$male == 0) & (finalData.noMiss.t0Jul2016.switchDTG.wt200d.scaled$time0_dob >= final.tree.wt200d$splits[2, "index"]))


# CI & treatment effect estimation
ci.data.wt200d <- finalData.noMiss.t0Jul2016.switchDTG.wt200d.scaled[ci.ind.wt200d, ]
dim(ci.data.wt200d)

ci.data.wt200d.2 <- ci.data.wt200d %>% 
  filter(male >= final.tree.wt200d$splits[1, "index"])
ci.data.wt200d.3 <- ci.data.wt200d %>% 
  filter(male < final.tree.wt200d$splits[1, "index"])
# sum(!is.na(as.matrix(ci.data.wt200d.3 %>%
#                                       filter((DTGOn_0 == 1) & (DTGOn_1 == 1) & (DTGOn_2 == 1) & (DTGOn_3 == 1)) %>%
#                                       select(Weight_4))), na.rm = T)

ci.data.wt200d.6 <- ci.data.wt200d.3 %>%
  filter(time0_dob < final.tree.wt200d$splits[2, "index"])
ci.data.wt200d.7 <- ci.data.wt200d.3 %>%
  filter(time0_dob >= final.tree.wt200d$splits[2, "index"])

vary.cov.int.used <- c("viralloadImptCtgr", "SystolicBP", "DiastolicBP", "Height",
                       "tbtx", "activeTB", "Married", "nhif")

# Effect estimate
res.wt200d.node1 <- ltmle(ci.data.wt200d, 
                          Anodes = paste0("DTGOn_", 0:3), 
                          Cnodes = paste0("censor_", 0:3),
                          Lnodes = c(paste0(vary.cov.int.used, "_1"), paste0(vary.cov.int.used, "_2"), paste0(vary.cov.int.used, "_3")),
                          Ynodes = paste0("Weight_", 1:4), 
                          abar            = list(treatment = rep(1, 4), control = rep(0, 4)), 
                          gbounds         = c(0.025, 0.975),
                          estimate.time   = F)
summ.wt200d.node1 <- summary(res.wt200d.node1)
eff.wt200d.node1  <- summ.wt200d.node1$effect.measures$ATE$estimate

res.wt200d.node2 <- ltmle(ci.data.wt200d.2, 
                          Anodes = paste0("DTGOn_", 0:3), 
                          Cnodes = paste0("censor_", 0:3),
                          Lnodes = c(paste0(vary.cov.int.used, "_1"), paste0(vary.cov.int.used, "_2"), paste0(vary.cov.int.used, "_3")),
                          Ynodes = paste0("Weight_", 1:4), 
                          abar            = list(treatment = rep(1, 4), control = rep(0, 4)), 
                          gbounds         = c(0.025, 0.975),
                          estimate.time   = F)
summ.wt200d.node2 <- summary(res.wt200d.node2)
eff.wt200d.node2  <- summ.wt200d.node2$effect.measures$ATE$estimate

res.wt200d.node3 <- ltmle(ci.data.wt200d.3, 
                          Anodes = paste0("DTGOn_", 0:3), 
                          Cnodes = paste0("censor_", 0:3),
                          Lnodes = c(paste0(vary.cov.int.used, "_1"), paste0(vary.cov.int.used, "_2"), paste0(vary.cov.int.used, "_3")),
                          Ynodes = paste0("Weight_", 1:4), 
                          abar            = list(treatment = rep(1, 4), control = rep(0, 4)), 
                          gbounds         = c(0.025, 0.975),
                          estimate.time   = F)
summ.wt200d.node3 <- summary(res.wt200d.node3)
eff.wt200d.node3  <- summ.wt200d.node3$effect.measures$ATE$estimate

res.wt200d.node6 <- ltmle(ci.data.wt200d.6, 
                          Anodes = paste0("DTGOn_", 0:3), 
                          Cnodes = paste0("censor_", 0:3),
                          Lnodes = c(paste0(vary.cov.int.used, "_1"), paste0(vary.cov.int.used, "_2"), paste0(vary.cov.int.used, "_3")),
                          Ynodes = paste0("Weight_", 1:4), 
                          abar            = list(treatment = rep(1, 4), control = rep(0, 4)), 
                          gbounds         = c(0.025, 0.975),
                          estimate.time   = F)
summ.wt200d.node6 <- summary(res.wt200d.node6)
eff.wt200d.node6  <- summ.wt200d.node6$effect.measures$ATE$estimate

res.wt200d.node7 <- ltmle(ci.data.wt200d.7, 
                          Anodes = paste0("DTGOn_", 0:3), 
                          Cnodes = paste0("censor_", 0:3),
                          Lnodes = c(paste0(vary.cov.int.used, "_1"), paste0(vary.cov.int.used, "_2"), paste0(vary.cov.int.used, "_3")),
                          Ynodes = paste0("Weight_", 1:4), 
                          abar            = list(treatment = rep(1, 4), control = rep(0, 4)), 
                          gbounds         = c(0.025, 0.975),
                          estimate.time   = F)
summ.wt200d.node7 <- summary(res.wt200d.node7)
eff.wt200d.node7  <- summ.wt200d.node7$effect.measures$ATE$estimate

# Bootstrap
iters.boot <- 10^3
nrow.ci.data.wt200d <- nrow(ci.data.wt200d)

set.seed(a[333])
df.eff.wt200d.nodes <- 
  foreach(iters.boot.i = 1:iters.boot,
          .combine   = rbind) %dopar% {
            
            tmp.res <- NULL
            tmp.ci.data.wt200d <- ci.data.wt200d[sample(1:nrow.ci.data.wt200d, nrow.ci.data.wt200d, replace = TRUE), ]
            
            tmp.ci.data.wt200d.2 <- tmp.ci.data.wt200d %>% 
              filter(male >= final.tree.wt200d$splits[1, "index"])
            tmp.ci.data.wt200d.3 <- tmp.ci.data.wt200d %>% 
              filter(male < final.tree.wt200d$splits[1, "index"])
            
            tmp.ci.data.wt200d.6 <- tmp.ci.data.wt200d.3 %>%
              filter(time0_dob < final.tree.wt200d$splits[2, "index"])
            tmp.ci.data.wt200d.7 <- tmp.ci.data.wt200d.3 %>%
              filter(time0_dob >= final.tree.wt200d$splits[2, "index"])
            
            tmp.res.wt200d.node1 <- ltmle(tmp.ci.data.wt200d, 
                                          Anodes = paste0("DTGOn_", 0:3), 
                                          Cnodes = paste0("censor_", 0:3),
                                          Lnodes = c(paste0(vary.cov.int.used, "_1"), paste0(vary.cov.int.used, "_2"), paste0(vary.cov.int.used, "_3")),
                                          Ynodes = paste0("Weight_", 1:4), 
                                          abar            = list(treatment = rep(1, 4), control = rep(0, 4)), 
                                          gbounds         = c(0.025, 0.975),
                                          estimate.time   = F)
            tmp.summ.wt200d.node1 <- summary(tmp.res.wt200d.node1)
            tmp.res <- tmp.summ.wt200d.node1$effect.measures$ATE$estimate
            
            tmp.res.wt200d.node2 <- ltmle(tmp.ci.data.wt200d.2, 
                                          Anodes = paste0("DTGOn_", 0:3), 
                                          Cnodes = paste0("censor_", 0:3),
                                          Lnodes = c(paste0(vary.cov.int.used, "_1"), paste0(vary.cov.int.used, "_2"), paste0(vary.cov.int.used, "_3")),
                                          Ynodes = paste0("Weight_", 1:4), 
                                          abar            = list(treatment = rep(1, 4), control = rep(0, 4)), 
                                          gbounds         = c(0.025, 0.975),
                                          estimate.time   = F)
            tmp.summ.wt200d.node2 <- summary(tmp.res.wt200d.node2)
            tmp.res <- c(tmp.res, tmp.summ.wt200d.node2$effect.measures$ATE$estimate)
            
            tmp.res.wt200d.node3 <- ltmle(tmp.ci.data.wt200d.3, 
                                          Anodes = paste0("DTGOn_", 0:3), 
                                          Cnodes = paste0("censor_", 0:3),
                                          Lnodes = c(paste0(vary.cov.int.used, "_1"), paste0(vary.cov.int.used, "_2"), paste0(vary.cov.int.used, "_3")),
                                          Ynodes = paste0("Weight_", 1:4), 
                                          abar            = list(treatment = rep(1, 4), control = rep(0, 4)), 
                                          gbounds         = c(0.025, 0.975),
                                          estimate.time   = F)
            tmp.summ.wt200d.node3 <- summary(tmp.res.wt200d.node3)
            tmp.res <- c(tmp.res, tmp.summ.wt200d.node3$effect.measures$ATE$estimate)
            
            tmp.res.wt200d.node6 <- ltmle(tmp.ci.data.wt200d.6, 
                                          Anodes = paste0("DTGOn_", 0:3), 
                                          Cnodes = paste0("censor_", 0:3),
                                          Lnodes = c(paste0(vary.cov.int.used, "_1"), paste0(vary.cov.int.used, "_2"), paste0(vary.cov.int.used, "_3")),
                                          Ynodes = paste0("Weight_", 1:4), 
                                          abar            = list(treatment = rep(1, 4), control = rep(0, 4)), 
                                          gbounds         = c(0.025, 0.975),
                                          estimate.time   = F)
            tmp.summ.wt200d.node6 <- summary(tmp.res.wt200d.node6)
            tmp.res <- c(tmp.res, tmp.summ.wt200d.node6$effect.measures$ATE$estimate)
            
            tmp.res.wt200d.node7 <- ltmle(tmp.ci.data.wt200d.7, 
                                          Anodes = paste0("DTGOn_", 0:3), 
                                          Cnodes = paste0("censor_", 0:3),
                                          Lnodes = c(paste0(vary.cov.int.used, "_1"), paste0(vary.cov.int.used, "_2"), paste0(vary.cov.int.used, "_3")),
                                          Ynodes = paste0("Weight_", 1:4), 
                                          abar            = list(treatment = rep(1, 4), control = rep(0, 4)), 
                                          gbounds         = c(0.025, 0.975),
                                          estimate.time   = F)
            tmp.summ.wt200d.node7 <- summary(tmp.res.wt200d.node7)
            tmp.res <- c(tmp.res, tmp.summ.wt200d.node7$effect.measures$ATE$estimate)
            
            tmp.res
          }

# save.image("IntermediateData.RData")
colnames(df.eff.wt200d.nodes) <- c("all", "node2", "node3", "node6", "node7")
dim(df.eff.wt200d.nodes)

summ.eff.wt200d.nodes <- t(apply(df.eff.wt200d.nodes, 2, quantile, probs = c(0.025, 0.5, 0.975)))
summ.eff.wt200d.nodes <- data.frame(node = rownames(summ.eff.wt200d.nodes),
  summ.eff.wt200d.nodes)
colnames(summ.eff.wt200d.nodes) <- c("node", "q0025", "q050", "q0975")

summ.eff.wt200d.nodes <- summ.eff.wt200d.nodes %>%
  mutate(eff = c(eff.wt200d.node1, eff.wt200d.node2, eff.wt200d.node3,
                 eff.wt200d.node6, eff.wt200d.node7)) %>%
  select(node, q0025, eff, q050, q0975)

save(summ.wt200d.node1, summ.wt200d.node2, summ.wt200d.node3, 
     summ.wt200d.node6, summ.wt200d.node7,
     df.eff.wt200d.nodes, summ.eff.wt200d.nodes, 
     file = "../../Data/Data20210512/Processed/WtSubAnal200dAllStp4_20220518.RData")
# load("WtSubAnal200dAllStp4_20220518.RData")
mean(df.eff.wt200d.nodes[, "node2"] < df.eff.wt200d.nodes[, "node3"])
mean(df.eff.wt200d.nodes[, "node6"] < df.eff.wt200d.nodes[, "node7"])



load("../../Data/Data20210512/Processed/Summ2AllWt20220329.RData")

mean.time0_dob <- mean(finalData.noMiss.t0Jul2016.switchDTG.wt200d$time0_dob)
sd.time0_dob   <- sd(finalData.noMiss.t0Jul2016.switchDTG.wt200d$time0_dob)

(final.tree.wt200d$splits[2, "index"] * sd.time0_dob + mean.time0_dob) / 365.25








