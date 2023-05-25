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
load("../../Data/Data20210512/Processed/WtSubAnal200dAllStp3_20220512.RData")

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
# Node 1
res.wt200d.node1.t1 <- ltmle(ci.data.wt200d %>%
                               select(male:Weight_1), 
                             Anodes = paste0("DTGOn_", 0), 
                             Cnodes = paste0("censor_", 0),
                             Lnodes = NULL,
                             Ynodes = paste0("Weight_", 1), 
                             abar            = list(treatment = 1, control = 0), 
                             gbounds         = c(0.025, 0.975),
                             estimate.time   = F)
summ.wt200d.node1.t1 <- summary(res.wt200d.node1.t1)
eff.wt200d.node1.t1  <- summ.wt200d.node1.t1$effect.measures$ATE$estimate

res.wt200d.node1.t2 <- ltmle(ci.data.wt200d %>%
                               select(male:Weight_2), 
                             Anodes = paste0("DTGOn_", 0:1), 
                             Cnodes = paste0("censor_", 0:1),
                             Lnodes = c(paste0(vary.cov.int.used, "_1")),
                             Ynodes = paste0("Weight_", 1:2), 
                             abar            = list(treatment = rep(1, 2), control = rep(0, 2)), 
                             gbounds         = c(0.025, 0.975),
                             estimate.time   = F)
summ.wt200d.node1.t2 <- summary(res.wt200d.node1.t2)
eff.wt200d.node1.t2  <- summ.wt200d.node1.t2$effect.measures$ATE$estimate

res.wt200d.node1.t3 <- ltmle(ci.data.wt200d %>%
                               select(male:Weight_3), 
                             Anodes = paste0("DTGOn_", 0:2), 
                             Cnodes = paste0("censor_", 0:2),
                             Lnodes = c(paste0(vary.cov.int.used, "_1"), paste0(vary.cov.int.used, "_2")),
                             Ynodes = paste0("Weight_", 1:3), 
                             abar            = list(treatment = rep(1, 3), control = rep(0, 3)), 
                             gbounds         = c(0.025, 0.975),
                             estimate.time   = F)
summ.wt200d.node1.t3 <- summary(res.wt200d.node1.t3)
eff.wt200d.node1.t3  <- summ.wt200d.node1.t3$effect.measures$ATE$estimate

res.wt200d.node1.t4 <- ltmle(ci.data.wt200d, 
                             Anodes = paste0("DTGOn_", 0:3), 
                             Cnodes = paste0("censor_", 0:3),
                             Lnodes = c(paste0(vary.cov.int.used, "_1"), paste0(vary.cov.int.used, "_2"), paste0(vary.cov.int.used, "_3")),
                             Ynodes = paste0("Weight_", 1:4), 
                             abar            = list(treatment = rep(1, 4), control = rep(0, 4)), 
                             gbounds         = c(0.025, 0.975),
                             estimate.time   = F)
summ.wt200d.node1.t4 <- summary(res.wt200d.node1.t4)
eff.wt200d.node1.t4  <- summ.wt200d.node1.t4$effect.measures$ATE$estimate

# Node 2
res.wt200d.node2.t1 <- ltmle(ci.data.wt200d.2 %>%
                               select(male:Weight_1), 
                             Anodes = paste0("DTGOn_", 0), 
                             Cnodes = paste0("censor_", 0),
                             Lnodes = NULL,
                             Ynodes = paste0("Weight_", 1), 
                             abar            = list(treatment = 1, control = 0), 
                             gbounds         = c(0.025, 0.975),
                             estimate.time   = F)
summ.wt200d.node2.t1 <- summary(res.wt200d.node2.t1)
eff.wt200d.node2.t1  <- summ.wt200d.node2.t1$effect.measures$ATE$estimate

res.wt200d.node2.t2 <- ltmle(ci.data.wt200d.2 %>%
                               select(male:Weight_2), 
                             Anodes = paste0("DTGOn_", 0:1), 
                             Cnodes = paste0("censor_", 0:1),
                             Lnodes = c(paste0(vary.cov.int.used, "_1")),
                             Ynodes = paste0("Weight_", 1:2), 
                             abar            = list(treatment = rep(1, 2), control = rep(0, 2)), 
                             gbounds         = c(0.025, 0.975),
                             estimate.time   = F)
summ.wt200d.node2.t2 <- summary(res.wt200d.node2.t2)
eff.wt200d.node2.t2  <- summ.wt200d.node2.t2$effect.measures$ATE$estimate

res.wt200d.node2.t3 <- ltmle(ci.data.wt200d.2 %>%
                               select(male:Weight_3), 
                             Anodes = paste0("DTGOn_", 0:2), 
                             Cnodes = paste0("censor_", 0:2),
                             Lnodes = c(paste0(vary.cov.int.used, "_1"), paste0(vary.cov.int.used, "_2")),
                             Ynodes = paste0("Weight_", 1:3), 
                             abar            = list(treatment = rep(1, 3), control = rep(0, 3)), 
                             gbounds         = c(0.025, 0.975),
                             estimate.time   = F)
summ.wt200d.node2.t3 <- summary(res.wt200d.node2.t3)
eff.wt200d.node2.t3  <- summ.wt200d.node2.t3$effect.measures$ATE$estimate

res.wt200d.node2.t4 <- ltmle(ci.data.wt200d.2, 
                             Anodes = paste0("DTGOn_", 0:3), 
                             Cnodes = paste0("censor_", 0:3),
                             Lnodes = c(paste0(vary.cov.int.used, "_1"), paste0(vary.cov.int.used, "_2"), paste0(vary.cov.int.used, "_3")),
                             Ynodes = paste0("Weight_", 1:4), 
                             abar            = list(treatment = rep(1, 4), control = rep(0, 4)), 
                             gbounds         = c(0.025, 0.975),
                             estimate.time   = F)
summ.wt200d.node2.t4 <- summary(res.wt200d.node2.t4)
eff.wt200d.node2.t4  <- summ.wt200d.node2.t4$effect.measures$ATE$estimate

# Node 3
res.wt200d.node3.t1 <- ltmle(ci.data.wt200d.3 %>%
                               select(male:Weight_1), 
                             Anodes = paste0("DTGOn_", 0), 
                             Cnodes = paste0("censor_", 0),
                             Lnodes = NULL,
                             Ynodes = paste0("Weight_", 1), 
                             abar            = list(treatment = 1, control = 0), 
                             gbounds         = c(0.025, 0.975),
                             estimate.time   = F)
summ.wt200d.node3.t1 <- summary(res.wt200d.node3.t1)
eff.wt200d.node3.t1  <- summ.wt200d.node3.t1$effect.measures$ATE$estimate

res.wt200d.node3.t2 <- ltmle(ci.data.wt200d.3 %>%
                               select(male:Weight_2), 
                             Anodes = paste0("DTGOn_", 0:1), 
                             Cnodes = paste0("censor_", 0:1),
                             Lnodes = c(paste0(vary.cov.int.used, "_1")),
                             Ynodes = paste0("Weight_", 1:2), 
                             abar            = list(treatment = rep(1, 2), control = rep(0, 2)), 
                             gbounds         = c(0.025, 0.975),
                             estimate.time   = F)
summ.wt200d.node3.t2 <- summary(res.wt200d.node3.t2)
eff.wt200d.node3.t2  <- summ.wt200d.node3.t2$effect.measures$ATE$estimate

res.wt200d.node3.t3 <- ltmle(ci.data.wt200d.3 %>%
                               select(male:Weight_3), 
                             Anodes = paste0("DTGOn_", 0:2), 
                             Cnodes = paste0("censor_", 0:2),
                             Lnodes = c(paste0(vary.cov.int.used, "_1"), paste0(vary.cov.int.used, "_2")),
                             Ynodes = paste0("Weight_", 1:3), 
                             abar            = list(treatment = rep(1, 3), control = rep(0, 3)), 
                             gbounds         = c(0.025, 0.975),
                             estimate.time   = F)
summ.wt200d.node3.t3 <- summary(res.wt200d.node3.t3)
eff.wt200d.node3.t3  <- summ.wt200d.node3.t3$effect.measures$ATE$estimate

res.wt200d.node3.t4 <- ltmle(ci.data.wt200d.3, 
                             Anodes = paste0("DTGOn_", 0:3), 
                             Cnodes = paste0("censor_", 0:3),
                             Lnodes = c(paste0(vary.cov.int.used, "_1"), paste0(vary.cov.int.used, "_2"), paste0(vary.cov.int.used, "_3")),
                             Ynodes = paste0("Weight_", 1:4), 
                             abar            = list(treatment = rep(1, 4), control = rep(0, 4)), 
                             gbounds         = c(0.025, 0.975),
                             estimate.time   = F)
summ.wt200d.node3.t4 <- summary(res.wt200d.node3.t4)
eff.wt200d.node3.t4  <- summ.wt200d.node3.t4$effect.measures$ATE$estimate

# Node 6
res.wt200d.node6.t1 <- ltmle(ci.data.wt200d.6 %>%
                               select(male:Weight_1), 
                             Anodes = paste0("DTGOn_", 0), 
                             Cnodes = paste0("censor_", 0),
                             Lnodes = NULL,
                             Ynodes = paste0("Weight_", 1), 
                             abar            = list(treatment = 1, control = 0), 
                             gbounds         = c(0.025, 0.975),
                             estimate.time   = F)
summ.wt200d.node6.t1 <- summary(res.wt200d.node6.t1)
eff.wt200d.node6.t1  <- summ.wt200d.node6.t1$effect.measures$ATE$estimate

res.wt200d.node6.t2 <- ltmle(ci.data.wt200d.6 %>%
                               select(male:Weight_2), 
                             Anodes = paste0("DTGOn_", 0:1), 
                             Cnodes = paste0("censor_", 0:1),
                             Lnodes = c(paste0(vary.cov.int.used, "_1")),
                             Ynodes = paste0("Weight_", 1:2), 
                             abar            = list(treatment = rep(1, 2), control = rep(0, 2)), 
                             gbounds         = c(0.025, 0.975),
                             estimate.time   = F)
summ.wt200d.node6.t2 <- summary(res.wt200d.node6.t2)
eff.wt200d.node6.t2  <- summ.wt200d.node6.t2$effect.measures$ATE$estimate

res.wt200d.node6.t3 <- ltmle(ci.data.wt200d.6 %>%
                               select(male:Weight_3), 
                             Anodes = paste0("DTGOn_", 0:2), 
                             Cnodes = paste0("censor_", 0:2),
                             Lnodes = c(paste0(vary.cov.int.used, "_1"), paste0(vary.cov.int.used, "_2")),
                             Ynodes = paste0("Weight_", 1:3), 
                             abar            = list(treatment = rep(1, 3), control = rep(0, 3)), 
                             gbounds         = c(0.025, 0.975),
                             estimate.time   = F)
summ.wt200d.node6.t3 <- summary(res.wt200d.node6.t3)
eff.wt200d.node6.t3  <- summ.wt200d.node6.t3$effect.measures$ATE$estimate

res.wt200d.node6.t4 <- ltmle(ci.data.wt200d.6, 
                             Anodes = paste0("DTGOn_", 0:3), 
                             Cnodes = paste0("censor_", 0:3),
                             Lnodes = c(paste0(vary.cov.int.used, "_1"), paste0(vary.cov.int.used, "_2"), paste0(vary.cov.int.used, "_3")),
                             Ynodes = paste0("Weight_", 1:4), 
                             abar            = list(treatment = rep(1, 4), control = rep(0, 4)), 
                             gbounds         = c(0.025, 0.975),
                             estimate.time   = F)
summ.wt200d.node6.t4 <- summary(res.wt200d.node6.t4)
eff.wt200d.node6.t4  <- summ.wt200d.node6.t4$effect.measures$ATE$estimate

# Node 7
res.wt200d.node7.t1 <- ltmle(ci.data.wt200d.7 %>%
                               select(male:Weight_1), 
                             Anodes = paste0("DTGOn_", 0), 
                             Cnodes = paste0("censor_", 0),
                             Lnodes = NULL,
                             Ynodes = paste0("Weight_", 1), 
                             abar            = list(treatment = 1, control = 0), 
                             gbounds         = c(0.025, 0.975),
                             estimate.time   = F)
summ.wt200d.node7.t1 <- summary(res.wt200d.node7.t1)
eff.wt200d.node7.t1  <- summ.wt200d.node7.t1$effect.measures$ATE$estimate

res.wt200d.node7.t2 <- ltmle(ci.data.wt200d.7 %>%
                               select(male:Weight_2), 
                             Anodes = paste0("DTGOn_", 0:1), 
                             Cnodes = paste0("censor_", 0:1),
                             Lnodes = c(paste0(vary.cov.int.used, "_1")),
                             Ynodes = paste0("Weight_", 1:2), 
                             abar            = list(treatment = rep(1, 2), control = rep(0, 2)), 
                             gbounds         = c(0.025, 0.975),
                             estimate.time   = F)
summ.wt200d.node7.t2 <- summary(res.wt200d.node7.t2)
eff.wt200d.node7.t2  <- summ.wt200d.node7.t2$effect.measures$ATE$estimate

res.wt200d.node7.t3 <- ltmle(ci.data.wt200d.7 %>%
                               select(male:Weight_3), 
                             Anodes = paste0("DTGOn_", 0:2), 
                             Cnodes = paste0("censor_", 0:2),
                             Lnodes = c(paste0(vary.cov.int.used, "_1"), paste0(vary.cov.int.used, "_2")),
                             Ynodes = paste0("Weight_", 1:3), 
                             abar            = list(treatment = rep(1, 3), control = rep(0, 3)), 
                             gbounds         = c(0.025, 0.975),
                             estimate.time   = F)
summ.wt200d.node7.t3 <- summary(res.wt200d.node7.t3)
eff.wt200d.node7.t3  <- summ.wt200d.node7.t3$effect.measures$ATE$estimate

res.wt200d.node7.t4 <- ltmle(ci.data.wt200d.7, 
                             Anodes = paste0("DTGOn_", 0:3), 
                             Cnodes = paste0("censor_", 0:3),
                             Lnodes = c(paste0(vary.cov.int.used, "_1"), paste0(vary.cov.int.used, "_2"), paste0(vary.cov.int.used, "_3")),
                             Ynodes = paste0("Weight_", 1:4), 
                             abar            = list(treatment = rep(1, 4), control = rep(0, 4)), 
                             gbounds         = c(0.025, 0.975),
                             estimate.time   = F)
summ.wt200d.node7.t4 <- summary(res.wt200d.node7.t4)
eff.wt200d.node7.t4  <- summ.wt200d.node7.t4$effect.measures$ATE$estimate

# Bootstrap
iters.boot <- 10^3
nrow.ci.data.wt200d <- nrow(ci.data.wt200d)

# set.seed(a[333])
df.eff.wt200d.nodes <- 
  foreach(iters.boot.i = 1:iters.boot,
          .combine   = rbind) %dopar% {
            
            set.seed(a[iters.boot.i])
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
            
            # Node 1
            tmp.res.wt200d.node1.t1 <- ltmle(tmp.ci.data.wt200d %>%
                                               select(male:Weight_1), 
                                             Anodes = paste0("DTGOn_", 0), 
                                             Cnodes = paste0("censor_", 0),
                                             Lnodes = NULL,
                                             Ynodes = paste0("Weight_", 1), 
                                             abar            = list(treatment = 1, control = 0), 
                                             gbounds         = c(0.025, 0.975),
                                             estimate.time   = F)
            tmp.summ.wt200d.node1.t1 <- summary(tmp.res.wt200d.node1.t1)
            tmp.res  <- c(tmp.res, tmp.summ.wt200d.node1.t1$effect.measures$ATE$estimate)
            
            tmp.res.wt200d.node1.t2 <- ltmle(tmp.ci.data.wt200d %>%
                                               select(male:Weight_2), 
                                             Anodes = paste0("DTGOn_", 0:1), 
                                             Cnodes = paste0("censor_", 0:1),
                                             Lnodes = c(paste0(vary.cov.int.used, "_1")),
                                             Ynodes = paste0("Weight_", 1:2), 
                                             abar            = list(treatment = rep(1, 2), control = rep(0, 2)), 
                                             gbounds         = c(0.025, 0.975),
                                             estimate.time   = F)
            tmp.summ.wt200d.node1.t2 <- summary(tmp.res.wt200d.node1.t2)
            tmp.res  <- c(tmp.res, tmp.summ.wt200d.node1.t2$effect.measures$ATE$estimate)
            
            tmp.res.wt200d.node1.t3 <- ltmle(tmp.ci.data.wt200d %>%
                                               select(male:Weight_3), 
                                             Anodes = paste0("DTGOn_", 0:2), 
                                             Cnodes = paste0("censor_", 0:2),
                                             Lnodes = c(paste0(vary.cov.int.used, "_1"), paste0(vary.cov.int.used, "_2")),
                                             Ynodes = paste0("Weight_", 1:3), 
                                             abar            = list(treatment = rep(1, 3), control = rep(0, 3)), 
                                             gbounds         = c(0.025, 0.975),
                                             estimate.time   = F)
            tmp.summ.wt200d.node1.t3 <- summary(tmp.res.wt200d.node1.t3)
            tmp.res  <- c(tmp.res, tmp.summ.wt200d.node1.t3$effect.measures$ATE$estimate)
            
            tmp.res.wt200d.node1.t4 <- ltmle(tmp.ci.data.wt200d, 
                                             Anodes = paste0("DTGOn_", 0:3), 
                                             Cnodes = paste0("censor_", 0:3),
                                             Lnodes = c(paste0(vary.cov.int.used, "_1"), paste0(vary.cov.int.used, "_2"), paste0(vary.cov.int.used, "_3")),
                                             Ynodes = paste0("Weight_", 1:4), 
                                             abar            = list(treatment = rep(1, 4), control = rep(0, 4)), 
                                             gbounds         = c(0.025, 0.975),
                                             estimate.time   = F)
            tmp.summ.wt200d.node1.t4 <- summary(tmp.res.wt200d.node1.t4)
            tmp.res <- c(tmp.res, tmp.summ.wt200d.node1.t4$effect.measures$ATE$estimate)
            
            # Node 2
            tmp.res.wt200d.node2.t1 <- ltmle(tmp.ci.data.wt200d.2 %>%
                                               select(male:Weight_1), 
                                             Anodes = paste0("DTGOn_", 0), 
                                             Cnodes = paste0("censor_", 0),
                                             Lnodes = NULL,
                                             Ynodes = paste0("Weight_", 1), 
                                             abar            = list(treatment = 1, control = 0), 
                                             gbounds         = c(0.025, 0.975),
                                             estimate.time   = F)
            tmp.summ.wt200d.node2.t1 <- summary(tmp.res.wt200d.node2.t1)
            tmp.res  <- c(tmp.res, tmp.summ.wt200d.node2.t1$effect.measures$ATE$estimate)
            
            tmp.res.wt200d.node2.t2 <- ltmle(tmp.ci.data.wt200d.2 %>%
                                               select(male:Weight_2), 
                                             Anodes = paste0("DTGOn_", 0:1), 
                                             Cnodes = paste0("censor_", 0:1),
                                             Lnodes = c(paste0(vary.cov.int.used, "_1")),
                                             Ynodes = paste0("Weight_", 1:2), 
                                             abar            = list(treatment = rep(1, 2), control = rep(0, 2)), 
                                             gbounds         = c(0.025, 0.975),
                                             estimate.time   = F)
            tmp.summ.wt200d.node2.t2 <- summary(tmp.res.wt200d.node2.t2)
            tmp.res  <- c(tmp.res, tmp.summ.wt200d.node2.t2$effect.measures$ATE$estimate)
            
            tmp.res.wt200d.node2.t3 <- ltmle(tmp.ci.data.wt200d.2 %>%
                                               select(male:Weight_3), 
                                             Anodes = paste0("DTGOn_", 0:2), 
                                             Cnodes = paste0("censor_", 0:2),
                                             Lnodes = c(paste0(vary.cov.int.used, "_1"), paste0(vary.cov.int.used, "_2")),
                                             Ynodes = paste0("Weight_", 1:3), 
                                             abar            = list(treatment = rep(1, 3), control = rep(0, 3)), 
                                             gbounds         = c(0.025, 0.975),
                                             estimate.time   = F)
            tmp.summ.wt200d.node2.t3 <- summary(tmp.res.wt200d.node2.t3)
            tmp.res  <- c(tmp.res, tmp.summ.wt200d.node2.t3$effect.measures$ATE$estimate)
            
            tmp.res.wt200d.node2.t4 <- ltmle(tmp.ci.data.wt200d.2, 
                                             Anodes = paste0("DTGOn_", 0:3), 
                                             Cnodes = paste0("censor_", 0:3),
                                             Lnodes = c(paste0(vary.cov.int.used, "_1"), paste0(vary.cov.int.used, "_2"), paste0(vary.cov.int.used, "_3")),
                                             Ynodes = paste0("Weight_", 1:4), 
                                             abar            = list(treatment = rep(1, 4), control = rep(0, 4)), 
                                             gbounds         = c(0.025, 0.975),
                                             estimate.time   = F)
            tmp.summ.wt200d.node2.t4 <- summary(tmp.res.wt200d.node2.t4)
            tmp.res <- c(tmp.res, tmp.summ.wt200d.node2.t4$effect.measures$ATE$estimate)
            
            # Node 3
            tmp.res.wt200d.node3.t1 <- ltmle(tmp.ci.data.wt200d.3 %>%
                                               select(male:Weight_1), 
                                             Anodes = paste0("DTGOn_", 0), 
                                             Cnodes = paste0("censor_", 0),
                                             Lnodes = NULL,
                                             Ynodes = paste0("Weight_", 1), 
                                             abar            = list(treatment = 1, control = 0), 
                                             gbounds         = c(0.025, 0.975),
                                             estimate.time   = F)
            tmp.summ.wt200d.node3.t1 <- summary(tmp.res.wt200d.node3.t1)
            tmp.res  <- c(tmp.res, tmp.summ.wt200d.node3.t1$effect.measures$ATE$estimate)
            
            tmp.res.wt200d.node3.t2 <- ltmle(tmp.ci.data.wt200d.3 %>%
                                               select(male:Weight_2), 
                                             Anodes = paste0("DTGOn_", 0:1), 
                                             Cnodes = paste0("censor_", 0:1),
                                             Lnodes = c(paste0(vary.cov.int.used, "_1")),
                                             Ynodes = paste0("Weight_", 1:2), 
                                             abar            = list(treatment = rep(1, 2), control = rep(0, 2)), 
                                             gbounds         = c(0.025, 0.975),
                                             estimate.time   = F)
            tmp.summ.wt200d.node3.t2 <- summary(tmp.res.wt200d.node3.t2)
            tmp.res  <- c(tmp.res, tmp.summ.wt200d.node3.t2$effect.measures$ATE$estimate)
            
            tmp.res.wt200d.node3.t3 <- ltmle(tmp.ci.data.wt200d.3 %>%
                                               select(male:Weight_3), 
                                             Anodes = paste0("DTGOn_", 0:2), 
                                             Cnodes = paste0("censor_", 0:2),
                                             Lnodes = c(paste0(vary.cov.int.used, "_1"), paste0(vary.cov.int.used, "_2")),
                                             Ynodes = paste0("Weight_", 1:3), 
                                             abar            = list(treatment = rep(1, 3), control = rep(0, 3)), 
                                             gbounds         = c(0.025, 0.975),
                                             estimate.time   = F)
            tmp.summ.wt200d.node3.t3 <- summary(tmp.res.wt200d.node3.t3)
            tmp.res  <- c(tmp.res, tmp.summ.wt200d.node3.t3$effect.measures$ATE$estimate)
            
            tmp.res.wt200d.node3.t4 <- ltmle(tmp.ci.data.wt200d.3, 
                                             Anodes = paste0("DTGOn_", 0:3), 
                                             Cnodes = paste0("censor_", 0:3),
                                             Lnodes = c(paste0(vary.cov.int.used, "_1"), paste0(vary.cov.int.used, "_2"), paste0(vary.cov.int.used, "_3")),
                                             Ynodes = paste0("Weight_", 1:4), 
                                             abar            = list(treatment = rep(1, 4), control = rep(0, 4)), 
                                             gbounds         = c(0.025, 0.975),
                                             estimate.time   = F)
            tmp.summ.wt200d.node3.t4 <- summary(tmp.res.wt200d.node3.t4)
            tmp.res <- c(tmp.res, tmp.summ.wt200d.node3.t4$effect.measures$ATE$estimate)
            
            # Node 6
            tmp.res.wt200d.node6.t1 <- ltmle(tmp.ci.data.wt200d.6 %>%
                                               select(male:Weight_1), 
                                             Anodes = paste0("DTGOn_", 0), 
                                             Cnodes = paste0("censor_", 0),
                                             Lnodes = NULL,
                                             Ynodes = paste0("Weight_", 1), 
                                             abar            = list(treatment = 1, control = 0), 
                                             gbounds         = c(0.025, 0.975),
                                             estimate.time   = F)
            tmp.summ.wt200d.node6.t1 <- summary(tmp.res.wt200d.node6.t1)
            tmp.res  <- c(tmp.res, tmp.summ.wt200d.node6.t1$effect.measures$ATE$estimate)
            
            tmp.res.wt200d.node6.t2 <- ltmle(tmp.ci.data.wt200d.6 %>%
                                               select(male:Weight_2), 
                                             Anodes = paste0("DTGOn_", 0:1), 
                                             Cnodes = paste0("censor_", 0:1),
                                             Lnodes = c(paste0(vary.cov.int.used, "_1")),
                                             Ynodes = paste0("Weight_", 1:2), 
                                             abar            = list(treatment = rep(1, 2), control = rep(0, 2)), 
                                             gbounds         = c(0.025, 0.975),
                                             estimate.time   = F)
            tmp.summ.wt200d.node6.t2 <- summary(tmp.res.wt200d.node6.t2)
            tmp.res  <- c(tmp.res, tmp.summ.wt200d.node6.t2$effect.measures$ATE$estimate)
            
            tmp.res.wt200d.node6.t3 <- ltmle(tmp.ci.data.wt200d.6 %>%
                                               select(male:Weight_3), 
                                             Anodes = paste0("DTGOn_", 0:2), 
                                             Cnodes = paste0("censor_", 0:2),
                                             Lnodes = c(paste0(vary.cov.int.used, "_1"), paste0(vary.cov.int.used, "_2")),
                                             Ynodes = paste0("Weight_", 1:3), 
                                             abar            = list(treatment = rep(1, 3), control = rep(0, 3)), 
                                             gbounds         = c(0.025, 0.975),
                                             estimate.time   = F)
            tmp.summ.wt200d.node6.t3 <- summary(tmp.res.wt200d.node6.t3)
            tmp.res  <- c(tmp.res, tmp.summ.wt200d.node6.t3$effect.measures$ATE$estimate)
            
            tmp.res.wt200d.node6.t4 <- ltmle(tmp.ci.data.wt200d.6, 
                                             Anodes = paste0("DTGOn_", 0:3), 
                                             Cnodes = paste0("censor_", 0:3),
                                             Lnodes = c(paste0(vary.cov.int.used, "_1"), paste0(vary.cov.int.used, "_2"), paste0(vary.cov.int.used, "_3")),
                                             Ynodes = paste0("Weight_", 1:4), 
                                             abar            = list(treatment = rep(1, 4), control = rep(0, 4)), 
                                             gbounds         = c(0.025, 0.975),
                                             estimate.time   = F)
            tmp.summ.wt200d.node6.t4 <- summary(tmp.res.wt200d.node6.t4)
            tmp.res <- c(tmp.res, tmp.summ.wt200d.node6.t4$effect.measures$ATE$estimate)
            
            # Node 7
            tmp.res.wt200d.node7.t1 <- ltmle(tmp.ci.data.wt200d.7 %>%
                                               select(male:Weight_1), 
                                             Anodes = paste0("DTGOn_", 0), 
                                             Cnodes = paste0("censor_", 0),
                                             Lnodes = NULL,
                                             Ynodes = paste0("Weight_", 1), 
                                             abar            = list(treatment = 1, control = 0), 
                                             gbounds         = c(0.025, 0.975),
                                             estimate.time   = F)
            tmp.summ.wt200d.node7.t1 <- summary(tmp.res.wt200d.node7.t1)
            tmp.res  <- c(tmp.res, tmp.summ.wt200d.node7.t1$effect.measures$ATE$estimate)
            
            tmp.res.wt200d.node7.t2 <- ltmle(tmp.ci.data.wt200d.7 %>%
                                               select(male:Weight_2), 
                                             Anodes = paste0("DTGOn_", 0:1), 
                                             Cnodes = paste0("censor_", 0:1),
                                             Lnodes = c(paste0(vary.cov.int.used, "_1")),
                                             Ynodes = paste0("Weight_", 1:2), 
                                             abar            = list(treatment = rep(1, 2), control = rep(0, 2)), 
                                             gbounds         = c(0.025, 0.975),
                                             estimate.time   = F)
            tmp.summ.wt200d.node7.t2 <- summary(tmp.res.wt200d.node7.t2)
            tmp.res  <- c(tmp.res, tmp.summ.wt200d.node7.t2$effect.measures$ATE$estimate)
            
            tmp.res.wt200d.node7.t3 <- ltmle(tmp.ci.data.wt200d.7 %>%
                                               select(male:Weight_3), 
                                             Anodes = paste0("DTGOn_", 0:2), 
                                             Cnodes = paste0("censor_", 0:2),
                                             Lnodes = c(paste0(vary.cov.int.used, "_1"), paste0(vary.cov.int.used, "_2")),
                                             Ynodes = paste0("Weight_", 1:3), 
                                             abar            = list(treatment = rep(1, 3), control = rep(0, 3)), 
                                             gbounds         = c(0.025, 0.975),
                                             estimate.time   = F)
            tmp.summ.wt200d.node7.t3 <- summary(tmp.res.wt200d.node7.t3)
            tmp.res  <- c(tmp.res, tmp.summ.wt200d.node7.t3$effect.measures$ATE$estimate)
            
            tmp.res.wt200d.node7.t4 <- ltmle(tmp.ci.data.wt200d.7, 
                                             Anodes = paste0("DTGOn_", 0:3), 
                                             Cnodes = paste0("censor_", 0:3),
                                             Lnodes = c(paste0(vary.cov.int.used, "_1"), paste0(vary.cov.int.used, "_2"), paste0(vary.cov.int.used, "_3")),
                                             Ynodes = paste0("Weight_", 1:4), 
                                             abar            = list(treatment = rep(1, 4), control = rep(0, 4)), 
                                             gbounds         = c(0.025, 0.975),
                                             estimate.time   = F)
            tmp.summ.wt200d.node7.t4 <- summary(tmp.res.wt200d.node7.t4)
            tmp.res <- c(tmp.res, tmp.summ.wt200d.node7.t4$effect.measures$ATE$estimate)
            
            tmp.res
            
          }

# save.image("IntermediateData.RData")
colnames(df.eff.wt200d.nodes) <- c("all_t1", "all_t2", "all_t3", "all_t4", 
                                   "node2_t1", "node2_t2", "node2_t3", "node2_t4", 
                                   "node3_t1", "node3_t2", "node3_t3", "node3_t4", 
                                   "node6_t1", "node6_t2", "node6_t3", "node6_t4", 
                                   "node7_t1", "node7_t2", "node7_t3", "node7_t4")
dim(df.eff.wt200d.nodes)

summ.eff.wt200d.nodes <- t(apply(df.eff.wt200d.nodes, 2, quantile, probs = c(0.025, 0.5, 0.975)))
summ.eff.wt200d.nodes <- data.frame(node = rep(c("all", "node2", "node3", "node6", "node7"), each = 4),
                                    time = rep(1:4, 5),
                                    summ.eff.wt200d.nodes, 
                                    eff  = c(eff.wt200d.node1.t1, eff.wt200d.node1.t2, eff.wt200d.node1.t3, eff.wt200d.node1.t4,
                                             eff.wt200d.node2.t1, eff.wt200d.node2.t2, eff.wt200d.node2.t3, eff.wt200d.node2.t4,
                                             eff.wt200d.node3.t1, eff.wt200d.node3.t2, eff.wt200d.node3.t3, eff.wt200d.node3.t4,
                                             eff.wt200d.node6.t1, eff.wt200d.node6.t2, eff.wt200d.node6.t3, eff.wt200d.node6.t4,
                                             eff.wt200d.node7.t1, eff.wt200d.node7.t2, eff.wt200d.node7.t3, eff.wt200d.node7.t4))
colnames(summ.eff.wt200d.nodes) <- c("node", "time", "q0025", "q050", "q0975", "eff")
summ.eff.wt200d.nodes <- summ.eff.wt200d.nodes %>%
  select(node, time, q0025, eff, q050, q0975)

save(df.eff.wt200d.nodes, summ.eff.wt200d.nodes, 
     summ.wt200d.node1.t1, summ.wt200d.node1.t2, summ.wt200d.node1.t3, summ.wt200d.node1.t4,
     summ.wt200d.node2.t1, summ.wt200d.node2.t2, summ.wt200d.node2.t3, summ.wt200d.node2.t4,
     summ.wt200d.node3.t1, summ.wt200d.node3.t2, summ.wt200d.node3.t3, summ.wt200d.node3.t4,
     summ.wt200d.node6.t1, summ.wt200d.node6.t2, summ.wt200d.node6.t3, summ.wt200d.node6.t4,
     summ.wt200d.node7.t1, summ.wt200d.node7.t2, summ.wt200d.node7.t3, summ.wt200d.node7.t4,
     file = "../../Data/Data20210512/Processed/WtSubAnal200dAllStp5SbgrpTrjctr_20220714.RData")
# load("WtSubAnal200dAllStp4_20220518.RData")
# mean(df.eff.wt200d.nodes[, "node2"] < df.eff.wt200d.nodes[, "node3"])
# mean(df.eff.wt200d.nodes[, "node6"] < df.eff.wt200d.nodes[, "node7"])

# load("../../Data/Data20210512/Processed/Summ2AllWt20220329.RData")

# mean.time0_dob <- mean(finalData.noMiss.t0Jul2016.switchDTG.wt200d$time0_dob)
# sd.time0_dob   <- sd(finalData.noMiss.t0Jul2016.switchDTG.wt200d$time0_dob)

# (final.tree.wt200d$splits[2, "index"] * sd.time0_dob + mean.time0_dob) / 365.25

library(ggplot2)
library(gridExtra)
load("../../Data/Processed/WtAvgTrtEff200dCiTrjctr20220512.RData")
load("../../Data/Processed/WtSubAnal200dAllStp5SbgrpTrjctr_20220714.RData")
# summ.wt200d.diffTrjctr 
# summ.eff.wt200d.nodes

summ.forFig <- summ.eff.wt200d.nodes
summ.forFig$q0025[summ.forFig$node == "all"] <- summ.wt200d.diffTrjctr$ci_025
summ.forFig$eff[summ.forFig$node == "all"]   <- summ.wt200d.diffTrjctr$avg
summ.forFig$q0975[summ.forFig$node == "all"] <- summ.wt200d.diffTrjctr$ci_975
# p1 <- ggplot(summ.forFig %>% filter(node %in% c("all", "node2", "node3")), aes(x = time, y = eff, group = node, color = node)) + 
summ.forFig <- summ.forFig %>%
  mutate(days = ifelse(time == 1, 200 * 1, NA)) %>%
  mutate(days = ifelse(time == 2, 200 * 2, days)) %>%
  mutate(days = ifelse(time == 3, 200 * 3, days)) %>%
  mutate(days = ifelse(time == 4, 200 * 4, days))
summ.eff.wt200d.nodes <- summ.eff.wt200d.nodes %>%
  mutate(days = ifelse(time == 1, 200 * 1, NA)) %>%
  mutate(days = ifelse(time == 2, 200 * 2, days)) %>%
  mutate(days = ifelse(time == 3, 200 * 3, days)) %>%
  mutate(days = ifelse(time == 4, 200 * 4, days))
p1 <- ggplot(summ.forFig %>% filter(node %in% c("node2", "node3")), aes(x = days, y = eff, group = node, color = node)) + 
  geom_point(position = position_dodge(width = 30)) + 
  geom_errorbar(aes(ymin = q0025, ymax = q0975), width = 30, position = position_dodge(width = 30)) +
  ylim(-0.05, 4) +
  labs(x = "Day", y = "Estimated DTG effect (kg)") + 
  scale_color_manual(breaks = c("node2", "node3"),
                     labels = c("Male", "Female"),
                     values = c("black", "darkgray")) +
  theme_bw() + 
  theme(legend.title    = element_blank(),
        legend.position = "bottom",
        legend.margin   = margin(t = -7, r = 0, b = 0, l = 0, unit = "pt"))

p2 <- ggplot(summ.eff.wt200d.nodes %>% filter(node %in% c("node6", "node7")), aes(x = days, y = eff, group = node, color = node)) + 
  geom_point(position = position_dodge(width = 30)) + 
  geom_errorbar(aes(ymin = q0025, ymax = q0975), width = 30, position = position_dodge(width = 30)) +
  ylim(-0.05, 4) +
  labs(x = "Day", y = "Estimated DTG effect (kg)") + 
  scale_color_manual(breaks = c("node6", "node7"),
                     labels = c("< 42.8 years", ">= 42.8 years"),
                     values = c("black", "darkgray"),
                     name   = "Female") +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.margin   = margin(t = -7, r = 0, b = 0, l = 0, unit = "pt"))

# pdf("../../Report/Manuscript20220706/WtSubAnal200dSbgrpTrjctr20220919.pdf", width = 7, height = 3.5)
# grid.arrange(p1, p2, nrow = 1)
# dev.off()

# pdf("../../Report/CROI//WtSubAnal200dSbgrpTrjctr20230128.pdf", width = 6.5, height = 4)
tiff("../../Report/CROI//WtSubAnal200dSbgrpTrjctr20230128.tiff", units="in", width=6.5, height=2.9, res=300)
grid.arrange(p1, p2, nrow = 1)
dev.off()
