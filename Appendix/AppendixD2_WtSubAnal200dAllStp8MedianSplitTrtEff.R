library(ltmle)
library(dplyr)
library(table1)
library(doParallel)

load("../../../Data/Data20210512/Processed/Summ2AllWt20230304.RData")
load("../../../Data/Data20210512/Processed/WtSubAnal200dAllStp3_20230304.RData")
load("../../Simulation/seed1000.rda")

n.iters.i <- ind.tree.size2.male[33]
load(paste0("../../../Data/Data20210512/Processed/TreeRes20230304/WtSubAnal200dAllStp1_seed", n.iters.i, "_20230304.RData"))
load(paste0("../../../Data/Data20210512/Processed/TreeRes20230304/WtSubAnal200dAllStp2_seed", n.iters.i, "_20230304.RData"))

registerDoParallel(cores=Sys.getenv("SLURM_CPUS_PER_TASK"))
# registerDoParallel(cores=28)

# tree building dataset
tree.data.wt200d <- subData.nomiss.wt200d.sbgrpWt.resPop.scaled[tree.ind.wt200d, ]
dim(tree.data.wt200d)

md.time0_dob <- median(tree.data.wt200d$time0_dob)
tree.data.wt200d.time0_dob.l <- tree.data.wt200d %>%
  filter(time0_dob < md.time0_dob)
tree.data.wt200d.time0_dob.r <- tree.data.wt200d %>%
  filter(time0_dob >= md.time0_dob)

md.time0_dob.m <- median(tree.data.wt200d$time0_dob[tree.data.wt200d$male == 1])
tree.data.wt200d.time0_dob.m.l <- tree.data.wt200d %>%
  filter((time0_dob < md.time0_dob.m) & (male == 1)) %>%
  select(-male)
tree.data.wt200d.time0_dob.m.r <- tree.data.wt200d %>%
  filter((time0_dob >= md.time0_dob.m) & (male == 1)) %>%
  select(-male)

md.time0_dob.f <- median(tree.data.wt200d$time0_dob[tree.data.wt200d$male == 0])
tree.data.wt200d.time0_dob.f.l <- tree.data.wt200d %>%
  filter((time0_dob < md.time0_dob.f) & (male == 0)) %>%
  select(-male)
tree.data.wt200d.time0_dob.f.r <- tree.data.wt200d %>%
  filter((time0_dob >= md.time0_dob.f) & (male == 0)) %>%
  select(-male)

# md.arvstart_dob <- median(tree.data.wt200d$arvstart_dob)
# tree.data.wt200d.arvstart_dob.l <- tree.data.wt200d %>%
#        filter(arvstart_dob < md.arvstart_dob)
# tree.data.wt200d.arvstart_dob.r <- tree.data.wt200d %>%
#        filter(arvstart_dob >= md.arvstart_dob)

vary.cov.int.used <- c("viralloadImptCtgr", "SystolicBP", "DiastolicBP", "Height",
                       "tbtx", "activeTB", "Married", "nhif")

# age
res.wt200d.time0_dob.l <- ltmle(tree.data.wt200d.time0_dob.l, 
                                Anodes = paste0("DTGOn_", 0:3), 
                                Cnodes = paste0("censor_", 0:3),
                                Lnodes = c(paste0(vary.cov.int.used, "_1"), paste0(vary.cov.int.used, "_2"), paste0(vary.cov.int.used, "_3")),
                                Ynodes = paste0("Weight_", 1:4), 
                                abar            = list(treatment = rep(1, 4), control = rep(0, 4)), 
                                gbounds         = c(0.025, 0.975),
                                estimate.time   = F)
summ.wt200d.time0_dob.l <- summary(res.wt200d.time0_dob.l)
eff.wt200d.time0_dob.l  <- summ.wt200d.time0_dob.l$effect.measures$ATE$estimate

res.wt200d.time0_dob.r <- ltmle(tree.data.wt200d.time0_dob.r, 
                                Anodes = paste0("DTGOn_", 0:3), 
                                Cnodes = paste0("censor_", 0:3),
                                Lnodes = c(paste0(vary.cov.int.used, "_1"), paste0(vary.cov.int.used, "_2"), paste0(vary.cov.int.used, "_3")),
                                Ynodes = paste0("Weight_", 1:4), 
                                abar            = list(treatment = rep(1, 4), control = rep(0, 4)), 
                                gbounds         = c(0.025, 0.975),
                                estimate.time   = F)
summ.wt200d.time0_dob.r <- summary(res.wt200d.time0_dob.r)
eff.wt200d.time0_dob.r  <- summ.wt200d.time0_dob.r$effect.measures$ATE$estimate

# male, age
res.wt200d.time0_dob.m.l <- ltmle(tree.data.wt200d.time0_dob.m.l, 
                                  Anodes = paste0("DTGOn_", 0:3), 
                                  Cnodes = paste0("censor_", 0:3),
                                  Lnodes = c(paste0(vary.cov.int.used, "_1"), paste0(vary.cov.int.used, "_2"), paste0(vary.cov.int.used, "_3")),
                                  Ynodes = paste0("Weight_", 1:4), 
                                  abar            = list(treatment = rep(1, 4), control = rep(0, 4)), 
                                  gbounds         = c(0.025, 0.975),
                                  estimate.time   = F)
summ.wt200d.time0_dob.m.l <- summary(res.wt200d.time0_dob.m.l)
eff.wt200d.time0_dob.m.l  <- summ.wt200d.time0_dob.m.l$effect.measures$ATE$estimate

res.wt200d.time0_dob.m.r <- ltmle(tree.data.wt200d.time0_dob.m.r, 
                                  Anodes = paste0("DTGOn_", 0:3), 
                                  Cnodes = paste0("censor_", 0:3),
                                  Lnodes = c(paste0(vary.cov.int.used, "_1"), paste0(vary.cov.int.used, "_2"), paste0(vary.cov.int.used, "_3")),
                                  Ynodes = paste0("Weight_", 1:4), 
                                  abar            = list(treatment = rep(1, 4), control = rep(0, 4)), 
                                  gbounds         = c(0.025, 0.975),
                                  estimate.time   = F)
summ.wt200d.time0_dob.m.r <- summary(res.wt200d.time0_dob.m.r)
eff.wt200d.time0_dob.m.r  <- summ.wt200d.time0_dob.m.r$effect.measures$ATE$estimate

# female, age
res.wt200d.time0_dob.f.l <- ltmle(tree.data.wt200d.time0_dob.f.l, 
                                  Anodes = paste0("DTGOn_", 0:3), 
                                  Cnodes = paste0("censor_", 0:3),
                                  Lnodes = c(paste0(vary.cov.int.used, "_1"), paste0(vary.cov.int.used, "_2"), paste0(vary.cov.int.used, "_3")),
                                  Ynodes = paste0("Weight_", 1:4), 
                                  abar            = list(treatment = rep(1, 4), control = rep(0, 4)), 
                                  gbounds         = c(0.025, 0.975),
                                  estimate.time   = F)
summ.wt200d.time0_dob.f.l <- summary(res.wt200d.time0_dob.f.l)
eff.wt200d.time0_dob.f.l  <- summ.wt200d.time0_dob.f.l$effect.measures$ATE$estimate

res.wt200d.time0_dob.f.r <- ltmle(tree.data.wt200d.time0_dob.f.r, 
                                  Anodes = paste0("DTGOn_", 0:3), 
                                  Cnodes = paste0("censor_", 0:3),
                                  Lnodes = c(paste0(vary.cov.int.used, "_1"), paste0(vary.cov.int.used, "_2"), paste0(vary.cov.int.used, "_3")),
                                  Ynodes = paste0("Weight_", 1:4), 
                                  abar            = list(treatment = rep(1, 4), control = rep(0, 4)), 
                                  gbounds         = c(0.025, 0.975),
                                  estimate.time   = F)
summ.wt200d.time0_dob.f.r <- summary(res.wt200d.time0_dob.f.r)
eff.wt200d.time0_dob.f.r  <- summ.wt200d.time0_dob.f.r$effect.measures$ATE$estimate

# # arvstart
# res.wt200d.arvstart_dob.l <- ltmle(tree.data.wt200d.arvstart_dob.l, 
#                              Anodes = paste0("DTGOn_", 0:3), 
#                              Cnodes = paste0("censor_", 0:3),
#                              Lnodes = c(paste0(vary.cov.int.used, "_1"), paste0(vary.cov.int.used, "_2"), paste0(vary.cov.int.used, "_3")),
#                              Ynodes = paste0("Weight_", 1:4), 
#                              abar            = list(treatment = rep(1, 4), control = rep(0, 4)), 
#                              gbounds         = c(0.025, 0.975),
#                              estimate.time   = F)
# summ.wt200d.arvstart_dob.l <- summary(res.wt200d.arvstart_dob.l)
# eff.wt200d.arvstart_dob.l  <- summ.wt200d.arvstart_dob.l$effect.measures$ATE$estimate

# res.wt200d.arvstart_dob.r <- ltmle(tree.data.wt200d.arvstart_dob.r, 
#                              Anodes = paste0("DTGOn_", 0:3), 
#                              Cnodes = paste0("censor_", 0:3),
#                              Lnodes = c(paste0(vary.cov.int.used, "_1"), paste0(vary.cov.int.used, "_2"), paste0(vary.cov.int.used, "_3")),
#                              Ynodes = paste0("Weight_", 1:4), 
#                              abar            = list(treatment = rep(1, 4), control = rep(0, 4)), 
#                              gbounds         = c(0.025, 0.975),
#                              estimate.time   = F)
# summ.wt200d.arvstart_dob.r <- summary(res.wt200d.arvstart_dob.r)
# eff.wt200d.arvstart_dob.r  <- summ.wt200d.arvstart_dob.r$effect.measures$ATE$estimate

# Bootstrap
iters.boot <- 10^3
nrow.tree.data.wt200d <- nrow(tree.data.wt200d)

set.seed(a[333])
df.eff.wt200d.nodes <- 
  foreach(iters.boot.i = 1:iters.boot,
          .combine   = rbind) %dopar% {
            
            set.seed(a[iters.boot.i])
            tmp.res <- NULL
            tmp.tree.data.wt200d <- tree.data.wt200d[sample(1:nrow.tree.data.wt200d, nrow.tree.data.wt200d, replace = TRUE), ]
            
            tmp.tree.data.wt200d.time0_dob.l <- tmp.tree.data.wt200d %>%
              filter(time0_dob < md.time0_dob)
            tmp.tree.data.wt200d.time0_dob.r <- tmp.tree.data.wt200d %>%
              filter(time0_dob >= md.time0_dob)
            
            tmp.tree.data.wt200d.time0_dob.m.l <- tmp.tree.data.wt200d %>%
              filter((time0_dob < md.time0_dob.m) & (male == 1)) %>%
              select(-male)
            tmp.tree.data.wt200d.time0_dob.m.r <- tmp.tree.data.wt200d %>%
              filter((time0_dob >= md.time0_dob.m) & (male == 1)) %>%
              select(-male)
            
            tmp.tree.data.wt200d.time0_dob.f.l <- tmp.tree.data.wt200d %>%
              filter((time0_dob < md.time0_dob.f) & (male == 0)) %>%
              select(-male)
            tmp.tree.data.wt200d.time0_dob.f.r <- tmp.tree.data.wt200d %>%
              filter((time0_dob >= md.time0_dob.f) & (male == 0)) %>%
              select(-male)
            
            tmp.res.wt200d.time0_dob.l <- ltmle(tmp.tree.data.wt200d.time0_dob.l, 
                                                Anodes = paste0("DTGOn_", 0:3), 
                                                Cnodes = paste0("censor_", 0:3),
                                                Lnodes = c(paste0(vary.cov.int.used, "_1"), paste0(vary.cov.int.used, "_2"), paste0(vary.cov.int.used, "_3")),
                                                Ynodes = paste0("Weight_", 1:4), 
                                                abar            = list(treatment = rep(1, 4), control = rep(0, 4)), 
                                                gbounds         = c(0.025, 0.975),
                                                estimate.time   = F)
            tmp.summ.wt200d.time0_dob.l <- summary(tmp.res.wt200d.time0_dob.l)
            tmp.res  <- c(tmp.res, tmp.summ.wt200d.time0_dob.l$effect.measures$ATE$estimate)
            
            tmp.res.wt200d.time0_dob.r <- ltmle(tmp.tree.data.wt200d.time0_dob.r, 
                                                Anodes = paste0("DTGOn_", 0:3), 
                                                Cnodes = paste0("censor_", 0:3),
                                                Lnodes = c(paste0(vary.cov.int.used, "_1"), paste0(vary.cov.int.used, "_2"), paste0(vary.cov.int.used, "_3")),
                                                Ynodes = paste0("Weight_", 1:4), 
                                                abar            = list(treatment = rep(1, 4), control = rep(0, 4)), 
                                                gbounds         = c(0.025, 0.975),
                                                estimate.time   = F)
            tmp.summ.wt200d.time0_dob.r <- summary(tmp.res.wt200d.time0_dob.r)
            tmp.res <- c(tmp.res, tmp.summ.wt200d.time0_dob.r$effect.measures$ATE$estimate)
            
            # male, age
            tmp.res.wt200d.time0_dob.m.l <- ltmle(tmp.tree.data.wt200d.time0_dob.m.l, 
                                                  Anodes = paste0("DTGOn_", 0:3), 
                                                  Cnodes = paste0("censor_", 0:3),
                                                  Lnodes = c(paste0(vary.cov.int.used, "_1"), paste0(vary.cov.int.used, "_2"), paste0(vary.cov.int.used, "_3")),
                                                  Ynodes = paste0("Weight_", 1:4), 
                                                  abar            = list(treatment = rep(1, 4), control = rep(0, 4)), 
                                                  gbounds         = c(0.025, 0.975),
                                                  estimate.time   = F)
            tmp.summ.wt200d.time0_dob.m.l <- summary(tmp.res.wt200d.time0_dob.m.l)
            tmp.res  <- c(tmp.res, tmp.summ.wt200d.time0_dob.m.l$effect.measures$ATE$estimate)
            
            tmp.res.wt200d.time0_dob.m.r <- ltmle(tmp.tree.data.wt200d.time0_dob.m.r, 
                                                  Anodes = paste0("DTGOn_", 0:3), 
                                                  Cnodes = paste0("censor_", 0:3),
                                                  Lnodes = c(paste0(vary.cov.int.used, "_1"), paste0(vary.cov.int.used, "_2"), paste0(vary.cov.int.used, "_3")),
                                                  Ynodes = paste0("Weight_", 1:4), 
                                                  abar            = list(treatment = rep(1, 4), control = rep(0, 4)), 
                                                  gbounds         = c(0.025, 0.975),
                                                  estimate.time   = F)
            tmp.summ.wt200d.time0_dob.m.r <- summary(tmp.res.wt200d.time0_dob.m.r)
            tmp.res  <- c(tmp.res, tmp.summ.wt200d.time0_dob.m.r$effect.measures$ATE$estimate)
            
            # female, age
            tmp.res.wt200d.time0_dob.f.l <- ltmle(tmp.tree.data.wt200d.time0_dob.f.l, 
                                                  Anodes = paste0("DTGOn_", 0:3), 
                                                  Cnodes = paste0("censor_", 0:3),
                                                  Lnodes = c(paste0(vary.cov.int.used, "_1"), paste0(vary.cov.int.used, "_2"), paste0(vary.cov.int.used, "_3")),
                                                  Ynodes = paste0("Weight_", 1:4), 
                                                  abar            = list(treatment = rep(1, 4), control = rep(0, 4)), 
                                                  gbounds         = c(0.025, 0.975),
                                                  estimate.time   = F)
            tmp.summ.wt200d.time0_dob.f.l <- summary(tmp.res.wt200d.time0_dob.f.l)
            tmp.res <- c(tmp.res, tmp.summ.wt200d.time0_dob.f.l$effect.measures$ATE$estimate)
            
            tmp.res.wt200d.time0_dob.f.r <- ltmle(tmp.tree.data.wt200d.time0_dob.f.r, 
                                                  Anodes = paste0("DTGOn_", 0:3), 
                                                  Cnodes = paste0("censor_", 0:3),
                                                  Lnodes = c(paste0(vary.cov.int.used, "_1"), paste0(vary.cov.int.used, "_2"), paste0(vary.cov.int.used, "_3")),
                                                  Ynodes = paste0("Weight_", 1:4), 
                                                  abar            = list(treatment = rep(1, 4), control = rep(0, 4)), 
                                                  gbounds         = c(0.025, 0.975),
                                                  estimate.time   = F)
            tmp.summ.wt200d.time0_dob.f.r <- summary(tmp.res.wt200d.time0_dob.f.r)
            tmp.res  <- c(tmp.res, tmp.summ.wt200d.time0_dob.f.r$effect.measures$ATE$estimate)
            
            tmp.res
            
          }

# save.image("IntermediateData.RData")
colnames(df.eff.wt200d.nodes) <- c("time_dob_l", "time0_dob_r", 
                                   "time0_dob_m_l", "time0_dob_m_r",
                                   "time0_dob_f_l", "time0_dob_f_r")
dim(df.eff.wt200d.nodes)

summ.eff.wt200d.nodes <- t(apply(df.eff.wt200d.nodes, 2, quantile, probs = c(0.025, 0.5, 0.975)))
summ.eff.wt200d.nodes <- data.frame(dataset = rep(c("all", "male", "female"), each = 2),
                                    split   = rep(c("< median", ">= median"), 3),
                                    summ.eff.wt200d.nodes, 
                                    eff  = c(eff.wt200d.time0_dob.l, eff.wt200d.time0_dob.r, 
                                             eff.wt200d.time0_dob.m.l, eff.wt200d.time0_dob.m.r,
                                             eff.wt200d.time0_dob.f.l, eff.wt200d.time0_dob.f.r))
colnames(summ.eff.wt200d.nodes) <- c("dataset", "split", "q0025", "q050", "q0975", "eff")
summ.eff.wt200d.nodes <- summ.eff.wt200d.nodes %>%
  select(dataset, split, q0025, eff, q050, q0975)

# split at median age 
mean.time0_dob <- mean(subData.nomiss.wt200d.sbgrpWt.resPop$time0_dob)
sd.time0_dob   <- sd(subData.nomiss.wt200d.sbgrpWt.resPop$time0_dob)
spltPt.time0_dob <- (md.time0_dob * sd.time0_dob) + mean.time0_dob
spltPt.time0_dob.m <- (md.time0_dob.m * sd.time0_dob) + mean.time0_dob
spltPt.time0_dob.f <- (md.time0_dob.f * sd.time0_dob) + mean.time0_dob

save(df.eff.wt200d.nodes, summ.eff.wt200d.nodes, 
     summ.wt200d.time0_dob.l, summ.wt200d.time0_dob.r, 
     summ.wt200d.time0_dob.m.l, summ.wt200d.time0_dob.m.r,
     summ.wt200d.time0_dob.f.l, summ.wt200d.time0_dob.f.r, 
     mean.time0_dob, sd.time0_dob, spltPt.time0_dob, spltPt.time0_dob.m, spltPt.time0_dob.f,
     file = "../../../Data/Data20210512/Processed/WtSubAnal200dAllStp8MedianSplitTrtEff_20230509.RData")

library(ggplot2)
load("../../../Data/Processed/WtSubAnal200dAllStp8MedianSplitTrtEff_20230509.RData")

summ.eff.wt200d.nodes <- data.frame(summ.eff.wt200d.nodes) %>%
  mutate(dataset = factor(dataset,
                          levels = c("all", "male", "female"),
                          labels = c("All", "Male", "Female")))

p1 <- ggplot(data = summ.eff.wt200d.nodes, aes(x = dataset, y = eff, color = split, group = interaction(dataset, split))) + 
  geom_point(position = position_dodge(width = 0.3)) + 
  geom_errorbar(aes(ymin = q0025, ymax = q0975), width = 0.3, position = position_dodge(width = 0.3)) +
  # ylim(-1.1, 3.5) +
  labs(x = "Dataset", 
       y = "Estimated DTG effect (kg)") + 
  scale_color_manual(breaks = c("< median", ">= median"),
                     labels = c("< Median age", ">= Median age"),
                     values = c("darkgray", "black")) +
  theme_bw() + 
  theme(legend.title    = element_blank(),
        legend.position = "bottom",
        legend.margin   = margin(t = -7, r = 0, b = 0, l = 0, unit = "pt"))

pdf("../../../Report/Manuscript20230402/WtSubAnal200dMedianSplitTrtEff20230509.pdf", width = 5, height = 3.5)
# grid.arrange(p1, p2, nrow = 1)
p1
dev.off()



