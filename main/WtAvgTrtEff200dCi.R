library(ltmle)
library(dplyr)
library(table1)
library(doParallel)

load("../../../Data/Data20210512/Processed/Summ2AllWt20230304.RData")
load("../../Simulation/seed1000.rda")

registerDoParallel(cores=Sys.getenv("SLURM_CPUS_PER_TASK"))
# registerDoParallel(cores=28)

# 200-day
subData.nomiss.wt200d.sbgrpWt.resPop <- subData.nomiss.wt200d.sbgrpWt.resPop %>%
  mutate(censor_0 = BinaryToCensoring(is.censored = censor_0),
         censor_1 = BinaryToCensoring(is.censored = censor_1),
         censor_2 = BinaryToCensoring(is.censored = censor_2),
         censor_3 = BinaryToCensoring(is.censored = censor_3),
         censor_4 = BinaryToCensoring(is.censored = censor_4),
         censor_5 = BinaryToCensoring(is.censored = censor_5),
         censor_6 = BinaryToCensoring(is.censored = censor_6),
         censor_7 = BinaryToCensoring(is.censored = censor_7))

subData.nomiss.wt200d.sbgrpWt.resPop.scaled <- subData.nomiss.wt200d.sbgrpWt.resPop %>%
  select(male:Weight_8) %>%
  select(-enroldate_dob, -enroldate_arvstart, -time0_enroldate) %>%
  mutate(arvstart_dob = scale(arvstart_dob), 
         # enroldate_dob = scale(enroldate_dob),
         time0_dob     = scale(time0_dob),
         # enroldate_arvstart = scale(enroldate_arvstart),
         time0_arvstart     = scale(time0_arvstart),
         # time0_enroldate    = scale(time0_enroldate),
         SystolicBP_0       = scale(SystolicBP_0),
         SystolicBP_1       = scale(SystolicBP_1),
         SystolicBP_2       = scale(SystolicBP_2),
         SystolicBP_3       = scale(SystolicBP_3),
         SystolicBP_4       = scale(SystolicBP_4),
         SystolicBP_5       = scale(SystolicBP_5),
         SystolicBP_6       = scale(SystolicBP_6),
         SystolicBP_7       = scale(SystolicBP_7),
         DiastolicBP_0      = scale(DiastolicBP_0),
         DiastolicBP_1      = scale(DiastolicBP_1),
         DiastolicBP_2      = scale(DiastolicBP_2),
         DiastolicBP_3      = scale(DiastolicBP_3),
         DiastolicBP_4      = scale(DiastolicBP_4),
         DiastolicBP_5      = scale(DiastolicBP_5),
         DiastolicBP_6      = scale(DiastolicBP_6),
         DiastolicBP_7      = scale(DiastolicBP_7),
         Weight_0       = scale(Weight_0),
         # Weight_1       = scale(Weight_1),
         # Weight_2       = scale(Weight_2),
         # Weight_3       = scale(Weight_3),
         # Weight_4       = scale(Weight_4),
         # Weight_5       = scale(Weight_5),
         # Weight_6       = scale(Weight_6),
         # Weight_7       = scale(Weight_7),
         Height_0       = scale(Height_0),
         Height_1       = scale(Height_1),
         Height_2       = scale(Height_2),
         Height_3       = scale(Height_3),
         Height_4       = scale(Height_4),
         Height_5       = scale(Height_5),
         Height_6       = scale(Height_6),
         Height_7       = scale(Height_7)) %>%
  mutate(male                 = ifelse(male == "Female", 0, 1),
         enroll_after20160701 = ifelse(enroll_after20160701, 1, 0),
         nhif_0               = ifelse(nhif_0 == "yes", 1, 0),
         nhif_1               = ifelse(nhif_1 == "yes", 1, 0),
         nhif_2               = ifelse(nhif_2 == "yes", 1, 0),
         nhif_3               = ifelse(nhif_3 == "yes", 1, 0),
         nhif_4               = ifelse(nhif_4 == "yes", 1, 0),
         nhif_5               = ifelse(nhif_5 == "yes", 1, 0),
         nhif_6               = ifelse(nhif_6 == "yes", 1, 0),
         nhif_7               = ifelse(nhif_7 == "yes", 1, 0), 
         artStatus            = ifelse(artStatus == "treatmentFree", 1, 0))

head(subData.nomiss.wt200d.sbgrpWt.resPop.scaled)

# no viral load because of large proportion of missing
# tmp.col.order <- grep("viralload_", colnames(finalData.noMiss.t0Jul2016.switchDTG.wt200d.scaled))
# finalData.noMiss.t0Jul2016.switchDTG.wt200d.scaled <- finalData.noMiss.t0Jul2016.switchDTG.wt200d.scaled[, -tmp.col.order]
# finalData.noMiss.t0Jul2016.switchDTG.wt200d.noOrigVl <- finalData.noMiss.t0Jul2016.switchDTG.wt200d[, -tmp.col.order]
subData.nomiss.wt200d.sbgrpWt.resPop.scaled <- subData.nomiss.wt200d.sbgrpWt.resPop.scaled %>%
  select(-starts_with(c("viralloadImptMn", "log10ViralloadImptMn"))) %>%
  select(-starts_with("pregnant")) %>%
  select(-starts_with("civilstatus"))

# head(finalData.noMiss.t0Jul2016.switchDTG.wt200d.scaled)
vary.cov.int.used <- c("viralloadImptCtgr", "SystolicBP", "DiastolicBP", "Height",
                       # c("log10Viralload", "SystolicBP", "DiastolicBP", "Height", "pregnant",
                       "tbtx", "activeTB", "Married", "nhif")

# time 1
result.tmle.wt200d.1 <- ltmle(subData.nomiss.wt200d.sbgrpWt.resPop.scaled %>%
                                select(male:Weight_1), 
                              Anodes = paste0("DTGOn_", 0), 
                              Cnodes = paste0("censor_", 0),
                              # Lnodes = c(25:34, 38:47, 51:60),
                              Ynodes = paste0("Weight_", 1), 
                              abar            = list(treatment = c(1), control = c(0)), 
                              gbounds         = c(0.025, 0.975),
                              estimate.time   = F)
summ.wt200d.1 <- summary(result.tmle.wt200d.1)

# time 2
result.tmle.wt200d.2 <- ltmle(subData.nomiss.wt200d.sbgrpWt.resPop.scaled %>%
                                select(male:Weight_2), 
                              Anodes = paste0("DTGOn_", 0:1), 
                              Cnodes = paste0("censor_", 0:1),
                              Lnodes = c(paste0(vary.cov.int.used, "_1")),
                              Ynodes = paste0("Weight_", 1:2), 
                              abar            = list(treatment = rep(1, 2), control = rep(0, 2)), 
                              gbounds         = c(0.025, 0.975),
                              estimate.time   = F)
summ.wt200d.2 <- summary(result.tmle.wt200d.2)

# time 3
result.tmle.wt200d.3 <- ltmle(subData.nomiss.wt200d.sbgrpWt.resPop.scaled %>%
                                select(male:Weight_3), 
                              Anodes = paste0("DTGOn_", 0:2), 
                              Cnodes = paste0("censor_", 0:2),
                              Lnodes = c(paste0(vary.cov.int.used, "_1"), paste0(vary.cov.int.used, "_2")),
                              Ynodes = paste0("Weight_", 1:3), 
                              abar            = list(treatment = rep(1, 3), control = rep(0, 3)), 
                              gbounds         = c(0.025, 0.975),
                              estimate.time   = F)
summ.wt200d.3 <- summary(result.tmle.wt200d.3)

# time 4
result.tmle.wt200d.4 <- ltmle(subData.nomiss.wt200d.sbgrpWt.resPop.scaled %>%
                                select(male:Weight_4), 
                              Anodes = paste0("DTGOn_", 0:3), 
                              Cnodes = paste0("censor_", 0:3),
                              Lnodes = c(paste0(vary.cov.int.used, "_1"), paste0(vary.cov.int.used, "_2"), paste0(vary.cov.int.used, "_3")),
                              Ynodes = paste0("Weight_", 1:4), 
                              abar            = list(treatment = rep(1, 4), control = rep(0, 4)), 
                              gbounds         = c(0.025, 0.975),
                              estimate.time   = F)
summ.wt200d.4 <- summary(result.tmle.wt200d.4)

mean(as.matrix(subData.nomiss.wt200d.sbgrpWt.resPop.scaled %>%
                 filter((DTGOn_0 == 0) & (DTGOn_1 == 0) & (DTGOn_2 == 0) & (DTGOn_3 == 0)) %>%
                 select(Weight_4)), na.rm = T)
mean(as.matrix(subData.nomiss.wt200d.sbgrpWt.resPop.scaled %>%
                 filter((DTGOn_0 == 1) & (DTGOn_1 == 1) & (DTGOn_2 == 1) & (DTGOn_3 == 1) & (DTGOn_4 == 1) & (DTGOn_5 == 1) & (DTGOn_6 == 1)) %>%
                 select(Weight_7)), na.rm = T)
sum(!is.na(as.matrix(subData.nomiss.wt200d.sbgrpWt.resPop.scaled %>%
                       filter((DTGOn_0 == 1) & (DTGOn_1 == 1) & (DTGOn_2 == 1) & (DTGOn_3 == 1) & (DTGOn_4 == 1) & (DTGOn_5 == 1) & (DTGOn_6 == 1)) %>%
                       select(Weight_7))), na.rm = T)
mean(as.matrix(subData.nomiss.wt200d.sbgrpWt.resPop.scaled %>%
                 filter((DTGOn_0 == 1) & (DTGOn_1 == 1) & (DTGOn_2 == 1) & (DTGOn_3 == 1) & (DTGOn_4 == 1) & (DTGOn_5 == 1) & (DTGOn_6 == 1) & (DTGOn_7 == 1)) %>%
                 select(Weight_8)), na.rm = T)
n0.wt200d <- c(sum(!is.na(as.matrix(subData.nomiss.wt200d.sbgrpWt.resPop.scaled %>%
                                      filter(DTGOn_0 == 0) %>%
                                      select(Weight_1))), na.rm = T),
               sum(!is.na(as.matrix(subData.nomiss.wt200d.sbgrpWt.resPop.scaled %>%
                                      filter((DTGOn_0 == 0) & (DTGOn_1 == 0)) %>%
                                      select(Weight_2))), na.rm = T),
               sum(!is.na(as.matrix(subData.nomiss.wt200d.sbgrpWt.resPop.scaled %>%
                                      filter((DTGOn_0 == 0) & (DTGOn_1 == 0) & (DTGOn_2 == 0)) %>%
                                      select(Weight_3))), na.rm = T), 
               sum(!is.na(as.matrix(subData.nomiss.wt200d.sbgrpWt.resPop.scaled %>%
                                      filter((DTGOn_0 == 0) & (DTGOn_1 == 0) & (DTGOn_2 == 0) & (DTGOn_3 == 0)) %>%
                                      select(Weight_4))), na.rm = T))
# , 
#                sum(!is.na(as.matrix(finalData.noMiss.t0Jul2016.switchDTG.wt200d %>%
#                                       filter((DTGOn_0 == 0) & (DTGOn_1 == 0) & (DTGOn_2 == 0) & (DTGOn_3 == 0)
#                                              & (DTGOn_4 == 0)) %>%
#                                       select(Weight_5))), na.rm = T), 
#                sum(!is.na(as.matrix(finalData.noMiss.t0Jul2016.switchDTG.wt200d %>%
#                                       filter((DTGOn_0 == 0) & (DTGOn_1 == 0) & (DTGOn_2 == 0) & (DTGOn_3 == 0)
#                                              & (DTGOn_4 == 0) & (DTGOn_5 == 0)) %>%
#                                       select(Weight_6))), na.rm = T), 
#                sum(!is.na(as.matrix(finalData.noMiss.t0Jul2016.switchDTG.wt200d %>%
#                                       filter((DTGOn_0 == 0) & (DTGOn_1 == 0) & (DTGOn_2 == 0) & (DTGOn_3 == 0)
#                                              & (DTGOn_4 == 0) & (DTGOn_5 == 0) & (DTGOn_6 == 0)) %>%
#                                       select(Weight_7))), na.rm = T), 
#                sum(!is.na(as.matrix(finalData.noMiss.t0Jul2016.switchDTG.wt200d %>%
#                                       filter((DTGOn_0 == 0) & (DTGOn_1 == 0) & (DTGOn_2 == 0) & (DTGOn_3 == 0)
#                                              & (DTGOn_4 == 0) & (DTGOn_5 == 0) & (DTGOn_6 == 0) & (DTGOn_7 == 0)) %>%
#                                       select(Weight_8))), na.rm = T))
n1.wt200d <- c(sum(!is.na(as.matrix(subData.nomiss.wt200d.sbgrpWt.resPop.scaled %>%
                                      filter(DTGOn_0 == 1) %>%
                                      select(Weight_1))), na.rm = T),
               sum(!is.na(as.matrix(subData.nomiss.wt200d.sbgrpWt.resPop.scaled %>%
                                      filter((DTGOn_0 == 1) & (DTGOn_1 == 1)) %>%
                                      select(Weight_2))), na.rm = T),
               sum(!is.na(as.matrix(subData.nomiss.wt200d.sbgrpWt.resPop.scaled %>%
                                      filter((DTGOn_0 == 1) & (DTGOn_1 == 1) & (DTGOn_2 == 1)) %>%
                                      select(Weight_3))), na.rm = T), 
               sum(!is.na(as.matrix(subData.nomiss.wt200d.sbgrpWt.resPop.scaled %>%
                                      filter((DTGOn_0 == 1) & (DTGOn_1 == 1) & (DTGOn_2 == 1) & (DTGOn_3 == 1)) %>%
                                      select(Weight_4))), na.rm = T))
# , 
#                sum(!is.na(as.matrix(finalData.noMiss.t0Jul2016.switchDTG.wt200d %>%
#                                       filter((DTGOn_0 == 1) & (DTGOn_1 == 1) & (DTGOn_2 == 1) & (DTGOn_3 == 1)
#                                              & (DTGOn_4 == 1)) %>%
#                                       select(Weight_5))), na.rm = T), 
#                sum(!is.na(as.matrix(finalData.noMiss.t0Jul2016.switchDTG.wt200d %>%
#                                       filter((DTGOn_0 == 1) & (DTGOn_1 == 1) & (DTGOn_2 == 1) & (DTGOn_3 == 1)
#                                              & (DTGOn_4 == 1) & (DTGOn_5 == 1)) %>%
#                                       select(Weight_6))), na.rm = T), 
#                sum(!is.na(as.matrix(finalData.noMiss.t0Jul2016.switchDTG.wt200d %>%
#                                       filter((DTGOn_0 == 1) & (DTGOn_1 == 1) & (DTGOn_2 == 1) & (DTGOn_3 == 1)
#                                              & (DTGOn_4 == 1) & (DTGOn_5 == 1) & (DTGOn_6 == 1)) %>%
#                                       select(Weight_7))), na.rm = T), 
#                sum(!is.na(as.matrix(finalData.noMiss.t0Jul2016.switchDTG.wt200d %>%
#                                       filter((DTGOn_0 == 1) & (DTGOn_1 == 1) & (DTGOn_2 == 1) & (DTGOn_3 == 1)
#                                              & (DTGOn_4 == 1) & (DTGOn_5 == 1) & (DTGOn_6 == 1) & (DTGOn_7 == 1)) %>%
#                                       select(Weight_8))), na.rm = T))

iters.boot <- 10^4
set.seed(a[333])

nrow.subData.nomiss.wt200d.sbgrpWt.resPop.scaled <- nrow(subData.nomiss.wt200d.sbgrpWt.resPop.scaled)

df.eff.wt200d <- 
  foreach(iters.boot.i = 1:iters.boot,
          .combine   = rbind) %dopar% {
            
            tmp.subData.nomiss.wt200d.sbgrpWt.resPop.scaled <- subData.nomiss.wt200d.sbgrpWt.resPop.scaled[sample(1:nrow.subData.nomiss.wt200d.sbgrpWt.resPop.scaled, 
                                                                                                                                nrow.subData.nomiss.wt200d.sbgrpWt.resPop.scaled, 
                                                                                                                                replace = TRUE), ]
            
            tmp.result.tmle.wt200d.1 <- ltmle(tmp.subData.nomiss.wt200d.sbgrpWt.resPop.scaled %>%
                                            select(male:Weight_1), 
                                          Anodes = paste0("DTGOn_", 0), 
                                          Cnodes = paste0("censor_", 0),
                                          # Lnodes = c(25:34, 38:47, 51:60),
                                          Ynodes = paste0("Weight_", 1), 
                                          abar            = list(treatment = c(1), control = c(0)), 
                                          gbounds         = c(0.025, 0.975),
                                          estimate.time   = F)
            tmp.summ.wt200d.1 <- summary(tmp.result.tmle.wt200d.1)
            
            # time 2
            tmp.result.tmle.wt200d.2 <- ltmle(tmp.subData.nomiss.wt200d.sbgrpWt.resPop.scaled %>%
                                            select(male:Weight_2), 
                                          Anodes = paste0("DTGOn_", 0:1), 
                                          Cnodes = paste0("censor_", 0:1),
                                          Lnodes = c(paste0(vary.cov.int.used, "_1")),
                                          Ynodes = paste0("Weight_", 1:2), 
                                          abar            = list(treatment = rep(1, 2), control = rep(0, 2)), 
                                          gbounds         = c(0.025, 0.975),
                                          estimate.time   = F)
            tmp.summ.wt200d.2 <- summary(tmp.result.tmle.wt200d.2)
            
            # time 3
            tmp.result.tmle.wt200d.3 <- ltmle(tmp.subData.nomiss.wt200d.sbgrpWt.resPop.scaled %>%
                                            select(male:Weight_3), 
                                          Anodes = paste0("DTGOn_", 0:2), 
                                          Cnodes = paste0("censor_", 0:2),
                                          Lnodes = c(paste0(vary.cov.int.used, "_1"), paste0(vary.cov.int.used, "_2")),
                                          Ynodes = paste0("Weight_", 1:3), 
                                          abar            = list(treatment = rep(1, 3), control = rep(0, 3)), 
                                          gbounds         = c(0.025, 0.975),
                                          estimate.time   = F)
            tmp.summ.wt200d.3 <- summary(tmp.result.tmle.wt200d.3)
            
            # time 4
            tmp.result.tmle.wt200d.4 <- ltmle(tmp.subData.nomiss.wt200d.sbgrpWt.resPop.scaled %>%
                                            select(male:Weight_4), 
                                          Anodes = paste0("DTGOn_", 0:3), 
                                          Cnodes = paste0("censor_", 0:3),
                                          Lnodes = c(paste0(vary.cov.int.used, "_1"), paste0(vary.cov.int.used, "_2"), paste0(vary.cov.int.used, "_3")),
                                          Ynodes = paste0("Weight_", 1:4), 
                                          abar            = list(treatment = rep(1, 4), control = rep(0, 4)), 
                                          gbounds         = c(0.025, 0.975),
                                          estimate.time   = F)
            tmp.summ.wt200d.4 <- summary(tmp.result.tmle.wt200d.4)
            
            tmp.res <- c(tmp.summ.wt200d.1$effect.measures$treatment$estimate, tmp.summ.wt200d.1$effect.measures$control$estimate, tmp.summ.wt200d.1$effect.measures$ATE$estimate,
                         tmp.summ.wt200d.2$effect.measures$treatment$estimate, tmp.summ.wt200d.2$effect.measures$control$estimate, tmp.summ.wt200d.2$effect.measures$ATE$estimate,
                         tmp.summ.wt200d.3$effect.measures$treatment$estimate, tmp.summ.wt200d.3$effect.measures$control$estimate, tmp.summ.wt200d.3$effect.measures$ATE$estimate,
                         tmp.summ.wt200d.4$effect.measures$treatment$estimate, tmp.summ.wt200d.4$effect.measures$control$estimate, tmp.summ.wt200d.4$effect.measures$ATE$estimate)

            tmp.res

          }

# save.image("IntermediateData.RData")
# load("IntermediateData.RData")
summ.eff.ci.wt200d <- apply(df.eff.wt200d, 2, quantile, probs = c(0.025, 0.975))

summ.wt200d.all <- rbind(c(n1.wt200d[1], summ.wt200d.1$effect.measures$treatment$estimate, summ.eff.ci.wt200d[, 1], 
                           n0.wt200d[1], summ.wt200d.1$effect.measures$control$estimate, summ.eff.ci.wt200d[, 2],
                           summ.wt200d.1$effect.measures$ATE$estimate, summ.eff.ci.wt200d[, 3]),
                         c(n1.wt200d[2], summ.wt200d.2$effect.measures$treatment$estimate, summ.eff.ci.wt200d[, 4], 
                           n0.wt200d[2], summ.wt200d.2$effect.measures$control$estimate, summ.eff.ci.wt200d[, 5],
                           summ.wt200d.2$effect.measures$ATE$estimate, summ.eff.ci.wt200d[, 6]),
                         c(n1.wt200d[3], summ.wt200d.3$effect.measures$treatment$estimate, summ.eff.ci.wt200d[, 7], 
                           n0.wt200d[3], summ.wt200d.3$effect.measures$control$estimate, summ.eff.ci.wt200d[, 8],
                           summ.wt200d.3$effect.measures$ATE$estimate, summ.eff.ci.wt200d[, 9]),
                         c(n1.wt200d[4], summ.wt200d.4$effect.measures$treatment$estimate, summ.eff.ci.wt200d[, 10], 
                           n0.wt200d[4], summ.wt200d.4$effect.measures$control$estimate, summ.eff.ci.wt200d[, 11],
                           summ.wt200d.4$effect.measures$ATE$estimate, summ.eff.ci.wt200d[, 12]))
# c(n1.wt200d[5], summ.wt200d.5$effect.measures$treatment$estimate, summ.wt200d.5$effect.measures$treatment$CI, 
#   n0.wt200d[5], summ.wt200d.5$effect.measures$control$estimate, summ.wt200d.5$effect.measures$control$CI,
#   summ.wt200d.5$effect.measures$ATE$estimate, summ.wt200d.5$effect.measures$ATE$CI),
# c(n1.wt200d[6], summ.wt200d.6$effect.measures$treatment$estimate, summ.wt200d.6$effect.measures$treatment$CI, 
#   n0.wt200d[6], summ.wt200d.6$effect.measures$control$estimate, summ.wt200d.6$effect.measures$control$CI,
#   summ.wt200d.6$effect.measures$ATE$estimate, summ.wt200d.6$effect.measures$ATE$CI),
# c(n1.wt200d[7], summ.wt200d.7$effect.measures$treatment$estimate, summ.wt200d.7$effect.measures$treatment$CI, 
#   n0.wt200d[7], summ.wt200d.7$effect.measures$control$estimate, summ.wt200d.7$effect.measures$control$CI,
#   summ.wt200d.7$effect.measures$ATE$estimate, summ.wt200d.7$effect.measures$ATE$CI),
# c(n1.wt200d[8], summ.wt200d.8$effect.measures$treatment$estimate, summ.wt200d.8$effect.measures$treatment$CI, 
#   n0.wt200d[8], summ.wt200d.8$effect.measures$control$estimate, summ.wt200d.8$effect.measures$control$CI,
#   summ.wt200d.8$effect.measures$ATE$estimate, summ.wt200d.8$effect.measures$ATE$CI))
summ.wt200d.all <- data.frame(summ.wt200d.all)
colnames(summ.wt200d.all) <- c("n_trt", "trt", "trt_025", "trt_975",
                               "n_ctrl", "ctrl", "ctrl_025", "ctrl_975",
                               "ate", "ate_025", "ate_975")

summ.wt200d.all <- summ.wt200d.all %>%
  mutate(n_all = apply(subData.nomiss.wt200d.sbgrpWt.resPop.scaled %>%
  select(starts_with("censor_")), 2, function(x) sum(!is.na(x)))[2:5])

summ.wt200d.all <- data.frame(summ.wt200d.all[, ncol(summ.wt200d.all)],
                         summ.wt200d.all[, 1:(ncol(summ.wt200d.all)-1)])
colnames(summ.wt200d.all)[1] <- "n_all"

library(xtable)
latex.summ.wt200d.all <- summ.wt200d.all %>%
  mutate(trt      = round(trt, 2),
         trt_025  = round(trt_025, 2),
         trt_975  = round(trt_975, 2),
         ctrl     = round(ctrl, 2),
         ctrl_025 = round(ctrl_025, 2),
         ctrl_975 = round(ctrl_975, 2),
         ate      = round(ate, 2), 
         ate_025  = round(ate_025, 2),
         ate_975  = round(ate_975, 2))
latex.summ.wt200d.all <- latex.summ.wt200d.all %>%
  mutate(summ_trt = paste0(trt, " (", trt_025, ", ", trt_975, ")"), 
         summ_ctrl = paste0(ctrl, " (", ctrl_025, ", ", ctrl_975, ")"), 
         summ_ate  = paste0(ate, " (", ate_025, ", ", ate_975, ")")) %>%
  select(n_all, n_trt, summ_trt, n_ctrl, summ_ctrl, summ_ate)
print(xtable(latex.summ.wt200d.all), include.rownames = FALSE)

save(n1.wt200d, n0.wt200d, 
     summ.wt200d.1, summ.wt200d.2, summ.wt200d.3, summ.wt200d.4, 
     df.eff.wt200d, summ.wt200d.all, latex.summ.wt200d.all,
     file = "../../../Data/Data20210512/Processed/WtAvgTrtEff200dCi20230331.RData")

load("../../../Data/Data20210512/Processed/WtAvgTrtEff200dCi20230331.RData")
summ.wt200d.avgTrjctr <- data.frame(time = rep(1:4, 2),
                                    grp  = rep(c("trt", "ctrl"), each = 4), 
                                    avg  = c(summ.wt200d.all$trt, summ.wt200d.all$ctrl),
                                    ci_025 = c(summ.wt200d.all$trt_025, summ.wt200d.all$ctrl_025),
                                    ci_975 = c(summ.wt200d.all$trt_975, summ.wt200d.all$ctrl_975))
summ.wt200d.diffTrjctr <- data.frame(time = 1:4,
                                     avg  = summ.wt200d.all$ate,
                                     ci_025 = summ.wt200d.all$ate_025,
                                     ci_975 = summ.wt200d.all$ate_975)
save(summ.wt200d.avgTrjctr, summ.wt200d.diffTrjctr,
     file = "../../../Data/Data20210512/Processed/WtAvgTrtEff200dCiTrjctr20230331.RData")

# Run locally
library(ggplot2)
library(gridExtra)
library(dplyr)
load("../../../Data/Processed/WtAvgTrtEff200dCiTrjctr20230331.RData")
summ.wt200d.avgTrjctr <- summ.wt200d.avgTrjctr %>%
  mutate(days = ifelse(time == 1, 200 * 1, NA)) %>%
  mutate(days = ifelse(time == 2, 200 * 2, days)) %>%
  mutate(days = ifelse(time == 3, 200 * 3, days)) %>%
  mutate(days = ifelse(time == 4, 200 * 4, days))
summ.wt200d.diffTrjctr <- summ.wt200d.diffTrjctr %>%
  mutate(days = ifelse(time == 1, 200 * 1, NA)) %>%
  mutate(days = ifelse(time == 2, 200 * 2, days)) %>%
  mutate(days = ifelse(time == 3, 200 * 3, days)) %>%
  mutate(days = ifelse(time == 4, 200 * 4, days))
p3 <- ggplot(summ.wt200d.avgTrjctr, aes(x = days, y = avg, group = grp, color = grp)) + 
  geom_point() + 
  # position = position_dodge(width = 0.2)
  geom_errorbar(aes(ymin = ci_025, ymax = ci_975), width = 30) +
  ylim(61.5, 65) +
  labs(x = "Day", y = "Estimated weight (kg)") + 
  scale_color_manual(breaks = c("trt", "ctrl"),
                       labels = c("Always DTG", "Never DTG"),
                       values = c("black", "darkgray")) +
  theme_bw() + 
  theme(legend.title=element_blank(),
        # legend.position = "bottom",
        legend.margin   = margin(t = -7, r = 0, b = 0, l = 0, unit = "pt"))

p4 <- ggplot(summ.wt200d.diffTrjctr, aes(x = days, y = avg)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = ci_025, ymax = ci_975), width = 30) + 
  ylim(0, 3.5) +
  labs(x = "Day", y = "Estimated DTG effect (kg)") +
  theme_bw()

pdf("../../../Report/Manuscript20230402/WtAvgTrtEff200dCiTrjctr20230402.pdf", width = 7, height = 3.5)
grid.arrange(p3, p4, nrow = 1, widths = c(1.5, 1))
dev.off()

# tiff("../../../Report/CROI//WtAvgTrtEff200dCiTrjctr20230331.tiff", units="in", width=6.5, height=2.7, res=300)
# grid.arrange(p3, p4, nrow = 1)
# dev.off()
