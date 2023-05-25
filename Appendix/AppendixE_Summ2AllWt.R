library(dplyr) 
library(tidyr)
library(doParallel)
library(table1)

# sinfo --long --Node -o"%N %.11T %C"
# srun --nodelist=compute002 --pty bash -i
# srun --pty bash -i
# registerDoParallel(cores=25)
# registerDoParallel(cores=(Sys.getenv("SLURM_NTASKS_PER_NODE")))
# registerDoParallel(cores=Sys.getenv("SLURM_CPUS_PER_TASK"))
# load("IntermediateData.RData")

load("../../Data/Data20210512/Processed/Summ1IncssttCarryOverV2OrigDataWt20220328.RData")

############################################################
# Create summary graphs for variables in the final dataset #
############################################################
head(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop)

# finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop <- finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop %>%
#   mutate(arvstart_dob  = as.numeric(arvstart_dob), 
#          enroldate_dob = as.numeric(enroldate_dob))
# finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop <- finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop %>%
#   mutate(arvstart_dob  = as.numeric(arvstart_dob), 
#          enroldate_dob = as.numeric(enroldate_dob))
# finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop <- finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop %>%
#   mutate(arvstart_dob  = as.numeric(arvstart_dob), 
#          enroldate_dob = as.numeric(enroldate_dob))

# # add in age in days at time 0 and whether or not time 0 is on or after 20160701
# tmp.demog <- ampath.visit.wt400d.resPop[!duplicated(ampath.visit.wt400d.resPop[, c("patient_id", "dob", "arvstart", "enroldate", "time0")]), 
#                                          c("patient_id", "dob", "arvstart", "enroldate", "time0")]
# dim(tmp.demog)
# tmp.demog <- tmp.demog %>%
#   mutate(time0_dob           = as.numeric(time0 - dob),
#          enroldate_arvstart  = as.numeric(enroldate - arvstart),
#          time0_enroldate     = as.numeric(time0 - enroldate), 
#          time0_after20160701 = (time0 >= as.Date("2016-07-01")))
# finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop <- finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop %>%
#   full_join(tmp.demog, by = "patient_id")
# dim(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop)

# tmp.demog <- ampath.visit.wt200d.resPop[!duplicated(ampath.visit.wt200d.resPop[, c("patient_id", "dob", "arvstart", "enroldate", "time0")]), 
#                                          c("patient_id", "dob", "arvstart", "enroldate", "time0")]
# dim(tmp.demog)
# tmp.demog <- tmp.demog %>%
#   mutate(time0_dob           = as.numeric(time0 - dob),
#          enroldate_arvstart  = as.numeric(enroldate - arvstart),
#          time0_enroldate     = as.numeric(time0 - enroldate), 
#          time0_after20160701 = (time0 >= as.Date("2016-07-01")))
# finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop <- finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop %>%
#   full_join(tmp.demog, by = "patient_id")
# dim(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop)

# tmp.demog <- ampath.visit.wt100d.resPop[!duplicated(ampath.visit.wt100d.resPop[, c("patient_id", "dob", "arvstart", "enroldate", "time0")]), 
#                                          c("patient_id", "dob", "arvstart", "enroldate", "time0")]
# dim(tmp.demog)
# tmp.demog <- tmp.demog %>%
#   mutate(time0_dob           = as.numeric(time0 - dob),
#          enroldate_arvstart  = as.numeric(enroldate - arvstart),
#          time0_enroldate     = as.numeric(time0 - enroldate), 
#          time0_after20160701 = (time0 >= as.Date("2016-07-01")))
# finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop <- finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop %>%
#   full_join(tmp.demog, by = "patient_id")
# dim(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop)

# finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop <- finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop %>%
#   select(patient_id, male, arvstart_dob, enroldate_dob, time0_dob,
#          enroldate_arvstart, time0_arvstart, time0_enroldate, enroll_after20160701, time0_after20160701,
#          EmployedOutsideHome:Weight_4)
# finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop <- finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop %>%
#   select(patient_id, male, arvstart_dob, enroldate_dob, time0_dob,
#          enroldate_arvstart, time0_arvstart, time0_enroldate, enroll_after20160701, time0_after20160701,
#          EmployedOutsideHome:Weight_8)
# finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop <- finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop %>%
#   select(patient_id, male, arvstart_dob, enroldate_dob, time0_dob,
#          enroldate_arvstart, time0_arvstart, time0_enroldate, enroll_after20160701, time0_after20160701,
#          EmployedOutsideHome:Weight_16)

# Add log10 scale viral load, need to add a small amount
summary(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$viralload_0)
min(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$viralload_0[finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$viralload_0 > 0], na.rm = T)
# minimum value greater than 0 is 0.01, thus add 0.001
finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop <- finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop %>%
  mutate(log10Viralload_0 = log10(viralload_0 + 0.001),
         log10Viralload_1 = log10(viralload_1 + 0.001),
         log10Viralload_2 = log10(viralload_2 + 0.001),
         log10Viralload_3 = log10(viralload_3 + 0.001))
finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop <- finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop %>%
  select(patient_id:viralload_0, log10Viralload_0, SystolicBP_0:viralload_1, log10Viralload_1,
         SystolicBP_1:viralload_2, log10Viralload_2, SystolicBP_2:viralload_3, log10Viralload_3,
         SystolicBP_3:Weight_4)

finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop <- finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop %>%
  mutate(log10Viralload_0 = log10(viralload_0 + 0.001),
         log10Viralload_1 = log10(viralload_1 + 0.001),
         log10Viralload_2 = log10(viralload_2 + 0.001),
         log10Viralload_3 = log10(viralload_3 + 0.001),
         log10Viralload_4 = log10(viralload_4 + 0.001),
         log10Viralload_5 = log10(viralload_5 + 0.001),
         log10Viralload_6 = log10(viralload_6 + 0.001),
         log10Viralload_7 = log10(viralload_7 + 0.001))
finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop <- finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop %>%
  select(patient_id:viralload_0, log10Viralload_0, SystolicBP_0:viralload_1, log10Viralload_1,
         SystolicBP_1:viralload_2, log10Viralload_2, SystolicBP_2:viralload_3, log10Viralload_3,
         SystolicBP_3:viralload_4, log10Viralload_4, SystolicBP_4:viralload_5, log10Viralload_5,
         SystolicBP_5:viralload_6, log10Viralload_6, SystolicBP_6:viralload_7, log10Viralload_7,
         SystolicBP_7:Weight_8)

finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop <- finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop %>%
  mutate(log10Viralload_0 = log10(viralload_0 + 0.001),
         log10Viralload_1 = log10(viralload_1 + 0.001),
         log10Viralload_2 = log10(viralload_2 + 0.001),
         log10Viralload_3 = log10(viralload_3 + 0.001),
         log10Viralload_4 = log10(viralload_4 + 0.001),
         log10Viralload_5 = log10(viralload_5 + 0.001),
         log10Viralload_6 = log10(viralload_6 + 0.001),
         log10Viralload_7 = log10(viralload_7 + 0.001),
         log10Viralload_8 = log10(viralload_8 + 0.001),
         log10Viralload_9 = log10(viralload_9 + 0.001),
         log10Viralload_10 = log10(viralload_10 + 0.001),
         log10Viralload_11 = log10(viralload_11 + 0.001),
         log10Viralload_12 = log10(viralload_12 + 0.001),
         log10Viralload_13 = log10(viralload_13 + 0.001),
         log10Viralload_14 = log10(viralload_14 + 0.001),
         log10Viralload_15 = log10(viralload_15 + 0.001))
finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop <- finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop %>%
  select(patient_id:viralload_0, log10Viralload_0, SystolicBP_0:viralload_1, log10Viralload_1,
         SystolicBP_1:viralload_2, log10Viralload_2, SystolicBP_2:viralload_3, log10Viralload_3,
         SystolicBP_3:viralload_4, log10Viralload_4, SystolicBP_4:viralload_5, log10Viralload_5,
         SystolicBP_5:viralload_6, log10Viralload_6, SystolicBP_6:viralload_7, log10Viralload_7,
         SystolicBP_7:viralload_8, log10Viralload_8, SystolicBP_8:viralload_9, log10Viralload_9,
         SystolicBP_9:viralload_10, log10Viralload_10, SystolicBP_10:viralload_11, log10Viralload_11,
         SystolicBP_11:viralload_12, log10Viralload_12, SystolicBP_12:viralload_13, log10Viralload_13,
         SystolicBP_13:viralload_14, log10Viralload_14, SystolicBP_14:viralload_15, log10Viralload_15,
         SystolicBP_15:Weight_16)



#############################################################
# Create a complete dataset for treatment effect estimation #
#############################################################
head(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop)
dim(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop)
dim(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop)
dim(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop)
apply(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop %>% select(male:WaterPipedInHome), 2, function(x) sum(is.na(x)))
apply(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop %>% select(male:WaterPipedInHome), 2, function(x) mean(is.na(x)))
apply(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop %>% select(male:WaterPipedInHome), 2, function(x) sum(is.na(x)))
apply(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop %>% select(male:WaterPipedInHome), 2, function(x) mean(is.na(x)))
apply(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop %>% select(male:WaterPipedInHome), 2, function(x) sum(is.na(x)))
apply(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop %>% select(male:WaterPipedInHome), 2, function(x) mean(is.na(x)))

# remove time 0 after 20160701 which is always true
# remove the 3 demographic variables with ~1/3 missing
finalData.noMiss.t0Jul2016.switchDTG.wt400d <- finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop %>%
  select(-time0_after20160701, -(EmployedOutsideHome:WaterPipedInHome))
dim(finalData.noMiss.t0Jul2016.switchDTG.wt400d)
finalData.noMiss.t0Jul2016.switchDTG.wt200d <- finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop %>%
  select(-time0_after20160701, -(EmployedOutsideHome:WaterPipedInHome))
dim(finalData.noMiss.t0Jul2016.switchDTG.wt200d)
finalData.noMiss.t0Jul2016.switchDTG.wt100d <- finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop %>%
  select(-time0_after20160701, -(EmployedOutsideHome:WaterPipedInHome))
dim(finalData.noMiss.t0Jul2016.switchDTG.wt100d)

# Censoring status
length.pt.ids.wt400d.resPop <- nrow(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop)
length.pt.ids.wt200d.resPop <- nrow(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop)
length.pt.ids.wt100d.resPop <- nrow(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop)

summCensor.wizmiss.t0Jul2016.switchDTG.wt400d.resPop <- rbind(apply(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop %>% select(censor_0, censor_1, censor_2, censor_3), 2, function(x) sum(x == 1, na.rm = T)),
                                                              cumsum(apply(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop %>% select(censor_0, censor_1, censor_2, censor_3), 2, function(x) sum(x == 1, na.rm = T))))
summCensor.wizmiss.t0Jul2016.switchDTG.wt400d.resPop <- data.frame(type = c("increment", "cumulative"),
                                                                   summCensor.wizmiss.t0Jul2016.switchDTG.wt400d.resPop)
colnames(summCensor.wizmiss.t0Jul2016.switchDTG.wt400d.resPop) <- c("type", paste0("n_", 0:(maxT.wizmiss.t0Jul2016.switchDTG.wt400d-2)))
summCensor.wizmiss.t0Jul2016.switchDTG.wt400d.resPop <- summCensor.wizmiss.t0Jul2016.switchDTG.wt400d.resPop %>%
  mutate(n_4 = NA)
tmp <- data.frame(type = "left",
                  n_0 = length.pt.ids.wt400d.resPop, 
                  data.frame(t(nrow(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop) - cumsum(apply(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop %>% select(censor_0, censor_1, censor_2, censor_3), 2, function(x) sum(x == 1, na.rm = T))))))
colnames(tmp) <- c("type", paste0("n_", 0:4))
summCensor.wizmiss.t0Jul2016.switchDTG.wt400d.resPop <- rbind(summCensor.wizmiss.t0Jul2016.switchDTG.wt400d.resPop,
                                                              tmp)
summCensor.wizmiss.t0Jul2016.switchDTG.wt400d.resPop <- summCensor.wizmiss.t0Jul2016.switchDTG.wt400d.resPop %>%
  mutate(p_0 = n_0 / length.pt.ids.wt400d.resPop,
         p_1 = n_1 / length.pt.ids.wt400d.resPop,
         p_2 = n_2 / length.pt.ids.wt400d.resPop,
         p_3 = n_3 / length.pt.ids.wt400d.resPop,
         p_4 = n_4 / length.pt.ids.wt400d.resPop)
summCensor.wizmiss.t0Jul2016.switchDTG.wt400d.resPop <- summCensor.wizmiss.t0Jul2016.switchDTG.wt400d.resPop %>%
  select(type, n_0, p_0, n_1, p_1, n_2, p_2, n_3, p_3, n_4, p_4)

summCensor.wizmiss.t0Jul2016.switchDTG.wt200d.resPop <- rbind(apply(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop %>% select(starts_with("censor_")), 2, function(x) sum(x == 1, na.rm = T)),
                                                              cumsum(apply(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop %>% select(starts_with("censor_")), 2, function(x) sum(x == 1, na.rm = T))))
summCensor.wizmiss.t0Jul2016.switchDTG.wt200d.resPop <- data.frame(type = c("increment", "cumulative"),
                                                                   summCensor.wizmiss.t0Jul2016.switchDTG.wt200d.resPop)
colnames(summCensor.wizmiss.t0Jul2016.switchDTG.wt200d.resPop) <- c("type", paste0("n_", 0:(maxT.wizmiss.t0Jul2016.switchDTG.wt200d-2)))
summCensor.wizmiss.t0Jul2016.switchDTG.wt200d.resPop <- summCensor.wizmiss.t0Jul2016.switchDTG.wt200d.resPop %>%
  mutate(n_8 = NA)
tmp <- data.frame(type = "left",
                  n_0 = length.pt.ids.wt200d.resPop, 
                  data.frame(t(nrow(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop) - cumsum(apply(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop %>% select(starts_with("censor_")), 2, function(x) sum(x == 1, na.rm = T))))))
colnames(tmp) <- c("type", paste0("n_", 0:(maxT.wizmiss.t0Jul2016.switchDTG.wt200d - 1)))
summCensor.wizmiss.t0Jul2016.switchDTG.wt200d.resPop <- rbind(summCensor.wizmiss.t0Jul2016.switchDTG.wt200d.resPop,
                                                              tmp)
summCensor.wizmiss.t0Jul2016.switchDTG.wt200d.resPop <- summCensor.wizmiss.t0Jul2016.switchDTG.wt200d.resPop %>%
  mutate(p_0 = n_0 / length.pt.ids.wt200d.resPop,
         p_1 = n_1 / length.pt.ids.wt200d.resPop,
         p_2 = n_2 / length.pt.ids.wt200d.resPop,
         p_3 = n_3 / length.pt.ids.wt200d.resPop,
         p_4 = n_4 / length.pt.ids.wt200d.resPop,
         p_5 = n_5 / length.pt.ids.wt200d.resPop,
         p_6 = n_6 / length.pt.ids.wt200d.resPop,
         p_7 = n_7 / length.pt.ids.wt200d.resPop,
         p_8 = n_8 / length.pt.ids.wt200d.resPop)
summCensor.wizmiss.t0Jul2016.switchDTG.wt200d.resPop <- summCensor.wizmiss.t0Jul2016.switchDTG.wt200d.resPop %>%
  select(type, n_0, p_0, n_1, p_1, n_2, p_2, n_3, p_3, n_4, p_4, n_5, p_5, n_6, p_6, n_7, p_7, n_8, p_8)

summCensor.wizmiss.t0Jul2016.switchDTG.wt100d.resPop <- rbind(apply(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop %>% select(starts_with("censor_")), 2, function(x) sum(x == 1, na.rm = T)),
                                                              cumsum(apply(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop %>% select(starts_with("censor_")), 2, function(x) sum(x == 1, na.rm = T))))
summCensor.wizmiss.t0Jul2016.switchDTG.wt100d.resPop <- data.frame(type = c("increment", "cumulative"),
                                                                   summCensor.wizmiss.t0Jul2016.switchDTG.wt100d.resPop)
colnames(summCensor.wizmiss.t0Jul2016.switchDTG.wt100d.resPop) <- c("type", paste0("n_", 0:(maxT.wizmiss.t0Jul2016.switchDTG.wt100d-2)))
summCensor.wizmiss.t0Jul2016.switchDTG.wt100d.resPop <- summCensor.wizmiss.t0Jul2016.switchDTG.wt100d.resPop %>%
  mutate(n_16 = NA)
tmp <- data.frame(type = "left",
                  n_0 = length.pt.ids.wt100d.resPop, 
                  data.frame(t(nrow(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop) - cumsum(apply(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop %>% select(starts_with("censor_")), 2, function(x) sum(x == 1, na.rm = T))))))
colnames(tmp) <- c("type", paste0("n_", 0:(maxT.wizmiss.t0Jul2016.switchDTG.wt100d - 1)))
summCensor.wizmiss.t0Jul2016.switchDTG.wt100d.resPop <- rbind(summCensor.wizmiss.t0Jul2016.switchDTG.wt100d.resPop,
                                                              tmp)
summCensor.wizmiss.t0Jul2016.switchDTG.wt100d.resPop <- summCensor.wizmiss.t0Jul2016.switchDTG.wt100d.resPop %>%
  mutate(p_0 = n_0 / length.pt.ids.wt100d.resPop,
         p_1 = n_1 / length.pt.ids.wt100d.resPop,
         p_2 = n_2 / length.pt.ids.wt100d.resPop,
         p_3 = n_3 / length.pt.ids.wt100d.resPop,
         p_4 = n_4 / length.pt.ids.wt100d.resPop,
         p_5 = n_5 / length.pt.ids.wt100d.resPop,
         p_6 = n_6 / length.pt.ids.wt100d.resPop,
         p_7 = n_7 / length.pt.ids.wt100d.resPop,
         p_8 = n_8 / length.pt.ids.wt100d.resPop,
         p_9 = n_9 / length.pt.ids.wt100d.resPop,
         p_10 = n_10 / length.pt.ids.wt100d.resPop,
         p_11 = n_11 / length.pt.ids.wt100d.resPop,
         p_12 = n_12 / length.pt.ids.wt100d.resPop,
         p_13 = n_13 / length.pt.ids.wt100d.resPop,
         p_14 = n_14 / length.pt.ids.wt100d.resPop,
         p_15 = n_15 / length.pt.ids.wt100d.resPop,
         p_16 = n_16 / length.pt.ids.wt100d.resPop)
summCensor.wizmiss.t0Jul2016.switchDTG.wt100d.resPop <- summCensor.wizmiss.t0Jul2016.switchDTG.wt100d.resPop %>%
  select(type, n_0, p_0, n_1, p_1, n_2, p_2, n_3, p_3, n_4, p_4, n_5, p_5, n_6, p_6, n_7, p_7, n_8, p_8,
         n_9, p_9, n_10, p_10, n_11, p_11, n_12, p_12, n_13, p_13, n_14, p_14, n_15, p_15, n_16, p_16)

apply(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop %>% select(Weight_0:censor_0), 2, function(x) mean(is.na(x)))
apply(finalData.noMiss.t0Jul2016.switchDTG.wt400d %>% select(Weight_0:censor_0), 2, function(x) mean(is.na(x)))

# remove cd4 with >90% missing
summDrop.wizmiss.t0Jul2016.switchDTG.wt400d.resPop <- data.frame(covariate = "cd4",
                                                                 n_0 = sum(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$cd4_0)),
                                                                 p_0 = mean(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$cd4_0)), 
                                                                 n_1 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$censor_0 == 0) & 
                                                                             is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$cd4_1)),
                                                                 n_2 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$censor_1 == 0) & 
                                                                             is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$cd4_2), na.rm = T),
                                                                 n_3 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$censor_2 == 0) & 
                                                                             is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$cd4_3), na.rm = T))
summLeft.wizmiss.t0Jul2016.switchDTG.wt400d.resPop <- summCensor.wizmiss.t0Jul2016.switchDTG.wt400d.resPop %>%
  filter(type == "left")
summDrop.wizmiss.t0Jul2016.switchDTG.wt400d.resPop <- summDrop.wizmiss.t0Jul2016.switchDTG.wt400d.resPop %>%
  mutate(p_1 = n_1 / summLeft.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$n_1,
         p_2 = n_2 / summLeft.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$n_2,
         p_3 = n_3 / summLeft.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$n_3)
summDrop.wizmiss.t0Jul2016.switchDTG.wt400d.resPop <- summDrop.wizmiss.t0Jul2016.switchDTG.wt400d.resPop %>%
  select(covariate:n_1, p_1, n_2, p_2, n_3, p_3)
finalData.noMiss.t0Jul2016.switchDTG.wt400d <- finalData.noMiss.t0Jul2016.switchDTG.wt400d %>%
  select(-cd4_0, -cd4_1, -cd4_2, -cd4_3)

# viralload
mean(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$viralload_0), na.rm = T)
summary(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$viralload_0)
summary(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$viralload_1)
summary(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$viralload_2)
summary(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$viralload_3)
apply(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop %>% select(starts_with("viralload")), 2, function(x) mean(is.na(x)))
# impute with average
tmp.summImpute <- data.frame(covariate      = "Viral load",
                             n_miss_0       = sum(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$viralload_0)),
                             p_miss_0       = mean(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$viralload_0)),
                             imputed_mean_0 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$viralload_0, na.rm = T),
                             n_miss_1       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$censor_0 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$viralload_1)),
                             p_miss_1       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$censor_0 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$viralload_1)) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$n_1,
                             imputed_mean_1 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$viralload_1, na.rm = T),
                             n_miss_2       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$censor_1 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$viralload_2), na.rm = T),
                             p_miss_2       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$censor_1 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$viralload_2), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$n_2, 
                             imputed_mean_2 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$viralload_2, na.rm = T),
                             n_miss_3       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$censor_2 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$viralload_3), na.rm = T),
                             p_miss_3       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$censor_2 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$viralload_3), na.rm = T) /
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$n_3,
                             imputed_mean_3 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$viralload_3, na.rm = T))
summImpute.wizmiss.t0Jul2016.switchDTG.wt400d.resPop <- tmp.summImpute
finalData.noMiss.t0Jul2016.switchDTG.wt400d <- finalData.noMiss.t0Jul2016.switchDTG.wt400d %>%
  mutate(viralloadImptMn_0 = ifelse(is.na(viralload_0), tmp.summImpute$imputed_mean_0, viralload_0)) %>%
  mutate(viralloadImptMn_1 = ifelse((is.na(viralload_1)) & (censor_0 == 0), tmp.summImpute$imputed_mean_1, viralload_1)) %>%
  mutate(viralloadImptMn_2 = ifelse((is.na(viralload_2)) & (censor_1 == 0), tmp.summImpute$imputed_mean_2, viralload_2)) %>%
  mutate(viralloadImptMn_3 = ifelse((is.na(viralload_3)) & (censor_2 == 0), tmp.summImpute$imputed_mean_3, viralload_3))
finalData.noMiss.t0Jul2016.switchDTG.wt400d <- finalData.noMiss.t0Jul2016.switchDTG.wt400d %>%
  mutate(tmpViralload_0 = ifelse(is.na(viralload_0), -1, viralload_0),
         tmpViralload_1 = ifelse(is.na(viralload_1) & (censor_0 == 0), -1, viralload_1),
         tmpViralload_2 = ifelse(is.na(viralload_2) & (censor_1 == 0), -1, viralload_2),
         tmpViralload_3 = ifelse(is.na(viralload_3) & (censor_2 == 0), -1, viralload_3)) 
finalData.noMiss.t0Jul2016.switchDTG.wt400d <- finalData.noMiss.t0Jul2016.switchDTG.wt400d %>%
  mutate(viralloadImptCtgr_0 = ifelse((tmpViralload_0 == -1), 1, NA)) %>%
  mutate(viralloadImptCtgr_0 = ifelse((tmpViralload_0 > -1) & (tmpViralload_0 < 10^3), 2, viralloadImptCtgr_0)) %>%
  mutate(viralloadImptCtgr_0 = ifelse((tmpViralload_0 >= 10^3) & (tmpViralload_0 < 10^4), 3, viralloadImptCtgr_0)) %>%
  mutate(viralloadImptCtgr_0 = ifelse((tmpViralload_0 >= 10^4), 4, viralloadImptCtgr_0)) %>%
  mutate(viralloadImptCtgr_1 = ifelse((tmpViralload_1 == -1), 1, NA)) %>%
  mutate(viralloadImptCtgr_1 = ifelse((tmpViralload_1 > -1) & (tmpViralload_1 < 10^3), 2, viralloadImptCtgr_1)) %>%
  mutate(viralloadImptCtgr_1 = ifelse((tmpViralload_1 >= 10^3) & (tmpViralload_1 < 10^4), 3, viralloadImptCtgr_1)) %>%
  mutate(viralloadImptCtgr_1 = ifelse((tmpViralload_1 >= 10^4), 4, viralloadImptCtgr_1)) %>%
  mutate(viralloadImptCtgr_2 = ifelse((tmpViralload_2 == -1), 1, NA)) %>%
  mutate(viralloadImptCtgr_2 = ifelse((tmpViralload_2 > -1) & (tmpViralload_2 < 10^3), 2, viralloadImptCtgr_2)) %>%
  mutate(viralloadImptCtgr_2 = ifelse((tmpViralload_2 >= 10^3) & (tmpViralload_2 < 10^4), 3, viralloadImptCtgr_2)) %>%
  mutate(viralloadImptCtgr_2 = ifelse((tmpViralload_2 >= 10^4), 4, viralloadImptCtgr_2)) %>%
  mutate(viralloadImptCtgr_3 = ifelse((tmpViralload_3 == -1), 1, NA)) %>%
  mutate(viralloadImptCtgr_3 = ifelse((tmpViralload_3 > -1) & (tmpViralload_3 < 10^3), 2, viralloadImptCtgr_3)) %>%
  mutate(viralloadImptCtgr_3 = ifelse((tmpViralload_3 >= 10^3) & (tmpViralload_3 < 10^4), 3, viralloadImptCtgr_3)) %>%
  mutate(viralloadImptCtgr_3 = ifelse((tmpViralload_3 >= 10^4), 4, viralloadImptCtgr_3)) 
summary(finalData.noMiss.t0Jul2016.switchDTG.wt400d)

# log10 viralload
mean(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$log10Viralload_0), na.rm = T)
summary(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$log10Viralload_0)
summary(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$log10Viralload_1)
summary(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$log10Viralload_2)
summary(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$log10Viralload_3)
# impute with average at each time point
tmp.summImpute <- data.frame(covariate      = "log10(viral load + 0.001)",
                             n_miss_0       = sum(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$log10Viralload_0)),
                             p_miss_0       = mean(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$log10Viralload_0)),
                             imputed_mean_0 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$log10Viralload_0, na.rm = T),
                             n_miss_1       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$censor_0 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$log10Viralload_1)),
                             p_miss_1       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$censor_0 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$log10Viralload_1)) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$n_1,
                             imputed_mean_1 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$log10Viralload_1, na.rm = T),
                             n_miss_2       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$censor_1 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$log10Viralload_2), na.rm = T),
                             p_miss_2       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$censor_1 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$log10Viralload_2), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$n_2, 
                             imputed_mean_2 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$log10Viralload_2, na.rm = T),
                             n_miss_3       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$censor_2 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$log10Viralload_3), na.rm = T),
                             p_miss_3       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$censor_2 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$log10Viralload_3), na.rm = T) /
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$n_3,
                             imputed_mean_3 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$log10Viralload_3, na.rm = T))
summImpute.wizmiss.t0Jul2016.switchDTG.wt400d.resPop <- rbind(summImpute.wizmiss.t0Jul2016.switchDTG.wt400d.resPop,
                                                              tmp.summImpute)
finalData.noMiss.t0Jul2016.switchDTG.wt400d <- finalData.noMiss.t0Jul2016.switchDTG.wt400d %>%
  mutate(log10ViralloadImptMn_0 = ifelse(is.na(log10Viralload_0), tmp.summImpute$imputed_mean_0, log10Viralload_0)) %>%
  mutate(log10ViralloadImptMn_1 = ifelse((is.na(log10Viralload_1)) & (censor_0 == 0), tmp.summImpute$imputed_mean_1, log10Viralload_1)) %>%
  mutate(log10ViralloadImptMn_2 = ifelse((is.na(log10Viralload_2)) & (censor_1 == 0), tmp.summImpute$imputed_mean_2, log10Viralload_2)) %>%
  mutate(log10ViralloadImptMn_3 = ifelse((is.na(log10Viralload_3)) & (censor_2 == 0), tmp.summImpute$imputed_mean_3, log10Viralload_3))
summary(finalData.noMiss.t0Jul2016.switchDTG.wt400d)
finalData.noMiss.t0Jul2016.switchDTG.wt400d <- finalData.noMiss.t0Jul2016.switchDTG.wt400d %>%
  select(patient_id:Weight_0, viralloadImptMn_0, log10ViralloadImptMn_0, viralloadImptCtgr_0, 
         SystolicBP_0:Weight_1, viralloadImptMn_1, log10ViralloadImptMn_1, viralloadImptCtgr_1, 
         SystolicBP_1:Weight_2, viralloadImptMn_2, log10ViralloadImptMn_2, viralloadImptCtgr_2, 
         SystolicBP_2:Weight_3, viralloadImptMn_3, log10ViralloadImptMn_3, viralloadImptCtgr_3, 
         SystolicBP_3:Weight_4)


# systolic blood pressure
# impute with average at each time point
tmp.summImpute <- data.frame(covariate      = "Systolic blood pressure",
                             n_miss_0       = sum(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$SystolicBP_0)),
                             p_miss_0       = mean(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$SystolicBP_0)),
                             imputed_mean_0 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$SystolicBP_0, na.rm = T),
                             n_miss_1       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$censor_0 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$SystolicBP_1)),
                             p_miss_1       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$censor_0 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$SystolicBP_1)) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$n_1,
                             imputed_mean_1 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$SystolicBP_1, na.rm = T),
                             n_miss_2       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$censor_1 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$SystolicBP_2), na.rm = T),
                             p_miss_2       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$censor_1 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$SystolicBP_2), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$n_2,
                             imputed_mean_2 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$SystolicBP_2, na.rm = T),
                             n_miss_3       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$censor_2 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$SystolicBP_3), na.rm = T),
                             p_miss_3       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$censor_2 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$SystolicBP_3), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$n_3,
                             imputed_mean_3 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$SystolicBP_3, na.rm = T))
summImpute.wizmiss.t0Jul2016.switchDTG.wt400d.resPop <- rbind(summImpute.wizmiss.t0Jul2016.switchDTG.wt400d.resPop,
                                                              tmp.summImpute)
finalData.noMiss.t0Jul2016.switchDTG.wt400d <- finalData.noMiss.t0Jul2016.switchDTG.wt400d %>%
  mutate(SystolicBP_0 = ifelse(is.na(SystolicBP_0), tmp.summImpute$imputed_mean_0, SystolicBP_0)) %>%
  mutate(SystolicBP_1 = ifelse((is.na(SystolicBP_1)) & (censor_0 == 0), tmp.summImpute$imputed_mean_1, SystolicBP_1)) %>%
  mutate(SystolicBP_2 = ifelse((is.na(SystolicBP_2)) & (censor_1 == 0), tmp.summImpute$imputed_mean_2, SystolicBP_2)) %>%
  mutate(SystolicBP_3 = ifelse((is.na(SystolicBP_3)) & (censor_2 == 0), tmp.summImpute$imputed_mean_3, SystolicBP_3))
mean(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$SystolicBP_0, na.rm = T)
mean(finalData.noMiss.t0Jul2016.switchDTG.wt400d$SystolicBP_0)

# expect the same
sum(is.na(finalData.noMiss.t0Jul2016.switchDTG.wt400d$SystolicBP_2))
sum(finalData.noMiss.t0Jul2016.switchDTG.wt400d$censor_1 == 1, na.rm = T) + sum(finalData.noMiss.t0Jul2016.switchDTG.wt400d$censor_0 == 1) 

# diastolic blood pressure
# impute with average at each time point
# mean(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$DiastolicBP_0), na.rm = T)
tmp.summImpute <- data.frame(covariate      = "Diastolic blood pressure",
                             n_miss_0       = sum(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$DiastolicBP_0)),
                             p_miss_0       = mean(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$DiastolicBP_0)),
                             imputed_mean_0 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$DiastolicBP_0, na.rm = T),
                             n_miss_1       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$censor_0 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$DiastolicBP_1)),
                             p_miss_1       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$censor_0 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$DiastolicBP_1)) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$n_1,
                             imputed_mean_1 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$DiastolicBP_1, na.rm = T),
                             n_miss_2       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$censor_1 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$DiastolicBP_2), na.rm = T),
                             p_miss_2       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$censor_1 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$DiastolicBP_2), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$n_2,
                             imputed_mean_2 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$DiastolicBP_2, na.rm = T),
                             n_miss_3       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$censor_2 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$DiastolicBP_3), na.rm = T),
                             p_miss_3       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$censor_2 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$DiastolicBP_3), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$n_3,
                             imputed_mean_3 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$DiastolicBP_3, na.rm = T))
summImpute.wizmiss.t0Jul2016.switchDTG.wt400d.resPop <- rbind(summImpute.wizmiss.t0Jul2016.switchDTG.wt400d.resPop,
                                                              tmp.summImpute)
finalData.noMiss.t0Jul2016.switchDTG.wt400d <- finalData.noMiss.t0Jul2016.switchDTG.wt400d %>%
  mutate(DiastolicBP_0 = ifelse(is.na(DiastolicBP_0), tmp.summImpute$imputed_mean_0, DiastolicBP_0)) %>%
  mutate(DiastolicBP_1 = ifelse((is.na(DiastolicBP_1)) & (censor_0 == 0), tmp.summImpute$imputed_mean_1, DiastolicBP_1)) %>%
  mutate(DiastolicBP_2 = ifelse((is.na(DiastolicBP_2)) & (censor_1 == 0), tmp.summImpute$imputed_mean_2, DiastolicBP_2)) %>%
  mutate(DiastolicBP_3 = ifelse((is.na(DiastolicBP_3)) & (censor_2 == 0), tmp.summImpute$imputed_mean_3, DiastolicBP_3))

# expect the same
sum(is.na(finalData.noMiss.t0Jul2016.switchDTG.wt400d$DiastolicBP_3))
sum(finalData.noMiss.t0Jul2016.switchDTG.wt400d$censor_2 == 1, na.rm = T) +
  sum(finalData.noMiss.t0Jul2016.switchDTG.wt400d$censor_1 == 1, na.rm = T) + 
  sum(finalData.noMiss.t0Jul2016.switchDTG.wt400d$censor_0 == 1) 

# Height
mean(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$Height_0), na.rm = T)
summary(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$Height_0)
summary(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$Height_1)
summary(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$Height_2)
summary(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$Height_3)
# remove heights that are greater than 240 cm
finalData.noMiss.t0Jul2016.switchDTG.wt400d %>%
  filter((Height_0 > 240) | (Height_1 > 240))
finalData.noMiss.t0Jul2016.switchDTG.wt400d <- finalData.noMiss.t0Jul2016.switchDTG.wt400d %>%
  mutate(Height_0 = ifelse(Height_0 > 240, NA, Height_0), 
         Height_1 = ifelse(Height_1 > 240, NA, Height_1))
# impute with average at each time point
tmp.summImpute <- data.frame(covariate      = "Height",
                             n_miss_0       = sum(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$Height_0)),
                             p_miss_0       = mean(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$Height_0)), 
                             imputed_mean_0 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$Height_0, na.rm = T),
                             n_miss_1       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$censor_0 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$Height_1)),
                             p_miss_1       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$censor_0 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$Height_1)) /
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$n_1,
                             imputed_mean_1 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$Height_1, na.rm = T),
                             n_miss_2       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$censor_1 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$Height_2), na.rm = T),
                             p_miss_2       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$censor_1 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$Height_2), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$n_2,
                             imputed_mean_2 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$Height_2, na.rm = T),
                             n_miss_3       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$censor_2 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$Height_3), na.rm = T),
                             p_miss_3       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$censor_2 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$Height_3), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$n_3,
                             imputed_mean_3 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$Height_3, na.rm = T))
summImpute.wizmiss.t0Jul2016.switchDTG.wt400d.resPop <- rbind(summImpute.wizmiss.t0Jul2016.switchDTG.wt400d.resPop,
                                                              tmp.summImpute)
finalData.noMiss.t0Jul2016.switchDTG.wt400d <- finalData.noMiss.t0Jul2016.switchDTG.wt400d %>%
  mutate(Height_0 = ifelse(is.na(Height_0), tmp.summImpute$imputed_mean_0, Height_0)) %>%
  mutate(Height_1 = ifelse((is.na(Height_1)) & (censor_0 == 0), tmp.summImpute$imputed_mean_1, Height_1)) %>%
  mutate(Height_2 = ifelse((is.na(Height_2)) & (censor_1 == 0), tmp.summImpute$imputed_mean_2, Height_2)) %>%
  mutate(Height_3 = ifelse((is.na(Height_3)) & (censor_2 == 0), tmp.summImpute$imputed_mean_3, Height_3))

# pregnancy
# do not need to impute any more because imputed before to censor pregnant women
summary(finalData.noMiss.t0Jul2016.switchDTG.wt400d$pregnant_0)
summary(finalData.noMiss.t0Jul2016.switchDTG.wt400d$pregnant_1)
summary(finalData.noMiss.t0Jul2016.switchDTG.wt400d$pregnant_2)
summary(finalData.noMiss.t0Jul2016.switchDTG.wt400d$pregnant_3)
# check if pregnancy missing if male
finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop <- finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop %>%
  mutate(male = gsub(" ", "", male))
finalData.noMiss.t0Jul2016.switchDTG.wt400d <- finalData.noMiss.t0Jul2016.switchDTG.wt400d %>%
  mutate(male = gsub(" ", "", male))
# ind.male <- (as.character(finalData.noMiss.t0Jul2016.switchDTG.wt400d$male) == "Male")
# # change pregnancy to 0 if male
# finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop <- finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop %>%
#   mutate(pregnant_0 = ifelse(male == "Male", 0, pregnant_0)) %>%
#   mutate(pregnant_1 = ifelse((male == "Male") & (censor_0 == 0), 0, pregnant_1)) %>%
#   mutate(pregnant_2 = ifelse((male == "Male") & (censor_1 == 0), 0, pregnant_2)) %>%
#   mutate(pregnant_3 = ifelse((male == "Male") & (censor_2 == 0), 0, pregnant_3))  
# finalData.noMiss.t0Jul2016.switchDTG.wt400d <- finalData.noMiss.t0Jul2016.switchDTG.wt400d %>%
#   mutate(pregnant_0 = ifelse(male == "Male", 0, pregnant_0)) %>%
#   mutate(pregnant_1 = ifelse((male == "Male") & (censor_0 == 0), 0, pregnant_1)) %>%
#   mutate(pregnant_2 = ifelse((male == "Male") & (censor_1 == 0), 0, pregnant_2)) %>%
#   mutate(pregnant_3 = ifelse((male == "Male") & (censor_2 == 0), 0, pregnant_3))  
# impute with 0 at each time point if female still missing
# tmp.summImpute <- data.frame(covariate      = "Pregnant",
#                              n_miss_0       = sum(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$pregnant_0)),
#                              p_miss_0       = mean(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$pregnant_0)), 
#                              imputed_mean_0 = 0,
#                              n_miss_1       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$censor_0 == 0) & 
#                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$pregnant_1)),
#                              p_miss_1       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$censor_0 == 0) & 
#                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$pregnant_1)) /
#                                summLeft.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$n_1,
#                              imputed_mean_1 = 0,
#                              n_miss_2       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$censor_1 == 0) & 
#                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$pregnant_2), na.rm = T),
#                              p_miss_2       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$censor_1 == 0) & 
#                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$pregnant_2), na.rm = T) / 
#                                summLeft.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$n_2,
#                              imputed_mean_2 = 0,
#                              n_miss_3       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$censor_2 == 0) & 
#                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$pregnant_3), na.rm = T),
#                              p_miss_3       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$censor_2 == 0) & 
#                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$pregnant_3), na.rm = T) / 
#                                summLeft.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$n_3,
#                              imputed_mean_3 = 0)
# summImpute.wizmiss.t0Jul2016.switchDTG.wt400d.resPop <- rbind(summImpute.wizmiss.t0Jul2016.switchDTG.wt400d.resPop,
#                                                                  tmp.summImpute)
# finalData.noMiss.t0Jul2016.switchDTG.wt400d <- finalData.noMiss.t0Jul2016.switchDTG.wt400d %>%
#   mutate(pregnant_0 = ifelse(is.na(pregnant_0), tmp.summImpute$imputed_mean_0, pregnant_0)) %>%
#   mutate(pregnant_1 = ifelse((is.na(pregnant_1)) & (censor_0 == 0), tmp.summImpute$imputed_mean_1, pregnant_1)) %>%
#   mutate(pregnant_2 = ifelse((is.na(pregnant_2)) & (censor_1 == 0), tmp.summImpute$imputed_mean_2, pregnant_2)) %>%
#   mutate(pregnant_3 = ifelse((is.na(pregnant_3)) & (censor_2 == 0), tmp.summImpute$imputed_mean_3, pregnant_3))

# tb and tb treatment
unique(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$activeTB_0)
unique(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$activeTB_1)
unique(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$activeTB_2)
unique(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$activeTB_3)
finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop <- finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop %>%
  mutate(activeTB_0 = ifelse(is.na(activeTB_0), 0, activeTB_0),
         activeTB_1 = ifelse(is.na(activeTB_1) & (censor_0 == 0), 0, activeTB_1),
         activeTB_2 = ifelse(is.na(activeTB_2) & (censor_1 == 0), 0, activeTB_2),
         activeTB_3 = ifelse(is.na(activeTB_3) & (censor_2 == 0), 0, activeTB_3))
finalData.noMiss.t0Jul2016.switchDTG.wt400d <- finalData.noMiss.t0Jul2016.switchDTG.wt400d %>%
  mutate(activeTB_0 = ifelse(is.na(activeTB_0), 0, activeTB_0),
         activeTB_1 = ifelse(is.na(activeTB_1) & (censor_0 == 0), 0, activeTB_1),
         activeTB_2 = ifelse(is.na(activeTB_2) & (censor_1 == 0), 0, activeTB_2),
         activeTB_3 = ifelse(is.na(activeTB_3) & (censor_2 == 0), 0, activeTB_3))
# Active TB no missing data
tmp.summImpute <- data.frame(covariate      = "Active TB",
                             n_miss_0       = sum(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$activeTB_0)),
                             p_miss_0       = mean(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$activeTB_0)), 
                             imputed_mean_0 = NA,
                             n_miss_1       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$censor_0 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$activeTB_1)),
                             p_miss_1       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$censor_0 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$activeTB_1)) /
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$n_1,
                             imputed_mean_1 = NA,
                             n_miss_2       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$censor_1 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$activeTB_2), na.rm = T),
                             p_miss_2       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$censor_1 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$activeTB_2), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$n_2,
                             imputed_mean_2 = NA,
                             n_miss_3       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$censor_2 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$activeTB_3), na.rm = T),
                             p_miss_3       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$censor_2 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$activeTB_3), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$n_3,
                             imputed_mean_3 = NA)
summImpute.wizmiss.t0Jul2016.switchDTG.wt400d.resPop <- rbind(summImpute.wizmiss.t0Jul2016.switchDTG.wt400d.resPop,
                                                              tmp.summImpute)
# tbtx
# expect 0: check if there are any pts on tbtx when not active TB
sum((finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$tbtx_0 == 1) & 
      (finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$activeTB_0 == 0), na.rm = T)
sum((finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$tbtx_1 == 1) & 
      (finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$activeTB_1 == 0), na.rm = T)
sum((finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$tbtx_2 == 1) & 
      (finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$activeTB_2 == 0), na.rm = T)
sum((finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$tbtx_3 == 1) & 
      (finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$activeTB_3 == 0), na.rm = T)
tmp.summImpute <- data.frame(covariate      = "TB treatment",
                             n_miss_0       = sum(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$tbtx_0)),
                             p_miss_0       = mean(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$tbtx_0)), 
                             imputed_mean_0 = 0,
                             n_miss_1       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$censor_0 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$tbtx_1)),
                             p_miss_1       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$censor_0 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$tbtx_1)) /
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$n_1,
                             imputed_mean_1 = 0,
                             n_miss_2       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$censor_1 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$tbtx_2), na.rm = T),
                             p_miss_2       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$censor_1 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$tbtx_2), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$n_2,
                             imputed_mean_2 = 0,
                             n_miss_3       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$censor_2 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$tbtx_3), na.rm = T),
                             p_miss_3       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$censor_2 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$tbtx_3), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$n_3,
                             imputed_mean_3 = 0)
summImpute.wizmiss.t0Jul2016.switchDTG.wt400d.resPop <- rbind(summImpute.wizmiss.t0Jul2016.switchDTG.wt400d.resPop,
                                                              tmp.summImpute)
finalData.noMiss.t0Jul2016.switchDTG.wt400d <- finalData.noMiss.t0Jul2016.switchDTG.wt400d %>%
  mutate(tbtx_0 = ifelse(is.na(tbtx_0), tmp.summImpute$imputed_mean_0, tbtx_0)) %>%
  mutate(tbtx_1 = ifelse((is.na(tbtx_1)) & (censor_0 == 0), tmp.summImpute$imputed_mean_1, tbtx_1)) %>%
  mutate(tbtx_2 = ifelse((is.na(tbtx_2)) & (censor_1 == 0), tmp.summImpute$imputed_mean_2, tbtx_2)) %>%
  mutate(tbtx_3 = ifelse((is.na(tbtx_3)) & (censor_2 == 0), tmp.summImpute$imputed_mean_3, tbtx_3))

# Married and civil status
# check if the missing status are the same
identical(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$Married_0), is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$civilstatus_0))
identical(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$Married_1), is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$civilstatus_1))
identical(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$Married_2), is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$civilstatus_2))
identical(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$Married_3), is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$civilstatus_3))
table(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$Married_0)
table(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$civilstatus_0)
table(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$Married_1)
table(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$civilstatus_1)
table(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$Married_2)
table(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$civilstatus_2)
table(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$Married_3)
table(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$civilstatus_3)
unique(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$civilstatus_0)
tmp.summImpute <- data.frame(covariate      = "Married or living w/ partner",
                             n_miss_0       = sum(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$Married_0)),
                             p_miss_0       = mean(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$Married_0)), 
                             imputed_mean_0 = 0,
                             n_miss_1       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$censor_0 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$Married_1)),
                             p_miss_1       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$censor_0 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$Married_1)) /
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$n_1,
                             imputed_mean_1 = 0,
                             n_miss_2       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$censor_1 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$Married_2), na.rm = T),
                             p_miss_2       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$censor_1 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$Married_2), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$n_2,
                             imputed_mean_2 = 0,
                             n_miss_3       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$censor_2 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$Married_3), na.rm = T),
                             p_miss_3       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$censor_2 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$Married_3), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$n_3,
                             imputed_mean_3 = 0)
summImpute.wizmiss.t0Jul2016.switchDTG.wt400d.resPop <- rbind(summImpute.wizmiss.t0Jul2016.switchDTG.wt400d.resPop,
                                                              tmp.summImpute)
finalData.noMiss.t0Jul2016.switchDTG.wt400d <- finalData.noMiss.t0Jul2016.switchDTG.wt400d %>%
  mutate(Married_0 = ifelse(is.na(Married_0), tmp.summImpute$imputed_mean_0, Married_0)) %>%
  mutate(Married_1 = ifelse((is.na(Married_1)) & (censor_0 == 0), tmp.summImpute$imputed_mean_1, Married_1)) %>%
  mutate(Married_2 = ifelse((is.na(Married_2)) & (censor_1 == 0), tmp.summImpute$imputed_mean_2, Married_2)) %>%
  mutate(Married_3 = ifelse((is.na(Married_3)) & (censor_2 == 0), tmp.summImpute$imputed_mean_3, Married_3))
# civil status
sum(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$civilstatus_0 == "Never Married and Not Living w/Partner", na.rm = T)
tmp.summImpute <- data.frame(covariate      = "Civil status",
                             n_miss_0       = sum(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$civilstatus_0)),
                             p_miss_0       = mean(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$civilstatus_0)), 
                             imputed_mean_0 = "Never Married and Not Living w/Partner",
                             n_miss_1       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$censor_0 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$civilstatus_1)),
                             p_miss_1       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$censor_0 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$civilstatus_1)) /
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$n_1,
                             imputed_mean_1 = "Never Married and Not Living w/Partner",
                             n_miss_2       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$censor_1 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$civilstatus_2), na.rm = T),
                             p_miss_2       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$censor_1 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$civilstatus_2), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$n_2,
                             imputed_mean_2 = "Never Married and Not Living w/Partner",
                             n_miss_3       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$censor_2 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$civilstatus_3), na.rm = T),
                             p_miss_3       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$censor_2 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$civilstatus_3), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$n_3,
                             imputed_mean_3 = "Never Married and Not Living w/Partner")
summImpute.wizmiss.t0Jul2016.switchDTG.wt400d.resPop <- rbind(summImpute.wizmiss.t0Jul2016.switchDTG.wt400d.resPop,
                                                              tmp.summImpute)
finalData.noMiss.t0Jul2016.switchDTG.wt400d <- finalData.noMiss.t0Jul2016.switchDTG.wt400d %>%
  mutate(civilstatus_0 = ifelse(is.na(civilstatus_0), tmp.summImpute$imputed_mean_0, as.character(civilstatus_0))) %>%
  mutate(civilstatus_1 = ifelse((is.na(civilstatus_1)) & (censor_0 == 0), tmp.summImpute$imputed_mean_1, as.character(civilstatus_1))) %>%
  mutate(civilstatus_2 = ifelse((is.na(civilstatus_2)) & (censor_1 == 0), tmp.summImpute$imputed_mean_2, as.character(civilstatus_2))) %>%
  mutate(civilstatus_3 = ifelse((is.na(civilstatus_3)) & (censor_2 == 0), tmp.summImpute$imputed_mean_3, as.character(civilstatus_3)))
# table(tmp$civilstatus_3)

# glucose
apply(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop %>% select(starts_with("glucose")), 2, function(x) mean(is.na(x)))
# remove glucose with >90% missing
tmp.summDrop <- data.frame(covariate = "Blood glucose",
                           n_0 = sum(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$glucose_0)),
                           p_0 = mean(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$glucose_0)), 
                           n_1 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$censor_0 == 0) & 
                                       is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$glucose_1)),
                           p_1 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$censor_0 == 0) & 
                                       is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$glucose_1)) /
                             summLeft.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$n_1,
                           n_2 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$censor_1 == 0) & 
                                       is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$glucose_2), na.rm = T),
                           p_2 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$censor_1 == 0) & 
                                       is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$glucose_2), na.rm = T) /
                             summLeft.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$n_2,
                           n_3 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$censor_2 == 0) & 
                                       is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$glucose_3), na.rm = T),
                           p_3 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$censor_2 == 0) & 
                                       is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$glucose_3), na.rm = T) /
                             summLeft.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$n_3)
summDrop.wizmiss.t0Jul2016.switchDTG.wt400d.resPop <- rbind(summDrop.wizmiss.t0Jul2016.switchDTG.wt400d.resPop,
                                                            tmp.summDrop)
finalData.noMiss.t0Jul2016.switchDTG.wt400d <- finalData.noMiss.t0Jul2016.switchDTG.wt400d %>%
  select(-glucose_0, -glucose_1, -glucose_2, -glucose_3)
# fasting glucose
tmp.summDrop <- data.frame(covariate = "Fasting serum glucose",
                           n_0 = sum(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$glucoseFasting_0)),
                           p_0 = mean(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$glucoseFasting_0)), 
                           n_1 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$censor_0 == 0) & 
                                       is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$glucoseFasting_1)),
                           p_1 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$censor_0 == 0) & 
                                       is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$glucoseFasting_1)) /
                             summLeft.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$n_1,
                           n_2 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$censor_1 == 0) & 
                                       is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$glucoseFasting_2), na.rm = T),
                           p_2 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$censor_1 == 0) & 
                                       is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$glucoseFasting_2), na.rm = T) /
                             summLeft.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$n_2,
                           n_3 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$censor_2 == 0) & 
                                       is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$glucoseFasting_3), na.rm = T),
                           p_3 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$censor_2 == 0) & 
                                       is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$glucoseFasting_3), na.rm = T) /
                             summLeft.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$n_3)
summDrop.wizmiss.t0Jul2016.switchDTG.wt400d.resPop <- rbind(summDrop.wizmiss.t0Jul2016.switchDTG.wt400d.resPop,
                                                            tmp.summDrop)
finalData.noMiss.t0Jul2016.switchDTG.wt400d <- finalData.noMiss.t0Jul2016.switchDTG.wt400d %>%
  select(-glucoseFasting_0, -glucoseFasting_1, -glucoseFasting_2, -glucoseFasting_3)

# Covered by NHIF
table(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$nhif_0)
sum(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$nhif_0 == "no", na.rm = T)
tmp.summImpute <- data.frame(covariate      = "Covered by NHIF",
                             n_miss_0       = sum(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$nhif_0)),
                             p_miss_0       = mean(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$nhif_0)), 
                             imputed_mean_0 = "no",
                             n_miss_1       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$censor_0 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$nhif_1)),
                             p_miss_1       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$censor_0 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$nhif_1)) /
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$n_1,
                             imputed_mean_1 = "no",
                             n_miss_2       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$censor_1 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$nhif_2), na.rm = T),
                             p_miss_2       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$censor_1 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$nhif_2), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$n_2,
                             imputed_mean_2 = "no",
                             n_miss_3       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$censor_2 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$nhif_3), na.rm = T),
                             p_miss_3       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$censor_2 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$nhif_3), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$n_3,
                             imputed_mean_3 = "no")
summImpute.wizmiss.t0Jul2016.switchDTG.wt400d.resPop <- rbind(summImpute.wizmiss.t0Jul2016.switchDTG.wt400d.resPop,
                                                              tmp.summImpute)
finalData.noMiss.t0Jul2016.switchDTG.wt400d <- finalData.noMiss.t0Jul2016.switchDTG.wt400d %>%
  mutate(nhif_0 = ifelse(is.na(nhif_0), tmp.summImpute$imputed_mean_0, as.character(nhif_0))) %>%
  mutate(nhif_1 = ifelse((is.na(nhif_1)) & (censor_0 == 0), tmp.summImpute$imputed_mean_1, as.character(nhif_1))) %>%
  mutate(nhif_2 = ifelse((is.na(nhif_2)) & (censor_1 == 0), tmp.summImpute$imputed_mean_2, as.character(nhif_2))) %>%
  mutate(nhif_3 = ifelse((is.na(nhif_3)) & (censor_2 == 0), tmp.summImpute$imputed_mean_3, as.character(nhif_3)))

apply(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop %>% select(Weight_0:censor_0), 2, function(x) mean(is.na(x)))
apply(finalData.noMiss.t0Jul2016.switchDTG.wt400d %>% select(Weight_0:Weight_4), 2, function(x) sum(is.na(x)))

mean(finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$DiastolicBP_0, na.rm = T)
mean(finalData.noMiss.t0Jul2016.switchDTG.wt400d$DiastolicBP_0, na.rm = T)


# 200-day
apply(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop %>% select(Weight_0:censor_0), 2, function(x) mean(is.na(x)))
apply(finalData.noMiss.t0Jul2016.switchDTG.wt200d %>% select(Weight_0:censor_0), 2, function(x) mean(is.na(x)))

# remove cd4 with >90% missing
summDrop.wizmiss.t0Jul2016.switchDTG.wt200d.resPop <- data.frame(covariate = "cd4",
                                                                 n_0 = sum(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$cd4_0)),
                                                                 p_0 = mean(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$cd4_0)), 
                                                                 n_1 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_0 == 0) & 
                                                                             is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$cd4_1)),
                                                                 n_2 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_1 == 0) & 
                                                                             is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$cd4_2), na.rm = T),
                                                                 n_3 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_2 == 0) & 
                                                                             is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$cd4_3), na.rm = T), 
                                                                 n_4 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_3 == 0) & 
                                                                             is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$cd4_4), na.rm = T),
                                                                 n_5 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_4 == 0) & 
                                                                             is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$cd4_5), na.rm = T),
                                                                 n_6 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_5 == 0) & 
                                                                             is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$cd4_6), na.rm = T),
                                                                 n_7 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_6 == 0) & 
                                                                             is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$cd4_7), na.rm = T))
summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop <- summCensor.wizmiss.t0Jul2016.switchDTG.wt200d.resPop %>%
  filter(type == "left")
summDrop.wizmiss.t0Jul2016.switchDTG.wt200d.resPop <- summDrop.wizmiss.t0Jul2016.switchDTG.wt200d.resPop %>%
  mutate(p_1 = n_1 / summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_1,
         p_2 = n_2 / summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_2,
         p_3 = n_3 / summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_3,
         p_4 = n_4 / summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_4,
         p_5 = n_5 / summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_5,
         p_6 = n_6 / summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_6,
         p_7 = n_7 / summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_7)
summDrop.wizmiss.t0Jul2016.switchDTG.wt200d.resPop <- summDrop.wizmiss.t0Jul2016.switchDTG.wt200d.resPop %>%
  select(covariate:n_1, p_1, n_2, p_2, n_3, p_3, n_4, p_4, n_5, p_5, n_6, p_6, n_7, p_7)
finalData.noMiss.t0Jul2016.switchDTG.wt200d <- finalData.noMiss.t0Jul2016.switchDTG.wt200d %>%
  select(-starts_with("cd4_"))

# viral load
apply(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop %>% select(starts_with("viralload")), 2, function(x) mean(is.na(x), na.rm = T))
summary(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$viralload_0)
summary(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$viralload_1)
summary(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$viralload_2)
summary(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$viralload_3)
# impute with average
tmp.summImpute <- data.frame(covariate      = "Viral load",
                             n_miss_0       = sum(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$viralload_0)),
                             p_miss_0       = mean(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$viralload_0)),
                             imputed_mean_0 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$viralload_0, na.rm = T),
                             n_miss_1       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_0 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$viralload_1)),
                             p_miss_1       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_0 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$viralload_1)) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_1,
                             imputed_mean_1 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$viralload_1, na.rm = T),
                             n_miss_2       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_1 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$viralload_2), na.rm = T),
                             p_miss_2       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_1 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$viralload_2), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_2, 
                             imputed_mean_2 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$viralload_2, na.rm = T),
                             n_miss_3       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_2 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$viralload_3), na.rm = T),
                             p_miss_3       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_2 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$viralload_3), na.rm = T) /
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_3,
                             imputed_mean_3 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$viralload_3, na.rm = T),
                             n_miss_4       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_3 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$viralload_4), na.rm = T),
                             p_miss_4       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_3 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$viralload_4), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_4, 
                               imputed_mean_4 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$viralload_4, na.rm = T),
                             n_miss_5       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_4 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$viralload_5), na.rm = T),
                             p_miss_5       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_4 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$viralload_5), na.rm = T) /
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_5,
                             imputed_mean_5 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$viralload_5, na.rm = T),
                             n_miss_6       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_5 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$viralload_6), na.rm = T),
                             p_miss_6       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_5 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$viralload_6), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_6, 
                             imputed_mean_6 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$viralload_6, na.rm = T),
                             n_miss_7       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_6 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$viralload_7), na.rm = T),
                             p_miss_7       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_6 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$viralload_7), na.rm = T) /
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_7,
                             imputed_mean_7 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$viralload_7, na.rm = T))
summImpute.wizmiss.t0Jul2016.switchDTG.wt200d.resPop <- tmp.summImpute
finalData.noMiss.t0Jul2016.switchDTG.wt200d <- finalData.noMiss.t0Jul2016.switchDTG.wt200d %>%
  mutate(viralloadImptMn_0 = ifelse(is.na(viralload_0), tmp.summImpute$imputed_mean_0, viralload_0)) %>%
  mutate(viralloadImptMn_1 = ifelse((is.na(viralload_1)) & (censor_0 == 0), tmp.summImpute$imputed_mean_1, viralload_1)) %>%
  mutate(viralloadImptMn_2 = ifelse((is.na(viralload_2)) & (censor_1 == 0), tmp.summImpute$imputed_mean_2, viralload_2)) %>%
  mutate(viralloadImptMn_3 = ifelse((is.na(viralload_3)) & (censor_2 == 0), tmp.summImpute$imputed_mean_3, viralload_3)) %>%
  mutate(viralloadImptMn_4 = ifelse((is.na(viralload_4)) & (censor_3 == 0), tmp.summImpute$imputed_mean_4, viralload_4)) %>%
  mutate(viralloadImptMn_5 = ifelse((is.na(viralload_5)) & (censor_4 == 0), tmp.summImpute$imputed_mean_5, viralload_5)) %>%
  mutate(viralloadImptMn_6 = ifelse((is.na(viralload_6)) & (censor_5 == 0), tmp.summImpute$imputed_mean_6, viralload_6)) %>%
  mutate(viralloadImptMn_7 = ifelse((is.na(viralload_7)) & (censor_6 == 0), tmp.summImpute$imputed_mean_7, viralload_7))
finalData.noMiss.t0Jul2016.switchDTG.wt200d <- finalData.noMiss.t0Jul2016.switchDTG.wt200d %>%
  mutate(tmpViralload_0 = ifelse(is.na(viralload_0), -1, viralload_0),
         tmpViralload_1 = ifelse(is.na(viralload_1) & (censor_0 == 0), -1, viralload_1),
         tmpViralload_2 = ifelse(is.na(viralload_2) & (censor_1 == 0), -1, viralload_2),
         tmpViralload_3 = ifelse(is.na(viralload_3) & (censor_2 == 0), -1, viralload_3),
         tmpViralload_4 = ifelse(is.na(viralload_4) & (censor_3 == 0), -1, viralload_4),
         tmpViralload_5 = ifelse(is.na(viralload_5) & (censor_4 == 0), -1, viralload_5),
         tmpViralload_6 = ifelse(is.na(viralload_6) & (censor_5 == 0), -1, viralload_6),
         tmpViralload_7 = ifelse(is.na(viralload_7) & (censor_6 == 0), -1, viralload_7)) 
finalData.noMiss.t0Jul2016.switchDTG.wt200d <- finalData.noMiss.t0Jul2016.switchDTG.wt200d %>%
  mutate(viralloadImptCtgr_0 = ifelse((tmpViralload_0 == -1), 1, NA)) %>%
  mutate(viralloadImptCtgr_0 = ifelse((tmpViralload_0 > -1) & (tmpViralload_0 < 10^3), 2, viralloadImptCtgr_0)) %>%
  mutate(viralloadImptCtgr_0 = ifelse((tmpViralload_0 >= 10^3) & (tmpViralload_0 < 10^4), 3, viralloadImptCtgr_0)) %>%
  mutate(viralloadImptCtgr_0 = ifelse((tmpViralload_0 >= 10^4), 4, viralloadImptCtgr_0)) %>%
  mutate(viralloadImptCtgr_1 = ifelse((tmpViralload_1 == -1), 1, NA)) %>%
  mutate(viralloadImptCtgr_1 = ifelse((tmpViralload_1 > -1) & (tmpViralload_1 < 10^3), 2, viralloadImptCtgr_1)) %>%
  mutate(viralloadImptCtgr_1 = ifelse((tmpViralload_1 >= 10^3) & (tmpViralload_1 < 10^4), 3, viralloadImptCtgr_1)) %>%
  mutate(viralloadImptCtgr_1 = ifelse((tmpViralload_1 >= 10^4), 4, viralloadImptCtgr_1)) %>%
  mutate(viralloadImptCtgr_2 = ifelse((tmpViralload_2 == -1), 1, NA)) %>%
  mutate(viralloadImptCtgr_2 = ifelse((tmpViralload_2 > -1) & (tmpViralload_2 < 10^3), 2, viralloadImptCtgr_2)) %>%
  mutate(viralloadImptCtgr_2 = ifelse((tmpViralload_2 >= 10^3) & (tmpViralload_2 < 10^4), 3, viralloadImptCtgr_2)) %>%
  mutate(viralloadImptCtgr_2 = ifelse((tmpViralload_2 >= 10^4), 4, viralloadImptCtgr_2)) %>%
  mutate(viralloadImptCtgr_3 = ifelse((tmpViralload_3 == -1), 1, NA)) %>%
  mutate(viralloadImptCtgr_3 = ifelse((tmpViralload_3 > -1) & (tmpViralload_3 < 10^3), 2, viralloadImptCtgr_3)) %>%
  mutate(viralloadImptCtgr_3 = ifelse((tmpViralload_3 >= 10^3) & (tmpViralload_3 < 10^4), 3, viralloadImptCtgr_3)) %>%
  mutate(viralloadImptCtgr_3 = ifelse((tmpViralload_3 >= 10^4), 4, viralloadImptCtgr_3)) %>%
  mutate(viralloadImptCtgr_4 = ifelse((tmpViralload_4 == -1), 1, NA)) %>%
  mutate(viralloadImptCtgr_4 = ifelse((tmpViralload_4 > -1) & (tmpViralload_4 < 10^3), 2, viralloadImptCtgr_4)) %>%
  mutate(viralloadImptCtgr_4 = ifelse((tmpViralload_4 >= 10^3) & (tmpViralload_4 < 10^4), 3, viralloadImptCtgr_4)) %>%
  mutate(viralloadImptCtgr_4 = ifelse((tmpViralload_4 >= 10^4), 4, viralloadImptCtgr_4)) %>%
  mutate(viralloadImptCtgr_5 = ifelse((tmpViralload_5 == -1), 1, NA)) %>%
  mutate(viralloadImptCtgr_5 = ifelse((tmpViralload_5 > -1) & (tmpViralload_5 < 10^3), 2, viralloadImptCtgr_5)) %>%
  mutate(viralloadImptCtgr_5 = ifelse((tmpViralload_5 >= 10^3) & (tmpViralload_5 < 10^4), 3, viralloadImptCtgr_5)) %>%
  mutate(viralloadImptCtgr_5 = ifelse((tmpViralload_5 >= 10^4), 4, viralloadImptCtgr_5))  %>%
  mutate(viralloadImptCtgr_6 = ifelse((tmpViralload_6 == -1), 1, NA)) %>%
  mutate(viralloadImptCtgr_6 = ifelse((tmpViralload_6 > -1) & (tmpViralload_6 < 10^3), 2, viralloadImptCtgr_6)) %>%
  mutate(viralloadImptCtgr_6 = ifelse((tmpViralload_6 >= 10^3) & (tmpViralload_6 < 10^4), 3, viralloadImptCtgr_6)) %>%
  mutate(viralloadImptCtgr_6 = ifelse((tmpViralload_6 >= 10^4), 4, viralloadImptCtgr_6)) %>%
  mutate(viralloadImptCtgr_7 = ifelse((tmpViralload_7 == -1), 1, NA)) %>%
  mutate(viralloadImptCtgr_7 = ifelse((tmpViralload_7 > -1) & (tmpViralload_7 < 10^3), 2, viralloadImptCtgr_7)) %>%
  mutate(viralloadImptCtgr_7 = ifelse((tmpViralload_7 >= 10^3) & (tmpViralload_7 < 10^4), 3, viralloadImptCtgr_7)) %>%
  mutate(viralloadImptCtgr_7 = ifelse((tmpViralload_7 >= 10^4), 4, viralloadImptCtgr_7)) 
summary(finalData.noMiss.t0Jul2016.switchDTG.wt200d)

# log10Viralload 
apply(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop %>% select(starts_with("log10Viralload")), 2, function(x) mean(is.na(x)))
summary(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$log10Viralload_0)
summary(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$log10Viralload_1)
summary(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$log10Viralload_2)
summary(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$log10Viralload_3)
# impute with average at each time point
tmp.summImpute <- data.frame(covariate      = "log10(viral load + 0.001)",
                             n_miss_0       = sum(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$log10Viralload_0)),
                             p_miss_0       = mean(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$log10Viralload_0)),
                             imputed_mean_0 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$log10Viralload_0, na.rm = T),
                             n_miss_1       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_0 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$log10Viralload_1)),
                             p_miss_1       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_0 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$log10Viralload_1)) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_1,
                             imputed_mean_1 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$log10Viralload_1, na.rm = T),
                             n_miss_2       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_1 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$log10Viralload_2), na.rm = T),
                             p_miss_2       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_1 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$log10Viralload_2), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_2, 
                             imputed_mean_2 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$log10Viralload_2, na.rm = T),
                             n_miss_3       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_2 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$log10Viralload_3), na.rm = T),
                             p_miss_3       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_2 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$log10Viralload_3), na.rm = T) /
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_3,
                             imputed_mean_3 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$log10Viralload_3, na.rm = T),
                             n_miss_4       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_3 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$log10Viralload_4), na.rm = T),
                             p_miss_4       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_3 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$log10Viralload_4), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_4, 
                             imputed_mean_4 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$log10Viralload_4, na.rm = T),
                             n_miss_5       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_4 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$log10Viralload_5), na.rm = T),
                             p_miss_5       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_4 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$log10Viralload_5), na.rm = T) /
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_5,
                             imputed_mean_5 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$log10Viralload_5, na.rm = T),
                             n_miss_6       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_5 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$log10Viralload_6), na.rm = T),
                             p_miss_6       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_5 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$log10Viralload_6), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_6, 
                             imputed_mean_6 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$log10Viralload_6, na.rm = T),
                             n_miss_7       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_6 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$log10Viralload_7), na.rm = T),
                             p_miss_7       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_6 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$log10Viralload_7), na.rm = T) /
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_7,
                             imputed_mean_7 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$log10Viralload_7, na.rm = T))
summImpute.wizmiss.t0Jul2016.switchDTG.wt200d.resPop <- rbind(summImpute.wizmiss.t0Jul2016.switchDTG.wt200d.resPop,
                                                              tmp.summImpute)
finalData.noMiss.t0Jul2016.switchDTG.wt200d <- finalData.noMiss.t0Jul2016.switchDTG.wt200d %>%
  mutate(log10ViralloadImptMn_0 = ifelse(is.na(log10Viralload_0), tmp.summImpute$imputed_mean_0, log10Viralload_0)) %>%
  mutate(log10ViralloadImptMn_1 = ifelse((is.na(log10Viralload_1)) & (censor_0 == 0), tmp.summImpute$imputed_mean_1, log10Viralload_1)) %>%
  mutate(log10ViralloadImptMn_2 = ifelse((is.na(log10Viralload_2)) & (censor_1 == 0), tmp.summImpute$imputed_mean_2, log10Viralload_2)) %>%
  mutate(log10ViralloadImptMn_3 = ifelse((is.na(log10Viralload_3)) & (censor_2 == 0), tmp.summImpute$imputed_mean_3, log10Viralload_3)) %>%
  mutate(log10ViralloadImptMn_4 = ifelse((is.na(log10Viralload_4)) & (censor_3 == 0), tmp.summImpute$imputed_mean_4, log10Viralload_4)) %>%
  mutate(log10ViralloadImptMn_5 = ifelse((is.na(log10Viralload_5)) & (censor_4 == 0), tmp.summImpute$imputed_mean_5, log10Viralload_5)) %>%
  mutate(log10ViralloadImptMn_6 = ifelse((is.na(log10Viralload_6)) & (censor_5 == 0), tmp.summImpute$imputed_mean_6, log10Viralload_6)) %>%
  mutate(log10ViralloadImptMn_7 = ifelse((is.na(log10Viralload_7)) & (censor_6 == 0), tmp.summImpute$imputed_mean_7, log10Viralload_7))
summary(finalData.noMiss.t0Jul2016.switchDTG.wt200d)
finalData.noMiss.t0Jul2016.switchDTG.wt200d <- finalData.noMiss.t0Jul2016.switchDTG.wt200d %>%
  select(patient_id:Weight_0, viralloadImptMn_0, log10ViralloadImptMn_0, viralloadImptCtgr_0, 
         SystolicBP_0:Weight_1, viralloadImptMn_1, log10ViralloadImptMn_1, viralloadImptCtgr_1, 
         SystolicBP_1:Weight_2, viralloadImptMn_2, log10ViralloadImptMn_2, viralloadImptCtgr_2, 
         SystolicBP_2:Weight_3, viralloadImptMn_3, log10ViralloadImptMn_3, viralloadImptCtgr_3, 
         SystolicBP_3:Weight_4, viralloadImptMn_4, log10ViralloadImptMn_4, viralloadImptCtgr_4, 
         SystolicBP_4:Weight_5, viralloadImptMn_5, log10ViralloadImptMn_5, viralloadImptCtgr_5, 
         SystolicBP_5:Weight_6, viralloadImptMn_6, log10ViralloadImptMn_6, viralloadImptCtgr_6, 
         SystolicBP_6:Weight_7, viralloadImptMn_7, log10ViralloadImptMn_7, viralloadImptCtgr_7, 
         SystolicBP_7:Weight_8)

# systolic blood pressure
# impute with average at each time point
tmp.summImpute <- data.frame(covariate      = "Systolic blood pressure",
                             n_miss_0       = sum(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$SystolicBP_0)),
                             p_miss_0       = mean(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$SystolicBP_0)),
                             imputed_mean_0 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$SystolicBP_0, na.rm = T),
                             n_miss_1       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_0 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$SystolicBP_1)),
                             p_miss_1       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_0 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$SystolicBP_1)) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_1,
                             imputed_mean_1 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$SystolicBP_1, na.rm = T),
                             n_miss_2       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_1 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$SystolicBP_2), na.rm = T),
                             p_miss_2       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_1 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$SystolicBP_2), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_2,
                             imputed_mean_2 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$SystolicBP_2, na.rm = T),
                             n_miss_3       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_2 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$SystolicBP_3), na.rm = T),
                             p_miss_3       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_2 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$SystolicBP_3), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_3,
                             imputed_mean_3 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$SystolicBP_3, na.rm = T),
                             n_miss_4       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_3 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$SystolicBP_4), na.rm = T),
                             p_miss_4       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_3 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$SystolicBP_4), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_4,
                             imputed_mean_4 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$SystolicBP_4, na.rm = T),
                             n_miss_5       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_4 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$SystolicBP_5), na.rm = T),
                             p_miss_5       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_4 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$SystolicBP_5), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_5,
                             imputed_mean_5 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$SystolicBP_5, na.rm = T),
                             n_miss_6       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_5 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$SystolicBP_6), na.rm = T),
                             p_miss_6       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_5 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$SystolicBP_6), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_6,
                             imputed_mean_6 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$SystolicBP_6, na.rm = T),
                             n_miss_7       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_6 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$SystolicBP_7), na.rm = T),
                             p_miss_7       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_6 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$SystolicBP_7), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_7,
                             imputed_mean_7 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$SystolicBP_7, na.rm = T))
summImpute.wizmiss.t0Jul2016.switchDTG.wt200d.resPop <- rbind(summImpute.wizmiss.t0Jul2016.switchDTG.wt200d.resPop,
                                                              tmp.summImpute)
finalData.noMiss.t0Jul2016.switchDTG.wt200d <- finalData.noMiss.t0Jul2016.switchDTG.wt200d %>%
  mutate(SystolicBP_0 = ifelse(is.na(SystolicBP_0), tmp.summImpute$imputed_mean_0, SystolicBP_0)) %>%
  mutate(SystolicBP_1 = ifelse((is.na(SystolicBP_1)) & (censor_0 == 0), tmp.summImpute$imputed_mean_1, SystolicBP_1)) %>%
  mutate(SystolicBP_2 = ifelse((is.na(SystolicBP_2)) & (censor_1 == 0), tmp.summImpute$imputed_mean_2, SystolicBP_2)) %>%
  mutate(SystolicBP_3 = ifelse((is.na(SystolicBP_3)) & (censor_2 == 0), tmp.summImpute$imputed_mean_3, SystolicBP_3)) %>%
  mutate(SystolicBP_4 = ifelse((is.na(SystolicBP_4)) & (censor_3 == 0), tmp.summImpute$imputed_mean_4, SystolicBP_4)) %>%
  mutate(SystolicBP_5 = ifelse((is.na(SystolicBP_5)) & (censor_4 == 0), tmp.summImpute$imputed_mean_5, SystolicBP_5)) %>%
  mutate(SystolicBP_6 = ifelse((is.na(SystolicBP_6)) & (censor_5 == 0), tmp.summImpute$imputed_mean_6, SystolicBP_6)) %>%
  mutate(SystolicBP_7 = ifelse((is.na(SystolicBP_7)) & (censor_6 == 0), tmp.summImpute$imputed_mean_7, SystolicBP_7))

mean(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$SystolicBP_0, na.rm = T)
mean(finalData.noMiss.t0Jul2016.switchDTG.wt200d$SystolicBP_0)

# expect the same
sum(is.na(finalData.noMiss.t0Jul2016.switchDTG.wt200d$SystolicBP_2))
sum(finalData.noMiss.t0Jul2016.switchDTG.wt200d$censor_1 == 1, na.rm = T) + sum(finalData.noMiss.t0Jul2016.switchDTG.wt200d$censor_0 == 1) 

# diastolic blood pressure
# impute with average at each time point
# mean(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$DiastolicBP_0), na.rm = T)
tmp.summImpute <- data.frame(covariate      = "Diastolic blood pressure",
                             n_miss_0       = sum(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$DiastolicBP_0)),
                             p_miss_0       = mean(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$DiastolicBP_0)),
                             imputed_mean_0 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$DiastolicBP_0, na.rm = T),
                             n_miss_1       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_0 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$DiastolicBP_1)),
                             p_miss_1       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_0 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$DiastolicBP_1)) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_1,
                             imputed_mean_1 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$DiastolicBP_1, na.rm = T),
                             n_miss_2       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_1 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$DiastolicBP_2), na.rm = T),
                             p_miss_2       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_1 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$DiastolicBP_2), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_2,
                             imputed_mean_2 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$DiastolicBP_2, na.rm = T),
                             n_miss_3       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_2 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$DiastolicBP_3), na.rm = T),
                             p_miss_3       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_2 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$DiastolicBP_3), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_3,
                             imputed_mean_3 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$DiastolicBP_3, na.rm = T),
                             n_miss_4       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_3 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$DiastolicBP_4), na.rm = T),
                             p_miss_4       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_3 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$DiastolicBP_4), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_4,
                             imputed_mean_4 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$DiastolicBP_4, na.rm = T),
                             n_miss_5       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_4 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$DiastolicBP_5), na.rm = T),
                             p_miss_5       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_4 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$DiastolicBP_5), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_5,
                             imputed_mean_5 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$DiastolicBP_5, na.rm = T),
                             n_miss_6       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_5 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$DiastolicBP_6), na.rm = T),
                             p_miss_6       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_5 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$DiastolicBP_6), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_6,
                             imputed_mean_6 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$DiastolicBP_6, na.rm = T),
                             n_miss_7       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_6 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$DiastolicBP_7), na.rm = T),
                             p_miss_7       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_6 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$DiastolicBP_7), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_7,
                             imputed_mean_7 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$DiastolicBP_7, na.rm = T))
summImpute.wizmiss.t0Jul2016.switchDTG.wt200d.resPop <- rbind(summImpute.wizmiss.t0Jul2016.switchDTG.wt200d.resPop,
                                                              tmp.summImpute)
finalData.noMiss.t0Jul2016.switchDTG.wt200d <- finalData.noMiss.t0Jul2016.switchDTG.wt200d %>%
  mutate(DiastolicBP_0 = ifelse(is.na(DiastolicBP_0), tmp.summImpute$imputed_mean_0, DiastolicBP_0)) %>%
  mutate(DiastolicBP_1 = ifelse((is.na(DiastolicBP_1)) & (censor_0 == 0), tmp.summImpute$imputed_mean_1, DiastolicBP_1)) %>%
  mutate(DiastolicBP_2 = ifelse((is.na(DiastolicBP_2)) & (censor_1 == 0), tmp.summImpute$imputed_mean_2, DiastolicBP_2)) %>%
  mutate(DiastolicBP_3 = ifelse((is.na(DiastolicBP_3)) & (censor_2 == 0), tmp.summImpute$imputed_mean_3, DiastolicBP_3)) %>%
  mutate(DiastolicBP_4 = ifelse((is.na(DiastolicBP_4)) & (censor_3 == 0), tmp.summImpute$imputed_mean_4, DiastolicBP_4)) %>%
  mutate(DiastolicBP_5 = ifelse((is.na(DiastolicBP_5)) & (censor_4 == 0), tmp.summImpute$imputed_mean_5, DiastolicBP_5)) %>%
  mutate(DiastolicBP_6 = ifelse((is.na(DiastolicBP_6)) & (censor_5 == 0), tmp.summImpute$imputed_mean_6, DiastolicBP_6)) %>%
  mutate(DiastolicBP_7 = ifelse((is.na(DiastolicBP_7)) & (censor_6 == 0), tmp.summImpute$imputed_mean_7, DiastolicBP_7))

# expect the same
sum(is.na(finalData.noMiss.t0Jul2016.switchDTG.wt200d$DiastolicBP_3))
sum(finalData.noMiss.t0Jul2016.switchDTG.wt200d$censor_2 == 1, na.rm = T) +
  sum(finalData.noMiss.t0Jul2016.switchDTG.wt200d$censor_1 == 1, na.rm = T) + 
  sum(finalData.noMiss.t0Jul2016.switchDTG.wt200d$censor_0 == 1) 

# Height
mean(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$Height_0), na.rm = T)
summary(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$Height_0)
summary(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$Height_1)
summary(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$Height_2)
summary(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$Height_3)
summary(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$Height_4)
summary(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$Height_5)
summary(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$Height_6)
summary(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$Height_7)
# remove heights that are greater than 240 cm
finalData.noMiss.t0Jul2016.switchDTG.wt200d %>%
  filter((Height_0 > 240) | (Height_1 > 240) | (Height_2 > 240) | (Height_3 > 240))
finalData.noMiss.t0Jul2016.switchDTG.wt200d <- finalData.noMiss.t0Jul2016.switchDTG.wt200d %>%
  mutate(Height_0 = ifelse(Height_0 > 240, NA, Height_0), 
         Height_1 = ifelse(Height_1 > 240, NA, Height_1), 
         Height_2 = ifelse(Height_2 > 240, NA, Height_2), 
         Height_3 = ifelse(Height_3 > 240, NA, Height_3))
# impute with average at each time point
tmp.summImpute <- data.frame(covariate      = "Height",
                             n_miss_0       = sum(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$Height_0)),
                             p_miss_0       = mean(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$Height_0)), 
                             imputed_mean_0 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$Height_0, na.rm = T),
                             n_miss_1       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_0 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$Height_1)),
                             p_miss_1       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_0 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$Height_1)) /
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_1,
                             imputed_mean_1 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$Height_1, na.rm = T),
                             n_miss_2       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_1 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$Height_2), na.rm = T),
                             p_miss_2       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_1 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$Height_2), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_2,
                             imputed_mean_2 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$Height_2, na.rm = T),
                             n_miss_3       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_2 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$Height_3), na.rm = T),
                             p_miss_3       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_2 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$Height_3), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_3,
                             imputed_mean_3 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$Height_3, na.rm = T),
                             n_miss_4       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_3 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$Height_4), na.rm = T),
                             p_miss_4       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_3 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$Height_4), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_4,
                             imputed_mean_4 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$Height_4, na.rm = T),
                             n_miss_5       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_4 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$Height_5), na.rm = T),
                             p_miss_5       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_4 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$Height_5), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_5,
                             imputed_mean_5 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$Height_5, na.rm = T),
                             n_miss_6       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_5 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$Height_6), na.rm = T),
                             p_miss_6       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_5 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$Height_6), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_6,
                             imputed_mean_6 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$Height_6, na.rm = T),
                             n_miss_7       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_6 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$Height_7), na.rm = T),
                             p_miss_7       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_6 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$Height_7), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_7,
                             imputed_mean_7 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$Height_7, na.rm = T))
summImpute.wizmiss.t0Jul2016.switchDTG.wt200d.resPop <- rbind(summImpute.wizmiss.t0Jul2016.switchDTG.wt200d.resPop,
                                                              tmp.summImpute)
finalData.noMiss.t0Jul2016.switchDTG.wt200d <- finalData.noMiss.t0Jul2016.switchDTG.wt200d %>%
  mutate(Height_0 = ifelse(is.na(Height_0), tmp.summImpute$imputed_mean_0, Height_0)) %>%
  mutate(Height_1 = ifelse((is.na(Height_1)) & (censor_0 == 0), tmp.summImpute$imputed_mean_1, Height_1)) %>%
  mutate(Height_2 = ifelse((is.na(Height_2)) & (censor_1 == 0), tmp.summImpute$imputed_mean_2, Height_2)) %>%
  mutate(Height_3 = ifelse((is.na(Height_3)) & (censor_2 == 0), tmp.summImpute$imputed_mean_3, Height_3)) %>%
  mutate(Height_4 = ifelse((is.na(Height_4)) & (censor_3 == 0), tmp.summImpute$imputed_mean_4, Height_4)) %>%
  mutate(Height_5 = ifelse((is.na(Height_5)) & (censor_4 == 0), tmp.summImpute$imputed_mean_5, Height_5)) %>%
  mutate(Height_6 = ifelse((is.na(Height_6)) & (censor_5 == 0), tmp.summImpute$imputed_mean_6, Height_6)) %>%
  mutate(Height_7 = ifelse((is.na(Height_7)) & (censor_6 == 0), tmp.summImpute$imputed_mean_7, Height_7))

# pregnancy
unique(finalData.noMiss.t0Jul2016.switchDTG.wt200d$pregnant_0)
# check if pregnancy missing if male
finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop <- finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop %>%
  mutate(male = gsub(" ", "", male))
finalData.noMiss.t0Jul2016.switchDTG.wt200d <- finalData.noMiss.t0Jul2016.switchDTG.wt200d %>%
  mutate(male = gsub(" ", "", male))
ind.male <- (as.character(finalData.noMiss.t0Jul2016.switchDTG.wt200d$male) == "Male")
table(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$male)
# expect 0s
sum(!(ind.male %in% is.na(finalData.noMiss.t0Jul2016.switchDTG.wt200d$pregnant_0)))
sum(!(ind.male %in% is.na(finalData.noMiss.t0Jul2016.switchDTG.wt200d$pregnant_1)))
sum(!(ind.male %in% is.na(finalData.noMiss.t0Jul2016.switchDTG.wt200d$pregnant_2)))
sum(!(ind.male %in% is.na(finalData.noMiss.t0Jul2016.switchDTG.wt200d$pregnant_3)))
sum(!(ind.male %in% is.na(finalData.noMiss.t0Jul2016.switchDTG.wt200d$pregnant_4)))
sum(!(ind.male %in% is.na(finalData.noMiss.t0Jul2016.switchDTG.wt200d$pregnant_5)))
sum(!(ind.male %in% is.na(finalData.noMiss.t0Jul2016.switchDTG.wt200d$pregnant_6)))
sum(!(ind.male %in% is.na(finalData.noMiss.t0Jul2016.switchDTG.wt200d$pregnant_7)))
# # change pregnancy to 0 if male
# finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop <- finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop %>%
#   mutate(pregnant_0 = ifelse(male == "Male", 0, pregnant_0)) %>%
#   mutate(pregnant_1 = ifelse((male == "Male") & (censor_0 == 0), 0, pregnant_1)) %>%
#   mutate(pregnant_2 = ifelse((male == "Male") & (censor_1 == 0), 0, pregnant_2)) %>%
#   mutate(pregnant_3 = ifelse((male == "Male") & (censor_2 == 0), 0, pregnant_3)) %>%
#   mutate(pregnant_4 = ifelse((male == "Male") & (censor_3 == 0), 0, pregnant_4)) %>%
#   mutate(pregnant_5 = ifelse((male == "Male") & (censor_4 == 0), 0, pregnant_5)) %>%
#   mutate(pregnant_6 = ifelse((male == "Male") & (censor_5 == 0), 0, pregnant_6)) %>%
#   mutate(pregnant_7 = ifelse((male == "Male") & (censor_6 == 0), 0, pregnant_7))  
# finalData.noMiss.t0Jul2016.switchDTG.wt200d <- finalData.noMiss.t0Jul2016.switchDTG.wt200d %>%
#   mutate(pregnant_0 = ifelse(male == "Male", 0, pregnant_0)) %>%
#   mutate(pregnant_1 = ifelse((male == "Male") & (censor_0 == 0), 0, pregnant_1)) %>%
#   mutate(pregnant_2 = ifelse((male == "Male") & (censor_1 == 0), 0, pregnant_2)) %>%
#   mutate(pregnant_3 = ifelse((male == "Male") & (censor_2 == 0), 0, pregnant_3)) %>%
#   mutate(pregnant_4 = ifelse((male == "Male") & (censor_3 == 0), 0, pregnant_4)) %>%
#   mutate(pregnant_5 = ifelse((male == "Male") & (censor_4 == 0), 0, pregnant_5)) %>%
#   mutate(pregnant_6 = ifelse((male == "Male") & (censor_5 == 0), 0, pregnant_6)) %>%
#   mutate(pregnant_7 = ifelse((male == "Male") & (censor_6 == 0), 0, pregnant_7))  
# table(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$pregnant_0)
# sum(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$pregnant_0))
# # impute with 0 at each time point if female still missing
# tmp.summImpute <- data.frame(covariate      = "Pregnant",
#                              n_miss_0       = sum(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$pregnant_0)),
#                              p_miss_0       = mean(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$pregnant_0)), 
#                              imputed_mean_0 = 0,
#                              n_miss_1       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_0 == 0) & 
#                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$pregnant_1)),
#                              p_miss_1       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_0 == 0) & 
#                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$pregnant_1)) /
#                                summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_1,
#                              imputed_mean_1 = 0,
#                              n_miss_2       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_1 == 0) & 
#                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$pregnant_2), na.rm = T),
#                              p_miss_2       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_1 == 0) & 
#                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$pregnant_2), na.rm = T) / 
#                                summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_2,
#                              imputed_mean_2 = 0,
#                              n_miss_3       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_2 == 0) & 
#                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$pregnant_3), na.rm = T),
#                              p_miss_3       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_2 == 0) & 
#                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$pregnant_3), na.rm = T) / 
#                                summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_3,
#                              imputed_mean_3 = 0,
#                              n_miss_4       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_3 == 0) & 
#                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$pregnant_4), na.rm = T),
#                              p_miss_4       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_3 == 0) & 
#                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$pregnant_4), na.rm = T) / 
#                                summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_4,
#                              imputed_mean_4 = 0,
#                              n_miss_5       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_4 == 0) & 
#                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$pregnant_5), na.rm = T),
#                              p_miss_5       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_4 == 0) & 
#                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$pregnant_5), na.rm = T) / 
#                                summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_5,
#                              imputed_mean_5 = 0,
#                              n_miss_6       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_5 == 0) & 
#                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$pregnant_6), na.rm = T),
#                              p_miss_6       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_5 == 0) & 
#                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$pregnant_6), na.rm = T) / 
#                                summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_6,
#                              imputed_mean_6 = 0,
#                              n_miss_7       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_6 == 0) & 
#                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$pregnant_7), na.rm = T),
#                              p_miss_7       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_6 == 0) & 
#                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$pregnant_7), na.rm = T) / 
#                                summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_7,
#                              imputed_mean_7 = 0)
# summImpute.wizmiss.t0Jul2016.switchDTG.wt200d.resPop <- rbind(summImpute.wizmiss.t0Jul2016.switchDTG.wt200d.resPop,
#                                                               tmp.summImpute)
# finalData.noMiss.t0Jul2016.switchDTG.wt200d <- finalData.noMiss.t0Jul2016.switchDTG.wt200d %>%
#   mutate(pregnant_0 = ifelse(is.na(pregnant_0), tmp.summImpute$imputed_mean_0, pregnant_0)) %>%
#   mutate(pregnant_1 = ifelse((is.na(pregnant_1)) & (censor_0 == 0), tmp.summImpute$imputed_mean_1, pregnant_1)) %>%
#   mutate(pregnant_2 = ifelse((is.na(pregnant_2)) & (censor_1 == 0), tmp.summImpute$imputed_mean_2, pregnant_2)) %>%
#   mutate(pregnant_3 = ifelse((is.na(pregnant_3)) & (censor_2 == 0), tmp.summImpute$imputed_mean_3, pregnant_3)) %>%
#   mutate(pregnant_4 = ifelse((is.na(pregnant_4)) & (censor_3 == 0), tmp.summImpute$imputed_mean_4, pregnant_4)) %>%
#   mutate(pregnant_5 = ifelse((is.na(pregnant_5)) & (censor_4 == 0), tmp.summImpute$imputed_mean_5, pregnant_5)) %>%
#   mutate(pregnant_6 = ifelse((is.na(pregnant_6)) & (censor_5 == 0), tmp.summImpute$imputed_mean_6, pregnant_6)) %>%
#   mutate(pregnant_7 = ifelse((is.na(pregnant_7)) & (censor_6 == 0), tmp.summImpute$imputed_mean_7, pregnant_7))

# tb and tb treatment
unique(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$activeTB_0)
unique(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$activeTB_1)
unique(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$activeTB_2)
unique(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$activeTB_3)
unique(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$activeTB_4)
unique(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$activeTB_5)
unique(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$activeTB_6)
unique(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$activeTB_7)
finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop <- finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop %>%
  mutate(activeTB_0 = ifelse(is.na(activeTB_0), 0, activeTB_0),
         activeTB_1 = ifelse(is.na(activeTB_1) & (censor_0 == 0), 0, activeTB_1),
         activeTB_2 = ifelse(is.na(activeTB_2) & (censor_1 == 0), 0, activeTB_2),
         activeTB_3 = ifelse(is.na(activeTB_3) & (censor_2 == 0), 0, activeTB_3),
         activeTB_4 = ifelse(is.na(activeTB_4) & (censor_3 == 0), 0, activeTB_4),
         activeTB_5 = ifelse(is.na(activeTB_5) & (censor_4 == 0), 0, activeTB_5),
         activeTB_6 = ifelse(is.na(activeTB_6) & (censor_5 == 0), 0, activeTB_6),
         activeTB_7 = ifelse(is.na(activeTB_7) & (censor_6 == 0), 0, activeTB_7))
finalData.noMiss.t0Jul2016.switchDTG.wt200d <- finalData.noMiss.t0Jul2016.switchDTG.wt200d %>%
  mutate(activeTB_0 = ifelse(is.na(activeTB_0), 0, activeTB_0),
         activeTB_1 = ifelse(is.na(activeTB_1) & (censor_0 == 0), 0, activeTB_1),
         activeTB_2 = ifelse(is.na(activeTB_2) & (censor_1 == 0), 0, activeTB_2),
         activeTB_3 = ifelse(is.na(activeTB_3) & (censor_2 == 0), 0, activeTB_3),
         activeTB_4 = ifelse(is.na(activeTB_4) & (censor_3 == 0), 0, activeTB_4),
         activeTB_5 = ifelse(is.na(activeTB_5) & (censor_4 == 0), 0, activeTB_5),
         activeTB_6 = ifelse(is.na(activeTB_6) & (censor_5 == 0), 0, activeTB_6),
         activeTB_7 = ifelse(is.na(activeTB_7) & (censor_6 == 0), 0, activeTB_7))
# Active TB no missing data
tmp.summImpute <- data.frame(covariate      = "Active TB",
                             n_miss_0       = sum(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$activeTB_0)),
                             p_miss_0       = mean(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$activeTB_0)), 
                             imputed_mean_0 = NA,
                             n_miss_1       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_0 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$activeTB_1)),
                             p_miss_1       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_0 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$activeTB_1)) /
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_1,
                             imputed_mean_1 = NA,
                             n_miss_2       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_1 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$activeTB_2), na.rm = T),
                             p_miss_2       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_1 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$activeTB_2), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_2,
                             imputed_mean_2 = NA,
                             n_miss_3       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_2 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$activeTB_3), na.rm = T),
                             p_miss_3       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_2 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$activeTB_3), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_3,
                             imputed_mean_3 = NA,
                             n_miss_4       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_3 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$activeTB_4), na.rm = T),
                             p_miss_4       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_3 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$activeTB_4), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_4,
                             imputed_mean_4 = NA,
                             n_miss_5       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_4 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$activeTB_5), na.rm = T),
                             p_miss_5       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_4 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$activeTB_5), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_5,
                             imputed_mean_5 = NA,
                             n_miss_6       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_5 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$activeTB_6), na.rm = T),
                             p_miss_6       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_5 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$activeTB_6), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_6,
                             imputed_mean_6 = NA,
                             n_miss_7       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_6 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$activeTB_7), na.rm = T),
                             p_miss_7       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_6 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$activeTB_7), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_7,
                             imputed_mean_7 = NA)
summImpute.wizmiss.t0Jul2016.switchDTG.wt200d.resPop <- rbind(summImpute.wizmiss.t0Jul2016.switchDTG.wt200d.resPop,
                                                              tmp.summImpute)
# tbtx
# expect 0: check if there are any pts on tbtx when not active TB
sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$tbtx_0 == 1) & 
      (finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$activeTB_0 == 0), na.rm = T)
sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$tbtx_1 == 1) & 
      (finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$activeTB_1 == 0), na.rm = T)
sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$tbtx_2 == 1) & 
      (finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$activeTB_2 == 0), na.rm = T)
sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$tbtx_3 == 1) & 
      (finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$activeTB_3 == 0), na.rm = T)
sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$tbtx_4 == 1) & 
      (finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$activeTB_4 == 0), na.rm = T)
sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$tbtx_5 == 1) & 
      (finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$activeTB_5 == 0), na.rm = T)
sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$tbtx_6 == 1) & 
      (finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$activeTB_6 == 0), na.rm = T)
sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$tbtx_7 == 1) & 
      (finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$activeTB_7 == 0), na.rm = T)
tmp.summImpute <- data.frame(covariate      = "TB treatment",
                             n_miss_0       = sum(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$tbtx_0)),
                             p_miss_0       = mean(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$tbtx_0)), 
                             imputed_mean_0 = 0,
                             n_miss_1       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_0 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$tbtx_1)),
                             p_miss_1       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_0 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$tbtx_1)) /
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_1,
                             imputed_mean_1 = 0,
                             n_miss_2       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_1 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$tbtx_2), na.rm = T),
                             p_miss_2       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_1 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$tbtx_2), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_2,
                             imputed_mean_2 = 0,
                             n_miss_3       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_2 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$tbtx_3), na.rm = T),
                             p_miss_3       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_2 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$tbtx_3), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_3,
                             imputed_mean_3 = 0,
                             n_miss_4       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_3 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$tbtx_4), na.rm = T),
                             p_miss_4       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_3 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$tbtx_4), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_4,
                             imputed_mean_4 = 0,
                             n_miss_5       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_4 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$tbtx_5), na.rm = T),
                             p_miss_5       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_4 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$tbtx_5), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_5,
                             imputed_mean_5 = 0,
                             n_miss_6       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_5 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$tbtx_6), na.rm = T),
                             p_miss_6       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_5 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$tbtx_6), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_6,
                             imputed_mean_6 = 0,
                             n_miss_7       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_6 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$tbtx_7), na.rm = T),
                             p_miss_7       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_6 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$tbtx_7), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_7,
                             imputed_mean_7 = 0)
summImpute.wizmiss.t0Jul2016.switchDTG.wt200d.resPop <- rbind(summImpute.wizmiss.t0Jul2016.switchDTG.wt200d.resPop,
                                                              tmp.summImpute)
finalData.noMiss.t0Jul2016.switchDTG.wt200d <- finalData.noMiss.t0Jul2016.switchDTG.wt200d %>%
  mutate(tbtx_0 = ifelse(is.na(tbtx_0), tmp.summImpute$imputed_mean_0, tbtx_0)) %>%
  mutate(tbtx_1 = ifelse((is.na(tbtx_1)) & (censor_0 == 0), tmp.summImpute$imputed_mean_1, tbtx_1)) %>%
  mutate(tbtx_2 = ifelse((is.na(tbtx_2)) & (censor_1 == 0), tmp.summImpute$imputed_mean_2, tbtx_2)) %>%
  mutate(tbtx_3 = ifelse((is.na(tbtx_3)) & (censor_2 == 0), tmp.summImpute$imputed_mean_3, tbtx_3)) %>%
  mutate(tbtx_4 = ifelse((is.na(tbtx_4)) & (censor_3 == 0), tmp.summImpute$imputed_mean_4, tbtx_4)) %>%
  mutate(tbtx_5 = ifelse((is.na(tbtx_5)) & (censor_4 == 0), tmp.summImpute$imputed_mean_5, tbtx_5)) %>%
  mutate(tbtx_6 = ifelse((is.na(tbtx_6)) & (censor_5 == 0), tmp.summImpute$imputed_mean_6, tbtx_6)) %>%
  mutate(tbtx_7 = ifelse((is.na(tbtx_7)) & (censor_6 == 0), tmp.summImpute$imputed_mean_7, tbtx_7))

# Married and civil status
# check if the missing status are the same
identical(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$Married_0), is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$civilstatus_0))
finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop[which(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$Married_0) != is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$civilstatus_0)), ]
# id 946180 missing civil status with married being 0
identical(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$Married_1), is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$civilstatus_1))
identical(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$Married_2), is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$civilstatus_2))
identical(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$Married_3), is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$civilstatus_3))
identical(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$Married_4), is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$civilstatus_4))
identical(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$Married_5), is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$civilstatus_5))
identical(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$Married_6), is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$civilstatus_6))
identical(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$Married_7), is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$civilstatus_7))
table(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$Married_0)
table(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$civilstatus_0)
table(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$Married_1)
table(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$civilstatus_1)
table(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$Married_2)
table(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$civilstatus_2)
table(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$Married_3)
table(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$civilstatus_3)
unique(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$civilstatus_0)
tmp.summImpute <- data.frame(covariate      = "Married or living w/ partner",
                             n_miss_0       = sum(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$Married_0)),
                             p_miss_0       = mean(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$Married_0)), 
                             imputed_mean_0 = 0,
                             n_miss_1       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_0 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$Married_1)),
                             p_miss_1       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_0 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$Married_1)) /
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_1,
                             imputed_mean_1 = 0,
                             n_miss_2       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_1 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$Married_2), na.rm = T),
                             p_miss_2       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_1 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$Married_2), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_2,
                             imputed_mean_2 = 0,
                             n_miss_3       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_2 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$Married_3), na.rm = T),
                             p_miss_3       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_2 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$Married_3), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_3,
                             imputed_mean_3 = 0,
                             n_miss_4       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_3 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$Married_4), na.rm = T),
                             p_miss_4       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_3 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$Married_4), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_4,
                             imputed_mean_4 = 0,
                             n_miss_5       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_4 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$Married_5), na.rm = T),
                             p_miss_5       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_4 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$Married_5), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_5,
                             imputed_mean_5 = 0,
                             n_miss_6       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_5 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$Married_6), na.rm = T),
                             p_miss_6       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_5 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$Married_6), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_6,
                             imputed_mean_6 = 0,
                             n_miss_7       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_6 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$Married_7), na.rm = T),
                             p_miss_7       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_6 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$Married_7), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_7,
                             imputed_mean_7 = 0)
summImpute.wizmiss.t0Jul2016.switchDTG.wt200d.resPop <- rbind(summImpute.wizmiss.t0Jul2016.switchDTG.wt200d.resPop,
                                                              tmp.summImpute)
finalData.noMiss.t0Jul2016.switchDTG.wt200d <- finalData.noMiss.t0Jul2016.switchDTG.wt200d %>%
  mutate(Married_0 = ifelse(is.na(Married_0), tmp.summImpute$imputed_mean_0, Married_0)) %>%
  mutate(Married_1 = ifelse((is.na(Married_1)) & (censor_0 == 0), tmp.summImpute$imputed_mean_1, Married_1)) %>%
  mutate(Married_2 = ifelse((is.na(Married_2)) & (censor_1 == 0), tmp.summImpute$imputed_mean_2, Married_2)) %>%
  mutate(Married_3 = ifelse((is.na(Married_3)) & (censor_2 == 0), tmp.summImpute$imputed_mean_3, Married_3)) %>%
  mutate(Married_4 = ifelse((is.na(Married_4)) & (censor_3 == 0), tmp.summImpute$imputed_mean_4, Married_4)) %>%
  mutate(Married_5 = ifelse((is.na(Married_5)) & (censor_4 == 0), tmp.summImpute$imputed_mean_5, Married_5)) %>%
  mutate(Married_6 = ifelse((is.na(Married_6)) & (censor_5 == 0), tmp.summImpute$imputed_mean_6, Married_6)) %>%
  mutate(Married_7 = ifelse((is.na(Married_7)) & (censor_6 == 0), tmp.summImpute$imputed_mean_7, Married_7))
# civil status
sum(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$civilstatus_0 == "Never Married and Not Living w/Partner", na.rm = T)
tmp.summImpute <- data.frame(covariate      = "Civil status",
                             n_miss_0       = sum(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$civilstatus_0)),
                             p_miss_0       = mean(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$civilstatus_0)), 
                             imputed_mean_0 = "Never Married and Not Living w/Partner",
                             n_miss_1       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_0 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$civilstatus_1)),
                             p_miss_1       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_0 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$civilstatus_1)) /
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_1,
                             imputed_mean_1 = "Never Married and Not Living w/Partner",
                             n_miss_2       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_1 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$civilstatus_2), na.rm = T),
                             p_miss_2       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_1 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$civilstatus_2), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_2,
                             imputed_mean_2 = "Never Married and Not Living w/Partner",
                             n_miss_3       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_2 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$civilstatus_3), na.rm = T),
                             p_miss_3       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_2 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$civilstatus_3), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_3,
                             imputed_mean_3 = "Never Married and Not Living w/Partner",
                             n_miss_4       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_3 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$civilstatus_4), na.rm = T),
                             p_miss_4       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_3 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$civilstatus_4), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_4,
                             imputed_mean_4 = "Never Married and Not Living w/Partner",
                             n_miss_5       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_4 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$civilstatus_5), na.rm = T),
                             p_miss_5       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_4 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$civilstatus_5), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_5,
                             imputed_mean_5 = "Never Married and Not Living w/Partner",
                             n_miss_6       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_5 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$civilstatus_6), na.rm = T),
                             p_miss_6       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_5 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$civilstatus_6), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_6,
                             imputed_mean_6 = "Never Married and Not Living w/Partner",
                             n_miss_7       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_6 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$civilstatus_7), na.rm = T),
                             p_miss_7       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_6 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$civilstatus_7), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_7,
                             imputed_mean_7 = "Never Married and Not Living w/Partner")
summImpute.wizmiss.t0Jul2016.switchDTG.wt200d.resPop <- rbind(summImpute.wizmiss.t0Jul2016.switchDTG.wt200d.resPop,
                                                              tmp.summImpute)
finalData.noMiss.t0Jul2016.switchDTG.wt200d <- finalData.noMiss.t0Jul2016.switchDTG.wt200d %>%
  mutate(civilstatus_0 = ifelse(is.na(civilstatus_0), tmp.summImpute$imputed_mean_0, as.character(civilstatus_0))) %>%
  mutate(civilstatus_1 = ifelse((is.na(civilstatus_1)) & (censor_0 == 0), tmp.summImpute$imputed_mean_1, as.character(civilstatus_1))) %>%
  mutate(civilstatus_2 = ifelse((is.na(civilstatus_2)) & (censor_1 == 0), tmp.summImpute$imputed_mean_2, as.character(civilstatus_2))) %>%
  mutate(civilstatus_3 = ifelse((is.na(civilstatus_3)) & (censor_2 == 0), tmp.summImpute$imputed_mean_3, as.character(civilstatus_3))) %>%
  mutate(civilstatus_4 = ifelse((is.na(civilstatus_4)) & (censor_3 == 0), tmp.summImpute$imputed_mean_4, as.character(civilstatus_4))) %>%
  mutate(civilstatus_5 = ifelse((is.na(civilstatus_5)) & (censor_4 == 0), tmp.summImpute$imputed_mean_5, as.character(civilstatus_5))) %>%
  mutate(civilstatus_6 = ifelse((is.na(civilstatus_6)) & (censor_5 == 0), tmp.summImpute$imputed_mean_6, as.character(civilstatus_6))) %>%
  mutate(civilstatus_7 = ifelse((is.na(civilstatus_7)) & (censor_6 == 0), tmp.summImpute$imputed_mean_7, as.character(civilstatus_7)))
# table(tmp$civilstatus_3)

# glucose
apply(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop %>% select(starts_with("glucose")), 2, function(x) mean(is.na(x)))
# remove glucose with >90% missing
tmp.summDrop <- data.frame(covariate = "Blood glucose",
                           n_0 = sum(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$glucose_0)),
                           p_0 = mean(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$glucose_0)), 
                           n_1 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_0 == 0) & 
                                       is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$glucose_1)),
                           p_1 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_0 == 0) & 
                                       is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$glucose_1)) /
                             summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_1,
                           n_2 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_1 == 0) & 
                                       is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$glucose_2), na.rm = T),
                           p_2 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_1 == 0) & 
                                       is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$glucose_2), na.rm = T) /
                             summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_2,
                           n_3 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_2 == 0) & 
                                       is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$glucose_3), na.rm = T),
                           p_3 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_2 == 0) & 
                                       is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$glucose_3), na.rm = T) /
                             summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_3,
                           n_4 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_3 == 0) & 
                                       is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$glucose_4), na.rm = T),
                           p_4 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_3 == 0) & 
                                       is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$glucose_4), na.rm = T) /
                             summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_4,
                           n_5 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_4 == 0) & 
                                       is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$glucose_5), na.rm = T),
                           p_5 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_4 == 0) & 
                                       is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$glucose_5), na.rm = T) /
                             summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_5,
                           n_6 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_5 == 0) & 
                                       is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$glucose_6), na.rm = T),
                           p_6 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_5 == 0) & 
                                       is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$glucose_6), na.rm = T) /
                             summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_6,
                           n_7 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_6 == 0) & 
                                       is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$glucose_7), na.rm = T),
                           p_7 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_6 == 0) & 
                                       is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$glucose_7), na.rm = T) /
                             summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_7)
summDrop.wizmiss.t0Jul2016.switchDTG.wt200d.resPop <- rbind(summDrop.wizmiss.t0Jul2016.switchDTG.wt200d.resPop,
                                                            tmp.summDrop)
# fasting glucose
tmp.summDrop <- data.frame(covariate = "Fasting serum glucose",
                           n_0 = sum(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$glucoseFasting_0)),
                           p_0 = mean(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$glucoseFasting_0)), 
                           n_1 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_0 == 0) & 
                                       is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$glucoseFasting_1)),
                           p_1 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_0 == 0) & 
                                       is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$glucoseFasting_1)) /
                             summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_1,
                           n_2 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_1 == 0) & 
                                       is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$glucoseFasting_2), na.rm = T),
                           p_2 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_1 == 0) & 
                                       is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$glucoseFasting_2), na.rm = T) /
                             summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_2,
                           n_3 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_2 == 0) & 
                                       is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$glucoseFasting_3), na.rm = T),
                           p_3 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_2 == 0) & 
                                       is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$glucoseFasting_3), na.rm = T) /
                             summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_3,
                           n_4 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_3 == 0) & 
                                       is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$glucoseFasting_4), na.rm = T),
                           p_4 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_3 == 0) & 
                                       is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$glucoseFasting_4), na.rm = T) /
                             summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_4,
                           n_5 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_4 == 0) & 
                                       is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$glucoseFasting_5), na.rm = T),
                           p_5 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_4 == 0) & 
                                       is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$glucoseFasting_5), na.rm = T) /
                             summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_5,
                           n_6 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_5 == 0) & 
                                       is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$glucoseFasting_6), na.rm = T),
                           p_6 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_5 == 0) & 
                                       is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$glucoseFasting_6), na.rm = T) /
                             summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_6,
                           n_7 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_6 == 0) & 
                                       is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$glucoseFasting_7), na.rm = T),
                           p_7 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_6 == 0) & 
                                       is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$glucoseFasting_7), na.rm = T) /
                             summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_7)
summDrop.wizmiss.t0Jul2016.switchDTG.wt200d.resPop <- rbind(summDrop.wizmiss.t0Jul2016.switchDTG.wt200d.resPop,
                                                            tmp.summDrop)
finalData.noMiss.t0Jul2016.switchDTG.wt200d <- finalData.noMiss.t0Jul2016.switchDTG.wt200d %>%
  select(-starts_with("glucose"))
# colnames(finalData.noMiss.t0Jul2016.switchDTG.wt200d)

# Covered by NHIF
table(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$nhif_0)
sum(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$nhif_0 == "no", na.rm = T)
tmp.summImpute <- data.frame(covariate      = "Covered by NHIF",
                             n_miss_0       = sum(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$nhif_0)),
                             p_miss_0       = mean(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$nhif_0)), 
                             imputed_mean_0 = "no",
                             n_miss_1       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_0 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$nhif_1)),
                             p_miss_1       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_0 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$nhif_1)) /
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_1,
                             imputed_mean_1 = "no",
                             n_miss_2       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_1 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$nhif_2), na.rm = T),
                             p_miss_2       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_1 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$nhif_2), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_2,
                             imputed_mean_2 = "no",
                             n_miss_3       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_2 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$nhif_3), na.rm = T),
                             p_miss_3       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_2 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$nhif_3), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_3,
                             imputed_mean_3 = "no",
                             n_miss_4       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_3 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$nhif_4), na.rm = T),
                             p_miss_4       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_3 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$nhif_4), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_4,
                             imputed_mean_4 = "no",
                             n_miss_5       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_4 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$nhif_5), na.rm = T),
                             p_miss_5       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_4 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$nhif_5), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_5,
                             imputed_mean_5 = "no",
                             n_miss_6       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_5 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$nhif_6), na.rm = T),
                             p_miss_6       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_5 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$nhif_6), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_6,
                             imputed_mean_6 = "no",
                             n_miss_7       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_6 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$nhif_7), na.rm = T),
                             p_miss_7       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$censor_6 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$nhif_7), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_7,
                             imputed_mean_7 = "no")
summImpute.wizmiss.t0Jul2016.switchDTG.wt200d.resPop <- rbind(summImpute.wizmiss.t0Jul2016.switchDTG.wt200d.resPop,
                                                              tmp.summImpute)
finalData.noMiss.t0Jul2016.switchDTG.wt200d <- finalData.noMiss.t0Jul2016.switchDTG.wt200d %>%
  mutate(nhif_0 = ifelse(is.na(nhif_0), tmp.summImpute$imputed_mean_0, as.character(nhif_0))) %>%
  mutate(nhif_1 = ifelse((is.na(nhif_1)) & (censor_0 == 0), tmp.summImpute$imputed_mean_1, as.character(nhif_1))) %>%
  mutate(nhif_2 = ifelse((is.na(nhif_2)) & (censor_1 == 0), tmp.summImpute$imputed_mean_2, as.character(nhif_2))) %>%
  mutate(nhif_3 = ifelse((is.na(nhif_3)) & (censor_2 == 0), tmp.summImpute$imputed_mean_3, as.character(nhif_3))) %>%
  mutate(nhif_4 = ifelse((is.na(nhif_4)) & (censor_3 == 0), tmp.summImpute$imputed_mean_4, as.character(nhif_4))) %>%
  mutate(nhif_5 = ifelse((is.na(nhif_5)) & (censor_4 == 0), tmp.summImpute$imputed_mean_5, as.character(nhif_5))) %>%
  mutate(nhif_6 = ifelse((is.na(nhif_6)) & (censor_5 == 0), tmp.summImpute$imputed_mean_6, as.character(nhif_6))) %>%
  mutate(nhif_7 = ifelse((is.na(nhif_7)) & (censor_6 == 0), tmp.summImpute$imputed_mean_7, as.character(nhif_7)))

apply(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop %>% select(Weight_0:censor_0), 2, function(x) mean(is.na(x)))
apply(finalData.noMiss.t0Jul2016.switchDTG.wt200d %>% select(Weight_0:censor_0), 2, function(x) mean(is.na(x)))

# 100-day
apply(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop %>% select(Weight_0:censor_0), 2, function(x) mean(is.na(x)))
apply(finalData.noMiss.t0Jul2016.switchDTG.wt100d %>% select(Weight_0:censor_0), 2, function(x) mean(is.na(x)))

# remove cd4 with >90% missing
summDrop.wizmiss.t0Jul2016.switchDTG.wt100d.resPop <- data.frame(covariate = "cd4",
                                                                 n_0 = sum(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$cd4_0)),
                                                                 p_0 = mean(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$cd4_0)), 
                                                                 n_1 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_0 == 0) & 
                                                                             is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$cd4_1)),
                                                                 n_2 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_1 == 0) & 
                                                                             is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$cd4_2), na.rm = T),
                                                                 n_3 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_2 == 0) & 
                                                                             is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$cd4_3), na.rm = T), 
                                                                 n_4 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_3 == 0) & 
                                                                             is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$cd4_4), na.rm = T),
                                                                 n_5 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_4 == 0) & 
                                                                             is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$cd4_5), na.rm = T),
                                                                 n_6 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_5 == 0) & 
                                                                             is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$cd4_6), na.rm = T),
                                                                 n_7 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_6 == 0) & 
                                                                             is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$cd4_7), na.rm = T),
                                                                 n_8 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_7 == 0) & 
                                                                             is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$cd4_8), na.rm = T),
                                                                 n_9 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_8 == 0) & 
                                                                             is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$cd4_9), na.rm = T),
                                                                 n_10 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_9 == 0) & 
                                                                              is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$cd4_10), na.rm = T), 
                                                                 n_11 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_10 == 0) & 
                                                                              is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$cd4_11), na.rm = T),
                                                                 n_12 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_11 == 0) & 
                                                                              is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$cd4_12), na.rm = T),
                                                                 n_13 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_12 == 0) & 
                                                                              is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$cd4_13), na.rm = T),
                                                                 n_14 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_13 == 0) & 
                                                                              is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$cd4_14), na.rm = T),
                                                                 n_15 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_14 == 0) & 
                                                                              is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$cd4_15), na.rm = T))
summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop <- summCensor.wizmiss.t0Jul2016.switchDTG.wt100d.resPop %>%
  filter(type == "left")
summDrop.wizmiss.t0Jul2016.switchDTG.wt100d.resPop <- summDrop.wizmiss.t0Jul2016.switchDTG.wt100d.resPop %>%
  mutate(p_1 = n_1 / summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_1,
         p_2 = n_2 / summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_2,
         p_3 = n_3 / summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_3,
         p_4 = n_4 / summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_4,
         p_5 = n_5 / summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_5,
         p_6 = n_6 / summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_6,
         p_7 = n_7 / summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_7,
         p_8 = n_8 / summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_8,
         p_9 = n_9 / summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_9,
         p_10 = n_10 / summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_10,
         p_11 = n_11 / summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_11,
         p_12 = n_12 / summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_12,
         p_13 = n_13 / summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_13,
         p_14 = n_14 / summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_14,
         p_15 = n_15 / summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_15)
summDrop.wizmiss.t0Jul2016.switchDTG.wt100d.resPop <- summDrop.wizmiss.t0Jul2016.switchDTG.wt100d.resPop %>%
  select(covariate:n_1, p_1, n_2, p_2, n_3, p_3, n_4, p_4, n_5, p_5, n_6, p_6, n_7, p_7,
         n_8, p_8, n_9, p_9, n_10, p_10, n_11, p_11, n_12, p_12, n_13, p_13, n_14, p_14, n_15, p_15)
finalData.noMiss.t0Jul2016.switchDTG.wt100d <- finalData.noMiss.t0Jul2016.switchDTG.wt100d %>%
  select(-starts_with("cd4_"))

# viralload 
apply(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop %>% select(starts_with("viralload")), 2, function(x) mean(is.na(x)))
summary(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$viralload_0)
summary(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$viralload_1)
summary(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$viralload_2)
summary(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$viralload_3)
# impute with average
tmp.summImpute <- data.frame(covariate      = "Viral load",
                             n_miss_0       = sum(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$viralload_0)),
                             p_miss_0       = mean(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$viralload_0)),
                             imputed_mean_0 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$viralload_0, na.rm = T),
                             n_miss_1       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_0 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$viralload_1)),
                             p_miss_1       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_0 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$viralload_1)) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_1,
                             imputed_mean_1 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$viralload_1, na.rm = T),
                             n_miss_2       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_1 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$viralload_2), na.rm = T),
                             p_miss_2       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_1 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$viralload_2), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_2, 
                             imputed_mean_2 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$viralload_2, na.rm = T),
                             n_miss_3       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_2 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$viralload_3), na.rm = T),
                             p_miss_3       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_2 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$viralload_3), na.rm = T) /
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_3,
                             imputed_mean_3 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$viralload_3, na.rm = T),
                             n_miss_4       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_3 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$viralload_4), na.rm = T),
                             p_miss_4       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_3 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$viralload_4), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_4,
                             imputed_mean_4 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$viralload_4, na.rm = T),
                             n_miss_5       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_4 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$viralload_5), na.rm = T),
                             p_miss_5       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_4 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$viralload_5), na.rm = T) /
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_5,
                             imputed_mean_5 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$viralload_5, na.rm = T),
                             n_miss_6       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_5 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$viralload_6), na.rm = T),
                             p_miss_6       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_5 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$viralload_6), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_6, 
                             imputed_mean_6 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$viralload_6, na.rm = T),
                             n_miss_7       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_6 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$viralload_7), na.rm = T),
                             p_miss_7       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_6 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$viralload_7), na.rm = T) /
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_7,
                             imputed_mean_7 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$viralload_7, na.rm = T),
                             n_miss_8       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_7 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$viralload_8), na.rm = T),
                             p_miss_8       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_7 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$viralload_8), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_8, 
                             imputed_mean_8 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$viralload_8, na.rm = T),
                             n_miss_9       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_8 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$viralload_9), na.rm = T),
                             p_miss_9       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_8 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$viralload_9), na.rm = T) /
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_9,
                             imputed_mean_9 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$viralload_9, na.rm = T),
                             n_miss_10       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_9 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$viralload_10), na.rm = T),
                             p_miss_10       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_9 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$viralload_10), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_10, 
                             imputed_mean_10 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$viralload_10, na.rm = T),
                             n_miss_11       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_10 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$viralload_11), na.rm = T),
                             p_miss_11       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_10 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$viralload_11), na.rm = T) /
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_11,
                             imputed_mean_11 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$viralload_11, na.rm = T),
                             n_miss_12       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_11 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$viralload_12), na.rm = T),
                             p_miss_12       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_11 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$viralload_12), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_12, 
                             imputed_mean_12 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$viralload_12, na.rm = T),
                             n_miss_13       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_12 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$viralload_13), na.rm = T),
                             p_miss_13       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_12 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$viralload_13), na.rm = T) /
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_13,
                             imputed_mean_13 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$viralload_13, na.rm = T),
                             n_miss_14       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_13 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$viralload_14), na.rm = T),
                             p_miss_14       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_13 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$viralload_14), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_14, 
                             imputed_mean_14 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$viralload_14, na.rm = T),
                             n_miss_15       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_14 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$viralload_15), na.rm = T),
                             p_miss_15       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_14 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$viralload_15), na.rm = T) /
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_15,
                             imputed_mean_15 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$viralload_15, na.rm = T))
summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop <- tmp.summImpute
finalData.noMiss.t0Jul2016.switchDTG.wt100d <- finalData.noMiss.t0Jul2016.switchDTG.wt100d %>%
  mutate(viralloadImptMn_0 = ifelse(is.na(viralload_0), tmp.summImpute$imputed_mean_0, viralload_0)) %>%
  mutate(viralloadImptMn_1 = ifelse((is.na(viralload_1)) & (censor_0 == 0), tmp.summImpute$imputed_mean_1, viralload_1)) %>%
  mutate(viralloadImptMn_2 = ifelse((is.na(viralload_2)) & (censor_1 == 0), tmp.summImpute$imputed_mean_2, viralload_2)) %>%
  mutate(viralloadImptMn_3 = ifelse((is.na(viralload_3)) & (censor_2 == 0), tmp.summImpute$imputed_mean_3, viralload_3)) %>%
  mutate(viralloadImptMn_4 = ifelse((is.na(viralload_4)) & (censor_3 == 0), tmp.summImpute$imputed_mean_4, viralload_4)) %>%
  mutate(viralloadImptMn_5 = ifelse((is.na(viralload_5)) & (censor_4 == 0), tmp.summImpute$imputed_mean_5, viralload_5)) %>%
  mutate(viralloadImptMn_6 = ifelse((is.na(viralload_6)) & (censor_5 == 0), tmp.summImpute$imputed_mean_6, viralload_6)) %>%
  mutate(viralloadImptMn_7 = ifelse((is.na(viralload_7)) & (censor_6 == 0), tmp.summImpute$imputed_mean_7, viralload_7)) %>%
  mutate(viralloadImptMn_8 = ifelse((is.na(viralload_8)) & (censor_7 == 0), tmp.summImpute$imputed_mean_8, viralload_8)) %>%
  mutate(viralloadImptMn_9 = ifelse((is.na(viralload_9)) & (censor_8 == 0), tmp.summImpute$imputed_mean_9, viralload_9)) %>%
  mutate(viralloadImptMn_10 = ifelse((is.na(viralload_10)) & (censor_9 == 0), tmp.summImpute$imputed_mean_10, viralload_10)) %>%
  mutate(viralloadImptMn_11 = ifelse((is.na(viralload_11)) & (censor_10 == 0), tmp.summImpute$imputed_mean_11, viralload_11)) %>%
  mutate(viralloadImptMn_12 = ifelse((is.na(viralload_12)) & (censor_11 == 0), tmp.summImpute$imputed_mean_12, viralload_12)) %>%
  mutate(viralloadImptMn_13 = ifelse((is.na(viralload_13)) & (censor_12 == 0), tmp.summImpute$imputed_mean_13, viralload_13)) %>%
  mutate(viralloadImptMn_14 = ifelse((is.na(viralload_14)) & (censor_13 == 0), tmp.summImpute$imputed_mean_14, viralload_14)) %>%
  mutate(viralloadImptMn_15 = ifelse((is.na(viralload_15)) & (censor_14 == 0), tmp.summImpute$imputed_mean_15, viralload_15))
finalData.noMiss.t0Jul2016.switchDTG.wt100d <- finalData.noMiss.t0Jul2016.switchDTG.wt100d %>%
  mutate(tmpViralload_0 = ifelse(is.na(viralload_0), -1, viralload_0),
         tmpViralload_1 = ifelse(is.na(viralload_1) & (censor_0 == 0), -1, viralload_1),
         tmpViralload_2 = ifelse(is.na(viralload_2) & (censor_1 == 0), -1, viralload_2),
         tmpViralload_3 = ifelse(is.na(viralload_3) & (censor_2 == 0), -1, viralload_3),
         tmpViralload_4 = ifelse(is.na(viralload_4) & (censor_3 == 0), -1, viralload_4),
         tmpViralload_5 = ifelse(is.na(viralload_5) & (censor_4 == 0), -1, viralload_5),
         tmpViralload_6 = ifelse(is.na(viralload_6) & (censor_5 == 0), -1, viralload_6),
         tmpViralload_7 = ifelse(is.na(viralload_7) & (censor_6 == 0), -1, viralload_7),
         tmpViralload_8 = ifelse(is.na(viralload_8) & (censor_7 == 0), -1, viralload_8),
         tmpViralload_9 = ifelse(is.na(viralload_9) & (censor_8 == 0), -1, viralload_9),
         tmpViralload_10 = ifelse(is.na(viralload_10) & (censor_9 == 0), -1, viralload_10),
         tmpViralload_11 = ifelse(is.na(viralload_11) & (censor_10 == 0), -1, viralload_11),
         tmpViralload_12 = ifelse(is.na(viralload_12) & (censor_11 == 0), -1, viralload_12),
         tmpViralload_13 = ifelse(is.na(viralload_13) & (censor_12 == 0), -1, viralload_13),
         tmpViralload_14 = ifelse(is.na(viralload_14) & (censor_13 == 0), -1, viralload_14),
         tmpViralload_15 = ifelse(is.na(viralload_15) & (censor_14 == 0), -1, viralload_15)) 
finalData.noMiss.t0Jul2016.switchDTG.wt100d <- finalData.noMiss.t0Jul2016.switchDTG.wt100d %>%
  mutate(viralloadImptCtgr_0 = ifelse((tmpViralload_0 == -1), 1, NA)) %>%
  mutate(viralloadImptCtgr_0 = ifelse((tmpViralload_0 > -1) & (tmpViralload_0 < 10^3), 2, viralloadImptCtgr_0)) %>%
  mutate(viralloadImptCtgr_0 = ifelse((tmpViralload_0 >= 10^3) & (tmpViralload_0 < 10^4), 3, viralloadImptCtgr_0)) %>%
  mutate(viralloadImptCtgr_0 = ifelse((tmpViralload_0 >= 10^4), 4, viralloadImptCtgr_0)) %>%
  mutate(viralloadImptCtgr_1 = ifelse((tmpViralload_1 == -1), 1, NA)) %>%
  mutate(viralloadImptCtgr_1 = ifelse((tmpViralload_1 > -1) & (tmpViralload_1 < 10^3), 2, viralloadImptCtgr_1)) %>%
  mutate(viralloadImptCtgr_1 = ifelse((tmpViralload_1 >= 10^3) & (tmpViralload_1 < 10^4), 3, viralloadImptCtgr_1)) %>%
  mutate(viralloadImptCtgr_1 = ifelse((tmpViralload_1 >= 10^4), 4, viralloadImptCtgr_1)) %>%
  mutate(viralloadImptCtgr_2 = ifelse((tmpViralload_2 == -1), 1, NA)) %>%
  mutate(viralloadImptCtgr_2 = ifelse((tmpViralload_2 > -1) & (tmpViralload_2 < 10^3), 2, viralloadImptCtgr_2)) %>%
  mutate(viralloadImptCtgr_2 = ifelse((tmpViralload_2 >= 10^3) & (tmpViralload_2 < 10^4), 3, viralloadImptCtgr_2)) %>%
  mutate(viralloadImptCtgr_2 = ifelse((tmpViralload_2 >= 10^4), 4, viralloadImptCtgr_2)) %>%
  mutate(viralloadImptCtgr_3 = ifelse((tmpViralload_3 == -1), 1, NA)) %>%
  mutate(viralloadImptCtgr_3 = ifelse((tmpViralload_3 > -1) & (tmpViralload_3 < 10^3), 2, viralloadImptCtgr_3)) %>%
  mutate(viralloadImptCtgr_3 = ifelse((tmpViralload_3 >= 10^3) & (tmpViralload_3 < 10^4), 3, viralloadImptCtgr_3)) %>%
  mutate(viralloadImptCtgr_3 = ifelse((tmpViralload_3 >= 10^4), 4, viralloadImptCtgr_3)) %>%
  mutate(viralloadImptCtgr_4 = ifelse((tmpViralload_4 == -1), 1, NA)) %>%
  mutate(viralloadImptCtgr_4 = ifelse((tmpViralload_4 > -1) & (tmpViralload_4 < 10^3), 2, viralloadImptCtgr_4)) %>%
  mutate(viralloadImptCtgr_4 = ifelse((tmpViralload_4 >= 10^3) & (tmpViralload_4 < 10^4), 3, viralloadImptCtgr_4)) %>%
  mutate(viralloadImptCtgr_4 = ifelse((tmpViralload_4 >= 10^4), 4, viralloadImptCtgr_4)) %>%
  mutate(viralloadImptCtgr_5 = ifelse((tmpViralload_5 == -1), 1, NA)) %>%
  mutate(viralloadImptCtgr_5 = ifelse((tmpViralload_5 > -1) & (tmpViralload_5 < 10^3), 2, viralloadImptCtgr_5)) %>%
  mutate(viralloadImptCtgr_5 = ifelse((tmpViralload_5 >= 10^3) & (tmpViralload_5 < 10^4), 3, viralloadImptCtgr_5)) %>%
  mutate(viralloadImptCtgr_5 = ifelse((tmpViralload_5 >= 10^4), 4, viralloadImptCtgr_5))  %>%
  mutate(viralloadImptCtgr_6 = ifelse((tmpViralload_6 == -1), 1, NA)) %>%
  mutate(viralloadImptCtgr_6 = ifelse((tmpViralload_6 > -1) & (tmpViralload_6 < 10^3), 2, viralloadImptCtgr_6)) %>%
  mutate(viralloadImptCtgr_6 = ifelse((tmpViralload_6 >= 10^3) & (tmpViralload_6 < 10^4), 3, viralloadImptCtgr_6)) %>%
  mutate(viralloadImptCtgr_6 = ifelse((tmpViralload_6 >= 10^4), 4, viralloadImptCtgr_6)) %>%
  mutate(viralloadImptCtgr_7 = ifelse((tmpViralload_7 == -1), 1, NA)) %>%
  mutate(viralloadImptCtgr_7 = ifelse((tmpViralload_7 > -1) & (tmpViralload_7 < 10^3), 2, viralloadImptCtgr_7)) %>%
  mutate(viralloadImptCtgr_7 = ifelse((tmpViralload_7 >= 10^3) & (tmpViralload_7 < 10^4), 3, viralloadImptCtgr_7)) %>%
  mutate(viralloadImptCtgr_7 = ifelse((tmpViralload_7 >= 10^4), 4, viralloadImptCtgr_7)) %>%
  mutate(viralloadImptCtgr_8 = ifelse((tmpViralload_8 == -1), 1, NA)) %>%
  mutate(viralloadImptCtgr_8 = ifelse((tmpViralload_8 > -1) & (tmpViralload_8 < 10^3), 2, viralloadImptCtgr_8)) %>%
  mutate(viralloadImptCtgr_8 = ifelse((tmpViralload_8 >= 10^3) & (tmpViralload_8 < 10^4), 3, viralloadImptCtgr_8)) %>%
  mutate(viralloadImptCtgr_8 = ifelse((tmpViralload_8 >= 10^4), 4, viralloadImptCtgr_8)) %>%
  mutate(viralloadImptCtgr_9 = ifelse((tmpViralload_9 == -1), 1, NA)) %>%
  mutate(viralloadImptCtgr_9 = ifelse((tmpViralload_9 > -1) & (tmpViralload_9 < 10^3), 2, viralloadImptCtgr_9)) %>%
  mutate(viralloadImptCtgr_9 = ifelse((tmpViralload_9 >= 10^3) & (tmpViralload_9 < 10^4), 3, viralloadImptCtgr_9)) %>%
  mutate(viralloadImptCtgr_9 = ifelse((tmpViralload_9 >= 10^4), 4, viralloadImptCtgr_9))  %>%
  mutate(viralloadImptCtgr_10 = ifelse((tmpViralload_10 == -1), 1, NA)) %>%
  mutate(viralloadImptCtgr_10 = ifelse((tmpViralload_10 > -1) & (tmpViralload_10 < 10^3), 2, viralloadImptCtgr_10)) %>%
  mutate(viralloadImptCtgr_10 = ifelse((tmpViralload_10 >= 10^3) & (tmpViralload_10 < 10^4), 3, viralloadImptCtgr_10)) %>%
  mutate(viralloadImptCtgr_10 = ifelse((tmpViralload_10 >= 10^4), 4, viralloadImptCtgr_10)) %>%
  mutate(viralloadImptCtgr_11 = ifelse((tmpViralload_11 == -1), 1, NA)) %>%
  mutate(viralloadImptCtgr_11 = ifelse((tmpViralload_11 > -1) & (tmpViralload_11 < 10^3), 2, viralloadImptCtgr_11)) %>%
  mutate(viralloadImptCtgr_11 = ifelse((tmpViralload_11 >= 10^3) & (tmpViralload_11 < 10^4), 3, viralloadImptCtgr_11)) %>%
  mutate(viralloadImptCtgr_11 = ifelse((tmpViralload_11 >= 10^4), 4, viralloadImptCtgr_11)) %>%
  mutate(viralloadImptCtgr_12 = ifelse((tmpViralload_12 == -1), 1, NA)) %>%
  mutate(viralloadImptCtgr_12 = ifelse((tmpViralload_12 > -1) & (tmpViralload_12 < 10^3), 2, viralloadImptCtgr_12)) %>%
  mutate(viralloadImptCtgr_12 = ifelse((tmpViralload_12 >= 10^3) & (tmpViralload_12 < 10^4), 3, viralloadImptCtgr_12)) %>%
  mutate(viralloadImptCtgr_12 = ifelse((tmpViralload_12 >= 10^4), 4, viralloadImptCtgr_12)) %>%
  mutate(viralloadImptCtgr_13 = ifelse((tmpViralload_13 == -1), 1, NA)) %>%
  mutate(viralloadImptCtgr_13 = ifelse((tmpViralload_13 > -1) & (tmpViralload_13 < 10^3), 2, viralloadImptCtgr_13)) %>%
  mutate(viralloadImptCtgr_13 = ifelse((tmpViralload_13 >= 10^3) & (tmpViralload_13 < 10^4), 3, viralloadImptCtgr_13)) %>%
  mutate(viralloadImptCtgr_13 = ifelse((tmpViralload_13 >= 10^4), 4, viralloadImptCtgr_13))  %>%
  mutate(viralloadImptCtgr_14 = ifelse((tmpViralload_14 == -1), 1, NA)) %>%
  mutate(viralloadImptCtgr_14 = ifelse((tmpViralload_14 > -1) & (tmpViralload_14 < 10^3), 2, viralloadImptCtgr_14)) %>%
  mutate(viralloadImptCtgr_14 = ifelse((tmpViralload_14 >= 10^3) & (tmpViralload_14 < 10^4), 3, viralloadImptCtgr_14)) %>%
  mutate(viralloadImptCtgr_14 = ifelse((tmpViralload_14 >= 10^4), 4, viralloadImptCtgr_14)) %>%
  mutate(viralloadImptCtgr_15 = ifelse((tmpViralload_15 == -1), 1, NA)) %>%
  mutate(viralloadImptCtgr_15 = ifelse((tmpViralload_15 > -1) & (tmpViralload_15 < 10^3), 2, viralloadImptCtgr_15)) %>%
  mutate(viralloadImptCtgr_15 = ifelse((tmpViralload_15 >= 10^3) & (tmpViralload_15 < 10^4), 3, viralloadImptCtgr_15)) %>%
  mutate(viralloadImptCtgr_15 = ifelse((tmpViralload_15 >= 10^4), 4, viralloadImptCtgr_15)) 
summary(finalData.noMiss.t0Jul2016.switchDTG.wt100d)

# remove log10Viralload with >50% missing in the first time window
apply(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop %>% select(starts_with("log10Viralload")), 2, function(x) mean(is.na(x)))
summary(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$log10Viralload_0)
summary(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$log10Viralload_1)
summary(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$log10Viralload_2)
summary(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$log10Viralload_3)
# impute with average at each time point
tmp.summImpute <- data.frame(covariate      = "log10(viral load + 0.001)",
                             n_miss_0       = sum(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$log10Viralload_0)),
                             p_miss_0       = mean(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$log10Viralload_0)),
                             imputed_mean_0 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$log10Viralload_0, na.rm = T),
                             n_miss_1       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_0 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$log10Viralload_1)),
                             p_miss_1       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_0 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$log10Viralload_1)) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_1,
                             imputed_mean_1 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$log10Viralload_1, na.rm = T),
                             n_miss_2       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_1 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$log10Viralload_2), na.rm = T),
                             p_miss_2       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_1 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$log10Viralload_2), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_2, 
                             imputed_mean_2 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$log10Viralload_2, na.rm = T),
                             n_miss_3       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_2 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$log10Viralload_3), na.rm = T),
                             p_miss_3       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_2 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$log10Viralload_3), na.rm = T) /
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_3,
                             imputed_mean_3 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$log10Viralload_3, na.rm = T),
                             n_miss_4       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_3 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$log10Viralload_4), na.rm = T),
                             p_miss_4       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_3 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$log10Viralload_4), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_4, 
                             imputed_mean_4 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$log10Viralload_4, na.rm = T),
                             n_miss_5       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_4 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$log10Viralload_5), na.rm = T),
                             p_miss_5       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_4 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$log10Viralload_5), na.rm = T) /
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_5,
                             imputed_mean_5 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$log10Viralload_5, na.rm = T),
                             n_miss_6       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_5 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$log10Viralload_6), na.rm = T),
                             p_miss_6       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_5 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$log10Viralload_6), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_6, 
                             imputed_mean_6 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$log10Viralload_6, na.rm = T),
                             n_miss_7       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_6 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$log10Viralload_7), na.rm = T),
                             p_miss_7       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_6 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$log10Viralload_7), na.rm = T) /
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_7,
                             imputed_mean_7 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$log10Viralload_7, na.rm = T),
                             n_miss_8       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_7 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$log10Viralload_8), na.rm = T),
                             p_miss_8       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_7 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$log10Viralload_8), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_8, 
                             imputed_mean_8 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$log10Viralload_8, na.rm = T),
                             n_miss_9       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_8 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$log10Viralload_9), na.rm = T),
                             p_miss_9       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_8 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$log10Viralload_9), na.rm = T) /
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_9,
                             imputed_mean_9 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$log10Viralload_9, na.rm = T),
                             n_miss_10       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_9 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$log10Viralload_10), na.rm = T),
                             p_miss_10       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_9 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$log10Viralload_10), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_10, 
                             imputed_mean_10 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$log10Viralload_10, na.rm = T),
                             n_miss_11       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_10 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$log10Viralload_11), na.rm = T),
                             p_miss_11       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_10 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$log10Viralload_11), na.rm = T) /
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_11,
                             imputed_mean_11 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$log10Viralload_11, na.rm = T),
                             n_miss_12       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_11 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$log10Viralload_12), na.rm = T),
                             p_miss_12       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_11 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$log10Viralload_12), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_12, 
                             imputed_mean_12 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$log10Viralload_12, na.rm = T),
                             n_miss_13       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_12 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$log10Viralload_13), na.rm = T),
                             p_miss_13       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_12 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$log10Viralload_13), na.rm = T) /
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_13,
                             imputed_mean_13 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$log10Viralload_13, na.rm = T),
                             n_miss_14       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_13 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$log10Viralload_14), na.rm = T),
                             p_miss_14       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_13 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$log10Viralload_14), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_14, 
                             imputed_mean_14 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$log10Viralload_14, na.rm = T),
                             n_miss_15       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_14 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$log10Viralload_15), na.rm = T),
                             p_miss_15       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_14 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$log10Viralload_15), na.rm = T) /
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_15,
                             imputed_mean_15 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$log10Viralload_15, na.rm = T))
summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop <- rbind(summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop,
                                                              tmp.summImpute)
finalData.noMiss.t0Jul2016.switchDTG.wt100d <- finalData.noMiss.t0Jul2016.switchDTG.wt100d %>%
  mutate(log10ViralloadImptMn_0 = ifelse(is.na(log10Viralload_0), tmp.summImpute$imputed_mean_0, log10Viralload_0)) %>%
  mutate(log10ViralloadImptMn_1 = ifelse((is.na(log10Viralload_1)) & (censor_0 == 0), tmp.summImpute$imputed_mean_1, log10Viralload_1)) %>%
  mutate(log10ViralloadImptMn_2 = ifelse((is.na(log10Viralload_2)) & (censor_1 == 0), tmp.summImpute$imputed_mean_2, log10Viralload_2)) %>%
  mutate(log10ViralloadImptMn_3 = ifelse((is.na(log10Viralload_3)) & (censor_2 == 0), tmp.summImpute$imputed_mean_3, log10Viralload_3)) %>%
  mutate(log10ViralloadImptMn_4 = ifelse((is.na(log10Viralload_4)) & (censor_3 == 0), tmp.summImpute$imputed_mean_4, log10Viralload_4)) %>%
  mutate(log10ViralloadImptMn_5 = ifelse((is.na(log10Viralload_5)) & (censor_4 == 0), tmp.summImpute$imputed_mean_5, log10Viralload_5)) %>%
  mutate(log10ViralloadImptMn_6 = ifelse((is.na(log10Viralload_6)) & (censor_5 == 0), tmp.summImpute$imputed_mean_6, log10Viralload_6)) %>%
  mutate(log10ViralloadImptMn_7 = ifelse((is.na(log10Viralload_7)) & (censor_6 == 0), tmp.summImpute$imputed_mean_7, log10Viralload_7)) %>%
  mutate(log10ViralloadImptMn_8 = ifelse((is.na(log10Viralload_8)) & (censor_7 == 0), tmp.summImpute$imputed_mean_8, log10Viralload_8)) %>%
  mutate(log10ViralloadImptMn_9 = ifelse((is.na(log10Viralload_9)) & (censor_8 == 0), tmp.summImpute$imputed_mean_9, log10Viralload_9)) %>%
  mutate(log10ViralloadImptMn_10 = ifelse((is.na(log10Viralload_10)) & (censor_9 == 0), tmp.summImpute$imputed_mean_10, log10Viralload_10)) %>%
  mutate(log10ViralloadImptMn_11 = ifelse((is.na(log10Viralload_11)) & (censor_10 == 0), tmp.summImpute$imputed_mean_11, log10Viralload_11)) %>%
  mutate(log10ViralloadImptMn_12 = ifelse((is.na(log10Viralload_12)) & (censor_11 == 0), tmp.summImpute$imputed_mean_12, log10Viralload_12)) %>%
  mutate(log10ViralloadImptMn_13 = ifelse((is.na(log10Viralload_13)) & (censor_12 == 0), tmp.summImpute$imputed_mean_13, log10Viralload_13)) %>%
  mutate(log10ViralloadImptMn_14 = ifelse((is.na(log10Viralload_14)) & (censor_13 == 0), tmp.summImpute$imputed_mean_14, log10Viralload_14)) %>%
  mutate(log10ViralloadImptMn_15 = ifelse((is.na(log10Viralload_15)) & (censor_14 == 0), tmp.summImpute$imputed_mean_15, log10Viralload_15))
summary(finalData.noMiss.t0Jul2016.switchDTG.wt100d)
finalData.noMiss.t0Jul2016.switchDTG.wt100d <- finalData.noMiss.t0Jul2016.switchDTG.wt100d %>%
  select(patient_id:Weight_0, viralloadImptMn_0, log10ViralloadImptMn_0, viralloadImptCtgr_0, 
         SystolicBP_0:Weight_1, viralloadImptMn_1, log10ViralloadImptMn_1, viralloadImptCtgr_1, 
         SystolicBP_1:Weight_2, viralloadImptMn_2, log10ViralloadImptMn_2, viralloadImptCtgr_2, 
         SystolicBP_2:Weight_3, viralloadImptMn_3, log10ViralloadImptMn_3, viralloadImptCtgr_3, 
         SystolicBP_3:Weight_4, viralloadImptMn_4, log10ViralloadImptMn_4, viralloadImptCtgr_4, 
         SystolicBP_4:Weight_5, viralloadImptMn_5, log10ViralloadImptMn_5, viralloadImptCtgr_5, 
         SystolicBP_5:Weight_6, viralloadImptMn_6, log10ViralloadImptMn_6, viralloadImptCtgr_6, 
         SystolicBP_6:Weight_7, viralloadImptMn_7, log10ViralloadImptMn_7, viralloadImptCtgr_7, 
         SystolicBP_7:Weight_8, viralloadImptMn_8, log10ViralloadImptMn_8, viralloadImptCtgr_8, 
         SystolicBP_8:Weight_9, viralloadImptMn_9, log10ViralloadImptMn_9, viralloadImptCtgr_9, 
         SystolicBP_9:Weight_10, viralloadImptMn_10, log10ViralloadImptMn_10, viralloadImptCtgr_10, 
         SystolicBP_10:Weight_11, viralloadImptMn_11, log10ViralloadImptMn_11, viralloadImptCtgr_11, 
         SystolicBP_11:Weight_12, viralloadImptMn_12, log10ViralloadImptMn_12, viralloadImptCtgr_12, 
         SystolicBP_12:Weight_13, viralloadImptMn_13, log10ViralloadImptMn_13, viralloadImptCtgr_13, 
         SystolicBP_13:Weight_14, viralloadImptMn_14, log10ViralloadImptMn_14, viralloadImptCtgr_14, 
         SystolicBP_14:Weight_15, viralloadImptMn_15, log10ViralloadImptMn_15, viralloadImptCtgr_15, 
         SystolicBP_15:Weight_16)

# systolic blood pressure
# impute with average at each time point
summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop <- data.frame(covariate      = "Systolic blood pressure",
                                                                   n_miss_0       = sum(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$SystolicBP_0)),
                                                                   p_miss_0       = mean(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$SystolicBP_0)),
                                                                   imputed_mean_0 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$SystolicBP_0, na.rm = T),
                                                                   n_miss_1       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_0 == 0) & 
                                                                                          is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$SystolicBP_1)),
                                                                   p_miss_1       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_0 == 0) & 
                                                                                          is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$SystolicBP_1)) / 
                                                                     summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_1,
                                                                   imputed_mean_1 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$SystolicBP_1, na.rm = T),
                                                                   n_miss_2       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_1 == 0) & 
                                                                                          is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$SystolicBP_2), na.rm = T),
                                                                   p_miss_2       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_1 == 0) & 
                                                                                          is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$SystolicBP_2), na.rm = T) / 
                                                                     summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_2,
                                                                   imputed_mean_2 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$SystolicBP_2, na.rm = T),
                                                                   n_miss_3       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_2 == 0) & 
                                                                                          is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$SystolicBP_3), na.rm = T),
                                                                   p_miss_3       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_2 == 0) & 
                                                                                          is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$SystolicBP_3), na.rm = T) / 
                                                                     summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_3,
                                                                   imputed_mean_3 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$SystolicBP_3, na.rm = T),
                                                                   n_miss_4       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_3 == 0) & 
                                                                                          is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$SystolicBP_4), na.rm = T),
                                                                   p_miss_4       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_3 == 0) & 
                                                                                          is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$SystolicBP_4), na.rm = T) / 
                                                                     summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_4,
                                                                   imputed_mean_4 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$SystolicBP_4, na.rm = T),
                                                                   n_miss_5       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_4 == 0) & 
                                                                                          is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$SystolicBP_5), na.rm = T),
                                                                   p_miss_5       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_4 == 0) & 
                                                                                          is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$SystolicBP_5), na.rm = T) / 
                                                                     summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_5,
                                                                   imputed_mean_5 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$SystolicBP_5, na.rm = T),
                                                                   n_miss_6       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_5 == 0) & 
                                                                                          is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$SystolicBP_6), na.rm = T),
                                                                   p_miss_6       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_5 == 0) & 
                                                                                          is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$SystolicBP_6), na.rm = T) / 
                                                                     summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_6,
                                                                   imputed_mean_6 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$SystolicBP_6, na.rm = T),
                                                                   n_miss_7       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_6 == 0) & 
                                                                                          is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$SystolicBP_7), na.rm = T),
                                                                   p_miss_7       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_6 == 0) & 
                                                                                          is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$SystolicBP_7), na.rm = T) / 
                                                                     summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_7,
                                                                   imputed_mean_7 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$SystolicBP_7, na.rm = T),
                                                                   n_miss_8       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_7 == 0) & 
                                                                                          is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$SystolicBP_8), na.rm = T),
                                                                   p_miss_8       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_7 == 0) & 
                                                                                          is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$SystolicBP_8), na.rm = T) / 
                                                                     summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_8,
                                                                   imputed_mean_8 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$SystolicBP_8, na.rm = T),
                                                                   n_miss_9       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_8 == 0) & 
                                                                                          is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$SystolicBP_9), na.rm = T),
                                                                   p_miss_9       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_8 == 0) & 
                                                                                          is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$SystolicBP_9), na.rm = T) / 
                                                                     summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_9,
                                                                   imputed_mean_9 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$SystolicBP_9, na.rm = T),
                                                                   n_miss_10       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_9 == 0) & 
                                                                                           is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$SystolicBP_10), na.rm = T),
                                                                   p_miss_10       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_9 == 0) & 
                                                                                           is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$SystolicBP_10), na.rm = T) / 
                                                                     summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_10,
                                                                   imputed_mean_10 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$SystolicBP_10, na.rm = T),
                                                                   n_miss_11       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_10 == 0) & 
                                                                                           is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$SystolicBP_11), na.rm = T),
                                                                   p_miss_11       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_10 == 0) & 
                                                                                           is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$SystolicBP_11), na.rm = T) / 
                                                                     summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_11,
                                                                   imputed_mean_11 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$SystolicBP_11, na.rm = T),
                                                                   n_miss_12       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_11 == 0) & 
                                                                                           is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$SystolicBP_12), na.rm = T),
                                                                   p_miss_12       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_11 == 0) & 
                                                                                           is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$SystolicBP_12), na.rm = T) / 
                                                                     summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_12,
                                                                   imputed_mean_12 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$SystolicBP_12, na.rm = T),
                                                                   n_miss_13       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_12 == 0) & 
                                                                                           is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$SystolicBP_13), na.rm = T),
                                                                   p_miss_13       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_12 == 0) & 
                                                                                           is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$SystolicBP_13), na.rm = T) / 
                                                                     summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_13,
                                                                   imputed_mean_13 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$SystolicBP_13, na.rm = T),
                                                                   n_miss_14       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_13 == 0) & 
                                                                                           is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$SystolicBP_14), na.rm = T),
                                                                   p_miss_14       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_13 == 0) & 
                                                                                           is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$SystolicBP_14), na.rm = T) / 
                                                                     summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_14,
                                                                   imputed_mean_14 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$SystolicBP_14, na.rm = T),
                                                                   n_miss_15       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_14 == 0) & 
                                                                                           is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$SystolicBP_15), na.rm = T),
                                                                   p_miss_15       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_14 == 0) & 
                                                                                           is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$SystolicBP_15), na.rm = T) / 
                                                                     summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_15,
                                                                   imputed_mean_15 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$SystolicBP_15, na.rm = T))
finalData.noMiss.t0Jul2016.switchDTG.wt100d <- finalData.noMiss.t0Jul2016.switchDTG.wt100d %>%
  mutate(SystolicBP_0 = ifelse(is.na(SystolicBP_0), summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$imputed_mean_0, SystolicBP_0)) %>%
  mutate(SystolicBP_1 = ifelse((is.na(SystolicBP_1)) & (censor_0 == 0), summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$imputed_mean_1, SystolicBP_1)) %>%
  mutate(SystolicBP_2 = ifelse((is.na(SystolicBP_2)) & (censor_1 == 0), summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$imputed_mean_2, SystolicBP_2)) %>%
  mutate(SystolicBP_3 = ifelse((is.na(SystolicBP_3)) & (censor_2 == 0), summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$imputed_mean_3, SystolicBP_3)) %>%
  mutate(SystolicBP_4 = ifelse((is.na(SystolicBP_4)) & (censor_3 == 0), summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$imputed_mean_4, SystolicBP_4)) %>%
  mutate(SystolicBP_5 = ifelse((is.na(SystolicBP_5)) & (censor_4 == 0), summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$imputed_mean_5, SystolicBP_5)) %>%
  mutate(SystolicBP_6 = ifelse((is.na(SystolicBP_6)) & (censor_5 == 0), summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$imputed_mean_6, SystolicBP_6)) %>%
  mutate(SystolicBP_7 = ifelse((is.na(SystolicBP_7)) & (censor_6 == 0), summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$imputed_mean_7, SystolicBP_7)) %>%
  mutate(SystolicBP_8 = ifelse((is.na(SystolicBP_8)) & (censor_7 == 0), summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$imputed_mean_8, SystolicBP_8)) %>%
  mutate(SystolicBP_9 = ifelse((is.na(SystolicBP_9)) & (censor_8 == 0), summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$imputed_mean_9, SystolicBP_9)) %>%
  mutate(SystolicBP_10 = ifelse((is.na(SystolicBP_10)) & (censor_9 == 0), summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$imputed_mean_10, SystolicBP_10)) %>%
  mutate(SystolicBP_11 = ifelse((is.na(SystolicBP_11)) & (censor_10 == 0), summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$imputed_mean_11, SystolicBP_11)) %>%
  mutate(SystolicBP_12 = ifelse((is.na(SystolicBP_12)) & (censor_11 == 0), summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$imputed_mean_12, SystolicBP_12)) %>%
  mutate(SystolicBP_13 = ifelse((is.na(SystolicBP_13)) & (censor_12 == 0), summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$imputed_mean_13, SystolicBP_13)) %>%
  mutate(SystolicBP_14 = ifelse((is.na(SystolicBP_14)) & (censor_13 == 0), summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$imputed_mean_14, SystolicBP_14)) %>%
  mutate(SystolicBP_15 = ifelse((is.na(SystolicBP_15)) & (censor_14 == 0), summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$imputed_mean_15, SystolicBP_15))

# expect the same
sum(is.na(finalData.noMiss.t0Jul2016.switchDTG.wt100d$SystolicBP_2))
sum(finalData.noMiss.t0Jul2016.switchDTG.wt100d$censor_1 == 1, na.rm = T) + sum(finalData.noMiss.t0Jul2016.switchDTG.wt100d$censor_0 == 1) 

# diastolic blood pressure
# impute with average at each time point
# mean(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$DiastolicBP_0), na.rm = T)
tmp.summImpute <- data.frame(covariate      = "Diastolic blood pressure",
                             n_miss_0       = sum(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$DiastolicBP_0)),
                             p_miss_0       = mean(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$DiastolicBP_0)),
                             imputed_mean_0 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$DiastolicBP_0, na.rm = T),
                             n_miss_1       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_0 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$DiastolicBP_1)),
                             p_miss_1       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_0 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$DiastolicBP_1)) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_1,
                             imputed_mean_1 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$DiastolicBP_1, na.rm = T),
                             n_miss_2       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_1 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$DiastolicBP_2), na.rm = T),
                             p_miss_2       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_1 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$DiastolicBP_2), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_2,
                             imputed_mean_2 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$DiastolicBP_2, na.rm = T),
                             n_miss_3       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_2 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$DiastolicBP_3), na.rm = T),
                             p_miss_3       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_2 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$DiastolicBP_3), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_3,
                             imputed_mean_3 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$DiastolicBP_3, na.rm = T),
                             n_miss_4       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_3 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$DiastolicBP_4), na.rm = T),
                             p_miss_4       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_3 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$DiastolicBP_4), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_4,
                             imputed_mean_4 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$DiastolicBP_4, na.rm = T),
                             n_miss_5       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_4 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$DiastolicBP_5), na.rm = T),
                             p_miss_5       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_4 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$DiastolicBP_5), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_5,
                             imputed_mean_5 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$DiastolicBP_5, na.rm = T),
                             n_miss_6       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_5 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$DiastolicBP_6), na.rm = T),
                             p_miss_6       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_5 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$DiastolicBP_6), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_6,
                             imputed_mean_6 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$DiastolicBP_6, na.rm = T),
                             n_miss_7       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_6 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$DiastolicBP_7), na.rm = T),
                             p_miss_7       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_6 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$DiastolicBP_7), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_7,
                             imputed_mean_7 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$DiastolicBP_7, na.rm = T),
                             n_miss_8       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_7 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$DiastolicBP_8), na.rm = T),
                             p_miss_8       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_7 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$DiastolicBP_8), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_8,
                             imputed_mean_8 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$DiastolicBP_8, na.rm = T),
                             n_miss_9       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_8 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$DiastolicBP_9), na.rm = T),
                             p_miss_9       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_8 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$DiastolicBP_9), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_9,
                             imputed_mean_9 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$DiastolicBP_9, na.rm = T),
                             n_miss_10       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_9 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$DiastolicBP_10), na.rm = T),
                             p_miss_10       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_9 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$DiastolicBP_10), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_10,
                             imputed_mean_10 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$DiastolicBP_10, na.rm = T),
                             n_miss_11       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_10 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$DiastolicBP_11), na.rm = T),
                             p_miss_11       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_10 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$DiastolicBP_11), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_11,
                             imputed_mean_11 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$DiastolicBP_11, na.rm = T),
                             n_miss_12       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_11 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$DiastolicBP_12), na.rm = T),
                             p_miss_12       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_11 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$DiastolicBP_12), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_12,
                             imputed_mean_12 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$DiastolicBP_12, na.rm = T),
                             n_miss_13       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_12 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$DiastolicBP_13), na.rm = T),
                             p_miss_13       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_12 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$DiastolicBP_13), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_13,
                             imputed_mean_13 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$DiastolicBP_13, na.rm = T),
                             n_miss_14       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_13 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$DiastolicBP_14), na.rm = T),
                             p_miss_14       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_13 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$DiastolicBP_14), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_14,
                             imputed_mean_14 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$DiastolicBP_14, na.rm = T),
                             n_miss_15       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_14 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$DiastolicBP_15), na.rm = T),
                             p_miss_15       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_14 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$DiastolicBP_15), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_15,
                             imputed_mean_15 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$DiastolicBP_15, na.rm = T))
summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop <- rbind(summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop,
                                                              tmp.summImpute)
finalData.noMiss.t0Jul2016.switchDTG.wt100d <- finalData.noMiss.t0Jul2016.switchDTG.wt100d %>%
  mutate(DiastolicBP_0 = ifelse(is.na(DiastolicBP_0), tmp.summImpute$imputed_mean_0, DiastolicBP_0)) %>%
  mutate(DiastolicBP_1 = ifelse((is.na(DiastolicBP_1)) & (censor_0 == 0), tmp.summImpute$imputed_mean_1, DiastolicBP_1)) %>%
  mutate(DiastolicBP_2 = ifelse((is.na(DiastolicBP_2)) & (censor_1 == 0), tmp.summImpute$imputed_mean_2, DiastolicBP_2)) %>%
  mutate(DiastolicBP_3 = ifelse((is.na(DiastolicBP_3)) & (censor_2 == 0), tmp.summImpute$imputed_mean_3, DiastolicBP_3)) %>%
  mutate(DiastolicBP_4 = ifelse((is.na(DiastolicBP_4)) & (censor_3 == 0), tmp.summImpute$imputed_mean_4, DiastolicBP_4)) %>%
  mutate(DiastolicBP_5 = ifelse((is.na(DiastolicBP_5)) & (censor_4 == 0), tmp.summImpute$imputed_mean_5, DiastolicBP_5)) %>%
  mutate(DiastolicBP_6 = ifelse((is.na(DiastolicBP_6)) & (censor_5 == 0), tmp.summImpute$imputed_mean_6, DiastolicBP_6)) %>%
  mutate(DiastolicBP_7 = ifelse((is.na(DiastolicBP_7)) & (censor_6 == 0), tmp.summImpute$imputed_mean_7, DiastolicBP_7)) %>%
  mutate(DiastolicBP_8 = ifelse((is.na(DiastolicBP_8)) & (censor_7 == 0), tmp.summImpute$imputed_mean_8, DiastolicBP_8)) %>%
  mutate(DiastolicBP_9 = ifelse((is.na(DiastolicBP_9)) & (censor_8 == 0), tmp.summImpute$imputed_mean_9, DiastolicBP_9)) %>%
  mutate(DiastolicBP_10 = ifelse((is.na(DiastolicBP_10)) & (censor_9 == 0), tmp.summImpute$imputed_mean_10, DiastolicBP_10)) %>%
  mutate(DiastolicBP_11 = ifelse((is.na(DiastolicBP_11)) & (censor_10 == 0), tmp.summImpute$imputed_mean_11, DiastolicBP_11)) %>%
  mutate(DiastolicBP_12 = ifelse((is.na(DiastolicBP_12)) & (censor_11 == 0), tmp.summImpute$imputed_mean_12, DiastolicBP_12)) %>%
  mutate(DiastolicBP_13 = ifelse((is.na(DiastolicBP_13)) & (censor_12 == 0), tmp.summImpute$imputed_mean_13, DiastolicBP_13)) %>%
  mutate(DiastolicBP_14 = ifelse((is.na(DiastolicBP_14)) & (censor_13 == 0), tmp.summImpute$imputed_mean_14, DiastolicBP_14)) %>%
  mutate(DiastolicBP_15 = ifelse((is.na(DiastolicBP_15)) & (censor_14 == 0), tmp.summImpute$imputed_mean_15, DiastolicBP_15))

# expect the same
sum(is.na(finalData.noMiss.t0Jul2016.switchDTG.wt100d$DiastolicBP_3))
sum(finalData.noMiss.t0Jul2016.switchDTG.wt100d$censor_2 == 1, na.rm = T) +
  sum(finalData.noMiss.t0Jul2016.switchDTG.wt100d$censor_1 == 1, na.rm = T) + 
  sum(finalData.noMiss.t0Jul2016.switchDTG.wt100d$censor_0 == 1) 

# Height
mean(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Height_0), na.rm = T)
summary(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Height_0)
summary(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Height_1)
summary(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Height_2)
summary(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Height_3)
summary(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Height_4)
summary(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Height_5)
summary(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Height_6)
summary(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Height_7)
summary(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Height_8)
summary(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Height_9)
summary(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Height_10)
summary(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Height_11)
summary(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Height_12)
summary(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Height_13)
summary(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Height_14)
summary(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Height_15)
# remove heights that are greater than 240 cm
finalData.noMiss.t0Jul2016.switchDTG.wt100d %>%
  filter((Height_0 > 240) | (Height_1 > 240) | (Height_2 > 240) | (Height_3 > 240) | 
           (Height_4 > 240) | (Height_5 > 240) | (Height_6 > 240) | (Height_7 > 240) | (Height_8 > 240))
finalData.noMiss.t0Jul2016.switchDTG.wt100d <- finalData.noMiss.t0Jul2016.switchDTG.wt100d %>%
  mutate(Height_0 = ifelse(Height_0 > 240, NA, Height_0), 
         Height_1 = ifelse(Height_1 > 240, NA, Height_1), 
         Height_2 = ifelse(Height_2 > 240, NA, Height_2), 
         Height_3 = ifelse(Height_3 > 240, NA, Height_3),
         Height_4 = ifelse(Height_4 > 240, NA, Height_4), 
         Height_5 = ifelse(Height_5 > 240, NA, Height_5), 
         Height_6 = ifelse(Height_6 > 240, NA, Height_6), 
         Height_7 = ifelse(Height_7 > 240, NA, Height_7),
         Height_8 = ifelse(Height_8 > 240, NA, Height_8))
# impute with average at each time point
tmp.summImpute <- data.frame(covariate      = "Height",
                             n_miss_0       = sum(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Height_0)),
                             p_miss_0       = mean(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Height_0)), 
                             imputed_mean_0 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Height_0, na.rm = T),
                             n_miss_1       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_0 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Height_1)),
                             p_miss_1       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_0 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Height_1)) /
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_1,
                             imputed_mean_1 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Height_1, na.rm = T),
                             n_miss_2       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_1 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Height_2), na.rm = T),
                             p_miss_2       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_1 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Height_2), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_2,
                             imputed_mean_2 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Height_2, na.rm = T),
                             n_miss_3       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_2 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Height_3), na.rm = T),
                             p_miss_3       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_2 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Height_3), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_3,
                             imputed_mean_3 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Height_3, na.rm = T),
                             n_miss_4       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_3 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Height_4), na.rm = T),
                             p_miss_4       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_3 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Height_4), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_4,
                             imputed_mean_4 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Height_4, na.rm = T),
                             n_miss_5       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_4 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Height_5), na.rm = T),
                             p_miss_5       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_4 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Height_5), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_5,
                             imputed_mean_5 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Height_5, na.rm = T),
                             n_miss_6       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_5 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Height_6), na.rm = T),
                             p_miss_6       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_5 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Height_6), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_6,
                             imputed_mean_6 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Height_6, na.rm = T),
                             n_miss_7       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_6 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Height_7), na.rm = T),
                             p_miss_7       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_6 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Height_7), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_7,
                             imputed_mean_7 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Height_7, na.rm = T),
                             n_miss_8       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_7 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Height_8), na.rm = T),
                             p_miss_8       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_7 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Height_8), na.rm = T) /
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_8,
                             imputed_mean_8 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Height_8, na.rm = T),
                             n_miss_9       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_8 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Height_9), na.rm = T),
                             p_miss_9       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_8 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Height_9), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_9,
                             imputed_mean_9 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Height_9, na.rm = T),
                             n_miss_10       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_9 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Height_10), na.rm = T),
                             p_miss_10       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_9 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Height_10), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_10,
                             imputed_mean_10 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Height_10, na.rm = T),
                             n_miss_11       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_10 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Height_11), na.rm = T),
                             p_miss_11       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_10 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Height_11), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_11,
                             imputed_mean_11 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Height_11, na.rm = T),
                             n_miss_12       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_11 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Height_12), na.rm = T),
                             p_miss_12       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_11 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Height_12), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_12,
                             imputed_mean_12 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Height_12, na.rm = T),
                             n_miss_13       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_12 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Height_13), na.rm = T),
                             p_miss_13       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_12 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Height_13), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_13,
                             imputed_mean_13 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Height_13, na.rm = T),
                             n_miss_14       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_13 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Height_14), na.rm = T),
                             p_miss_14       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_13 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Height_14), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_14,
                             imputed_mean_14 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Height_14, na.rm = T),
                             n_miss_15       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_14 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Height_15), na.rm = T),
                             p_miss_15       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_14 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Height_15), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_15,
                             imputed_mean_15 = mean(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Height_15, na.rm = T))
summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop <- rbind(summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop,
                                                              tmp.summImpute)
finalData.noMiss.t0Jul2016.switchDTG.wt100d <- finalData.noMiss.t0Jul2016.switchDTG.wt100d %>%
  mutate(Height_0 = ifelse(is.na(Height_0), tmp.summImpute$imputed_mean_0, Height_0)) %>%
  mutate(Height_1 = ifelse((is.na(Height_1)) & (censor_0 == 0), tmp.summImpute$imputed_mean_1, Height_1)) %>%
  mutate(Height_2 = ifelse((is.na(Height_2)) & (censor_1 == 0), tmp.summImpute$imputed_mean_2, Height_2)) %>%
  mutate(Height_3 = ifelse((is.na(Height_3)) & (censor_2 == 0), tmp.summImpute$imputed_mean_3, Height_3)) %>%
  mutate(Height_4 = ifelse((is.na(Height_4)) & (censor_3 == 0), tmp.summImpute$imputed_mean_4, Height_4)) %>%
  mutate(Height_5 = ifelse((is.na(Height_5)) & (censor_4 == 0), tmp.summImpute$imputed_mean_5, Height_5)) %>%
  mutate(Height_6 = ifelse((is.na(Height_6)) & (censor_5 == 0), tmp.summImpute$imputed_mean_6, Height_6)) %>%
  mutate(Height_7 = ifelse((is.na(Height_7)) & (censor_6 == 0), tmp.summImpute$imputed_mean_7, Height_7)) %>%
  mutate(Height_8 = ifelse((is.na(Height_8)) & (censor_7 == 0), tmp.summImpute$imputed_mean_8, Height_8)) %>%
  mutate(Height_9 = ifelse((is.na(Height_9)) & (censor_8 == 0), tmp.summImpute$imputed_mean_9, Height_9)) %>%
  mutate(Height_10 = ifelse((is.na(Height_10)) & (censor_9 == 0), tmp.summImpute$imputed_mean_10, Height_10)) %>%
  mutate(Height_11 = ifelse((is.na(Height_11)) & (censor_10 == 0), tmp.summImpute$imputed_mean_11, Height_11)) %>%
  mutate(Height_12 = ifelse((is.na(Height_12)) & (censor_11 == 0), tmp.summImpute$imputed_mean_12, Height_12)) %>%
  mutate(Height_13 = ifelse((is.na(Height_13)) & (censor_12 == 0), tmp.summImpute$imputed_mean_13, Height_13)) %>%
  mutate(Height_14 = ifelse((is.na(Height_14)) & (censor_13 == 0), tmp.summImpute$imputed_mean_14, Height_14)) %>%
  mutate(Height_15 = ifelse((is.na(Height_15)) & (censor_14 == 0), tmp.summImpute$imputed_mean_15, Height_15))

# pregnancy
unique(finalData.noMiss.t0Jul2016.switchDTG.wt100d$pregnant_0)
# check if pregnancy missing if male
finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop <- finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop %>%
  mutate(male = gsub(" ", "", male))
finalData.noMiss.t0Jul2016.switchDTG.wt100d <- finalData.noMiss.t0Jul2016.switchDTG.wt100d %>%
  mutate(male = gsub(" ", "", male))
ind.male <- (as.character(finalData.noMiss.t0Jul2016.switchDTG.wt100d$male) == "Male")
table(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$male)
# expect 0s
sum(!(ind.male %in% is.na(finalData.noMiss.t0Jul2016.switchDTG.wt100d$pregnant_0)))
sum(!(ind.male %in% is.na(finalData.noMiss.t0Jul2016.switchDTG.wt100d$pregnant_1)))
sum(!(ind.male %in% is.na(finalData.noMiss.t0Jul2016.switchDTG.wt100d$pregnant_2)))
sum(!(ind.male %in% is.na(finalData.noMiss.t0Jul2016.switchDTG.wt100d$pregnant_3)))
sum(!(ind.male %in% is.na(finalData.noMiss.t0Jul2016.switchDTG.wt100d$pregnant_4)))
sum(!(ind.male %in% is.na(finalData.noMiss.t0Jul2016.switchDTG.wt100d$pregnant_5)))
sum(!(ind.male %in% is.na(finalData.noMiss.t0Jul2016.switchDTG.wt100d$pregnant_6)))
sum(!(ind.male %in% is.na(finalData.noMiss.t0Jul2016.switchDTG.wt100d$pregnant_7)))
sum(!(ind.male %in% is.na(finalData.noMiss.t0Jul2016.switchDTG.wt100d$pregnant_8)))
sum(!(ind.male %in% is.na(finalData.noMiss.t0Jul2016.switchDTG.wt100d$pregnant_9)))
sum(!(ind.male %in% is.na(finalData.noMiss.t0Jul2016.switchDTG.wt100d$pregnant_10)))
sum(!(ind.male %in% is.na(finalData.noMiss.t0Jul2016.switchDTG.wt100d$pregnant_11)))
sum(!(ind.male %in% is.na(finalData.noMiss.t0Jul2016.switchDTG.wt100d$pregnant_12)))
sum(!(ind.male %in% is.na(finalData.noMiss.t0Jul2016.switchDTG.wt100d$pregnant_13)))
sum(!(ind.male %in% is.na(finalData.noMiss.t0Jul2016.switchDTG.wt100d$pregnant_14)))
sum(!(ind.male %in% is.na(finalData.noMiss.t0Jul2016.switchDTG.wt100d$pregnant_15)))
# # change pregnancy to 0 if male
# finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop <- finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop %>%
#   mutate(pregnant_0 = ifelse(male == "Male", 0, pregnant_0)) %>%
#   mutate(pregnant_1 = ifelse((male == "Male") & (censor_0 == 0), 0, pregnant_1)) %>%
#   mutate(pregnant_2 = ifelse((male == "Male") & (censor_1 == 0), 0, pregnant_2)) %>%
#   mutate(pregnant_3 = ifelse((male == "Male") & (censor_2 == 0), 0, pregnant_3)) %>%
#   mutate(pregnant_4 = ifelse((male == "Male") & (censor_3 == 0), 0, pregnant_4)) %>%
#   mutate(pregnant_5 = ifelse((male == "Male") & (censor_4 == 0), 0, pregnant_5)) %>%
#   mutate(pregnant_6 = ifelse((male == "Male") & (censor_5 == 0), 0, pregnant_6)) %>%
#   mutate(pregnant_7 = ifelse((male == "Male") & (censor_6 == 0), 0, pregnant_7)) %>%
#   mutate(pregnant_8 = ifelse((male == "Male") & (censor_7 == 0), 0, pregnant_8)) %>%
#   mutate(pregnant_9 = ifelse((male == "Male") & (censor_8 == 0), 0, pregnant_9)) %>%
#   mutate(pregnant_10 = ifelse((male == "Male") & (censor_9 == 0), 0, pregnant_10)) %>%
#   mutate(pregnant_11 = ifelse((male == "Male") & (censor_10 == 0), 0, pregnant_11)) %>%
#   mutate(pregnant_12 = ifelse((male == "Male") & (censor_11 == 0), 0, pregnant_12)) %>%
#   mutate(pregnant_13 = ifelse((male == "Male") & (censor_12 == 0), 0, pregnant_13)) %>%
#   mutate(pregnant_14 = ifelse((male == "Male") & (censor_13 == 0), 0, pregnant_14)) %>%
#   mutate(pregnant_15 = ifelse((male == "Male") & (censor_14 == 0), 0, pregnant_15))  
# finalData.noMiss.t0Jul2016.switchDTG.wt100d <- finalData.noMiss.t0Jul2016.switchDTG.wt100d %>%
#   mutate(pregnant_0 = ifelse(male == "Male", 0, pregnant_0)) %>%
#   mutate(pregnant_1 = ifelse((male == "Male") & (censor_0 == 0), 0, pregnant_1)) %>%
#   mutate(pregnant_2 = ifelse((male == "Male") & (censor_1 == 0), 0, pregnant_2)) %>%
#   mutate(pregnant_3 = ifelse((male == "Male") & (censor_2 == 0), 0, pregnant_3)) %>%
#   mutate(pregnant_4 = ifelse((male == "Male") & (censor_3 == 0), 0, pregnant_4)) %>%
#   mutate(pregnant_5 = ifelse((male == "Male") & (censor_4 == 0), 0, pregnant_5)) %>%
#   mutate(pregnant_6 = ifelse((male == "Male") & (censor_5 == 0), 0, pregnant_6)) %>%
#   mutate(pregnant_7 = ifelse((male == "Male") & (censor_6 == 0), 0, pregnant_7)) %>%
#   mutate(pregnant_8 = ifelse((male == "Male") & (censor_7 == 0), 0, pregnant_8)) %>%
#   mutate(pregnant_9 = ifelse((male == "Male") & (censor_8 == 0), 0, pregnant_9)) %>%
#   mutate(pregnant_10 = ifelse((male == "Male") & (censor_9 == 0), 0, pregnant_10)) %>%
#   mutate(pregnant_11 = ifelse((male == "Male") & (censor_10 == 0), 0, pregnant_11)) %>%
#   mutate(pregnant_12 = ifelse((male == "Male") & (censor_11 == 0), 0, pregnant_12)) %>%
#   mutate(pregnant_13 = ifelse((male == "Male") & (censor_12 == 0), 0, pregnant_13)) %>%
#   mutate(pregnant_14 = ifelse((male == "Male") & (censor_13 == 0), 0, pregnant_14)) %>%
#   mutate(pregnant_15 = ifelse((male == "Male") & (censor_14 == 0), 0, pregnant_15))    
# table(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$pregnant_0)
# sum(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$pregnant_0))
# # impute with 0 at each time point if female still missing
# tmp.summImpute <- data.frame(covariate      = "Pregnant",
#                              n_miss_0       = sum(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$pregnant_0)),
#                              p_miss_0       = mean(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$pregnant_0)), 
#                              imputed_mean_0 = 0,
#                              n_miss_1       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_0 == 0) & 
#                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$pregnant_1)),
#                              p_miss_1       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_0 == 0) & 
#                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$pregnant_1)) /
#                                summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_1,
#                              imputed_mean_1 = 0,
#                              n_miss_2       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_1 == 0) & 
#                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$pregnant_2), na.rm = T),
#                              p_miss_2       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_1 == 0) & 
#                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$pregnant_2), na.rm = T) / 
#                                summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_2,
#                              imputed_mean_2 = 0,
#                              n_miss_3       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_2 == 0) & 
#                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$pregnant_3), na.rm = T),
#                              p_miss_3       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_2 == 0) & 
#                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$pregnant_3), na.rm = T) / 
#                                summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_3,
#                              imputed_mean_3 = 0,
#                              n_miss_4       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_3 == 0) & 
#                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$pregnant_4), na.rm = T),
#                              p_miss_4       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_3 == 0) & 
#                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$pregnant_4), na.rm = T) / 
#                                summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_4,
#                              imputed_mean_4 = 0,
#                              n_miss_5       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_4 == 0) & 
#                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$pregnant_5), na.rm = T),
#                              p_miss_5       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_4 == 0) & 
#                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$pregnant_5), na.rm = T) / 
#                                summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_5,
#                              imputed_mean_5 = 0,
#                              n_miss_6       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_5 == 0) & 
#                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$pregnant_6), na.rm = T),
#                              p_miss_6       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_5 == 0) & 
#                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$pregnant_6), na.rm = T) / 
#                                summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_6,
#                              imputed_mean_6 = 0,
#                              n_miss_7       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_6 == 0) & 
#                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$pregnant_7), na.rm = T),
#                              p_miss_7       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_6 == 0) & 
#                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$pregnant_7), na.rm = T) / 
#                                summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_7,
#                              imputed_mean_7 = 0,
#                              n_miss_8       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_7 == 0) & 
#                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$pregnant_8), na.rm = T),
#                              p_miss_8       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_7 == 0) & 
#                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$pregnant_8), na.rm = T) /
#                                summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_8,
#                              imputed_mean_8 = 0,
#                              n_miss_9       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_8 == 0) & 
#                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$pregnant_9), na.rm = T),
#                              p_miss_9       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_8 == 0) & 
#                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$pregnant_9), na.rm = T) / 
#                                summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_9,
#                              imputed_mean_9 = 0,
#                              n_miss_10       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_9 == 0) & 
#                                                      is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$pregnant_10), na.rm = T),
#                              p_miss_10       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_9 == 0) & 
#                                                      is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$pregnant_10), na.rm = T) / 
#                                summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_10,
#                              imputed_mean_10 = 0,
#                              n_miss_11       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_10 == 0) & 
#                                                      is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$pregnant_11), na.rm = T),
#                              p_miss_11       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_10 == 0) & 
#                                                      is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$pregnant_11), na.rm = T) / 
#                                summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_11,
#                              imputed_mean_11 = 0,
#                              n_miss_12       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_11 == 0) & 
#                                                      is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$pregnant_12), na.rm = T),
#                              p_miss_12       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_11 == 0) & 
#                                                      is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$pregnant_12), na.rm = T) / 
#                                summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_12,
#                              imputed_mean_12 = 0,
#                              n_miss_13       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_12 == 0) & 
#                                                      is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$pregnant_13), na.rm = T),
#                              p_miss_13       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_12 == 0) & 
#                                                      is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$pregnant_13), na.rm = T) / 
#                                summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_13,
#                              imputed_mean_13 = 0,
#                              n_miss_14       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_13 == 0) & 
#                                                      is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$pregnant_14), na.rm = T),
#                              p_miss_14       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_13 == 0) & 
#                                                      is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$pregnant_14), na.rm = T) / 
#                                summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_14,
#                              imputed_mean_14 = 0,
#                              n_miss_15       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_14 == 0) & 
#                                                      is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$pregnant_15), na.rm = T),
#                              p_miss_15       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_14 == 0) & 
#                                                      is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$pregnant_15), na.rm = T) / 
#                                summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_15,
#                              imputed_mean_15 = 0)
# summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop <- rbind(summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop,
#                                                               tmp.summImpute)
# finalData.noMiss.t0Jul2016.switchDTG.wt100d <- finalData.noMiss.t0Jul2016.switchDTG.wt100d %>%
#   mutate(pregnant_0 = ifelse(is.na(pregnant_0), tmp.summImpute$imputed_mean_0, pregnant_0)) %>%
#   mutate(pregnant_1 = ifelse((is.na(pregnant_1)) & (censor_0 == 0), tmp.summImpute$imputed_mean_1, pregnant_1)) %>%
#   mutate(pregnant_2 = ifelse((is.na(pregnant_2)) & (censor_1 == 0), tmp.summImpute$imputed_mean_2, pregnant_2)) %>%
#   mutate(pregnant_3 = ifelse((is.na(pregnant_3)) & (censor_2 == 0), tmp.summImpute$imputed_mean_3, pregnant_3)) %>%
#   mutate(pregnant_4 = ifelse((is.na(pregnant_4)) & (censor_3 == 0), tmp.summImpute$imputed_mean_4, pregnant_4)) %>%
#   mutate(pregnant_5 = ifelse((is.na(pregnant_5)) & (censor_4 == 0), tmp.summImpute$imputed_mean_5, pregnant_5)) %>%
#   mutate(pregnant_6 = ifelse((is.na(pregnant_6)) & (censor_5 == 0), tmp.summImpute$imputed_mean_6, pregnant_6)) %>%
#   mutate(pregnant_7 = ifelse((is.na(pregnant_7)) & (censor_6 == 0), tmp.summImpute$imputed_mean_7, pregnant_7)) %>%
#   mutate(pregnant_8 = ifelse((is.na(pregnant_8)) & (censor_7 == 0), tmp.summImpute$imputed_mean_8, pregnant_8)) %>%
#   mutate(pregnant_9 = ifelse((is.na(pregnant_9)) & (censor_8 == 0), tmp.summImpute$imputed_mean_9, pregnant_9)) %>%
#   mutate(pregnant_10 = ifelse((is.na(pregnant_10)) & (censor_9 == 0), tmp.summImpute$imputed_mean_10, pregnant_10)) %>%
#   mutate(pregnant_11 = ifelse((is.na(pregnant_11)) & (censor_10 == 0), tmp.summImpute$imputed_mean_11, pregnant_11)) %>%
#   mutate(pregnant_12 = ifelse((is.na(pregnant_12)) & (censor_11 == 0), tmp.summImpute$imputed_mean_12, pregnant_12)) %>%
#   mutate(pregnant_13 = ifelse((is.na(pregnant_13)) & (censor_12 == 0), tmp.summImpute$imputed_mean_13, pregnant_13)) %>%
#   mutate(pregnant_14 = ifelse((is.na(pregnant_14)) & (censor_13 == 0), tmp.summImpute$imputed_mean_14, pregnant_14)) %>%
#   mutate(pregnant_15 = ifelse((is.na(pregnant_15)) & (censor_14 == 0), tmp.summImpute$imputed_mean_15, pregnant_15))

# tb and tb treatment
unique(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$activeTB_0)
unique(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$activeTB_1)
unique(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$activeTB_2)
unique(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$activeTB_3)
unique(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$activeTB_4)
unique(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$activeTB_5)
unique(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$activeTB_6)
unique(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$activeTB_7)
unique(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$activeTB_8)
unique(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$activeTB_9)
unique(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$activeTB_10)
unique(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$activeTB_11)
unique(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$activeTB_12)
unique(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$activeTB_13)
unique(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$activeTB_14)
unique(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$activeTB_15)
finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop <- finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop %>%
  mutate(activeTB_0 = ifelse(is.na(activeTB_0), 0, activeTB_0),
         activeTB_1 = ifelse(is.na(activeTB_1) & (censor_0 == 0), 0, activeTB_1),
         activeTB_2 = ifelse(is.na(activeTB_2) & (censor_1 == 0), 0, activeTB_2),
         activeTB_3 = ifelse(is.na(activeTB_3) & (censor_2 == 0), 0, activeTB_3),
         activeTB_4 = ifelse(is.na(activeTB_4) & (censor_3 == 0), 0, activeTB_4),
         activeTB_5 = ifelse(is.na(activeTB_5) & (censor_4 == 0), 0, activeTB_5),
         activeTB_6 = ifelse(is.na(activeTB_6) & (censor_5 == 0), 0, activeTB_6),
         activeTB_7 = ifelse(is.na(activeTB_7) & (censor_6 == 0), 0, activeTB_7),
         activeTB_8 = ifelse(is.na(activeTB_8) & (censor_7 == 0), 0, activeTB_8),
         activeTB_9 = ifelse(is.na(activeTB_9) & (censor_8 == 0), 0, activeTB_9),
         activeTB_10 = ifelse(is.na(activeTB_10) & (censor_9 == 0), 0, activeTB_10),
         activeTB_11 = ifelse(is.na(activeTB_11) & (censor_10 == 0), 0, activeTB_11),
         activeTB_12 = ifelse(is.na(activeTB_12) & (censor_11 == 0), 0, activeTB_12),
         activeTB_13 = ifelse(is.na(activeTB_13) & (censor_12 == 0), 0, activeTB_13),
         activeTB_14 = ifelse(is.na(activeTB_14) & (censor_13 == 0), 0, activeTB_14),
         activeTB_15 = ifelse(is.na(activeTB_15) & (censor_14 == 0), 0, activeTB_15))
finalData.noMiss.t0Jul2016.switchDTG.wt100d <- finalData.noMiss.t0Jul2016.switchDTG.wt100d %>%
  mutate(activeTB_0 = ifelse(is.na(activeTB_0), 0, activeTB_0),
         activeTB_1 = ifelse(is.na(activeTB_1) & (censor_0 == 0), 0, activeTB_1),
         activeTB_2 = ifelse(is.na(activeTB_2) & (censor_1 == 0), 0, activeTB_2),
         activeTB_3 = ifelse(is.na(activeTB_3) & (censor_2 == 0), 0, activeTB_3),
         activeTB_4 = ifelse(is.na(activeTB_4) & (censor_3 == 0), 0, activeTB_4),
         activeTB_5 = ifelse(is.na(activeTB_5) & (censor_4 == 0), 0, activeTB_5),
         activeTB_6 = ifelse(is.na(activeTB_6) & (censor_5 == 0), 0, activeTB_6),
         activeTB_7 = ifelse(is.na(activeTB_7) & (censor_6 == 0), 0, activeTB_7),
         activeTB_8 = ifelse(is.na(activeTB_8) & (censor_7 == 0), 0, activeTB_8),
         activeTB_9 = ifelse(is.na(activeTB_9) & (censor_8 == 0), 0, activeTB_9),
         activeTB_10 = ifelse(is.na(activeTB_10) & (censor_9 == 0), 0, activeTB_10),
         activeTB_11 = ifelse(is.na(activeTB_11) & (censor_10 == 0), 0, activeTB_11),
         activeTB_12 = ifelse(is.na(activeTB_12) & (censor_11 == 0), 0, activeTB_12),
         activeTB_13 = ifelse(is.na(activeTB_13) & (censor_12 == 0), 0, activeTB_13),
         activeTB_14 = ifelse(is.na(activeTB_14) & (censor_13 == 0), 0, activeTB_14),
         activeTB_15 = ifelse(is.na(activeTB_15) & (censor_14 == 0), 0, activeTB_15))
# Active TB no missing data
tmp.summImpute <- data.frame(covariate      = "Active TB",
                             n_miss_0       = sum(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$activeTB_0)),
                             p_miss_0       = mean(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$activeTB_0)), 
                             imputed_mean_0 = NA,
                             n_miss_1       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_0 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$activeTB_1)),
                             p_miss_1       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_0 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$activeTB_1)) /
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_1,
                             imputed_mean_1 = NA,
                             n_miss_2       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_1 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$activeTB_2), na.rm = T),
                             p_miss_2       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_1 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$activeTB_2), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_2,
                             imputed_mean_2 = NA,
                             n_miss_3       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_2 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$activeTB_3), na.rm = T),
                             p_miss_3       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_2 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$activeTB_3), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_3,
                             imputed_mean_3 = NA,
                             n_miss_4       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_3 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$activeTB_4), na.rm = T),
                             p_miss_4       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_3 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$activeTB_4), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_4,
                             imputed_mean_4 = NA,
                             n_miss_5       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_4 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$activeTB_5), na.rm = T),
                             p_miss_5       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_4 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$activeTB_5), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_5,
                             imputed_mean_5 = NA,
                             n_miss_6       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_5 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$activeTB_6), na.rm = T),
                             p_miss_6       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_5 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$activeTB_6), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_6,
                             imputed_mean_6 = NA,
                             n_miss_7       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_6 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$activeTB_7), na.rm = T),
                             p_miss_7       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_6 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$activeTB_7), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_7,
                             imputed_mean_7 = NA,
                             n_miss_8       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_7 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$activeTB_8), na.rm = T),
                             p_miss_8       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_7 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$activeTB_8), na.rm = T) /
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_8,
                             imputed_mean_8 = NA,
                             n_miss_9       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_8 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$activeTB_9), na.rm = T),
                             p_miss_9       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_8 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$activeTB_9), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_9,
                             imputed_mean_9 = NA,
                             n_miss_10       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_9 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$activeTB_10), na.rm = T),
                             p_miss_10       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_9 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$activeTB_10), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_10,
                             imputed_mean_10 = NA,
                             n_miss_11       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_10 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$activeTB_11), na.rm = T),
                             p_miss_11       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_10 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$activeTB_11), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_11,
                             imputed_mean_11 = NA,
                             n_miss_12       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_11 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$activeTB_12), na.rm = T),
                             p_miss_12       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_11 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$activeTB_12), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_12,
                             imputed_mean_12 = NA,
                             n_miss_13       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_12 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$activeTB_13), na.rm = T),
                             p_miss_13       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_12 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$activeTB_13), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_13,
                             imputed_mean_13 = NA,
                             n_miss_14       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_13 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$activeTB_14), na.rm = T),
                             p_miss_14       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_13 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$activeTB_14), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_14,
                             imputed_mean_14 = NA,
                             n_miss_15       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_14 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$activeTB_15), na.rm = T),
                             p_miss_15       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_14 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$activeTB_15), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_15,
                             imputed_mean_15 = NA)
summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop <- rbind(summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop,
                                                              tmp.summImpute)
# tbtx
# expect 0: check if there are any pts on tbtx when not active TB
sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$tbtx_0 == 1) & 
      (finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$activeTB_0 == 0), na.rm = T)
sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$tbtx_1 == 1) & 
      (finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$activeTB_1 == 0), na.rm = T)
sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$tbtx_2 == 1) & 
      (finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$activeTB_2 == 0), na.rm = T)
sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$tbtx_3 == 1) & 
      (finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$activeTB_3 == 0), na.rm = T)
sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$tbtx_4 == 1) & 
      (finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$activeTB_4 == 0), na.rm = T)
sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$tbtx_5 == 1) & 
      (finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$activeTB_5 == 0), na.rm = T)
sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$tbtx_6 == 1) & 
      (finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$activeTB_6 == 0), na.rm = T)
sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$tbtx_7 == 1) & 
      (finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$activeTB_7 == 0), na.rm = T)
sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$tbtx_8 == 1) & 
      (finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$activeTB_8 == 0), na.rm = T)
sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$tbtx_9 == 1) & 
      (finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$activeTB_9 == 0), na.rm = T)
sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$tbtx_10 == 1) & 
      (finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$activeTB_10 == 0), na.rm = T)
sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$tbtx_11 == 1) & 
      (finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$activeTB_11 == 0), na.rm = T)
sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$tbtx_12 == 1) & 
      (finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$activeTB_12 == 0), na.rm = T)
sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$tbtx_13 == 1) & 
      (finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$activeTB_13 == 0), na.rm = T)
sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$tbtx_14 == 1) & 
      (finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$activeTB_14 == 0), na.rm = T)
sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$tbtx_15 == 1) & 
      (finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$activeTB_15 == 0), na.rm = T)
tmp.summImpute <- data.frame(covariate      = "TB treatment",
                             n_miss_0       = sum(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$tbtx_0)),
                             p_miss_0       = mean(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$tbtx_0)), 
                             imputed_mean_0 = 0,
                             n_miss_1       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_0 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$tbtx_1)),
                             p_miss_1       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_0 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$tbtx_1)) /
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_1,
                             imputed_mean_1 = 0,
                             n_miss_2       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_1 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$tbtx_2), na.rm = T),
                             p_miss_2       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_1 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$tbtx_2), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_2,
                             imputed_mean_2 = 0,
                             n_miss_3       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_2 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$tbtx_3), na.rm = T),
                             p_miss_3       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_2 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$tbtx_3), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_3,
                             imputed_mean_3 = 0,
                             n_miss_4       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_3 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$tbtx_4), na.rm = T),
                             p_miss_4       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_3 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$tbtx_4), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_4,
                             imputed_mean_4 = 0,
                             n_miss_5       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_4 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$tbtx_5), na.rm = T),
                             p_miss_5       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_4 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$tbtx_5), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_5,
                             imputed_mean_5 = 0,
                             n_miss_6       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_5 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$tbtx_6), na.rm = T),
                             p_miss_6       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_5 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$tbtx_6), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_6,
                             imputed_mean_6 = 0,
                             n_miss_7       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_6 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$tbtx_7), na.rm = T),
                             p_miss_7       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_6 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$tbtx_7), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_7,
                             imputed_mean_7 = 0,
                             n_miss_8       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_7 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$tbtx_8), na.rm = T),
                             p_miss_8       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_7 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$tbtx_8), na.rm = T) /
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_8,
                             imputed_mean_8 = 0,
                             n_miss_9       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_8 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$tbtx_9), na.rm = T),
                             p_miss_9       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_8 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$tbtx_9), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_9,
                             imputed_mean_9 = 0,
                             n_miss_10       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_9 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$tbtx_10), na.rm = T),
                             p_miss_10       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_9 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$tbtx_10), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_10,
                             imputed_mean_10 = 0,
                             n_miss_11       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_10 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$tbtx_11), na.rm = T),
                             p_miss_11       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_10 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$tbtx_11), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_11,
                             imputed_mean_11 = 0,
                             n_miss_12       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_11 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$tbtx_12), na.rm = T),
                             p_miss_12       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_11 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$tbtx_12), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_12,
                             imputed_mean_12 = 0,
                             n_miss_13       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_12 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$tbtx_13), na.rm = T),
                             p_miss_13       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_12 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$tbtx_13), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_13,
                             imputed_mean_13 = 0,
                             n_miss_14       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_13 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$tbtx_14), na.rm = T),
                             p_miss_14       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_13 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$tbtx_14), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_14,
                             imputed_mean_14 = 0,
                             n_miss_15       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_14 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$tbtx_15), na.rm = T),
                             p_miss_15       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_14 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$tbtx_15), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_15,
                             imputed_mean_15 = 0)
summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop <- rbind(summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop,
                                                              tmp.summImpute)
finalData.noMiss.t0Jul2016.switchDTG.wt100d <- finalData.noMiss.t0Jul2016.switchDTG.wt100d %>%
  mutate(tbtx_0 = ifelse(is.na(tbtx_0), tmp.summImpute$imputed_mean_0, tbtx_0)) %>%
  mutate(tbtx_1 = ifelse((is.na(tbtx_1)) & (censor_0 == 0), tmp.summImpute$imputed_mean_1, tbtx_1)) %>%
  mutate(tbtx_2 = ifelse((is.na(tbtx_2)) & (censor_1 == 0), tmp.summImpute$imputed_mean_2, tbtx_2)) %>%
  mutate(tbtx_3 = ifelse((is.na(tbtx_3)) & (censor_2 == 0), tmp.summImpute$imputed_mean_3, tbtx_3)) %>%
  mutate(tbtx_4 = ifelse((is.na(tbtx_4)) & (censor_3 == 0), tmp.summImpute$imputed_mean_4, tbtx_4)) %>%
  mutate(tbtx_5 = ifelse((is.na(tbtx_5)) & (censor_4 == 0), tmp.summImpute$imputed_mean_5, tbtx_5)) %>%
  mutate(tbtx_6 = ifelse((is.na(tbtx_6)) & (censor_5 == 0), tmp.summImpute$imputed_mean_6, tbtx_6)) %>%
  mutate(tbtx_7 = ifelse((is.na(tbtx_7)) & (censor_6 == 0), tmp.summImpute$imputed_mean_7, tbtx_7)) %>%
  mutate(tbtx_8 = ifelse((is.na(tbtx_8)) & (censor_7 == 0), tmp.summImpute$imputed_mean_8, tbtx_8)) %>%
  mutate(tbtx_9 = ifelse((is.na(tbtx_9)) & (censor_8 == 0), tmp.summImpute$imputed_mean_9, tbtx_9)) %>%
  mutate(tbtx_10 = ifelse((is.na(tbtx_10)) & (censor_9 == 0), tmp.summImpute$imputed_mean_10, tbtx_10)) %>%
  mutate(tbtx_11 = ifelse((is.na(tbtx_11)) & (censor_10 == 0), tmp.summImpute$imputed_mean_11, tbtx_11)) %>%
  mutate(tbtx_12 = ifelse((is.na(tbtx_12)) & (censor_11 == 0), tmp.summImpute$imputed_mean_12, tbtx_12)) %>%
  mutate(tbtx_13 = ifelse((is.na(tbtx_13)) & (censor_12 == 0), tmp.summImpute$imputed_mean_13, tbtx_13)) %>%
  mutate(tbtx_14 = ifelse((is.na(tbtx_14)) & (censor_13 == 0), tmp.summImpute$imputed_mean_14, tbtx_14)) %>%
  mutate(tbtx_15 = ifelse((is.na(tbtx_15)) & (censor_14 == 0), tmp.summImpute$imputed_mean_15, tbtx_15))

# Married and civil status
# check if the missing status are the same
identical(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Married_0), is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$civilstatus_0))
finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop[which(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Married_0) != is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$civilstatus_0)), ]
# id 946180 missing civil status with married being 0
identical(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Married_1), is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$civilstatus_1))
identical(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Married_2), is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$civilstatus_2))
identical(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Married_3), is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$civilstatus_3))
identical(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Married_4), is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$civilstatus_4))
identical(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Married_5), is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$civilstatus_5))
identical(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Married_6), is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$civilstatus_6))
identical(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Married_7), is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$civilstatus_7))
table(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Married_0)
table(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$civilstatus_0)
table(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Married_1)
table(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$civilstatus_1)
table(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Married_2)
table(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$civilstatus_2)
table(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Married_3)
table(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$civilstatus_3)
unique(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$civilstatus_0)
tmp.summImpute <- data.frame(covariate      = "Married or living w/ partner",
                             n_miss_0       = sum(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Married_0)),
                             p_miss_0       = mean(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Married_0)), 
                             imputed_mean_0 = 0,
                             n_miss_1       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_0 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Married_1)),
                             p_miss_1       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_0 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Married_1)) /
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_1,
                             imputed_mean_1 = 0,
                             n_miss_2       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_1 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Married_2), na.rm = T),
                             p_miss_2       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_1 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Married_2), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_2,
                             imputed_mean_2 = 0,
                             n_miss_3       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_2 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Married_3), na.rm = T),
                             p_miss_3       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_2 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Married_3), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_3,
                             imputed_mean_3 = 0,
                             n_miss_4       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_3 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Married_4), na.rm = T),
                             p_miss_4       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_3 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Married_4), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_4,
                             imputed_mean_4 = 0,
                             n_miss_5       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_4 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Married_5), na.rm = T),
                             p_miss_5       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_4 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Married_5), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_5,
                             imputed_mean_5 = 0,
                             n_miss_6       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_5 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Married_6), na.rm = T),
                             p_miss_6       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_5 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Married_6), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_6,
                             imputed_mean_6 = 0,
                             n_miss_7       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_6 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Married_7), na.rm = T),
                             p_miss_7       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_6 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Married_7), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_7,
                             imputed_mean_7 = 0,
                             n_miss_8       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_7 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Married_8), na.rm = T),
                             p_miss_8       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_7 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Married_8), na.rm = T) /
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_8,
                             imputed_mean_8 = 0,
                             n_miss_9       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_8 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Married_9), na.rm = T),
                             p_miss_9       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_8 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Married_9), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_9,
                             imputed_mean_9 = 0,
                             n_miss_10       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_9 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Married_10), na.rm = T),
                             p_miss_10       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_9 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Married_10), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_10,
                             imputed_mean_10 = 0,
                             n_miss_11       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_10 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Married_11), na.rm = T),
                             p_miss_11       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_10 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Married_11), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_11,
                             imputed_mean_11 = 0,
                             n_miss_12       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_11 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Married_12), na.rm = T),
                             p_miss_12       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_11 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Married_12), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_12,
                             imputed_mean_12 = 0,
                             n_miss_13       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_12 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Married_13), na.rm = T),
                             p_miss_13       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_12 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Married_13), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_13,
                             imputed_mean_13 = 0,
                             n_miss_14       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_13 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Married_14), na.rm = T),
                             p_miss_14       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_13 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Married_14), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_14,
                             imputed_mean_14 = 0,
                             n_miss_15       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_14 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Married_15), na.rm = T),
                             p_miss_15       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_14 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$Married_15), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_15,
                             imputed_mean_15 = 0)
summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop <- rbind(summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop,
                                                              tmp.summImpute)
finalData.noMiss.t0Jul2016.switchDTG.wt100d <- finalData.noMiss.t0Jul2016.switchDTG.wt100d %>%
  mutate(Married_0 = ifelse(is.na(Married_0), tmp.summImpute$imputed_mean_0, Married_0)) %>%
  mutate(Married_1 = ifelse((is.na(Married_1)) & (censor_0 == 0), tmp.summImpute$imputed_mean_1, Married_1)) %>%
  mutate(Married_2 = ifelse((is.na(Married_2)) & (censor_1 == 0), tmp.summImpute$imputed_mean_2, Married_2)) %>%
  mutate(Married_3 = ifelse((is.na(Married_3)) & (censor_2 == 0), tmp.summImpute$imputed_mean_3, Married_3)) %>%
  mutate(Married_4 = ifelse((is.na(Married_4)) & (censor_3 == 0), tmp.summImpute$imputed_mean_4, Married_4)) %>%
  mutate(Married_5 = ifelse((is.na(Married_5)) & (censor_4 == 0), tmp.summImpute$imputed_mean_5, Married_5)) %>%
  mutate(Married_6 = ifelse((is.na(Married_6)) & (censor_5 == 0), tmp.summImpute$imputed_mean_6, Married_6)) %>%
  mutate(Married_7 = ifelse((is.na(Married_7)) & (censor_6 == 0), tmp.summImpute$imputed_mean_7, Married_7)) %>%
  mutate(Married_8 = ifelse((is.na(Married_8)) & (censor_7 == 0), tmp.summImpute$imputed_mean_8, Married_8)) %>%
  mutate(Married_9 = ifelse((is.na(Married_9)) & (censor_8 == 0), tmp.summImpute$imputed_mean_9, Married_9)) %>%
  mutate(Married_10 = ifelse((is.na(Married_10)) & (censor_9 == 0), tmp.summImpute$imputed_mean_10, Married_10)) %>%
  mutate(Married_11 = ifelse((is.na(Married_11)) & (censor_10 == 0), tmp.summImpute$imputed_mean_11, Married_11)) %>%
  mutate(Married_12 = ifelse((is.na(Married_12)) & (censor_11 == 0), tmp.summImpute$imputed_mean_12, Married_12)) %>%
  mutate(Married_13 = ifelse((is.na(Married_13)) & (censor_12 == 0), tmp.summImpute$imputed_mean_13, Married_13)) %>%
  mutate(Married_14 = ifelse((is.na(Married_14)) & (censor_13 == 0), tmp.summImpute$imputed_mean_14, Married_14)) %>%
  mutate(Married_15 = ifelse((is.na(Married_15)) & (censor_14 == 0), tmp.summImpute$imputed_mean_15, Married_15))
# civil status
sum(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$civilstatus_0 == "Never Married and Not Living w/Partner", na.rm = T)
tmp.summImpute <- data.frame(covariate      = "Civil status",
                             n_miss_0       = sum(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$civilstatus_0)),
                             p_miss_0       = mean(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$civilstatus_0)), 
                             imputed_mean_0 = "Never Married and Not Living w/Partner",
                             n_miss_1       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_0 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$civilstatus_1)),
                             p_miss_1       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_0 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$civilstatus_1)) /
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_1,
                             imputed_mean_1 = "Never Married and Not Living w/Partner",
                             n_miss_2       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_1 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$civilstatus_2), na.rm = T),
                             p_miss_2       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_1 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$civilstatus_2), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_2,
                             imputed_mean_2 = "Never Married and Not Living w/Partner",
                             n_miss_3       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_2 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$civilstatus_3), na.rm = T),
                             p_miss_3       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_2 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$civilstatus_3), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_3,
                             imputed_mean_3 = "Never Married and Not Living w/Partner",
                             n_miss_4       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_3 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$civilstatus_4), na.rm = T),
                             p_miss_4       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_3 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$civilstatus_4), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_4,
                             imputed_mean_4 = "Never Married and Not Living w/Partner",
                             n_miss_5       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_4 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$civilstatus_5), na.rm = T),
                             p_miss_5       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_4 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$civilstatus_5), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_5,
                             imputed_mean_5 = "Never Married and Not Living w/Partner",
                             n_miss_6       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_5 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$civilstatus_6), na.rm = T),
                             p_miss_6       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_5 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$civilstatus_6), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_6,
                             imputed_mean_6 = "Never Married and Not Living w/Partner",
                             n_miss_7       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_6 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$civilstatus_7), na.rm = T),
                             p_miss_7       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_6 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$civilstatus_7), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_7,
                             imputed_mean_7 = "Never Married and Not Living w/Partner",
                             n_miss_8       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_7 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$civilstatus_8), na.rm = T),
                             p_miss_8       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_7 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$civilstatus_8), na.rm = T) /
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_8,
                             imputed_mean_8 = "Never Married and Not Living w/Partner",
                             n_miss_9       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_8 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$civilstatus_9), na.rm = T),
                             p_miss_9       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_8 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$civilstatus_9), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_9,
                             imputed_mean_9 = "Never Married and Not Living w/Partner",
                             n_miss_10       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_9 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$civilstatus_10), na.rm = T),
                             p_miss_10       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_9 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$civilstatus_10), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_10,
                             imputed_mean_10 = "Never Married and Not Living w/Partner",
                             n_miss_11       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_10 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$civilstatus_11), na.rm = T),
                             p_miss_11       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_10 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$civilstatus_11), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_11,
                             imputed_mean_11 = "Never Married and Not Living w/Partner",
                             n_miss_12       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_11 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$civilstatus_12), na.rm = T),
                             p_miss_12       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_11 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$civilstatus_12), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_12,
                             imputed_mean_12 = "Never Married and Not Living w/Partner",
                             n_miss_13       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_12 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$civilstatus_13), na.rm = T),
                             p_miss_13       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_12 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$civilstatus_13), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_13,
                             imputed_mean_13 = "Never Married and Not Living w/Partner",
                             n_miss_14       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_13 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$civilstatus_14), na.rm = T),
                             p_miss_14       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_13 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$civilstatus_14), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_14,
                             imputed_mean_14 = "Never Married and Not Living w/Partner",
                             n_miss_15       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_14 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$civilstatus_15), na.rm = T),
                             p_miss_15       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_14 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$civilstatus_15), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_15,
                             imputed_mean_15 = "Never Married and Not Living w/Partner")
summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop <- rbind(summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop,
                                                              tmp.summImpute)
finalData.noMiss.t0Jul2016.switchDTG.wt100d <- finalData.noMiss.t0Jul2016.switchDTG.wt100d %>%
  mutate(civilstatus_0 = ifelse(is.na(civilstatus_0), tmp.summImpute$imputed_mean_0, as.character(civilstatus_0))) %>%
  mutate(civilstatus_1 = ifelse((is.na(civilstatus_1)) & (censor_0 == 0), tmp.summImpute$imputed_mean_1, as.character(civilstatus_1))) %>%
  mutate(civilstatus_2 = ifelse((is.na(civilstatus_2)) & (censor_1 == 0), tmp.summImpute$imputed_mean_2, as.character(civilstatus_2))) %>%
  mutate(civilstatus_3 = ifelse((is.na(civilstatus_3)) & (censor_2 == 0), tmp.summImpute$imputed_mean_3, as.character(civilstatus_3))) %>%
  mutate(civilstatus_4 = ifelse((is.na(civilstatus_4)) & (censor_3 == 0), tmp.summImpute$imputed_mean_4, as.character(civilstatus_4))) %>%
  mutate(civilstatus_5 = ifelse((is.na(civilstatus_5)) & (censor_4 == 0), tmp.summImpute$imputed_mean_5, as.character(civilstatus_5))) %>%
  mutate(civilstatus_6 = ifelse((is.na(civilstatus_6)) & (censor_5 == 0), tmp.summImpute$imputed_mean_6, as.character(civilstatus_6))) %>%
  mutate(civilstatus_7 = ifelse((is.na(civilstatus_7)) & (censor_6 == 0), tmp.summImpute$imputed_mean_7, as.character(civilstatus_7))) %>%
  mutate(civilstatus_8 = ifelse((is.na(civilstatus_8)) & (censor_7 == 0), tmp.summImpute$imputed_mean_8, as.character(civilstatus_8))) %>%
  mutate(civilstatus_9 = ifelse((is.na(civilstatus_9)) & (censor_8 == 0), tmp.summImpute$imputed_mean_9, as.character(civilstatus_9))) %>%
  mutate(civilstatus_10 = ifelse((is.na(civilstatus_10)) & (censor_9 == 0), tmp.summImpute$imputed_mean_10, as.character(civilstatus_10))) %>%
  mutate(civilstatus_11 = ifelse((is.na(civilstatus_11)) & (censor_10 == 0), tmp.summImpute$imputed_mean_11, as.character(civilstatus_11))) %>%
  mutate(civilstatus_12 = ifelse((is.na(civilstatus_12)) & (censor_11 == 0), tmp.summImpute$imputed_mean_12, as.character(civilstatus_12))) %>%
  mutate(civilstatus_13 = ifelse((is.na(civilstatus_13)) & (censor_12 == 0), tmp.summImpute$imputed_mean_13, as.character(civilstatus_13))) %>%
  mutate(civilstatus_14 = ifelse((is.na(civilstatus_14)) & (censor_13 == 0), tmp.summImpute$imputed_mean_14, as.character(civilstatus_14))) %>%
  mutate(civilstatus_15 = ifelse((is.na(civilstatus_15)) & (censor_14 == 0), tmp.summImpute$imputed_mean_15, as.character(civilstatus_15)))
# table(tmp$civilstatus_3)

# glucose
apply(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop %>% select(starts_with("glucose")), 2, function(x) mean(is.na(x)))
# remove glucose with >90% missing
tmp.summDrop <- data.frame(covariate = "Blood glucose",
                           n_0 = sum(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$glucose_0)),
                           p_0 = mean(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$glucose_0)), 
                           n_1 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_0 == 0) & 
                                       is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$glucose_1)),
                           p_1 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_0 == 0) & 
                                       is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$glucose_1)) /
                             summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_1,
                           n_2 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_1 == 0) & 
                                       is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$glucose_2), na.rm = T),
                           p_2 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_1 == 0) & 
                                       is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$glucose_2), na.rm = T) /
                             summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_2,
                           n_3 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_2 == 0) & 
                                       is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$glucose_3), na.rm = T),
                           p_3 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_2 == 0) & 
                                       is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$glucose_3), na.rm = T) /
                             summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_3,
                           n_4 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_3 == 0) & 
                                       is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$glucose_4), na.rm = T),
                           p_4 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_3 == 0) & 
                                       is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$glucose_4), na.rm = T) /
                             summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_4,
                           n_5 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_4 == 0) & 
                                       is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$glucose_5), na.rm = T),
                           p_5 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_4 == 0) & 
                                       is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$glucose_5), na.rm = T) /
                             summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_5,
                           n_6 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_5 == 0) & 
                                       is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$glucose_6), na.rm = T),
                           p_6 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_5 == 0) & 
                                       is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$glucose_6), na.rm = T) /
                             summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_6,
                           n_7 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_6 == 0) & 
                                       is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$glucose_7), na.rm = T),
                           p_7 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_6 == 0) & 
                                       is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$glucose_7), na.rm = T) /
                             summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_7, 
                           n_8 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_7 == 0) & 
                                       is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$glucose_8), na.rm = T),
                           p_8 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_7 == 0) & 
                                       is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$glucose_8), na.rm = T) /
                             summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_8,
                           n_9 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_8 == 0) & 
                                       is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$glucose_9), na.rm = T),
                           p_9 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_8 == 0) & 
                                       is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$glucose_9), na.rm = T) /
                             summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_9,
                           n_10 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_9 == 0) & 
                                        is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$glucose_10), na.rm = T),
                           p_10 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_9 == 0) & 
                                        is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$glucose_10), na.rm = T) /
                             summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_10,
                           n_11 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_10 == 0) & 
                                        is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$glucose_11), na.rm = T),
                           p_11 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_10 == 0) & 
                                        is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$glucose_11), na.rm = T) /
                             summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_11,
                           n_12 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_11 == 0) & 
                                        is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$glucose_12), na.rm = T),
                           p_12 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_11 == 0) & 
                                        is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$glucose_12), na.rm = T) /
                             summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_12,
                           n_13 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_12 == 0) & 
                                        is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$glucose_13), na.rm = T),
                           p_13 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_12 == 0) & 
                                        is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$glucose_13), na.rm = T) /
                             summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_13,
                           n_14 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_13 == 0) & 
                                        is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$glucose_14), na.rm = T),
                           p_14 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_13 == 0) & 
                                        is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$glucose_14), na.rm = T) /
                             summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_14,
                           n_15 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_14 == 0) & 
                                        is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$glucose_15), na.rm = T),
                           p_15 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_14 == 0) & 
                                        is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$glucose_15), na.rm = T) /
                             summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_15)
summDrop.wizmiss.t0Jul2016.switchDTG.wt100d.resPop <- rbind(summDrop.wizmiss.t0Jul2016.switchDTG.wt100d.resPop,
                                                            tmp.summDrop)
# fasting glucose
tmp.summDrop <- data.frame(covariate = "Fasting serum glucose",
                           n_0 = sum(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$glucoseFasting_0)),
                           p_0 = mean(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$glucoseFasting_0)), 
                           n_1 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_0 == 0) & 
                                       is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$glucoseFasting_1)),
                           p_1 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_0 == 0) & 
                                       is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$glucoseFasting_1)) /
                             summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_1,
                           n_2 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_1 == 0) & 
                                       is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$glucoseFasting_2), na.rm = T),
                           p_2 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_1 == 0) & 
                                       is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$glucoseFasting_2), na.rm = T) /
                             summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_2,
                           n_3 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_2 == 0) & 
                                       is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$glucoseFasting_3), na.rm = T),
                           p_3 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_2 == 0) & 
                                       is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$glucoseFasting_3), na.rm = T) /
                             summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_3,
                           n_4 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_3 == 0) & 
                                       is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$glucoseFasting_4), na.rm = T),
                           p_4 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_3 == 0) & 
                                       is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$glucoseFasting_4), na.rm = T) /
                             summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_4,
                           n_5 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_4 == 0) & 
                                       is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$glucoseFasting_5), na.rm = T),
                           p_5 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_4 == 0) & 
                                       is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$glucoseFasting_5), na.rm = T) /
                             summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_5,
                           n_6 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_5 == 0) & 
                                       is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$glucoseFasting_6), na.rm = T),
                           p_6 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_5 == 0) & 
                                       is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$glucoseFasting_6), na.rm = T) /
                             summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_6,
                           n_7 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_6 == 0) & 
                                       is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$glucoseFasting_7), na.rm = T),
                           p_7 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_6 == 0) & 
                                       is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$glucoseFasting_7), na.rm = T) /
                             summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_7, 
                           n_8 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_7 == 0) & 
                                       is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$glucoseFasting_8), na.rm = T),
                           p_8 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_7 == 0) & 
                                       is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$glucoseFasting_8), na.rm = T) /
                             summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_8,
                           n_9 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_8 == 0) & 
                                       is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$glucoseFasting_9), na.rm = T),
                           p_9 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_8 == 0) & 
                                       is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$glucoseFasting_9), na.rm = T) /
                             summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_9,
                           n_10 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_9 == 0) & 
                                        is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$glucoseFasting_10), na.rm = T),
                           p_10 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_9 == 0) & 
                                        is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$glucoseFasting_10), na.rm = T) /
                             summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_10,
                           n_11 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_10 == 0) & 
                                        is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$glucoseFasting_11), na.rm = T),
                           p_11 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_10 == 0) & 
                                        is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$glucoseFasting_11), na.rm = T) /
                             summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_11,
                           n_12 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_11 == 0) & 
                                        is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$glucoseFasting_12), na.rm = T),
                           p_12 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_11 == 0) & 
                                        is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$glucoseFasting_12), na.rm = T) /
                             summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_12,
                           n_13 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_12 == 0) & 
                                        is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$glucoseFasting_13), na.rm = T),
                           p_13 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_12 == 0) & 
                                        is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$glucoseFasting_13), na.rm = T) /
                             summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_13,
                           n_14 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_13 == 0) & 
                                        is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$glucoseFasting_14), na.rm = T),
                           p_14 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_13 == 0) & 
                                        is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$glucoseFasting_14), na.rm = T) /
                             summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_14,
                           n_15 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_14 == 0) & 
                                        is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$glucoseFasting_15), na.rm = T),
                           p_15 = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_14 == 0) & 
                                        is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$glucoseFasting_15), na.rm = T) /
                             summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_15)
summDrop.wizmiss.t0Jul2016.switchDTG.wt100d.resPop <- rbind(summDrop.wizmiss.t0Jul2016.switchDTG.wt100d.resPop,
                                                            tmp.summDrop)
finalData.noMiss.t0Jul2016.switchDTG.wt100d <- finalData.noMiss.t0Jul2016.switchDTG.wt100d %>%
  select(-starts_with("glucose"))
# colnames(finalData.noMiss.t0Jul2016.switchDTG.wt100d)

# Covered by NHIF
table(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$nhif_0)
sum(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$nhif_0 == "no", na.rm = T)
tmp.summImpute <- data.frame(covariate      = "Covered by NHIF",
                             n_miss_0       = sum(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$nhif_0)),
                             p_miss_0       = mean(is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$nhif_0)), 
                             imputed_mean_0 = "no",
                             n_miss_1       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_0 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$nhif_1)),
                             p_miss_1       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_0 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$nhif_1)) /
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_1,
                             imputed_mean_1 = "no",
                             n_miss_2       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_1 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$nhif_2), na.rm = T),
                             p_miss_2       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_1 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$nhif_2), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_2,
                             imputed_mean_2 = "no",
                             n_miss_3       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_2 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$nhif_3), na.rm = T),
                             p_miss_3       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_2 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$nhif_3), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_3,
                             imputed_mean_3 = "no",
                             n_miss_4       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_3 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$nhif_4), na.rm = T),
                             p_miss_4       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_3 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$nhif_4), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_4,
                             imputed_mean_4 = "no",
                             n_miss_5       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_4 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$nhif_5), na.rm = T),
                             p_miss_5       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_4 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$nhif_5), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_5,
                             imputed_mean_5 = "no",
                             n_miss_6       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_5 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$nhif_6), na.rm = T),
                             p_miss_6       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_5 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$nhif_6), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_6,
                             imputed_mean_6 = "no",
                             n_miss_7       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_6 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$nhif_7), na.rm = T),
                             p_miss_7       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_6 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$nhif_7), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_7,
                             imputed_mean_7 = "no",
                             n_miss_8       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_7 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$nhif_8), na.rm = T),
                             p_miss_8       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_7 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$nhif_8), na.rm = T) /
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_8,
                             imputed_mean_8 = "no",
                             n_miss_9       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_8 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$nhif_9), na.rm = T),
                             p_miss_9       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_8 == 0) & 
                                                    is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$nhif_9), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_9,
                             imputed_mean_9 = "no",
                             n_miss_10       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_9 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$nhif_10), na.rm = T),
                             p_miss_10       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_9 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$nhif_10), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_10,
                             imputed_mean_10 = "no",
                             n_miss_11       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_10 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$nhif_11), na.rm = T),
                             p_miss_11       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_10 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$nhif_11), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_11,
                             imputed_mean_11 = "no",
                             n_miss_12       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_11 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$nhif_12), na.rm = T),
                             p_miss_12       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_11 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$nhif_12), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_12,
                             imputed_mean_12 = "no",
                             n_miss_13       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_12 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$nhif_13), na.rm = T),
                             p_miss_13       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_12 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$nhif_13), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_13,
                             imputed_mean_13 = "no",
                             n_miss_14       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_13 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$nhif_14), na.rm = T),
                             p_miss_14       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_13 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$nhif_14), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_14,
                             imputed_mean_14 = "no",
                             n_miss_15       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_14 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$nhif_15), na.rm = T),
                             p_miss_15       = sum((finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$censor_14 == 0) & 
                                                     is.na(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$nhif_15), na.rm = T) / 
                               summLeft.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$n_15,
                             imputed_mean_15 = "no")
summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop <- rbind(summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop,
                                                              tmp.summImpute)
finalData.noMiss.t0Jul2016.switchDTG.wt100d <- finalData.noMiss.t0Jul2016.switchDTG.wt100d %>%
  mutate(nhif_0 = ifelse(is.na(nhif_0), tmp.summImpute$imputed_mean_0, as.character(nhif_0))) %>%
  mutate(nhif_1 = ifelse((is.na(nhif_1)) & (censor_0 == 0), tmp.summImpute$imputed_mean_1, as.character(nhif_1))) %>%
  mutate(nhif_2 = ifelse((is.na(nhif_2)) & (censor_1 == 0), tmp.summImpute$imputed_mean_2, as.character(nhif_2))) %>%
  mutate(nhif_3 = ifelse((is.na(nhif_3)) & (censor_2 == 0), tmp.summImpute$imputed_mean_3, as.character(nhif_3))) %>%
  mutate(nhif_4 = ifelse((is.na(nhif_4)) & (censor_3 == 0), tmp.summImpute$imputed_mean_4, as.character(nhif_4))) %>%
  mutate(nhif_5 = ifelse((is.na(nhif_5)) & (censor_4 == 0), tmp.summImpute$imputed_mean_5, as.character(nhif_5))) %>%
  mutate(nhif_6 = ifelse((is.na(nhif_6)) & (censor_5 == 0), tmp.summImpute$imputed_mean_6, as.character(nhif_6))) %>%
  mutate(nhif_7 = ifelse((is.na(nhif_7)) & (censor_6 == 0), tmp.summImpute$imputed_mean_7, as.character(nhif_7))) %>%
  mutate(nhif_8 = ifelse((is.na(nhif_8)) & (censor_7 == 0), tmp.summImpute$imputed_mean_8, as.character(nhif_8))) %>%
  mutate(nhif_9 = ifelse((is.na(nhif_9)) & (censor_8 == 0), tmp.summImpute$imputed_mean_9, as.character(nhif_9))) %>%
  mutate(nhif_10 = ifelse((is.na(nhif_10)) & (censor_9 == 0), tmp.summImpute$imputed_mean_10, as.character(nhif_10))) %>%
  mutate(nhif_11 = ifelse((is.na(nhif_11)) & (censor_10 == 0), tmp.summImpute$imputed_mean_11, as.character(nhif_11))) %>%
  mutate(nhif_12 = ifelse((is.na(nhif_12)) & (censor_11 == 0), tmp.summImpute$imputed_mean_12, as.character(nhif_12))) %>%
  mutate(nhif_13 = ifelse((is.na(nhif_13)) & (censor_12 == 0), tmp.summImpute$imputed_mean_13, as.character(nhif_13))) %>%
  mutate(nhif_14 = ifelse((is.na(nhif_14)) & (censor_13 == 0), tmp.summImpute$imputed_mean_14, as.character(nhif_14))) %>%
  mutate(nhif_15 = ifelse((is.na(nhif_15)) & (censor_14 == 0), tmp.summImpute$imputed_mean_15, as.character(nhif_15)))

apply(finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop %>% select(Weight_0:censor_0), 2, function(x) mean(is.na(x)))
apply(finalData.noMiss.t0Jul2016.switchDTG.wt100d %>% select(Weight_0:censor_0), 2, function(x) mean(is.na(x)))
# load("IntermediateData.RData")




# latex code
library(xtable)

# censor
# 400
latex.summCensor.wizmiss.t0Jul2016.switchDTG.wt400d.resPop <- summCensor.wizmiss.t0Jul2016.switchDTG.wt400d.resPop %>%
  mutate(p_0 = round(p_0 * 100, 1),
         p_1 = round(p_1 * 100, 1),
         p_2 = round(p_2 * 100, 1),
         p_3 = round(p_3 * 100, 1),
         p_4 = round(p_4 * 100, 1))
latex.summCensor.wizmiss.t0Jul2016.switchDTG.wt400d.resPop <- latex.summCensor.wizmiss.t0Jul2016.switchDTG.wt400d.resPop %>%
  mutate(t_0 = paste0(n_0, " (", p_0, "%)"), 
         t_1 = paste0(n_1, " (", p_1, "%)"), 
         t_2 = paste0(n_2, " (", p_2, "%)"), 
         t_3 = paste0(n_3, " (", p_3, "%)"), 
         t_4 = paste0(n_4, " (", p_4, "%)"))
latex.summCensor.wizmiss.t0Jul2016.switchDTG.wt400d.resPop <- latex.summCensor.wizmiss.t0Jul2016.switchDTG.wt400d.resPop %>%
  select(-(n_0:p_4))
print(xtable(latex.summCensor.wizmiss.t0Jul2016.switchDTG.wt400d.resPop), include.rownames = FALSE)
# 200
latex.summCensor.wizmiss.t0Jul2016.switchDTG.wt200d.resPop.1 <- summCensor.wizmiss.t0Jul2016.switchDTG.wt200d.resPop %>%
  select(type:p_4)
latex.summCensor.wizmiss.t0Jul2016.switchDTG.wt200d.resPop.2 <- summCensor.wizmiss.t0Jul2016.switchDTG.wt200d.resPop %>%
  select(type, n_5:p_8)
latex.summCensor.wizmiss.t0Jul2016.switchDTG.wt200d.resPop.1 <- latex.summCensor.wizmiss.t0Jul2016.switchDTG.wt200d.resPop.1 %>%
  mutate(p_0 = round(p_0 * 100, 1),
         p_1 = round(p_1 * 100, 1),
         p_2 = round(p_2 * 100, 1),
         p_3 = round(p_3 * 100, 1),
         p_4 = round(p_4 * 100, 1))
latex.summCensor.wizmiss.t0Jul2016.switchDTG.wt200d.resPop.1 <- latex.summCensor.wizmiss.t0Jul2016.switchDTG.wt200d.resPop.1 %>%
  mutate(t_0 = paste0(n_0, " (", p_0, "%)"), 
         t_1 = paste0(n_1, " (", p_1, "%)"), 
         t_2 = paste0(n_2, " (", p_2, "%)"), 
         t_3 = paste0(n_3, " (", p_3, "%)"), 
         t_4 = paste0(n_4, " (", p_4, "%)"))
latex.summCensor.wizmiss.t0Jul2016.switchDTG.wt200d.resPop.1 <- latex.summCensor.wizmiss.t0Jul2016.switchDTG.wt200d.resPop.1 %>%
  select(-(n_0:p_4))
print(xtable(latex.summCensor.wizmiss.t0Jul2016.switchDTG.wt200d.resPop.1), include.rownames = FALSE)
latex.summCensor.wizmiss.t0Jul2016.switchDTG.wt200d.resPop.2 <- latex.summCensor.wizmiss.t0Jul2016.switchDTG.wt200d.resPop.2 %>%
  mutate(p_5 = round(p_5 * 100, 1),
         p_6 = round(p_6 * 100, 1),
         p_7 = round(p_7 * 100, 1),
         p_8 = round(p_8 * 100, 1))
latex.summCensor.wizmiss.t0Jul2016.switchDTG.wt200d.resPop.2 <- latex.summCensor.wizmiss.t0Jul2016.switchDTG.wt200d.resPop.2 %>%
  mutate(t_5 = paste0(n_5, " (", p_5, "%)"), 
         t_6 = paste0(n_6, " (", p_6, "%)"), 
         t_7 = paste0(n_7, " (", p_7, "%)"), 
         t_8 = paste0(n_8, " (", p_8, "%)"))
latex.summCensor.wizmiss.t0Jul2016.switchDTG.wt200d.resPop.2 <- latex.summCensor.wizmiss.t0Jul2016.switchDTG.wt200d.resPop.2 %>%
  select(-(n_5:p_8))
print(xtable(latex.summCensor.wizmiss.t0Jul2016.switchDTG.wt200d.resPop.2), include.rownames = FALSE)
# 100
latex.summCensor.wizmiss.t0Jul2016.switchDTG.wt100d.resPop.1 <- summCensor.wizmiss.t0Jul2016.switchDTG.wt100d.resPop %>%
  select(type:p_4)
latex.summCensor.wizmiss.t0Jul2016.switchDTG.wt100d.resPop.2 <- summCensor.wizmiss.t0Jul2016.switchDTG.wt100d.resPop %>%
  select(type, n_5:p_9)
latex.summCensor.wizmiss.t0Jul2016.switchDTG.wt100d.resPop.3 <- summCensor.wizmiss.t0Jul2016.switchDTG.wt100d.resPop %>%
  select(type, n_10:p_14)
latex.summCensor.wizmiss.t0Jul2016.switchDTG.wt100d.resPop.4 <- summCensor.wizmiss.t0Jul2016.switchDTG.wt100d.resPop %>%
  select(type, n_15:p_16)
latex.summCensor.wizmiss.t0Jul2016.switchDTG.wt100d.resPop.1 <- latex.summCensor.wizmiss.t0Jul2016.switchDTG.wt100d.resPop.1 %>%
  mutate(p_0 = round(p_0 * 100, 1),
         p_1 = round(p_1 * 100, 1),
         p_2 = round(p_2 * 100, 1),
         p_3 = round(p_3 * 100, 1),
         p_4 = round(p_4 * 100, 1))
latex.summCensor.wizmiss.t0Jul2016.switchDTG.wt100d.resPop.1 <- latex.summCensor.wizmiss.t0Jul2016.switchDTG.wt100d.resPop.1 %>%
  mutate(t_0 = paste0(n_0, " (", p_0, "%)"), 
         t_1 = paste0(n_1, " (", p_1, "%)"), 
         t_2 = paste0(n_2, " (", p_2, "%)"), 
         t_3 = paste0(n_3, " (", p_3, "%)"), 
         t_4 = paste0(n_4, " (", p_4, "%)"))
latex.summCensor.wizmiss.t0Jul2016.switchDTG.wt100d.resPop.1 <- latex.summCensor.wizmiss.t0Jul2016.switchDTG.wt100d.resPop.1 %>%
  select(-(n_0:p_4))
print(xtable(latex.summCensor.wizmiss.t0Jul2016.switchDTG.wt100d.resPop.1), include.rownames = FALSE)
latex.summCensor.wizmiss.t0Jul2016.switchDTG.wt100d.resPop.2 <- latex.summCensor.wizmiss.t0Jul2016.switchDTG.wt100d.resPop.2 %>%
  mutate(p_5 = round(p_5 * 100, 1),
         p_6 = round(p_6 * 100, 1),
         p_7 = round(p_7 * 100, 1),
         p_8 = round(p_8 * 100, 1),
         p_9 = round(p_9 * 100, 1))
latex.summCensor.wizmiss.t0Jul2016.switchDTG.wt100d.resPop.2 <- latex.summCensor.wizmiss.t0Jul2016.switchDTG.wt100d.resPop.2 %>%
  mutate(t_5 = paste0(n_5, " (", p_5, "%)"), 
         t_6 = paste0(n_6, " (", p_6, "%)"), 
         t_7 = paste0(n_7, " (", p_7, "%)"), 
         t_8 = paste0(n_8, " (", p_8, "%)"), 
         t_9 = paste0(n_9, " (", p_9, "%)"))
latex.summCensor.wizmiss.t0Jul2016.switchDTG.wt100d.resPop.2 <- latex.summCensor.wizmiss.t0Jul2016.switchDTG.wt100d.resPop.2 %>%
  select(-(n_5:p_9))
print(xtable(latex.summCensor.wizmiss.t0Jul2016.switchDTG.wt100d.resPop.2), include.rownames = FALSE)
latex.summCensor.wizmiss.t0Jul2016.switchDTG.wt100d.resPop.3 <- latex.summCensor.wizmiss.t0Jul2016.switchDTG.wt100d.resPop.3 %>%
  mutate(p_10 = round(p_10 * 100, 1),
         p_11 = round(p_11 * 100, 1),
         p_12 = round(p_12 * 100, 1),
         p_13 = round(p_13 * 100, 1),
         p_14 = round(p_14 * 100, 1))
latex.summCensor.wizmiss.t0Jul2016.switchDTG.wt100d.resPop.3 <- latex.summCensor.wizmiss.t0Jul2016.switchDTG.wt100d.resPop.3 %>%
  mutate(t_10 = paste0(n_10, " (", p_10, "%)"), 
         t_11 = paste0(n_11, " (", p_11, "%)"), 
         t_12 = paste0(n_12, " (", p_12, "%)"), 
         t_13 = paste0(n_13, " (", p_13, "%)"), 
         t_14 = paste0(n_14, " (", p_14, "%)"))
latex.summCensor.wizmiss.t0Jul2016.switchDTG.wt100d.resPop.3 <- latex.summCensor.wizmiss.t0Jul2016.switchDTG.wt100d.resPop.3 %>%
  select(-(n_10:p_14))
print(xtable(latex.summCensor.wizmiss.t0Jul2016.switchDTG.wt100d.resPop.3), include.rownames = FALSE)
latex.summCensor.wizmiss.t0Jul2016.switchDTG.wt100d.resPop.4 <- latex.summCensor.wizmiss.t0Jul2016.switchDTG.wt100d.resPop.4 %>%
  mutate(p_15 = round(p_15 * 100, 1),
         p_16 = round(p_16 * 100, 1))
latex.summCensor.wizmiss.t0Jul2016.switchDTG.wt100d.resPop.4 <- latex.summCensor.wizmiss.t0Jul2016.switchDTG.wt100d.resPop.4 %>%
  mutate(t_15 = paste0(n_15, " (", p_15, "%)"), 
         t_16 = paste0(n_16, " (", p_16, "%)"))
latex.summCensor.wizmiss.t0Jul2016.switchDTG.wt100d.resPop.4 <- latex.summCensor.wizmiss.t0Jul2016.switchDTG.wt100d.resPop.4 %>%
  select(-(n_15:p_16))
print(xtable(latex.summCensor.wizmiss.t0Jul2016.switchDTG.wt100d.resPop.4), include.rownames = FALSE)


# drop
# 400
latex.summDrop.wizmiss.t0Jul2016.switchDTG.wt400d.resPop <- summDrop.wizmiss.t0Jul2016.switchDTG.wt400d.resPop %>%
  mutate(p_0 = round(p_0 * 100, 1),
         p_1 = round(p_1 * 100, 1),
         p_2 = round(p_2 * 100, 1),
         p_3 = round(p_3 * 100, 1))
latex.summDrop.wizmiss.t0Jul2016.switchDTG.wt400d.resPop <- latex.summDrop.wizmiss.t0Jul2016.switchDTG.wt400d.resPop %>%
  mutate(t_0 = paste0(n_0, " (", p_0, "%)"), 
         t_1 = paste0(n_1, " (", p_1, "%)"), 
         t_2 = paste0(n_2, " (", p_2, "%)"), 
         t_3 = paste0(n_3, " (", p_3, "%)"))
latex.summDrop.wizmiss.t0Jul2016.switchDTG.wt400d.resPop <- latex.summDrop.wizmiss.t0Jul2016.switchDTG.wt400d.resPop %>%
  select(-(n_0:p_3))
print(xtable(latex.summDrop.wizmiss.t0Jul2016.switchDTG.wt400d.resPop), include.rownames = FALSE)
# 200
latex.summDrop.wizmiss.t0Jul2016.switchDTG.wt200d.resPop <- summDrop.wizmiss.t0Jul2016.switchDTG.wt200d.resPop %>%
  mutate(p_0 = round(p_0 * 100, 1),
         p_1 = round(p_1 * 100, 1),
         p_2 = round(p_2 * 100, 1),
         p_3 = round(p_3 * 100, 1), 
         p_4 = round(p_4 * 100, 1),
         p_5 = round(p_5 * 100, 1),
         p_6 = round(p_6 * 100, 1),
         p_7 = round(p_7 * 100, 1))
latex.summDrop.wizmiss.t0Jul2016.switchDTG.wt200d.resPop <- latex.summDrop.wizmiss.t0Jul2016.switchDTG.wt200d.resPop %>%
  mutate(t_0 = paste0(n_0, " (", p_0, "%)"), 
         t_1 = paste0(n_1, " (", p_1, "%)"), 
         t_2 = paste0(n_2, " (", p_2, "%)"), 
         t_3 = paste0(n_3, " (", p_3, "%)"),
         t_4 = paste0(n_4, " (", p_4, "%)"), 
         t_5 = paste0(n_5, " (", p_5, "%)"), 
         t_6 = paste0(n_6, " (", p_6, "%)"), 
         t_7 = paste0(n_7, " (", p_7, "%)"))
latex.summDrop.wizmiss.t0Jul2016.switchDTG.wt200d.resPop.1 <- latex.summDrop.wizmiss.t0Jul2016.switchDTG.wt200d.resPop %>%
  select(covariate, t_0:t_3)
latex.summDrop.wizmiss.t0Jul2016.switchDTG.wt200d.resPop.2 <- latex.summDrop.wizmiss.t0Jul2016.switchDTG.wt200d.resPop %>%
  select(covariate, t_4:t_7)
print(xtable(latex.summDrop.wizmiss.t0Jul2016.switchDTG.wt200d.resPop.1), include.rownames = FALSE)
print(xtable(latex.summDrop.wizmiss.t0Jul2016.switchDTG.wt200d.resPop.2), include.rownames = FALSE)
# 100
latex.summDrop.wizmiss.t0Jul2016.switchDTG.wt100d.resPop <- summDrop.wizmiss.t0Jul2016.switchDTG.wt100d.resPop %>%
  mutate(p_0 = round(p_0 * 100, 1),
         p_1 = round(p_1 * 100, 1),
         p_2 = round(p_2 * 100, 1),
         p_3 = round(p_3 * 100, 1), 
         p_4 = round(p_4 * 100, 1),
         p_5 = round(p_5 * 100, 1),
         p_6 = round(p_6 * 100, 1),
         p_7 = round(p_7 * 100, 1),
         p_8 = round(p_8 * 100, 1),
         p_9 = round(p_9 * 100, 1),
         p_10 = round(p_10 * 100, 1),
         p_11 = round(p_11 * 100, 1), 
         p_12 = round(p_12 * 100, 1),
         p_13 = round(p_13 * 100, 1),
         p_14 = round(p_14 * 100, 1),
         p_15 = round(p_15 * 100, 1))
latex.summDrop.wizmiss.t0Jul2016.switchDTG.wt100d.resPop <- latex.summDrop.wizmiss.t0Jul2016.switchDTG.wt100d.resPop %>%
  mutate(t_0 = paste0(n_0, " (", p_0, "%)"), 
         t_1 = paste0(n_1, " (", p_1, "%)"), 
         t_2 = paste0(n_2, " (", p_2, "%)"), 
         t_3 = paste0(n_3, " (", p_3, "%)"),
         t_4 = paste0(n_4, " (", p_4, "%)"), 
         t_5 = paste0(n_5, " (", p_5, "%)"), 
         t_6 = paste0(n_6, " (", p_6, "%)"), 
         t_7 = paste0(n_7, " (", p_7, "%)"),
         t_8 = paste0(n_8, " (", p_8, "%)"), 
         t_9 = paste0(n_9, " (", p_9, "%)"), 
         t_10 = paste0(n_10, " (", p_10, "%)"), 
         t_11 = paste0(n_11, " (", p_11, "%)"),
         t_12 = paste0(n_12, " (", p_12, "%)"), 
         t_13 = paste0(n_13, " (", p_13, "%)"), 
         t_14 = paste0(n_14, " (", p_14, "%)"), 
         t_15 = paste0(n_15, " (", p_15, "%)"))
latex.summDrop.wizmiss.t0Jul2016.switchDTG.wt100d.resPop.1 <- latex.summDrop.wizmiss.t0Jul2016.switchDTG.wt100d.resPop %>%
  select(covariate, t_0:t_3)
latex.summDrop.wizmiss.t0Jul2016.switchDTG.wt100d.resPop.2 <- latex.summDrop.wizmiss.t0Jul2016.switchDTG.wt100d.resPop %>%
  select(covariate, t_4:t_7)
latex.summDrop.wizmiss.t0Jul2016.switchDTG.wt100d.resPop.3 <- latex.summDrop.wizmiss.t0Jul2016.switchDTG.wt100d.resPop %>%
  select(covariate, t_8:t_11)
latex.summDrop.wizmiss.t0Jul2016.switchDTG.wt100d.resPop.4 <- latex.summDrop.wizmiss.t0Jul2016.switchDTG.wt100d.resPop %>%
  select(covariate, t_12:t_15)
print(xtable(latex.summDrop.wizmiss.t0Jul2016.switchDTG.wt100d.resPop.1), include.rownames = FALSE)
print(xtable(latex.summDrop.wizmiss.t0Jul2016.switchDTG.wt100d.resPop.2), include.rownames = FALSE)
print(xtable(latex.summDrop.wizmiss.t0Jul2016.switchDTG.wt100d.resPop.3), include.rownames = FALSE)
print(xtable(latex.summDrop.wizmiss.t0Jul2016.switchDTG.wt100d.resPop.4), include.rownames = FALSE)

# impute
# 400
latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt400d.resPop <- summImpute.wizmiss.t0Jul2016.switchDTG.wt400d.resPop %>%
  mutate(p_miss_0 = round(p_miss_0 * 100, 1),
         p_miss_1 = round(p_miss_1 * 100, 1),
         p_miss_2 = round(p_miss_2 * 100, 1),
         p_miss_3 = round(p_miss_3 * 100, 1))
latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$round_imputed_0 <-
  c(round(as.numeric(latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$imputed_mean_0[1:5]), 2),
    latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$imputed_mean_0[6:11])
latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$round_imputed_1 <-
  c(round(as.numeric(latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$imputed_mean_1[1:5]), 2),
    latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$imputed_mean_1[6:11])
latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$round_imputed_2 <-
  c(round(as.numeric(latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$imputed_mean_2[1:5]), 2),
    latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$imputed_mean_2[6:11])
latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$round_imputed_3 <-
  c(round(as.numeric(latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$imputed_mean_3[1:5]), 2),
    latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt400d.resPop$imputed_mean_3[6:11])
latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt400d.resPop <- latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt400d.resPop %>%
  mutate(t_0 = paste0(n_miss_0, " (", p_miss_0, "%)"), 
         t_1 = paste0(n_miss_1, " (", p_miss_1, "%)"), 
         t_2 = paste0(n_miss_2, " (", p_miss_2, "%)"), 
         t_3 = paste0(n_miss_3, " (", p_miss_3, "%)")) %>%
  mutate(e_1 = NA,
         e_2 = NA, 
         e_3 = NA)
latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt400d.resPop <- latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt400d.resPop %>%
  select(covariate, t_0, round_imputed_0, e_1, t_1, round_imputed_1, e_2, t_2, round_imputed_2, e_3, t_3, round_imputed_3)
print(xtable(latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt400d.resPop), include.rownames = FALSE)
# 200
latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt200d.resPop <- summImpute.wizmiss.t0Jul2016.switchDTG.wt200d.resPop %>%
  mutate(p_miss_0 = round(p_miss_0 * 100, 1),
         p_miss_1 = round(p_miss_1 * 100, 1),
         p_miss_2 = round(p_miss_2 * 100, 1),
         p_miss_3 = round(p_miss_3 * 100, 1),
         p_miss_4 = round(p_miss_4 * 100, 1),
         p_miss_5 = round(p_miss_5 * 100, 1),
         p_miss_6 = round(p_miss_6 * 100, 1),
         p_miss_7 = round(p_miss_7 * 100, 1))
latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$round_imputed_0 <-
  c(round(as.numeric(latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$imputed_mean_0[1:5]), 2),
    latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$imputed_mean_0[6:10])
latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$round_imputed_1 <-
  c(round(as.numeric(latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$imputed_mean_1[1:5]), 2),
    latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$imputed_mean_1[6:10])
latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$round_imputed_2 <-
  c(round(as.numeric(latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$imputed_mean_2[1:5]), 2),
    latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$imputed_mean_2[6:10])
latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$round_imputed_3 <-
  c(round(as.numeric(latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$imputed_mean_3[1:5]), 2),
    latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$imputed_mean_3[6:10])
latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$round_imputed_4 <-
  c(round(as.numeric(latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$imputed_mean_4[1:5]), 2),
    latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$imputed_mean_4[6:10])
latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$round_imputed_5 <-
  c(round(as.numeric(latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$imputed_mean_5[1:5]), 2),
    latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$imputed_mean_5[6:10])
latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$round_imputed_6 <-
  c(round(as.numeric(latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$imputed_mean_6[1:5]), 2),
    latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$imputed_mean_6[6:10])
latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$round_imputed_7 <-
  c(round(as.numeric(latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$imputed_mean_7[1:5]), 2),
    latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$imputed_mean_7[6:10])
latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt200d.resPop <- latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt200d.resPop %>%
  mutate(t_0 = paste0(n_miss_0, " (", p_miss_0, "%)"), 
         t_1 = paste0(n_miss_1, " (", p_miss_1, "%)"), 
         t_2 = paste0(n_miss_2, " (", p_miss_2, "%)"), 
         t_3 = paste0(n_miss_3, " (", p_miss_3, "%)"),
         t_4 = paste0(n_miss_4, " (", p_miss_4, "%)"), 
         t_5 = paste0(n_miss_5, " (", p_miss_5, "%)"), 
         t_6 = paste0(n_miss_6, " (", p_miss_6, "%)"), 
         t_7 = paste0(n_miss_7, " (", p_miss_7, "%)")) %>%
  mutate(e_1 = NA,
         e_2 = NA, 
         e_3 = NA)
latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt200d.resPop.1 <- latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt200d.resPop %>%
  select(covariate, t_0, round_imputed_0, e_1, t_1, round_imputed_1, e_2, t_2, round_imputed_2, e_3, t_3, round_imputed_3)
latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt200d.resPop.1 <- latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt200d.resPop %>%
  select(covariate, t_0, t_1, t_2, t_3)  
latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt200d.resPop.2 <- latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt200d.resPop %>%
  select(covariate, t_4, round_imputed_4, e_1, t_5, round_imputed_5, e_2, t_6, round_imputed_6, e_3, t_7, round_imputed_7)
print(xtable(latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt200d.resPop.1), include.rownames = FALSE)
print(xtable(latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt200d.resPop.2), include.rownames = FALSE)
# 100
latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop <- summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop %>%
  mutate(p_miss_0 = round(p_miss_0 * 100, 1),
         p_miss_1 = round(p_miss_1 * 100, 1),
         p_miss_2 = round(p_miss_2 * 100, 1),
         p_miss_3 = round(p_miss_3 * 100, 1),
         p_miss_4 = round(p_miss_4 * 100, 1),
         p_miss_5 = round(p_miss_5 * 100, 1),
         p_miss_6 = round(p_miss_6 * 100, 1),
         p_miss_7 = round(p_miss_7 * 100, 1),
         p_miss_8 = round(p_miss_8 * 100, 1),
         p_miss_9 = round(p_miss_9 * 100, 1),
         p_miss_10 = round(p_miss_10 * 100, 1),
         p_miss_11 = round(p_miss_11 * 100, 1),
         p_miss_12 = round(p_miss_12 * 100, 1),
         p_miss_13 = round(p_miss_13 * 100, 1),
         p_miss_14 = round(p_miss_14 * 100, 1),
         p_miss_15 = round(p_miss_15 * 100, 1))
latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$round_imputed_0 <-
  c(round(as.numeric(latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$imputed_mean_0[1:3]), 2),
    latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$imputed_mean_0[4:9])
latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$round_imputed_1 <-
  c(round(as.numeric(latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$imputed_mean_1[1:3]), 2),
    latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$imputed_mean_1[4:9])
latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$round_imputed_2 <-
  c(round(as.numeric(latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$imputed_mean_2[1:3]), 2),
    latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$imputed_mean_2[4:9])
latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$round_imputed_3 <-
  c(round(as.numeric(latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$imputed_mean_3[1:3]), 2),
    latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$imputed_mean_3[4:9])
latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$round_imputed_4 <-
  c(round(as.numeric(latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$imputed_mean_4[1:3]), 2),
    latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$imputed_mean_4[4:9])
latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$round_imputed_5 <-
  c(round(as.numeric(latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$imputed_mean_5[1:3]), 2),
    latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$imputed_mean_5[4:9])
latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$round_imputed_6 <-
  c(round(as.numeric(latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$imputed_mean_6[1:3]), 2),
    latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$imputed_mean_6[4:9])
latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$round_imputed_7 <-
  c(round(as.numeric(latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$imputed_mean_7[1:3]), 2),
    latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$imputed_mean_7[4:9])
latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$round_imputed_8 <-
  c(round(as.numeric(latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$imputed_mean_8[1:3]), 2),
    latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$imputed_mean_8[4:9])
latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$round_imputed_9 <-
  c(round(as.numeric(latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$imputed_mean_9[1:3]), 2),
    latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$imputed_mean_9[4:9])
latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$round_imputed_10 <-
  c(round(as.numeric(latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$imputed_mean_10[1:3]), 2),
    latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$imputed_mean_10[4:9])
latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$round_imputed_11 <-
  c(round(as.numeric(latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$imputed_mean_11[1:3]), 2),
    latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$imputed_mean_11[4:9])
latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$round_imputed_12 <-
  c(round(as.numeric(latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$imputed_mean_12[1:3]), 2),
    latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$imputed_mean_12[4:9])
latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$round_imputed_13 <-
  c(round(as.numeric(latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$imputed_mean_13[1:3]), 2),
    latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$imputed_mean_13[4:9])
latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$round_imputed_14 <-
  c(round(as.numeric(latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$imputed_mean_14[1:3]), 2),
    latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$imputed_mean_14[4:9])
latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$round_imputed_15 <-
  c(round(as.numeric(latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$imputed_mean_15[1:3]), 2),
    latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop$imputed_mean_15[4:9])
latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop <- latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop %>%
  mutate(t_0 = paste0(n_miss_0, " (", p_miss_0, "%)"), 
         t_1 = paste0(n_miss_1, " (", p_miss_1, "%)"), 
         t_2 = paste0(n_miss_2, " (", p_miss_2, "%)"), 
         t_3 = paste0(n_miss_3, " (", p_miss_3, "%)"),
         t_4 = paste0(n_miss_4, " (", p_miss_4, "%)"), 
         t_5 = paste0(n_miss_5, " (", p_miss_5, "%)"), 
         t_6 = paste0(n_miss_6, " (", p_miss_6, "%)"), 
         t_7 = paste0(n_miss_7, " (", p_miss_7, "%)"),
         t_8 = paste0(n_miss_8, " (", p_miss_8, "%)"), 
         t_9 = paste0(n_miss_9, " (", p_miss_9, "%)"), 
         t_10 = paste0(n_miss_10, " (", p_miss_10, "%)"), 
         t_11 = paste0(n_miss_11, " (", p_miss_11, "%)"),
         t_12 = paste0(n_miss_12, " (", p_miss_12, "%)"), 
         t_13 = paste0(n_miss_13, " (", p_miss_13, "%)"), 
         t_14 = paste0(n_miss_14, " (", p_miss_14, "%)"), 
         t_15 = paste0(n_miss_15, " (", p_miss_15, "%)")) %>%
  mutate(e_1 = NA,
         e_2 = NA, 
         e_3 = NA)
latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop.1 <- latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop %>%
  select(covariate, t_0, round_imputed_0, e_1, t_1, round_imputed_1, e_2, t_2, round_imputed_2, e_3, t_3, round_imputed_3)
latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop.2 <- latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop %>%
  select(covariate, t_4, round_imputed_4, e_1, t_5, round_imputed_5, e_2, t_6, round_imputed_6, e_3, t_7, round_imputed_7)
latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop.3 <- latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop %>%
  select(covariate, t_8, round_imputed_8, e_1, t_9, round_imputed_9, e_2, t_10, round_imputed_10, e_3, t_11, round_imputed_11)
latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop.4 <- latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop %>%
  select(covariate, t_12, round_imputed_12, e_1, t_13, round_imputed_13, e_2, t_14, round_imputed_14, e_3, t_15, round_imputed_15)
print(xtable(latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop.1), include.rownames = FALSE)
print(xtable(latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop.2), include.rownames = FALSE)
print(xtable(latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop.3), include.rownames = FALSE)
print(xtable(latex.summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop.4), include.rownames = FALSE)


# Paper Analysis section
tmp.wizMiss <- finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop %>%
  mutate(male         = ifelse(male == "Female", 0, 1),
         arvstart_dob = as.numeric(arvstart_dob) / 365.25,
         time0_dob    = as.numeric(time0_dob) / 365.25,
         time0_arvstart = as.numeric(time0_arvstart) /365.25,
         enroll_after20160701 = as.numeric(enroll_after20160701),
         # Weight_0     = as.numeric(Weight_0), 
         # SystolicBP_0 = as.numeric(SystolicBP_0),
         # DiastolicBP_0 = as.numeric(DiastolicBP_0),
         # Height_0      = as.numeric(Height_0),
         # tbtx_0        = as.numeric(tbtx_0),
         # Married_0     = as.numeric(Married_0),
         nhif_0        = ifelse(nhif_0 == "yes", 1, 0),
         nhif_1        = ifelse(nhif_1 == "yes", 1, 0),
         nhif_2        = ifelse(nhif_2 == "yes", 1, 0),
         nhif_3        = ifelse(nhif_3 == "yes", 1, 0))
tmp.noMiss <- finalData.noMiss.t0Jul2016.switchDTG.wt200d %>%
  mutate(male         = ifelse(male == "Female", 0, 1),
         arvstart_dob = as.numeric(arvstart_dob) / 365.25,
         time0_dob    = as.numeric(time0_dob) / 365.25,
         time0_arvstart = as.numeric(time0_arvstart) /365.25,
         enroll_after20160701 = as.numeric(enroll_after20160701),
         # Weight_0     = as.numeric(Weight_0), 
         # SystolicBP_0 = as.numeric(SystolicBP_0),
         # DiastolicBP_0 = as.numeric(DiastolicBP_0),
         # Height_0      = as.numeric(Height_0),
         # tbtx_0        = as.numeric(tbtx_0),
         # Married_0     = as.numeric(Married_0),
         nhif_0        = ifelse(nhif_0 == "yes", 1, 0),
         nhif_1        = ifelse(nhif_1 == "yes", 1, 0),
         nhif_2        = ifelse(nhif_2 == "yes", 1, 0),
         nhif_3        = ifelse(nhif_3 == "yes", 1, 0))

apply(tmp.wizMiss %>% select(male, arvstart_dob, time0_dob, time0_arvstart, enroll_after20160701), 2, sum)
apply(tmp.wizMiss %>% select(male, arvstart_dob, time0_dob, time0_arvstart, enroll_after20160701), 2, mean)
apply(tmp.wizMiss %>% select(male, arvstart_dob, time0_dob, time0_arvstart, enroll_after20160701), 2, sd)

# Weight_0 + viralloadImptCtgr_0 +
# SystolicBP_0 + DiastolicBP_0 + Height_0 + tbtx_0 + 
# activeTB_0 + Married_0 + nhif_0,

apply(tmp.wizMiss %>% select(Weight_0, SystolicBP_0:nhif_0), 2, function(x) mean(as.numeric(x), na.rm = T))
apply(tmp.wizMiss %>% select(Weight_0, SystolicBP_0:nhif_0), 2, function(x) sd(as.numeric(x), na.rm = T))

apply(tmp.noMiss %>% select(Weight_0, SystolicBP_0:nhif_0), 2, function(x) mean(as.numeric(x), na.rm = T))
apply(tmp.noMiss %>% select(Weight_0, SystolicBP_0:nhif_0), 2, function(x) sd(as.numeric(x), na.rm = T))

apply(tmp.wizMiss %>% select(tbtx_0, activeTB_0, Married_0, nhif_0), 2, function(x) sum(as.numeric(x), na.rm = T))
apply(tmp.noMiss %>% select(tbtx_0, activeTB_0, Married_0, nhif_0), 2, function(x) sum(as.numeric(x)))
# summary(tmp$nhif_0)
apply(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop %>% select(Weight_0), 2, sd)

table(tmp.noMiss$viralloadImptCtgr_0)
table(tmp.noMiss$viralloadImptCtgr_1)
table(tmp.noMiss$viralloadImptCtgr_2)
table(tmp.noMiss$viralloadImptCtgr_3)

table(tmp.noMiss$viralloadImptCtgr_0) / summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_0
table(tmp.noMiss$viralloadImptCtgr_1) / summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_1
table(tmp.noMiss$viralloadImptCtgr_2) / summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_2
table(tmp.noMiss$viralloadImptCtgr_3) / summLeft.wizmiss.t0Jul2016.switchDTG.wt200d.resPop$n_3

# time 1
apply(tmp.wizMiss %>% select(SystolicBP_1:nhif_1), 2, function(x) mean(as.numeric(x), na.rm = T))
apply(tmp.wizMiss %>% select(SystolicBP_1:nhif_1), 2, function(x) sd(as.numeric(x), na.rm = T))

apply(tmp.noMiss %>% select(SystolicBP_1:nhif_1), 2, function(x) mean(as.numeric(x), na.rm = T))
apply(tmp.noMiss %>% select(SystolicBP_1:nhif_1), 2, function(x) sd(as.numeric(x), na.rm = T))

apply(tmp.wizMiss %>% select(tbtx_1, activeTB_1, Married_1, nhif_1), 2, function(x) sum(as.numeric(x), na.rm = T))
apply(tmp.noMiss %>% select(tbtx_1, activeTB_1, Married_1, nhif_1), 2, function(x) sum(as.numeric(x), na.rm = T))

# time 2
apply(tmp.wizMiss %>% select(SystolicBP_2:nhif_2), 2, function(x) mean(as.numeric(x), na.rm = T))
apply(tmp.wizMiss %>% select(SystolicBP_2:Height_2), 2, function(x) sd(as.numeric(x), na.rm = T))

apply(tmp.noMiss %>% select(SystolicBP_2:nhif_2), 2, function(x) mean(as.numeric(x), na.rm = T))
apply(tmp.noMiss %>% select(SystolicBP_2:Height_2), 2, function(x) sd(as.numeric(x), na.rm = T))

apply(tmp.wizMiss %>% select(tbtx_2, activeTB_2, Married_2, nhif_2), 2, function(x) sum(as.numeric(x), na.rm = T))
apply(tmp.noMiss %>% select(tbtx_2, activeTB_2, Married_2, nhif_2), 2, function(x) sum(as.numeric(x), na.rm = T))

# time 3
apply(tmp.wizMiss %>% select(SystolicBP_3:nhif_3), 2, function(x) mean(as.numeric(x), na.rm = T))
apply(tmp.wizMiss %>% select(SystolicBP_3:Height_3), 2, function(x) sd(as.numeric(x), na.rm = T))

apply(tmp.noMiss %>% select(SystolicBP_3:nhif_3), 2, function(x) mean(as.numeric(x), na.rm = T))
apply(tmp.noMiss %>% select(SystolicBP_3:Height_3), 2, function(x) sd(as.numeric(x), na.rm = T))

apply(tmp.wizMiss %>% select(tbtx_3, activeTB_3, Married_3, nhif_3), 2, function(x) sum(as.numeric(x), na.rm = T))
apply(tmp.noMiss %>% select(tbtx_3, activeTB_3, Married_3, nhif_3), 2, function(x) sum(as.numeric(x), na.rm = T))



# save.image("../../Data/Data20210512/Processed/Summ2AllWt20220329.RData")
# load("../../Data/Data20210512/Summ2All20210922.RData")
save(ampath.demog, ampath.demog.used,
     ampath.visit, ampath.visit.used, 
     ampath.visit.wt400d.wtTime0, ampath.visit.wt200d.wtTime0, ampath.visit.wt100d.wtTime0,
     pt.ids,
     finalData.noMiss.t0Jul2016.switchDTG.wt400d, finalData.noMiss.t0Jul2016.switchDTG.wt200d, finalData.noMiss.t0Jul2016.switchDTG.wt100d,
     finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop, finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop, finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop, 
     # finalData.wizmiss.t0Jul2016.switchDTG.wt400d.carryOver, finalData.wizmiss.t0Jul2016.switchDTG.wt200d.carryOver, finalData.wizmiss.t0Jul2016.switchDTG.wt100d.carryOver, 
     # finalData.wizmiss.t0Jul2016.switchDTG.wt400d.wtTime0, finalData.wizmiss.t0Jul2016.switchDTG.wt200d.wtTime0, finalData.wizmiss.t0Jul2016.switchDTG.wt100d.wtTime0, 
     # onOffs.wizmiss.t0Jul2016.switchDTG.wt400d, onOffs.wizmiss.t0Jul2016.switchDTG.wt200d, onOffs.wizmiss.t0Jul2016.switchDTG.wt100d,
     # numTherapies.wizmiss.t0Jul2016.switchDTG.wt400d, numTherapies.wizmiss.t0Jul2016.switchDTG.wt200d, numTherapies.wizmiss.t0Jul2016.switchDTG.wt100d,
     maxT.wizmiss.t0Jul2016.switchDTG.wt400d, maxT.wizmiss.t0Jul2016.switchDTG.wt200d, maxT.wizmiss.t0Jul2016.switchDTG.wt100d,
     # tWt.wizmiss.t0Jul2016.switchDTG.wt400d.carryOver, tWt.wizmiss.t0Jul2016.switchDTG.wt200d.carryOver, tWt.wizmiss.t0Jul2016.switchDTG.wt100d.carryOver,
     # wtNew.pt.wt400d, wtNew.pt.wt200d, wtNew.pt.wt100d,
     # pt.someWt.wt400d, pt.someWt.wt200d, pt.someWt.wt100d, 
     # pt.someThrpGteq3Or1.wt400d, pt.someThrpGteq3Or1.wt200d, pt.someThrpGteq3Or1.wt100d,
     # pt.someThrpGteq3Or1AfterWt.wt400d, pt.someThrpGteq3Or1AfterWt.wt200d, pt.someThrpGteq3Or1AfterWt.wt100d,
     # incsstt.yIn.dataCleaning.t0Jul2016.switchDTG.wt400d, incsstt.yIn.dataCleaning.t0Jul2016.switchDTG.wt200d, incsstt.yIn.dataCleaning.t0Jul2016.switchDTG.wt100d,
     # incsstt.yIn.carryOver.t0Jul2016.switchDTG.wt400d, incsstt.yIn.carryOver.t0Jul2016.switchDTG.wt200d, incsstt.yIn.carryOver.t0Jul2016.switchDTG.wt100d,
     # incsstt.yIn.all.t0Jul2016.switchDTG.wt400d, incsstt.yIn.all.t0Jul2016.switchDTG.wt200d, incsstt.yIn.all.t0Jul2016.switchDTG.wt100d,
     # incsstt.yOut.dataCleaning.t0Jul2016.switchDTG.wt400d, incsstt.yOut.dataCleaning.t0Jul2016.switchDTG.wt200d, incsstt.yOut.dataCleaning.t0Jul2016.switchDTG.wt100d,
     # incsstt.yOut.carryOver.t0Jul2016.switchDTG.wt400d, incsstt.yOut.carryOver.t0Jul2016.switchDTG.wt200d, incsstt.yOut.carryOver.t0Jul2016.switchDTG.wt100d,
     # incsstt.yOut.all.t0Jul2016.switchDTG.wt400d, incsstt.yOut.all.t0Jul2016.switchDTG.wt200d, incsstt.yOut.all.t0Jul2016.switchDTG.wt100d,
     summCensor.wizmiss.t0Jul2016.switchDTG.wt400d.resPop, summCensor.wizmiss.t0Jul2016.switchDTG.wt200d.resPop, summCensor.wizmiss.t0Jul2016.switchDTG.wt100d.resPop,
     summDrop.wizmiss.t0Jul2016.switchDTG.wt400d.resPop, summDrop.wizmiss.t0Jul2016.switchDTG.wt200d.resPop, summDrop.wizmiss.t0Jul2016.switchDTG.wt100d.resPop,
     summImpute.wizmiss.t0Jul2016.switchDTG.wt400d.resPop, summImpute.wizmiss.t0Jul2016.switchDTG.wt200d.resPop, summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop,
     file = "../../Data/Data20210512/Processed/Summ2AllWt20220511.RData")



