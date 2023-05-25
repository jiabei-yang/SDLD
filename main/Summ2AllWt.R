library(dplyr)

load("../../../Data/Data20210512/Processed/Stp7SbgrpWt20230226.RData")

finalData.wizmiss.wt200d.sbgrpWt.resPop <- finalData.wizmiss.wt200d.sbgrpWt.resPop %>%
  mutate(log10Viralload_0 = log10(viralload_0 + 0.001),
         log10Viralload_1 = log10(viralload_1 + 0.001),
         log10Viralload_2 = log10(viralload_2 + 0.001),
         log10Viralload_3 = log10(viralload_3 + 0.001),
         log10Viralload_4 = log10(viralload_4 + 0.001),
         log10Viralload_5 = log10(viralload_5 + 0.001),
         log10Viralload_6 = log10(viralload_6 + 0.001),
         log10Viralload_7 = log10(viralload_7 + 0.001))
finalData.wizmiss.wt200d.sbgrpWt.resPop <- finalData.wizmiss.wt200d.sbgrpWt.resPop %>%
  select(patient_id:viralload_0, log10Viralload_0, SystolicBP_0:viralload_1, log10Viralload_1,
         SystolicBP_1:viralload_2, log10Viralload_2, SystolicBP_2:viralload_3, log10Viralload_3,
         SystolicBP_3:viralload_4, log10Viralload_4, SystolicBP_4:viralload_5, log10Viralload_5,
         SystolicBP_5:viralload_6, log10Viralload_6, SystolicBP_6:viralload_7, log10Viralload_7,
         SystolicBP_7:Weight_8)

table(finalData.wizmiss.wt200d.sbgrpWt.resPop$artStatus)
subData.wizmiss.wt200d.sbgrpWt.resPop <- finalData.wizmiss.wt200d.sbgrpWt.resPop %>%
  filter(artStatus %in% c("naive", "treatmentFree"))
dim(subData.wizmiss.wt200d.sbgrpWt.resPop)
# head(subData.wizmiss.wt200d.sbgrpWt.resPop)

tmp.trtPttn <- subData.wizmiss.wt200d.sbgrpWt.resPop %>%
  select(patient_id, artStatus, DTGOn_0, DTGOn_1, DTGOn_2, DTGOn_3, Weight_4) %>%
  unite(dtgPttn, DTGOn_0:DTGOn_3, remove = F) 

tmp.trtPttn.naive <- tmp.trtPttn %>%
  filter(artStatus %in% c("naive", "treatmentFree"))
table(tmp.trtPttn.naive$dtgPttn)
table(tmp.trtPttn.naive$dtgPttn[!is.na(tmp.trtPttn.naive$Weight_4)])




#############################################################
# Create a complete dataset for treatment effect estimation #
#############################################################
dim(subData.wizmiss.wt200d.sbgrpWt.resPop)
apply(subData.wizmiss.wt200d.sbgrpWt.resPop %>% select(male:WaterPipedInHome), 2, function(x) sum(is.na(x)))
apply(subData.wizmiss.wt200d.sbgrpWt.resPop %>% select(male:WaterPipedInHome), 2, function(x) mean(is.na(x)))

# remove time 0 after 20160701 which is always true
# remove the 3 demographic variables with ~1/3 missing
table(subData.wizmiss.wt200d.sbgrpWt.resPop$time0_after20160701)
# there are participants who start arv after time 0, time0_arvstart 0
identical(subData.wizmiss.wt200d.sbgrpWt.resPop$time0_dob - subData.wizmiss.wt200d.sbgrpWt.resPop$arvstart_dob,
          subData.wizmiss.wt200d.sbgrpWt.resPop$time0_arvstart)
# which(subData.wizmiss.wt200d.sbgrpWt.resPop$time0_dob - subData.wizmiss.wt200d.sbgrpWt.resPop$arvstart_dob !=
#         subData.wizmiss.wt200d.sbgrpWt.resPop$time0_arvstart)
table(subData.wizmiss.wt200d.sbgrpWt.resPop[which(subData.wizmiss.wt200d.sbgrpWt.resPop$time0_dob - subData.wizmiss.wt200d.sbgrpWt.resPop$arvstart_dob !=
                                                    subData.wizmiss.wt200d.sbgrpWt.resPop$time0_arvstart), "time0_arvstart"])

subData.nomiss.wt200d.sbgrpWt.resPop <- subData.wizmiss.wt200d.sbgrpWt.resPop %>%
  select(-time0_after20160701, -(EmployedOutsideHome:WaterPipedInHome))
dim(subData.nomiss.wt200d.sbgrpWt.resPop)

# Censoring status
length.pt.ids.wt200d.resPop <- nrow(subData.nomiss.wt200d.sbgrpWt.resPop)

summCensor.wizmiss.wt200d.sbgrpWt.resPop <- rbind(apply(subData.wizmiss.wt200d.sbgrpWt.resPop %>% select(starts_with("censor_")), 2, function(x) sum(x == 1, na.rm = T)),
                                                  cumsum(apply(subData.wizmiss.wt200d.sbgrpWt.resPop %>% select(starts_with("censor_")), 2, function(x) sum(x == 1, na.rm = T))))
summCensor.wizmiss.wt200d.sbgrpWt.resPop <- data.frame(type = c("increment", "cumulative"),
                                                       summCensor.wizmiss.wt200d.sbgrpWt.resPop)
colnames(summCensor.wizmiss.wt200d.sbgrpWt.resPop) <- c("type", paste0("n_", 0:(maxT.wizmiss.t0Jul2016.switchDTG.wt200d-2)))
summCensor.wizmiss.wt200d.sbgrpWt.resPop <- summCensor.wizmiss.wt200d.sbgrpWt.resPop %>%
  mutate(n_8 = NA)
tmp <- data.frame(type = "left",
                  n_0 = length.pt.ids.wt200d.resPop, 
                  data.frame(t(nrow(subData.wizmiss.wt200d.sbgrpWt.resPop) - cumsum(apply(subData.wizmiss.wt200d.sbgrpWt.resPop %>% select(starts_with("censor_")), 2, function(x) sum(x == 1, na.rm = T))))))
colnames(tmp) <- c("type", paste0("n_", 0:(maxT.wizmiss.t0Jul2016.switchDTG.wt200d - 1)))
summCensor.wizmiss.wt200d.sbgrpWt.resPop <- rbind(summCensor.wizmiss.wt200d.sbgrpWt.resPop,
                                                  tmp)
summCensor.wizmiss.wt200d.sbgrpWt.resPop <- summCensor.wizmiss.wt200d.sbgrpWt.resPop %>%
  mutate(p_0 = n_0 / length.pt.ids.wt200d.resPop,
         p_1 = n_1 / length.pt.ids.wt200d.resPop,
         p_2 = n_2 / length.pt.ids.wt200d.resPop,
         p_3 = n_3 / length.pt.ids.wt200d.resPop,
         p_4 = n_4 / length.pt.ids.wt200d.resPop,
         p_5 = n_5 / length.pt.ids.wt200d.resPop,
         p_6 = n_6 / length.pt.ids.wt200d.resPop,
         p_7 = n_7 / length.pt.ids.wt200d.resPop,
         p_8 = n_8 / length.pt.ids.wt200d.resPop)
summCensor.wizmiss.wt200d.sbgrpWt.resPop <- summCensor.wizmiss.wt200d.sbgrpWt.resPop %>%
  select(type, n_0, p_0, n_1, p_1, n_2, p_2, n_3, p_3, n_4, p_4, n_5, p_5, n_6, p_6, n_7, p_7, n_8, p_8)

# 200-day
apply(subData.wizmiss.wt200d.sbgrpWt.resPop %>% select(Weight_0:censor_0), 2, function(x) mean(is.na(x)))
apply(subData.nomiss.wt200d.sbgrpWt.resPop %>% select(Weight_0:censor_0), 2, function(x) mean(is.na(x)))

# remove cd4 with >90% missing
summDrop.wizmiss.wt200d.sbgrpWt.resPop <- data.frame(covariate = "cd4",
                                                     n_0 = sum(is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$cd4_0)),
                                                     p_0 = mean(is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$cd4_0)), 
                                                     n_1 = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_0 == 0) & 
                                                                 is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$cd4_1)),
                                                     n_2 = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_1 == 0) & 
                                                                 is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$cd4_2), na.rm = T),
                                                     n_3 = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_2 == 0) & 
                                                                 is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$cd4_3), na.rm = T), 
                                                     n_4 = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_3 == 0) & 
                                                                 is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$cd4_4), na.rm = T),
                                                     n_5 = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_4 == 0) & 
                                                                 is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$cd4_5), na.rm = T),
                                                     n_6 = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_5 == 0) & 
                                                                 is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$cd4_6), na.rm = T),
                                                     n_7 = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_6 == 0) & 
                                                                 is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$cd4_7), na.rm = T))
# check why n_1 na
# ####### STOPPED HERE FIX N_1 NA
# subData.wizmiss.wt200d.sbgrpWt.resPop %>%
#   filter(is.na(censor_0)) %>%
#   select(patient_id, artStatus, censor_0)
# finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop %>%
#   filter(patient_id == 894654)
# finalData.wizmiss.t0Jul2016.switchDTG.wt200d.sbgrpWt %>%
#   filter(patient_id == 894654)
# finalData.wizmiss.t0Jul2016.switchDTG.wt200d.sbgrpWt$patient_id[ind.needToMoveTime0[1]]
# finalData.wizmiss.t0Jul2016.switchDTG.wt200d.sbgrpWt %>%
#   filter(patient_id == 3309)
# finalData.wizmiss.wt200d.sbgrpWt.carryOver %>%
#   filter(patient_id == 894654)
# finalData.wizmiss.wt200d.sbgrpWt.carryOver %>%
#   filter(patient_id == 3309)

summLeft.wizmiss.wt200d.sbgrpWt.resPop <- summCensor.wizmiss.wt200d.sbgrpWt.resPop %>%
  filter(type == "left")
summDrop.wizmiss.wt200d.sbgrpWt.resPop <- summDrop.wizmiss.wt200d.sbgrpWt.resPop %>%
  mutate(p_1 = n_1 / summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_1,
         p_2 = n_2 / summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_2,
         p_3 = n_3 / summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_3,
         p_4 = n_4 / summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_4,
         p_5 = n_5 / summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_5,
         p_6 = n_6 / summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_6,
         p_7 = n_7 / summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_7)
summDrop.wizmiss.wt200d.sbgrpWt.resPop <- summDrop.wizmiss.wt200d.sbgrpWt.resPop %>%
  select(covariate:n_1, p_1, n_2, p_2, n_3, p_3, n_4, p_4, n_5, p_5, n_6, p_6, n_7, p_7)
subData.nomiss.wt200d.sbgrpWt.resPop <- subData.nomiss.wt200d.sbgrpWt.resPop %>%
  select(-starts_with("cd4_"))

# viral load
apply(subData.wizmiss.wt200d.sbgrpWt.resPop %>% select(starts_with("viralload")), 2, function(x) mean(is.na(x), na.rm = T))
summary(subData.wizmiss.wt200d.sbgrpWt.resPop$viralload_0)
summary(subData.wizmiss.wt200d.sbgrpWt.resPop$viralload_1)
summary(subData.wizmiss.wt200d.sbgrpWt.resPop$viralload_2)
summary(subData.wizmiss.wt200d.sbgrpWt.resPop$viralload_3)
# impute with average
tmp.summImpute <- data.frame(covariate      = "Viral load",
                             n_miss_0       = sum(is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$viralload_0)),
                             p_miss_0       = mean(is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$viralload_0)),
                             imputed_mean_0 = mean(subData.wizmiss.wt200d.sbgrpWt.resPop$viralload_0, na.rm = T),
                             n_miss_1       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_0 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$viralload_1)),
                             p_miss_1       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_0 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$viralload_1)) / 
                               summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_1,
                             imputed_mean_1 = mean(subData.wizmiss.wt200d.sbgrpWt.resPop$viralload_1, na.rm = T),
                             n_miss_2       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_1 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$viralload_2), na.rm = T),
                             p_miss_2       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_1 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$viralload_2), na.rm = T) / 
                               summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_2, 
                             imputed_mean_2 = mean(subData.wizmiss.wt200d.sbgrpWt.resPop$viralload_2, na.rm = T),
                             n_miss_3       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_2 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$viralload_3), na.rm = T),
                             p_miss_3       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_2 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$viralload_3), na.rm = T) /
                               summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_3,
                             imputed_mean_3 = mean(subData.wizmiss.wt200d.sbgrpWt.resPop$viralload_3, na.rm = T),
                             n_miss_4       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_3 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$viralload_4), na.rm = T),
                             p_miss_4       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_3 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$viralload_4), na.rm = T) / 
                               summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_4, 
                             imputed_mean_4 = mean(subData.wizmiss.wt200d.sbgrpWt.resPop$viralload_4, na.rm = T),
                             n_miss_5       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_4 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$viralload_5), na.rm = T),
                             p_miss_5       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_4 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$viralload_5), na.rm = T) /
                               summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_5,
                             imputed_mean_5 = mean(subData.wizmiss.wt200d.sbgrpWt.resPop$viralload_5, na.rm = T),
                             n_miss_6       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_5 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$viralload_6), na.rm = T),
                             p_miss_6       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_5 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$viralload_6), na.rm = T) / 
                               summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_6, 
                             imputed_mean_6 = mean(subData.wizmiss.wt200d.sbgrpWt.resPop$viralload_6, na.rm = T),
                             n_miss_7       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_6 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$viralload_7), na.rm = T),
                             p_miss_7       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_6 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$viralload_7), na.rm = T) /
                               summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_7,
                             imputed_mean_7 = mean(subData.wizmiss.wt200d.sbgrpWt.resPop$viralload_7, na.rm = T))
summImpute.wizmiss.wt200d.sbgrpWt.resPop <- tmp.summImpute
subData.nomiss.wt200d.sbgrpWt.resPop <- subData.nomiss.wt200d.sbgrpWt.resPop %>%
  mutate(viralloadImptMn_0 = ifelse(is.na(viralload_0), tmp.summImpute$imputed_mean_0, viralload_0)) %>%
  mutate(viralloadImptMn_1 = ifelse((is.na(viralload_1)) & (censor_0 == 0), tmp.summImpute$imputed_mean_1, viralload_1)) %>%
  mutate(viralloadImptMn_2 = ifelse((is.na(viralload_2)) & (censor_1 == 0), tmp.summImpute$imputed_mean_2, viralload_2)) %>%
  mutate(viralloadImptMn_3 = ifelse((is.na(viralload_3)) & (censor_2 == 0), tmp.summImpute$imputed_mean_3, viralload_3)) %>%
  mutate(viralloadImptMn_4 = ifelse((is.na(viralload_4)) & (censor_3 == 0), tmp.summImpute$imputed_mean_4, viralload_4)) %>%
  mutate(viralloadImptMn_5 = ifelse((is.na(viralload_5)) & (censor_4 == 0), tmp.summImpute$imputed_mean_5, viralload_5)) %>%
  mutate(viralloadImptMn_6 = ifelse((is.na(viralload_6)) & (censor_5 == 0), tmp.summImpute$imputed_mean_6, viralload_6)) %>%
  mutate(viralloadImptMn_7 = ifelse((is.na(viralload_7)) & (censor_6 == 0), tmp.summImpute$imputed_mean_7, viralload_7))
subData.nomiss.wt200d.sbgrpWt.resPop <- subData.nomiss.wt200d.sbgrpWt.resPop %>%
  mutate(tmpViralload_0 = ifelse(is.na(viralload_0), -1, viralload_0),
         tmpViralload_1 = ifelse(is.na(viralload_1) & (censor_0 == 0), -1, viralload_1),
         tmpViralload_2 = ifelse(is.na(viralload_2) & (censor_1 == 0), -1, viralload_2),
         tmpViralload_3 = ifelse(is.na(viralload_3) & (censor_2 == 0), -1, viralload_3),
         tmpViralload_4 = ifelse(is.na(viralload_4) & (censor_3 == 0), -1, viralload_4),
         tmpViralload_5 = ifelse(is.na(viralload_5) & (censor_4 == 0), -1, viralload_5),
         tmpViralload_6 = ifelse(is.na(viralload_6) & (censor_5 == 0), -1, viralload_6),
         tmpViralload_7 = ifelse(is.na(viralload_7) & (censor_6 == 0), -1, viralload_7)) 
subData.nomiss.wt200d.sbgrpWt.resPop <- subData.nomiss.wt200d.sbgrpWt.resPop %>%
  mutate(viralloadImptCtgr_0 = ifelse((tmpViralload_0 == -1), 4, NA)) %>%
  mutate(viralloadImptCtgr_0 = ifelse((tmpViralload_0 > -1) & (tmpViralload_0 < 10^3), 1, viralloadImptCtgr_0)) %>%
  mutate(viralloadImptCtgr_0 = ifelse((tmpViralload_0 >= 10^3) & (tmpViralload_0 < 10^4), 2, viralloadImptCtgr_0)) %>%
  mutate(viralloadImptCtgr_0 = ifelse((tmpViralload_0 >= 10^4), 3, viralloadImptCtgr_0)) %>%
  mutate(viralloadImptCtgr_1 = ifelse((tmpViralload_1 == -1), 4, NA)) %>%
  mutate(viralloadImptCtgr_1 = ifelse((tmpViralload_1 > -1) & (tmpViralload_1 < 10^3), 1, viralloadImptCtgr_1)) %>%
  mutate(viralloadImptCtgr_1 = ifelse((tmpViralload_1 >= 10^3) & (tmpViralload_1 < 10^4), 2, viralloadImptCtgr_1)) %>%
  mutate(viralloadImptCtgr_1 = ifelse((tmpViralload_1 >= 10^4), 3, viralloadImptCtgr_1)) %>%
  mutate(viralloadImptCtgr_2 = ifelse((tmpViralload_2 == -1), 4, NA)) %>%
  mutate(viralloadImptCtgr_2 = ifelse((tmpViralload_2 > -1) & (tmpViralload_2 < 10^3), 1, viralloadImptCtgr_2)) %>%
  mutate(viralloadImptCtgr_2 = ifelse((tmpViralload_2 >= 10^3) & (tmpViralload_2 < 10^4), 2, viralloadImptCtgr_2)) %>%
  mutate(viralloadImptCtgr_2 = ifelse((tmpViralload_2 >= 10^4), 3, viralloadImptCtgr_2)) %>%
  mutate(viralloadImptCtgr_3 = ifelse((tmpViralload_3 == -1), 4, NA)) %>%
  mutate(viralloadImptCtgr_3 = ifelse((tmpViralload_3 > -1) & (tmpViralload_3 < 10^3), 1, viralloadImptCtgr_3)) %>%
  mutate(viralloadImptCtgr_3 = ifelse((tmpViralload_3 >= 10^3) & (tmpViralload_3 < 10^4), 2, viralloadImptCtgr_3)) %>%
  mutate(viralloadImptCtgr_3 = ifelse((tmpViralload_3 >= 10^4), 3, viralloadImptCtgr_3)) %>%
  mutate(viralloadImptCtgr_4 = ifelse((tmpViralload_4 == -1), 4, NA)) %>%
  mutate(viralloadImptCtgr_4 = ifelse((tmpViralload_4 > -1) & (tmpViralload_4 < 10^3), 1, viralloadImptCtgr_4)) %>%
  mutate(viralloadImptCtgr_4 = ifelse((tmpViralload_4 >= 10^3) & (tmpViralload_4 < 10^4), 2, viralloadImptCtgr_4)) %>%
  mutate(viralloadImptCtgr_4 = ifelse((tmpViralload_4 >= 10^4), 3, viralloadImptCtgr_4)) %>%
  mutate(viralloadImptCtgr_5 = ifelse((tmpViralload_5 == -1), 4, NA)) %>%
  mutate(viralloadImptCtgr_5 = ifelse((tmpViralload_5 > -1) & (tmpViralload_5 < 10^3), 1, viralloadImptCtgr_5)) %>%
  mutate(viralloadImptCtgr_5 = ifelse((tmpViralload_5 >= 10^3) & (tmpViralload_5 < 10^4), 2, viralloadImptCtgr_5)) %>%
  mutate(viralloadImptCtgr_5 = ifelse((tmpViralload_5 >= 10^4), 3, viralloadImptCtgr_5))  %>%
  mutate(viralloadImptCtgr_6 = ifelse((tmpViralload_6 == -1), 4, NA)) %>%
  mutate(viralloadImptCtgr_6 = ifelse((tmpViralload_6 > -1) & (tmpViralload_6 < 10^3), 1, viralloadImptCtgr_6)) %>%
  mutate(viralloadImptCtgr_6 = ifelse((tmpViralload_6 >= 10^3) & (tmpViralload_6 < 10^4), 2, viralloadImptCtgr_6)) %>%
  mutate(viralloadImptCtgr_6 = ifelse((tmpViralload_6 >= 10^4), 3, viralloadImptCtgr_6)) %>%
  mutate(viralloadImptCtgr_7 = ifelse((tmpViralload_7 == -1), 4, NA)) %>%
  mutate(viralloadImptCtgr_7 = ifelse((tmpViralload_7 > -1) & (tmpViralload_7 < 10^3), 1, viralloadImptCtgr_7)) %>%
  mutate(viralloadImptCtgr_7 = ifelse((tmpViralload_7 >= 10^3) & (tmpViralload_7 < 10^4), 2, viralloadImptCtgr_7)) %>%
  mutate(viralloadImptCtgr_7 = ifelse((tmpViralload_7 >= 10^4), 3, viralloadImptCtgr_7)) 
summary(subData.nomiss.wt200d.sbgrpWt.resPop %>% select(starts_with("viralload")))

# log10Viralload 
apply(subData.wizmiss.wt200d.sbgrpWt.resPop %>% select(starts_with("log10Viralload")), 2, function(x) mean(is.na(x)))
summary(subData.wizmiss.wt200d.sbgrpWt.resPop$log10Viralload_0)
summary(subData.wizmiss.wt200d.sbgrpWt.resPop$log10Viralload_1)
summary(subData.wizmiss.wt200d.sbgrpWt.resPop$log10Viralload_2)
summary(subData.wizmiss.wt200d.sbgrpWt.resPop$log10Viralload_3)
# impute with average at each time point
tmp.summImpute <- data.frame(covariate      = "log10(viral load + 0.001)",
                             n_miss_0       = sum(is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$log10Viralload_0)),
                             p_miss_0       = mean(is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$log10Viralload_0)),
                             imputed_mean_0 = mean(subData.wizmiss.wt200d.sbgrpWt.resPop$log10Viralload_0, na.rm = T),
                             n_miss_1       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_0 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$log10Viralload_1)),
                             p_miss_1       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_0 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$log10Viralload_1)) / 
                               summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_1,
                             imputed_mean_1 = mean(subData.wizmiss.wt200d.sbgrpWt.resPop$log10Viralload_1, na.rm = T),
                             n_miss_2       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_1 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$log10Viralload_2), na.rm = T),
                             p_miss_2       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_1 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$log10Viralload_2), na.rm = T) / 
                               summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_2, 
                             imputed_mean_2 = mean(subData.wizmiss.wt200d.sbgrpWt.resPop$log10Viralload_2, na.rm = T),
                             n_miss_3       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_2 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$log10Viralload_3), na.rm = T),
                             p_miss_3       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_2 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$log10Viralload_3), na.rm = T) /
                               summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_3,
                             imputed_mean_3 = mean(subData.wizmiss.wt200d.sbgrpWt.resPop$log10Viralload_3, na.rm = T),
                             n_miss_4       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_3 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$log10Viralload_4), na.rm = T),
                             p_miss_4       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_3 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$log10Viralload_4), na.rm = T) / 
                               summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_4, 
                             imputed_mean_4 = mean(subData.wizmiss.wt200d.sbgrpWt.resPop$log10Viralload_4, na.rm = T),
                             n_miss_5       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_4 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$log10Viralload_5), na.rm = T),
                             p_miss_5       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_4 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$log10Viralload_5), na.rm = T) /
                               summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_5,
                             imputed_mean_5 = mean(subData.wizmiss.wt200d.sbgrpWt.resPop$log10Viralload_5, na.rm = T),
                             n_miss_6       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_5 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$log10Viralload_6), na.rm = T),
                             p_miss_6       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_5 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$log10Viralload_6), na.rm = T) / 
                               summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_6, 
                             imputed_mean_6 = mean(subData.wizmiss.wt200d.sbgrpWt.resPop$log10Viralload_6, na.rm = T),
                             n_miss_7       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_6 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$log10Viralload_7), na.rm = T),
                             p_miss_7       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_6 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$log10Viralload_7), na.rm = T) /
                               summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_7,
                             imputed_mean_7 = mean(subData.wizmiss.wt200d.sbgrpWt.resPop$log10Viralload_7, na.rm = T))
summImpute.wizmiss.wt200d.sbgrpWt.resPop <- rbind(summImpute.wizmiss.wt200d.sbgrpWt.resPop,
                                                  tmp.summImpute)
subData.nomiss.wt200d.sbgrpWt.resPop <- subData.nomiss.wt200d.sbgrpWt.resPop %>%
  mutate(log10ViralloadImptMn_0 = ifelse(is.na(log10Viralload_0), tmp.summImpute$imputed_mean_0, log10Viralload_0)) %>%
  mutate(log10ViralloadImptMn_1 = ifelse((is.na(log10Viralload_1)) & (censor_0 == 0), tmp.summImpute$imputed_mean_1, log10Viralload_1)) %>%
  mutate(log10ViralloadImptMn_2 = ifelse((is.na(log10Viralload_2)) & (censor_1 == 0), tmp.summImpute$imputed_mean_2, log10Viralload_2)) %>%
  mutate(log10ViralloadImptMn_3 = ifelse((is.na(log10Viralload_3)) & (censor_2 == 0), tmp.summImpute$imputed_mean_3, log10Viralload_3)) %>%
  mutate(log10ViralloadImptMn_4 = ifelse((is.na(log10Viralload_4)) & (censor_3 == 0), tmp.summImpute$imputed_mean_4, log10Viralload_4)) %>%
  mutate(log10ViralloadImptMn_5 = ifelse((is.na(log10Viralload_5)) & (censor_4 == 0), tmp.summImpute$imputed_mean_5, log10Viralload_5)) %>%
  mutate(log10ViralloadImptMn_6 = ifelse((is.na(log10Viralload_6)) & (censor_5 == 0), tmp.summImpute$imputed_mean_6, log10Viralload_6)) %>%
  mutate(log10ViralloadImptMn_7 = ifelse((is.na(log10Viralload_7)) & (censor_6 == 0), tmp.summImpute$imputed_mean_7, log10Viralload_7))
summary(subData.nomiss.wt200d.sbgrpWt.resPop)
subData.nomiss.wt200d.sbgrpWt.resPop <- subData.nomiss.wt200d.sbgrpWt.resPop %>%
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
                             n_miss_0       = sum(is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$SystolicBP_0)),
                             p_miss_0       = mean(is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$SystolicBP_0)),
                             imputed_mean_0 = mean(subData.wizmiss.wt200d.sbgrpWt.resPop$SystolicBP_0, na.rm = T),
                             n_miss_1       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_0 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$SystolicBP_1)),
                             p_miss_1       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_0 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$SystolicBP_1)) / 
                               summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_1,
                             imputed_mean_1 = mean(subData.wizmiss.wt200d.sbgrpWt.resPop$SystolicBP_1, na.rm = T),
                             n_miss_2       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_1 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$SystolicBP_2), na.rm = T),
                             p_miss_2       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_1 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$SystolicBP_2), na.rm = T) / 
                               summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_2,
                             imputed_mean_2 = mean(subData.wizmiss.wt200d.sbgrpWt.resPop$SystolicBP_2, na.rm = T),
                             n_miss_3       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_2 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$SystolicBP_3), na.rm = T),
                             p_miss_3       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_2 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$SystolicBP_3), na.rm = T) / 
                               summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_3,
                             imputed_mean_3 = mean(subData.wizmiss.wt200d.sbgrpWt.resPop$SystolicBP_3, na.rm = T),
                             n_miss_4       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_3 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$SystolicBP_4), na.rm = T),
                             p_miss_4       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_3 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$SystolicBP_4), na.rm = T) / 
                               summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_4,
                             imputed_mean_4 = mean(subData.wizmiss.wt200d.sbgrpWt.resPop$SystolicBP_4, na.rm = T),
                             n_miss_5       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_4 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$SystolicBP_5), na.rm = T),
                             p_miss_5       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_4 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$SystolicBP_5), na.rm = T) / 
                               summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_5,
                             imputed_mean_5 = mean(subData.wizmiss.wt200d.sbgrpWt.resPop$SystolicBP_5, na.rm = T),
                             n_miss_6       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_5 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$SystolicBP_6), na.rm = T),
                             p_miss_6       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_5 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$SystolicBP_6), na.rm = T) / 
                               summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_6,
                             imputed_mean_6 = mean(subData.wizmiss.wt200d.sbgrpWt.resPop$SystolicBP_6, na.rm = T),
                             n_miss_7       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_6 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$SystolicBP_7), na.rm = T),
                             p_miss_7       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_6 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$SystolicBP_7), na.rm = T) / 
                               summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_7,
                             imputed_mean_7 = mean(subData.wizmiss.wt200d.sbgrpWt.resPop$SystolicBP_7, na.rm = T))
summImpute.wizmiss.wt200d.sbgrpWt.resPop <- rbind(summImpute.wizmiss.wt200d.sbgrpWt.resPop,
                                                  tmp.summImpute)
subData.nomiss.wt200d.sbgrpWt.resPop <- subData.nomiss.wt200d.sbgrpWt.resPop %>%
  mutate(SystolicBP_0 = ifelse(is.na(SystolicBP_0), tmp.summImpute$imputed_mean_0, SystolicBP_0)) %>%
  mutate(SystolicBP_1 = ifelse((is.na(SystolicBP_1)) & (censor_0 == 0), tmp.summImpute$imputed_mean_1, SystolicBP_1)) %>%
  mutate(SystolicBP_2 = ifelse((is.na(SystolicBP_2)) & (censor_1 == 0), tmp.summImpute$imputed_mean_2, SystolicBP_2)) %>%
  mutate(SystolicBP_3 = ifelse((is.na(SystolicBP_3)) & (censor_2 == 0), tmp.summImpute$imputed_mean_3, SystolicBP_3)) %>%
  mutate(SystolicBP_4 = ifelse((is.na(SystolicBP_4)) & (censor_3 == 0), tmp.summImpute$imputed_mean_4, SystolicBP_4)) %>%
  mutate(SystolicBP_5 = ifelse((is.na(SystolicBP_5)) & (censor_4 == 0), tmp.summImpute$imputed_mean_5, SystolicBP_5)) %>%
  mutate(SystolicBP_6 = ifelse((is.na(SystolicBP_6)) & (censor_5 == 0), tmp.summImpute$imputed_mean_6, SystolicBP_6)) %>%
  mutate(SystolicBP_7 = ifelse((is.na(SystolicBP_7)) & (censor_6 == 0), tmp.summImpute$imputed_mean_7, SystolicBP_7))

mean(subData.wizmiss.wt200d.sbgrpWt.resPop$SystolicBP_0, na.rm = T)
mean(subData.nomiss.wt200d.sbgrpWt.resPop$SystolicBP_0)

# expect the same
sum(is.na(subData.nomiss.wt200d.sbgrpWt.resPop$SystolicBP_2))
sum(subData.nomiss.wt200d.sbgrpWt.resPop$censor_1 == 1, na.rm = T) + sum(subData.nomiss.wt200d.sbgrpWt.resPop$censor_0 == 1) 

# diastolic blood pressure
# impute with average at each time point
# mean(is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$DiastolicBP_0), na.rm = T)
tmp.summImpute <- data.frame(covariate      = "Diastolic blood pressure",
                             n_miss_0       = sum(is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$DiastolicBP_0)),
                             p_miss_0       = mean(is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$DiastolicBP_0)),
                             imputed_mean_0 = mean(subData.wizmiss.wt200d.sbgrpWt.resPop$DiastolicBP_0, na.rm = T),
                             n_miss_1       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_0 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$DiastolicBP_1)),
                             p_miss_1       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_0 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$DiastolicBP_1)) / 
                               summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_1,
                             imputed_mean_1 = mean(subData.wizmiss.wt200d.sbgrpWt.resPop$DiastolicBP_1, na.rm = T),
                             n_miss_2       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_1 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$DiastolicBP_2), na.rm = T),
                             p_miss_2       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_1 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$DiastolicBP_2), na.rm = T) / 
                               summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_2,
                             imputed_mean_2 = mean(subData.wizmiss.wt200d.sbgrpWt.resPop$DiastolicBP_2, na.rm = T),
                             n_miss_3       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_2 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$DiastolicBP_3), na.rm = T),
                             p_miss_3       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_2 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$DiastolicBP_3), na.rm = T) / 
                               summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_3,
                             imputed_mean_3 = mean(subData.wizmiss.wt200d.sbgrpWt.resPop$DiastolicBP_3, na.rm = T),
                             n_miss_4       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_3 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$DiastolicBP_4), na.rm = T),
                             p_miss_4       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_3 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$DiastolicBP_4), na.rm = T) / 
                               summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_4,
                             imputed_mean_4 = mean(subData.wizmiss.wt200d.sbgrpWt.resPop$DiastolicBP_4, na.rm = T),
                             n_miss_5       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_4 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$DiastolicBP_5), na.rm = T),
                             p_miss_5       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_4 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$DiastolicBP_5), na.rm = T) / 
                               summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_5,
                             imputed_mean_5 = mean(subData.wizmiss.wt200d.sbgrpWt.resPop$DiastolicBP_5, na.rm = T),
                             n_miss_6       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_5 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$DiastolicBP_6), na.rm = T),
                             p_miss_6       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_5 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$DiastolicBP_6), na.rm = T) / 
                               summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_6,
                             imputed_mean_6 = mean(subData.wizmiss.wt200d.sbgrpWt.resPop$DiastolicBP_6, na.rm = T),
                             n_miss_7       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_6 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$DiastolicBP_7), na.rm = T),
                             p_miss_7       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_6 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$DiastolicBP_7), na.rm = T) / 
                               summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_7,
                             imputed_mean_7 = mean(subData.wizmiss.wt200d.sbgrpWt.resPop$DiastolicBP_7, na.rm = T))
summImpute.wizmiss.wt200d.sbgrpWt.resPop <- rbind(summImpute.wizmiss.wt200d.sbgrpWt.resPop,
                                                  tmp.summImpute)
subData.nomiss.wt200d.sbgrpWt.resPop <- subData.nomiss.wt200d.sbgrpWt.resPop %>%
  mutate(DiastolicBP_0 = ifelse(is.na(DiastolicBP_0), tmp.summImpute$imputed_mean_0, DiastolicBP_0)) %>%
  mutate(DiastolicBP_1 = ifelse((is.na(DiastolicBP_1)) & (censor_0 == 0), tmp.summImpute$imputed_mean_1, DiastolicBP_1)) %>%
  mutate(DiastolicBP_2 = ifelse((is.na(DiastolicBP_2)) & (censor_1 == 0), tmp.summImpute$imputed_mean_2, DiastolicBP_2)) %>%
  mutate(DiastolicBP_3 = ifelse((is.na(DiastolicBP_3)) & (censor_2 == 0), tmp.summImpute$imputed_mean_3, DiastolicBP_3)) %>%
  mutate(DiastolicBP_4 = ifelse((is.na(DiastolicBP_4)) & (censor_3 == 0), tmp.summImpute$imputed_mean_4, DiastolicBP_4)) %>%
  mutate(DiastolicBP_5 = ifelse((is.na(DiastolicBP_5)) & (censor_4 == 0), tmp.summImpute$imputed_mean_5, DiastolicBP_5)) %>%
  mutate(DiastolicBP_6 = ifelse((is.na(DiastolicBP_6)) & (censor_5 == 0), tmp.summImpute$imputed_mean_6, DiastolicBP_6)) %>%
  mutate(DiastolicBP_7 = ifelse((is.na(DiastolicBP_7)) & (censor_6 == 0), tmp.summImpute$imputed_mean_7, DiastolicBP_7))

# expect the same
sum(is.na(subData.nomiss.wt200d.sbgrpWt.resPop$DiastolicBP_3))
sum(subData.nomiss.wt200d.sbgrpWt.resPop$censor_2 == 1, na.rm = T) +
  sum(subData.nomiss.wt200d.sbgrpWt.resPop$censor_1 == 1, na.rm = T) + 
  sum(subData.nomiss.wt200d.sbgrpWt.resPop$censor_0 == 1) 

# Height
mean(is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$Height_0), na.rm = T)
summary(subData.wizmiss.wt200d.sbgrpWt.resPop$Height_0)
summary(subData.wizmiss.wt200d.sbgrpWt.resPop$Height_1)
summary(subData.wizmiss.wt200d.sbgrpWt.resPop$Height_2)
summary(subData.wizmiss.wt200d.sbgrpWt.resPop$Height_3)
summary(subData.wizmiss.wt200d.sbgrpWt.resPop$Height_4)
summary(subData.wizmiss.wt200d.sbgrpWt.resPop$Height_5)
summary(subData.wizmiss.wt200d.sbgrpWt.resPop$Height_6)
summary(subData.wizmiss.wt200d.sbgrpWt.resPop$Height_7)
# remove heights that are greater than 240 cm
subData.nomiss.wt200d.sbgrpWt.resPop %>%
  filter((Height_0 > 240) | (Height_1 > 240) | (Height_2 > 240) | (Height_3 > 240))
subData.nomiss.wt200d.sbgrpWt.resPop <- subData.nomiss.wt200d.sbgrpWt.resPop %>%
  mutate(Height_0 = ifelse(Height_0 > 240, NA, Height_0), 
         Height_1 = ifelse(Height_1 > 240, NA, Height_1), 
         Height_2 = ifelse(Height_2 > 240, NA, Height_2), 
         Height_3 = ifelse(Height_3 > 240, NA, Height_3))
# impute with average at each time point
tmp.summImpute <- data.frame(covariate      = "Height",
                             n_miss_0       = sum(is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$Height_0)),
                             p_miss_0       = mean(is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$Height_0)), 
                             imputed_mean_0 = mean(subData.wizmiss.wt200d.sbgrpWt.resPop$Height_0, na.rm = T),
                             n_miss_1       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_0 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$Height_1)),
                             p_miss_1       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_0 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$Height_1)) /
                               summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_1,
                             imputed_mean_1 = mean(subData.wizmiss.wt200d.sbgrpWt.resPop$Height_1, na.rm = T),
                             n_miss_2       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_1 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$Height_2), na.rm = T),
                             p_miss_2       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_1 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$Height_2), na.rm = T) / 
                               summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_2,
                             imputed_mean_2 = mean(subData.wizmiss.wt200d.sbgrpWt.resPop$Height_2, na.rm = T),
                             n_miss_3       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_2 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$Height_3), na.rm = T),
                             p_miss_3       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_2 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$Height_3), na.rm = T) / 
                               summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_3,
                             imputed_mean_3 = mean(subData.wizmiss.wt200d.sbgrpWt.resPop$Height_3, na.rm = T),
                             n_miss_4       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_3 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$Height_4), na.rm = T),
                             p_miss_4       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_3 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$Height_4), na.rm = T) / 
                               summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_4,
                             imputed_mean_4 = mean(subData.wizmiss.wt200d.sbgrpWt.resPop$Height_4, na.rm = T),
                             n_miss_5       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_4 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$Height_5), na.rm = T),
                             p_miss_5       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_4 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$Height_5), na.rm = T) / 
                               summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_5,
                             imputed_mean_5 = mean(subData.wizmiss.wt200d.sbgrpWt.resPop$Height_5, na.rm = T),
                             n_miss_6       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_5 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$Height_6), na.rm = T),
                             p_miss_6       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_5 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$Height_6), na.rm = T) / 
                               summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_6,
                             imputed_mean_6 = mean(subData.wizmiss.wt200d.sbgrpWt.resPop$Height_6, na.rm = T),
                             n_miss_7       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_6 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$Height_7), na.rm = T),
                             p_miss_7       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_6 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$Height_7), na.rm = T) / 
                               summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_7,
                             imputed_mean_7 = mean(subData.wizmiss.wt200d.sbgrpWt.resPop$Height_7, na.rm = T))
summImpute.wizmiss.wt200d.sbgrpWt.resPop <- rbind(summImpute.wizmiss.wt200d.sbgrpWt.resPop,
                                                  tmp.summImpute)
subData.nomiss.wt200d.sbgrpWt.resPop <- subData.nomiss.wt200d.sbgrpWt.resPop %>%
  mutate(Height_0 = ifelse(is.na(Height_0), tmp.summImpute$imputed_mean_0, Height_0)) %>%
  mutate(Height_1 = ifelse((is.na(Height_1)) & (censor_0 == 0), tmp.summImpute$imputed_mean_1, Height_1)) %>%
  mutate(Height_2 = ifelse((is.na(Height_2)) & (censor_1 == 0), tmp.summImpute$imputed_mean_2, Height_2)) %>%
  mutate(Height_3 = ifelse((is.na(Height_3)) & (censor_2 == 0), tmp.summImpute$imputed_mean_3, Height_3)) %>%
  mutate(Height_4 = ifelse((is.na(Height_4)) & (censor_3 == 0), tmp.summImpute$imputed_mean_4, Height_4)) %>%
  mutate(Height_5 = ifelse((is.na(Height_5)) & (censor_4 == 0), tmp.summImpute$imputed_mean_5, Height_5)) %>%
  mutate(Height_6 = ifelse((is.na(Height_6)) & (censor_5 == 0), tmp.summImpute$imputed_mean_6, Height_6)) %>%
  mutate(Height_7 = ifelse((is.na(Height_7)) & (censor_6 == 0), tmp.summImpute$imputed_mean_7, Height_7))

# pregnancy
unique(subData.nomiss.wt200d.sbgrpWt.resPop$pregnant_0)
# check if pregnancy missing if male
subData.wizmiss.wt200d.sbgrpWt.resPop <- subData.wizmiss.wt200d.sbgrpWt.resPop %>%
  mutate(male = gsub(" ", "", male))
subData.nomiss.wt200d.sbgrpWt.resPop <- subData.nomiss.wt200d.sbgrpWt.resPop %>%
  mutate(male = gsub(" ", "", male))
ind.male <- (as.character(subData.nomiss.wt200d.sbgrpWt.resPop$male) == "Male")
table(subData.wizmiss.wt200d.sbgrpWt.resPop$male)
# expect 0s
sum(!(ind.male %in% is.na(subData.nomiss.wt200d.sbgrpWt.resPop$pregnant_0)))
sum(!(ind.male %in% is.na(subData.nomiss.wt200d.sbgrpWt.resPop$pregnant_1)))
sum(!(ind.male %in% is.na(subData.nomiss.wt200d.sbgrpWt.resPop$pregnant_2)))
sum(!(ind.male %in% is.na(subData.nomiss.wt200d.sbgrpWt.resPop$pregnant_3)))
sum(!(ind.male %in% is.na(subData.nomiss.wt200d.sbgrpWt.resPop$pregnant_4)))
sum(!(ind.male %in% is.na(subData.nomiss.wt200d.sbgrpWt.resPop$pregnant_5)))
sum(!(ind.male %in% is.na(subData.nomiss.wt200d.sbgrpWt.resPop$pregnant_6)))
sum(!(ind.male %in% is.na(subData.nomiss.wt200d.sbgrpWt.resPop$pregnant_7)))
# # change pregnancy to 0 if male
# subData.wizmiss.wt200d.sbgrpWt.resPop <- subData.wizmiss.wt200d.sbgrpWt.resPop %>%
#   mutate(pregnant_0 = ifelse(male == "Male", 0, pregnant_0)) %>%
#   mutate(pregnant_1 = ifelse((male == "Male") & (censor_0 == 0), 0, pregnant_1)) %>%
#   mutate(pregnant_2 = ifelse((male == "Male") & (censor_1 == 0), 0, pregnant_2)) %>%
#   mutate(pregnant_3 = ifelse((male == "Male") & (censor_2 == 0), 0, pregnant_3)) %>%
#   mutate(pregnant_4 = ifelse((male == "Male") & (censor_3 == 0), 0, pregnant_4)) %>%
#   mutate(pregnant_5 = ifelse((male == "Male") & (censor_4 == 0), 0, pregnant_5)) %>%
#   mutate(pregnant_6 = ifelse((male == "Male") & (censor_5 == 0), 0, pregnant_6)) %>%
#   mutate(pregnant_7 = ifelse((male == "Male") & (censor_6 == 0), 0, pregnant_7))  
# subData.nomiss.wt200d.sbgrpWt.resPop <- subData.nomiss.wt200d.sbgrpWt.resPop %>%
#   mutate(pregnant_0 = ifelse(male == "Male", 0, pregnant_0)) %>%
#   mutate(pregnant_1 = ifelse((male == "Male") & (censor_0 == 0), 0, pregnant_1)) %>%
#   mutate(pregnant_2 = ifelse((male == "Male") & (censor_1 == 0), 0, pregnant_2)) %>%
#   mutate(pregnant_3 = ifelse((male == "Male") & (censor_2 == 0), 0, pregnant_3)) %>%
#   mutate(pregnant_4 = ifelse((male == "Male") & (censor_3 == 0), 0, pregnant_4)) %>%
#   mutate(pregnant_5 = ifelse((male == "Male") & (censor_4 == 0), 0, pregnant_5)) %>%
#   mutate(pregnant_6 = ifelse((male == "Male") & (censor_5 == 0), 0, pregnant_6)) %>%
#   mutate(pregnant_7 = ifelse((male == "Male") & (censor_6 == 0), 0, pregnant_7))  
# table(subData.wizmiss.wt200d.sbgrpWt.resPop$pregnant_0)
# sum(is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$pregnant_0))
# # impute with 0 at each time point if female still missing
# tmp.summImpute <- data.frame(covariate      = "Pregnant",
#                              n_miss_0       = sum(is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$pregnant_0)),
#                              p_miss_0       = mean(is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$pregnant_0)), 
#                              imputed_mean_0 = 0,
#                              n_miss_1       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_0 == 0) & 
#                                                     is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$pregnant_1)),
#                              p_miss_1       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_0 == 0) & 
#                                                     is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$pregnant_1)) /
#                                summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_1,
#                              imputed_mean_1 = 0,
#                              n_miss_2       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_1 == 0) & 
#                                                     is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$pregnant_2), na.rm = T),
#                              p_miss_2       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_1 == 0) & 
#                                                     is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$pregnant_2), na.rm = T) / 
#                                summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_2,
#                              imputed_mean_2 = 0,
#                              n_miss_3       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_2 == 0) & 
#                                                     is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$pregnant_3), na.rm = T),
#                              p_miss_3       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_2 == 0) & 
#                                                     is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$pregnant_3), na.rm = T) / 
#                                summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_3,
#                              imputed_mean_3 = 0,
#                              n_miss_4       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_3 == 0) & 
#                                                     is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$pregnant_4), na.rm = T),
#                              p_miss_4       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_3 == 0) & 
#                                                     is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$pregnant_4), na.rm = T) / 
#                                summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_4,
#                              imputed_mean_4 = 0,
#                              n_miss_5       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_4 == 0) & 
#                                                     is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$pregnant_5), na.rm = T),
#                              p_miss_5       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_4 == 0) & 
#                                                     is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$pregnant_5), na.rm = T) / 
#                                summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_5,
#                              imputed_mean_5 = 0,
#                              n_miss_6       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_5 == 0) & 
#                                                     is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$pregnant_6), na.rm = T),
#                              p_miss_6       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_5 == 0) & 
#                                                     is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$pregnant_6), na.rm = T) / 
#                                summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_6,
#                              imputed_mean_6 = 0,
#                              n_miss_7       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_6 == 0) & 
#                                                     is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$pregnant_7), na.rm = T),
#                              p_miss_7       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_6 == 0) & 
#                                                     is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$pregnant_7), na.rm = T) / 
#                                summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_7,
#                              imputed_mean_7 = 0)
# summImpute.wizmiss.wt200d.sbgrpWt.resPop <- rbind(summImpute.wizmiss.wt200d.sbgrpWt.resPop,
#                                                               tmp.summImpute)
# subData.nomiss.wt200d.sbgrpWt.resPop <- subData.nomiss.wt200d.sbgrpWt.resPop %>%
#   mutate(pregnant_0 = ifelse(is.na(pregnant_0), tmp.summImpute$imputed_mean_0, pregnant_0)) %>%
#   mutate(pregnant_1 = ifelse((is.na(pregnant_1)) & (censor_0 == 0), tmp.summImpute$imputed_mean_1, pregnant_1)) %>%
#   mutate(pregnant_2 = ifelse((is.na(pregnant_2)) & (censor_1 == 0), tmp.summImpute$imputed_mean_2, pregnant_2)) %>%
#   mutate(pregnant_3 = ifelse((is.na(pregnant_3)) & (censor_2 == 0), tmp.summImpute$imputed_mean_3, pregnant_3)) %>%
#   mutate(pregnant_4 = ifelse((is.na(pregnant_4)) & (censor_3 == 0), tmp.summImpute$imputed_mean_4, pregnant_4)) %>%
#   mutate(pregnant_5 = ifelse((is.na(pregnant_5)) & (censor_4 == 0), tmp.summImpute$imputed_mean_5, pregnant_5)) %>%
#   mutate(pregnant_6 = ifelse((is.na(pregnant_6)) & (censor_5 == 0), tmp.summImpute$imputed_mean_6, pregnant_6)) %>%
#   mutate(pregnant_7 = ifelse((is.na(pregnant_7)) & (censor_6 == 0), tmp.summImpute$imputed_mean_7, pregnant_7))

# tb and tb treatment
unique(subData.wizmiss.wt200d.sbgrpWt.resPop$activeTB_0)
unique(subData.wizmiss.wt200d.sbgrpWt.resPop$activeTB_1)
unique(subData.wizmiss.wt200d.sbgrpWt.resPop$activeTB_2)
unique(subData.wizmiss.wt200d.sbgrpWt.resPop$activeTB_3)
unique(subData.wizmiss.wt200d.sbgrpWt.resPop$activeTB_4)
unique(subData.wizmiss.wt200d.sbgrpWt.resPop$activeTB_5)
unique(subData.wizmiss.wt200d.sbgrpWt.resPop$activeTB_6)
unique(subData.wizmiss.wt200d.sbgrpWt.resPop$activeTB_7)
subData.wizmiss.wt200d.sbgrpWt.resPop <- subData.wizmiss.wt200d.sbgrpWt.resPop %>%
  mutate(activeTB_0 = ifelse(is.na(activeTB_0), 0, activeTB_0),
         activeTB_1 = ifelse(is.na(activeTB_1) & (censor_0 == 0), 0, activeTB_1),
         activeTB_2 = ifelse(is.na(activeTB_2) & (censor_1 == 0), 0, activeTB_2),
         activeTB_3 = ifelse(is.na(activeTB_3) & (censor_2 == 0), 0, activeTB_3),
         activeTB_4 = ifelse(is.na(activeTB_4) & (censor_3 == 0), 0, activeTB_4),
         activeTB_5 = ifelse(is.na(activeTB_5) & (censor_4 == 0), 0, activeTB_5),
         activeTB_6 = ifelse(is.na(activeTB_6) & (censor_5 == 0), 0, activeTB_6),
         activeTB_7 = ifelse(is.na(activeTB_7) & (censor_6 == 0), 0, activeTB_7))
subData.nomiss.wt200d.sbgrpWt.resPop <- subData.nomiss.wt200d.sbgrpWt.resPop %>%
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
                             n_miss_0       = sum(is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$activeTB_0)),
                             p_miss_0       = mean(is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$activeTB_0)), 
                             imputed_mean_0 = NA,
                             n_miss_1       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_0 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$activeTB_1)),
                             p_miss_1       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_0 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$activeTB_1)) /
                               summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_1,
                             imputed_mean_1 = NA,
                             n_miss_2       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_1 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$activeTB_2), na.rm = T),
                             p_miss_2       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_1 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$activeTB_2), na.rm = T) / 
                               summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_2,
                             imputed_mean_2 = NA,
                             n_miss_3       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_2 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$activeTB_3), na.rm = T),
                             p_miss_3       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_2 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$activeTB_3), na.rm = T) / 
                               summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_3,
                             imputed_mean_3 = NA,
                             n_miss_4       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_3 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$activeTB_4), na.rm = T),
                             p_miss_4       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_3 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$activeTB_4), na.rm = T) / 
                               summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_4,
                             imputed_mean_4 = NA,
                             n_miss_5       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_4 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$activeTB_5), na.rm = T),
                             p_miss_5       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_4 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$activeTB_5), na.rm = T) / 
                               summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_5,
                             imputed_mean_5 = NA,
                             n_miss_6       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_5 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$activeTB_6), na.rm = T),
                             p_miss_6       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_5 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$activeTB_6), na.rm = T) / 
                               summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_6,
                             imputed_mean_6 = NA,
                             n_miss_7       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_6 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$activeTB_7), na.rm = T),
                             p_miss_7       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_6 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$activeTB_7), na.rm = T) / 
                               summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_7,
                             imputed_mean_7 = NA)
summImpute.wizmiss.wt200d.sbgrpWt.resPop <- rbind(summImpute.wizmiss.wt200d.sbgrpWt.resPop,
                                                  tmp.summImpute)
# tbtx
# expect 0: check if there are any pts on tbtx when not active TB
sum((subData.wizmiss.wt200d.sbgrpWt.resPop$tbtx_0 == 1) & 
      (subData.wizmiss.wt200d.sbgrpWt.resPop$activeTB_0 == 0), na.rm = T)
sum((subData.wizmiss.wt200d.sbgrpWt.resPop$tbtx_1 == 1) & 
      (subData.wizmiss.wt200d.sbgrpWt.resPop$activeTB_1 == 0), na.rm = T)
sum((subData.wizmiss.wt200d.sbgrpWt.resPop$tbtx_2 == 1) & 
      (subData.wizmiss.wt200d.sbgrpWt.resPop$activeTB_2 == 0), na.rm = T)
sum((subData.wizmiss.wt200d.sbgrpWt.resPop$tbtx_3 == 1) & 
      (subData.wizmiss.wt200d.sbgrpWt.resPop$activeTB_3 == 0), na.rm = T)
sum((subData.wizmiss.wt200d.sbgrpWt.resPop$tbtx_4 == 1) & 
      (subData.wizmiss.wt200d.sbgrpWt.resPop$activeTB_4 == 0), na.rm = T)
sum((subData.wizmiss.wt200d.sbgrpWt.resPop$tbtx_5 == 1) & 
      (subData.wizmiss.wt200d.sbgrpWt.resPop$activeTB_5 == 0), na.rm = T)
sum((subData.wizmiss.wt200d.sbgrpWt.resPop$tbtx_6 == 1) & 
      (subData.wizmiss.wt200d.sbgrpWt.resPop$activeTB_6 == 0), na.rm = T)
sum((subData.wizmiss.wt200d.sbgrpWt.resPop$tbtx_7 == 1) & 
      (subData.wizmiss.wt200d.sbgrpWt.resPop$activeTB_7 == 0), na.rm = T)
tmp.summImpute <- data.frame(covariate      = "TB treatment",
                             n_miss_0       = sum(is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$tbtx_0)),
                             p_miss_0       = mean(is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$tbtx_0)), 
                             imputed_mean_0 = 0,
                             n_miss_1       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_0 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$tbtx_1)),
                             p_miss_1       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_0 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$tbtx_1)) /
                               summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_1,
                             imputed_mean_1 = 0,
                             n_miss_2       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_1 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$tbtx_2), na.rm = T),
                             p_miss_2       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_1 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$tbtx_2), na.rm = T) / 
                               summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_2,
                             imputed_mean_2 = 0,
                             n_miss_3       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_2 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$tbtx_3), na.rm = T),
                             p_miss_3       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_2 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$tbtx_3), na.rm = T) / 
                               summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_3,
                             imputed_mean_3 = 0,
                             n_miss_4       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_3 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$tbtx_4), na.rm = T),
                             p_miss_4       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_3 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$tbtx_4), na.rm = T) / 
                               summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_4,
                             imputed_mean_4 = 0,
                             n_miss_5       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_4 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$tbtx_5), na.rm = T),
                             p_miss_5       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_4 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$tbtx_5), na.rm = T) / 
                               summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_5,
                             imputed_mean_5 = 0,
                             n_miss_6       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_5 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$tbtx_6), na.rm = T),
                             p_miss_6       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_5 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$tbtx_6), na.rm = T) / 
                               summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_6,
                             imputed_mean_6 = 0,
                             n_miss_7       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_6 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$tbtx_7), na.rm = T),
                             p_miss_7       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_6 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$tbtx_7), na.rm = T) / 
                               summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_7,
                             imputed_mean_7 = 0)
summImpute.wizmiss.wt200d.sbgrpWt.resPop <- rbind(summImpute.wizmiss.wt200d.sbgrpWt.resPop,
                                                  tmp.summImpute)
subData.nomiss.wt200d.sbgrpWt.resPop <- subData.nomiss.wt200d.sbgrpWt.resPop %>%
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
identical(is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$Married_0), is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$civilstatus_0))
subData.wizmiss.wt200d.sbgrpWt.resPop[which(is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$Married_0) != is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$civilstatus_0)), ]
# id 946180 missing civil status with married being 0
identical(is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$Married_1), is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$civilstatus_1))
identical(is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$Married_2), is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$civilstatus_2))
identical(is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$Married_3), is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$civilstatus_3))
identical(is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$Married_4), is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$civilstatus_4))
identical(is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$Married_5), is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$civilstatus_5))
identical(is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$Married_6), is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$civilstatus_6))
identical(is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$Married_7), is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$civilstatus_7))
table(subData.wizmiss.wt200d.sbgrpWt.resPop$Married_0)
table(subData.wizmiss.wt200d.sbgrpWt.resPop$civilstatus_0)
table(subData.wizmiss.wt200d.sbgrpWt.resPop$Married_1)
table(subData.wizmiss.wt200d.sbgrpWt.resPop$civilstatus_1)
table(subData.wizmiss.wt200d.sbgrpWt.resPop$Married_2)
table(subData.wizmiss.wt200d.sbgrpWt.resPop$civilstatus_2)
table(subData.wizmiss.wt200d.sbgrpWt.resPop$Married_3)
table(subData.wizmiss.wt200d.sbgrpWt.resPop$civilstatus_3)
unique(subData.wizmiss.wt200d.sbgrpWt.resPop$civilstatus_0)
tmp.summImpute <- data.frame(covariate      = "Married or living w/ partner",
                             n_miss_0       = sum(is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$Married_0)),
                             p_miss_0       = mean(is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$Married_0)), 
                             imputed_mean_0 = 0,
                             n_miss_1       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_0 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$Married_1)),
                             p_miss_1       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_0 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$Married_1)) /
                               summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_1,
                             imputed_mean_1 = 0,
                             n_miss_2       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_1 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$Married_2), na.rm = T),
                             p_miss_2       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_1 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$Married_2), na.rm = T) / 
                               summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_2,
                             imputed_mean_2 = 0,
                             n_miss_3       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_2 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$Married_3), na.rm = T),
                             p_miss_3       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_2 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$Married_3), na.rm = T) / 
                               summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_3,
                             imputed_mean_3 = 0,
                             n_miss_4       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_3 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$Married_4), na.rm = T),
                             p_miss_4       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_3 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$Married_4), na.rm = T) / 
                               summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_4,
                             imputed_mean_4 = 0,
                             n_miss_5       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_4 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$Married_5), na.rm = T),
                             p_miss_5       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_4 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$Married_5), na.rm = T) / 
                               summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_5,
                             imputed_mean_5 = 0,
                             n_miss_6       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_5 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$Married_6), na.rm = T),
                             p_miss_6       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_5 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$Married_6), na.rm = T) / 
                               summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_6,
                             imputed_mean_6 = 0,
                             n_miss_7       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_6 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$Married_7), na.rm = T),
                             p_miss_7       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_6 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$Married_7), na.rm = T) / 
                               summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_7,
                             imputed_mean_7 = 0)
summImpute.wizmiss.wt200d.sbgrpWt.resPop <- rbind(summImpute.wizmiss.wt200d.sbgrpWt.resPop,
                                                  tmp.summImpute)
subData.nomiss.wt200d.sbgrpWt.resPop <- subData.nomiss.wt200d.sbgrpWt.resPop %>%
  mutate(Married_0 = ifelse(is.na(Married_0), tmp.summImpute$imputed_mean_0, Married_0)) %>%
  mutate(Married_1 = ifelse((is.na(Married_1)) & (censor_0 == 0), tmp.summImpute$imputed_mean_1, Married_1)) %>%
  mutate(Married_2 = ifelse((is.na(Married_2)) & (censor_1 == 0), tmp.summImpute$imputed_mean_2, Married_2)) %>%
  mutate(Married_3 = ifelse((is.na(Married_3)) & (censor_2 == 0), tmp.summImpute$imputed_mean_3, Married_3)) %>%
  mutate(Married_4 = ifelse((is.na(Married_4)) & (censor_3 == 0), tmp.summImpute$imputed_mean_4, Married_4)) %>%
  mutate(Married_5 = ifelse((is.na(Married_5)) & (censor_4 == 0), tmp.summImpute$imputed_mean_5, Married_5)) %>%
  mutate(Married_6 = ifelse((is.na(Married_6)) & (censor_5 == 0), tmp.summImpute$imputed_mean_6, Married_6)) %>%
  mutate(Married_7 = ifelse((is.na(Married_7)) & (censor_6 == 0), tmp.summImpute$imputed_mean_7, Married_7))
# civil status
sum(subData.wizmiss.wt200d.sbgrpWt.resPop$civilstatus_0 == "Never Married and Not Living w/Partner", na.rm = T)
tmp.summImpute <- data.frame(covariate      = "Civil status",
                             n_miss_0       = sum(is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$civilstatus_0)),
                             p_miss_0       = mean(is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$civilstatus_0)), 
                             imputed_mean_0 = "Never Married and Not Living w/Partner",
                             n_miss_1       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_0 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$civilstatus_1)),
                             p_miss_1       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_0 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$civilstatus_1)) /
                               summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_1,
                             imputed_mean_1 = "Never Married and Not Living w/Partner",
                             n_miss_2       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_1 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$civilstatus_2), na.rm = T),
                             p_miss_2       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_1 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$civilstatus_2), na.rm = T) / 
                               summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_2,
                             imputed_mean_2 = "Never Married and Not Living w/Partner",
                             n_miss_3       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_2 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$civilstatus_3), na.rm = T),
                             p_miss_3       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_2 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$civilstatus_3), na.rm = T) / 
                               summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_3,
                             imputed_mean_3 = "Never Married and Not Living w/Partner",
                             n_miss_4       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_3 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$civilstatus_4), na.rm = T),
                             p_miss_4       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_3 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$civilstatus_4), na.rm = T) / 
                               summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_4,
                             imputed_mean_4 = "Never Married and Not Living w/Partner",
                             n_miss_5       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_4 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$civilstatus_5), na.rm = T),
                             p_miss_5       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_4 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$civilstatus_5), na.rm = T) / 
                               summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_5,
                             imputed_mean_5 = "Never Married and Not Living w/Partner",
                             n_miss_6       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_5 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$civilstatus_6), na.rm = T),
                             p_miss_6       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_5 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$civilstatus_6), na.rm = T) / 
                               summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_6,
                             imputed_mean_6 = "Never Married and Not Living w/Partner",
                             n_miss_7       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_6 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$civilstatus_7), na.rm = T),
                             p_miss_7       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_6 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$civilstatus_7), na.rm = T) / 
                               summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_7,
                             imputed_mean_7 = "Never Married and Not Living w/Partner")
summImpute.wizmiss.wt200d.sbgrpWt.resPop <- rbind(summImpute.wizmiss.wt200d.sbgrpWt.resPop,
                                                  tmp.summImpute)
subData.nomiss.wt200d.sbgrpWt.resPop <- subData.nomiss.wt200d.sbgrpWt.resPop %>%
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
apply(subData.wizmiss.wt200d.sbgrpWt.resPop %>% select(starts_with("glucose")), 2, function(x) mean(is.na(x)))
# remove glucose with >90% missing
tmp.summDrop <- data.frame(covariate = "Blood glucose",
                           n_0 = sum(is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$glucose_0)),
                           p_0 = mean(is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$glucose_0)), 
                           n_1 = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_0 == 0) & 
                                       is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$glucose_1)),
                           p_1 = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_0 == 0) & 
                                       is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$glucose_1)) /
                             summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_1,
                           n_2 = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_1 == 0) & 
                                       is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$glucose_2), na.rm = T),
                           p_2 = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_1 == 0) & 
                                       is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$glucose_2), na.rm = T) /
                             summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_2,
                           n_3 = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_2 == 0) & 
                                       is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$glucose_3), na.rm = T),
                           p_3 = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_2 == 0) & 
                                       is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$glucose_3), na.rm = T) /
                             summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_3,
                           n_4 = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_3 == 0) & 
                                       is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$glucose_4), na.rm = T),
                           p_4 = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_3 == 0) & 
                                       is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$glucose_4), na.rm = T) /
                             summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_4,
                           n_5 = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_4 == 0) & 
                                       is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$glucose_5), na.rm = T),
                           p_5 = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_4 == 0) & 
                                       is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$glucose_5), na.rm = T) /
                             summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_5,
                           n_6 = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_5 == 0) & 
                                       is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$glucose_6), na.rm = T),
                           p_6 = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_5 == 0) & 
                                       is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$glucose_6), na.rm = T) /
                             summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_6,
                           n_7 = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_6 == 0) & 
                                       is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$glucose_7), na.rm = T),
                           p_7 = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_6 == 0) & 
                                       is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$glucose_7), na.rm = T) /
                             summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_7)
summDrop.wizmiss.wt200d.sbgrpWt.resPop <- rbind(summDrop.wizmiss.wt200d.sbgrpWt.resPop,
                                                tmp.summDrop)
# fasting glucose
tmp.summDrop <- data.frame(covariate = "Fasting serum glucose",
                           n_0 = sum(is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$glucoseFasting_0)),
                           p_0 = mean(is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$glucoseFasting_0)), 
                           n_1 = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_0 == 0) & 
                                       is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$glucoseFasting_1)),
                           p_1 = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_0 == 0) & 
                                       is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$glucoseFasting_1)) /
                             summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_1,
                           n_2 = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_1 == 0) & 
                                       is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$glucoseFasting_2), na.rm = T),
                           p_2 = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_1 == 0) & 
                                       is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$glucoseFasting_2), na.rm = T) /
                             summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_2,
                           n_3 = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_2 == 0) & 
                                       is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$glucoseFasting_3), na.rm = T),
                           p_3 = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_2 == 0) & 
                                       is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$glucoseFasting_3), na.rm = T) /
                             summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_3,
                           n_4 = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_3 == 0) & 
                                       is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$glucoseFasting_4), na.rm = T),
                           p_4 = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_3 == 0) & 
                                       is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$glucoseFasting_4), na.rm = T) /
                             summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_4,
                           n_5 = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_4 == 0) & 
                                       is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$glucoseFasting_5), na.rm = T),
                           p_5 = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_4 == 0) & 
                                       is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$glucoseFasting_5), na.rm = T) /
                             summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_5,
                           n_6 = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_5 == 0) & 
                                       is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$glucoseFasting_6), na.rm = T),
                           p_6 = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_5 == 0) & 
                                       is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$glucoseFasting_6), na.rm = T) /
                             summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_6,
                           n_7 = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_6 == 0) & 
                                       is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$glucoseFasting_7), na.rm = T),
                           p_7 = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_6 == 0) & 
                                       is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$glucoseFasting_7), na.rm = T) /
                             summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_7)
summDrop.wizmiss.wt200d.sbgrpWt.resPop <- rbind(summDrop.wizmiss.wt200d.sbgrpWt.resPop,
                                                tmp.summDrop)
subData.nomiss.wt200d.sbgrpWt.resPop <- subData.nomiss.wt200d.sbgrpWt.resPop %>%
  select(-starts_with("glucose"))
# colnames(subData.nomiss.wt200d.sbgrpWt.resPop)

# Covered by NHIF
table(subData.wizmiss.wt200d.sbgrpWt.resPop$nhif_0)
sum(subData.wizmiss.wt200d.sbgrpWt.resPop$nhif_0 == "no", na.rm = T)
tmp.summImpute <- data.frame(covariate      = "Covered by NHIF",
                             n_miss_0       = sum(is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$nhif_0)),
                             p_miss_0       = mean(is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$nhif_0)), 
                             imputed_mean_0 = "no",
                             n_miss_1       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_0 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$nhif_1)),
                             p_miss_1       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_0 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$nhif_1)) /
                               summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_1,
                             imputed_mean_1 = "no",
                             n_miss_2       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_1 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$nhif_2), na.rm = T),
                             p_miss_2       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_1 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$nhif_2), na.rm = T) / 
                               summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_2,
                             imputed_mean_2 = "no",
                             n_miss_3       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_2 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$nhif_3), na.rm = T),
                             p_miss_3       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_2 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$nhif_3), na.rm = T) / 
                               summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_3,
                             imputed_mean_3 = "no",
                             n_miss_4       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_3 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$nhif_4), na.rm = T),
                             p_miss_4       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_3 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$nhif_4), na.rm = T) / 
                               summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_4,
                             imputed_mean_4 = "no",
                             n_miss_5       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_4 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$nhif_5), na.rm = T),
                             p_miss_5       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_4 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$nhif_5), na.rm = T) / 
                               summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_5,
                             imputed_mean_5 = "no",
                             n_miss_6       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_5 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$nhif_6), na.rm = T),
                             p_miss_6       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_5 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$nhif_6), na.rm = T) / 
                               summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_6,
                             imputed_mean_6 = "no",
                             n_miss_7       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_6 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$nhif_7), na.rm = T),
                             p_miss_7       = sum((subData.wizmiss.wt200d.sbgrpWt.resPop$censor_6 == 0) & 
                                                    is.na(subData.wizmiss.wt200d.sbgrpWt.resPop$nhif_7), na.rm = T) / 
                               summLeft.wizmiss.wt200d.sbgrpWt.resPop$n_7,
                             imputed_mean_7 = "no")
summImpute.wizmiss.wt200d.sbgrpWt.resPop <- rbind(summImpute.wizmiss.wt200d.sbgrpWt.resPop,
                                                  tmp.summImpute)
subData.nomiss.wt200d.sbgrpWt.resPop <- subData.nomiss.wt200d.sbgrpWt.resPop %>%
  mutate(nhif_0 = ifelse(is.na(nhif_0), tmp.summImpute$imputed_mean_0, as.character(nhif_0))) %>%
  mutate(nhif_1 = ifelse((is.na(nhif_1)) & (censor_0 == 0), tmp.summImpute$imputed_mean_1, as.character(nhif_1))) %>%
  mutate(nhif_2 = ifelse((is.na(nhif_2)) & (censor_1 == 0), tmp.summImpute$imputed_mean_2, as.character(nhif_2))) %>%
  mutate(nhif_3 = ifelse((is.na(nhif_3)) & (censor_2 == 0), tmp.summImpute$imputed_mean_3, as.character(nhif_3))) %>%
  mutate(nhif_4 = ifelse((is.na(nhif_4)) & (censor_3 == 0), tmp.summImpute$imputed_mean_4, as.character(nhif_4))) %>%
  mutate(nhif_5 = ifelse((is.na(nhif_5)) & (censor_4 == 0), tmp.summImpute$imputed_mean_5, as.character(nhif_5))) %>%
  mutate(nhif_6 = ifelse((is.na(nhif_6)) & (censor_5 == 0), tmp.summImpute$imputed_mean_6, as.character(nhif_6))) %>%
  mutate(nhif_7 = ifelse((is.na(nhif_7)) & (censor_6 == 0), tmp.summImpute$imputed_mean_7, as.character(nhif_7)))

apply(subData.wizmiss.wt200d.sbgrpWt.resPop %>% select(Weight_0:censor_0), 2, function(x) mean(is.na(x)))
apply(subData.nomiss.wt200d.sbgrpWt.resPop %>% select(Weight_0:censor_0), 2, function(x) mean(is.na(x)))

save(ampath.demog, ampath.demog.used,
     ampath.visit, ampath.visit.used, 
     ampath.visit.wt200d.sbgrpWt,
     pt.ids,
     finalData.wizmiss.wt200d.sbgrpWt.resPop, finalData.wizmiss.wt200d.sbgrpWt.carryOver, 
     finalData.wizmiss.t0Jul2016.switchDTG.wt200d.sbgrpWt, finalData.wizmiss.t0Jul2016.switchDTG.wt200d.artStatus,
     # finalData.noMiss.t0Jul2016.switchDTG.wt400d, finalData.noMiss.t0Jul2016.switchDTG.wt200d, finalData.noMiss.t0Jul2016.switchDTG.wt100d,
     finalData.wizmiss.t0Jul2016.switchDTG.wt400d.resPop, finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop, finalData.wizmiss.t0Jul2016.switchDTG.wt100d.resPop, 
     # finalData.wizmiss.t0Jul2016.switchDTG.wt400d.carryOver, finalData.wizmiss.t0Jul2016.switchDTG.wt200d.carryOver, finalData.wizmiss.t0Jul2016.switchDTG.wt100d.carryOver, 
     # finalData.wizmiss.t0Jul2016.switchDTG.wt400d.wtTime0, finalData.wizmiss.t0Jul2016.switchDTG.wt200d.wtTime0, finalData.wizmiss.t0Jul2016.switchDTG.wt100d.wtTime0, 
     # onOffs.wizmiss.t0Jul2016.switchDTG.wt400d, onOffs.wizmiss.t0Jul2016.switchDTG.wt200d, onOffs.wizmiss.t0Jul2016.switchDTG.wt100d,
     # numTherapies.wizmiss.t0Jul2016.switchDTG.wt400d, numTherapies.wizmiss.t0Jul2016.switchDTG.wt200d, numTherapies.wizmiss.t0Jul2016.switchDTG.wt100d,
     subData.nomiss.wt200d.sbgrpWt.resPop,
     summCensor.wizmiss.wt200d.sbgrpWt.resPop, summDrop.wizmiss.wt200d.sbgrpWt.resPop, summImpute.wizmiss.wt200d.sbgrpWt.resPop,
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
     # summCensor.wizmiss.t0Jul2016.switchDTG.wt400d.resPop, summCensor.wizmiss.t0Jul2016.switchDTG.wt200d.resPop, summCensor.wizmiss.t0Jul2016.switchDTG.wt100d.resPop,
     # summDrop.wizmiss.t0Jul2016.switchDTG.wt400d.resPop, summDrop.wizmiss.t0Jul2016.switchDTG.wt200d.resPop, summDrop.wizmiss.t0Jul2016.switchDTG.wt100d.resPop,
     # summImpute.wizmiss.t0Jul2016.switchDTG.wt400d.resPop, summImpute.wizmiss.t0Jul2016.switchDTG.wt200d.resPop, summImpute.wizmiss.t0Jul2016.switchDTG.wt100d.resPop,
     file = "../../../Data/Data20210512/Processed/Summ2AllWt20230304.RData")






# latex code
library(xtable)
library(dplyr)

# censor
# 200
latex.summCensor.wizmiss.wt200d.sbgrpWt.resPop.1 <- summCensor.wizmiss.wt200d.sbgrpWt.resPop %>%
  select(type:p_4)
latex.summCensor.wizmiss.wt200d.sbgrpWt.resPop.2 <- summCensor.wizmiss.wt200d.sbgrpWt.resPop %>%
  select(type, n_5:p_8)
latex.summCensor.wizmiss.wt200d.sbgrpWt.resPop.1 <- latex.summCensor.wizmiss.wt200d.sbgrpWt.resPop.1 %>%
  mutate(p_0 = round(p_0 * 100, 1),
         p_1 = round(p_1 * 100, 1),
         p_2 = round(p_2 * 100, 1),
         p_3 = round(p_3 * 100, 1),
         p_4 = round(p_4 * 100, 1))
latex.summCensor.wizmiss.wt200d.sbgrpWt.resPop.1 <- latex.summCensor.wizmiss.wt200d.sbgrpWt.resPop.1 %>%
  mutate(t_0 = paste0(n_0, " (", p_0, "%)"), 
         t_1 = paste0(n_1, " (", p_1, "%)"), 
         t_2 = paste0(n_2, " (", p_2, "%)"), 
         t_3 = paste0(n_3, " (", p_3, "%)"), 
         t_4 = paste0(n_4, " (", p_4, "%)"))
latex.summCensor.wizmiss.wt200d.sbgrpWt.resPop.1 <- latex.summCensor.wizmiss.wt200d.sbgrpWt.resPop.1 %>%
  select(-(n_0:p_4))
print(xtable(latex.summCensor.wizmiss.wt200d.sbgrpWt.resPop.1), include.rownames = FALSE)

latex.summCensor.wizmiss.wt200d.sbgrpWt.resPop.2 <- latex.summCensor.wizmiss.wt200d.sbgrpWt.resPop.2 %>%
  mutate(p_5 = round(p_5 * 100, 1),
         p_6 = round(p_6 * 100, 1),
         p_7 = round(p_7 * 100, 1),
         p_8 = round(p_8 * 100, 1))
latex.summCensor.wizmiss.wt200d.sbgrpWt.resPop.2 <- latex.summCensor.wizmiss.wt200d.sbgrpWt.resPop.2 %>%
  mutate(t_5 = paste0(n_5, " (", p_5, "%)"), 
         t_6 = paste0(n_6, " (", p_6, "%)"), 
         t_7 = paste0(n_7, " (", p_7, "%)"), 
         t_8 = paste0(n_8, " (", p_8, "%)"))
latex.summCensor.wizmiss.wt200d.sbgrpWt.resPop.2 <- latex.summCensor.wizmiss.wt200d.sbgrpWt.resPop.2 %>%
  select(-(n_5:p_8))
print(xtable(latex.summCensor.wizmiss.wt200d.sbgrpWt.resPop.2), include.rownames = FALSE)

# drop
# 200
latex.summDrop.wizmiss.wt200d.sbgrpWt.resPop <- summDrop.wizmiss.wt200d.sbgrpWt.resPop %>%
  mutate(p_0 = round(p_0 * 100, 1),
         p_1 = round(p_1 * 100, 1),
         p_2 = round(p_2 * 100, 1),
         p_3 = round(p_3 * 100, 1), 
         p_4 = round(p_4 * 100, 1),
         p_5 = round(p_5 * 100, 1),
         p_6 = round(p_6 * 100, 1),
         p_7 = round(p_7 * 100, 1))
latex.summDrop.wizmiss.wt200d.sbgrpWt.resPop <- latex.summDrop.wizmiss.wt200d.sbgrpWt.resPop %>%
  mutate(t_0 = paste0(n_0, " (", p_0, "%)"), 
         t_1 = paste0(n_1, " (", p_1, "%)"), 
         t_2 = paste0(n_2, " (", p_2, "%)"), 
         t_3 = paste0(n_3, " (", p_3, "%)"),
         t_4 = paste0(n_4, " (", p_4, "%)"), 
         t_5 = paste0(n_5, " (", p_5, "%)"), 
         t_6 = paste0(n_6, " (", p_6, "%)"), 
         t_7 = paste0(n_7, " (", p_7, "%)"))
latex.summDrop.wizmiss.wt200d.sbgrpWt.resPop.1 <- latex.summDrop.wizmiss.wt200d.sbgrpWt.resPop %>%
  select(covariate, t_0:t_3)
latex.summDrop.wizmiss.wt200d.sbgrpWt.resPop.2 <- latex.summDrop.wizmiss.wt200d.sbgrpWt.resPop %>%
  select(covariate, t_4:t_7)
print(xtable(latex.summDrop.wizmiss.wt200d.sbgrpWt.resPop.1), include.rownames = FALSE)
print(xtable(latex.summDrop.wizmiss.wt200d.sbgrpWt.resPop.2), include.rownames = FALSE)

# impute
# 200
latex.summImpute.wizmiss.wt200d.sbgrpWt.resPop <- summImpute.wizmiss.wt200d.sbgrpWt.resPop %>%
  mutate(p_miss_0 = round(p_miss_0 * 100, 1),
         p_miss_1 = round(p_miss_1 * 100, 1),
         p_miss_2 = round(p_miss_2 * 100, 1),
         p_miss_3 = round(p_miss_3 * 100, 1),
         p_miss_4 = round(p_miss_4 * 100, 1),
         p_miss_5 = round(p_miss_5 * 100, 1),
         p_miss_6 = round(p_miss_6 * 100, 1),
         p_miss_7 = round(p_miss_7 * 100, 1))
latex.summImpute.wizmiss.wt200d.sbgrpWt.resPop$round_imputed_0 <-
  c(round(as.numeric(latex.summImpute.wizmiss.wt200d.sbgrpWt.resPop$imputed_mean_0[1:5]), 2),
    latex.summImpute.wizmiss.wt200d.sbgrpWt.resPop$imputed_mean_0[6:10])
latex.summImpute.wizmiss.wt200d.sbgrpWt.resPop$round_imputed_1 <-
  c(round(as.numeric(latex.summImpute.wizmiss.wt200d.sbgrpWt.resPop$imputed_mean_1[1:5]), 2),
    latex.summImpute.wizmiss.wt200d.sbgrpWt.resPop$imputed_mean_1[6:10])
latex.summImpute.wizmiss.wt200d.sbgrpWt.resPop$round_imputed_2 <-
  c(round(as.numeric(latex.summImpute.wizmiss.wt200d.sbgrpWt.resPop$imputed_mean_2[1:5]), 2),
    latex.summImpute.wizmiss.wt200d.sbgrpWt.resPop$imputed_mean_2[6:10])
latex.summImpute.wizmiss.wt200d.sbgrpWt.resPop$round_imputed_3 <-
  c(round(as.numeric(latex.summImpute.wizmiss.wt200d.sbgrpWt.resPop$imputed_mean_3[1:5]), 2),
    latex.summImpute.wizmiss.wt200d.sbgrpWt.resPop$imputed_mean_3[6:10])
latex.summImpute.wizmiss.wt200d.sbgrpWt.resPop$round_imputed_4 <-
  c(round(as.numeric(latex.summImpute.wizmiss.wt200d.sbgrpWt.resPop$imputed_mean_4[1:5]), 2),
    latex.summImpute.wizmiss.wt200d.sbgrpWt.resPop$imputed_mean_4[6:10])
latex.summImpute.wizmiss.wt200d.sbgrpWt.resPop$round_imputed_5 <-
  c(round(as.numeric(latex.summImpute.wizmiss.wt200d.sbgrpWt.resPop$imputed_mean_5[1:5]), 2),
    latex.summImpute.wizmiss.wt200d.sbgrpWt.resPop$imputed_mean_5[6:10])
latex.summImpute.wizmiss.wt200d.sbgrpWt.resPop$round_imputed_6 <-
  c(round(as.numeric(latex.summImpute.wizmiss.wt200d.sbgrpWt.resPop$imputed_mean_6[1:5]), 2),
    latex.summImpute.wizmiss.wt200d.sbgrpWt.resPop$imputed_mean_6[6:10])
latex.summImpute.wizmiss.wt200d.sbgrpWt.resPop$round_imputed_7 <-
  c(round(as.numeric(latex.summImpute.wizmiss.wt200d.sbgrpWt.resPop$imputed_mean_7[1:5]), 2),
    latex.summImpute.wizmiss.wt200d.sbgrpWt.resPop$imputed_mean_7[6:10])
latex.summImpute.wizmiss.wt200d.sbgrpWt.resPop <- latex.summImpute.wizmiss.wt200d.sbgrpWt.resPop %>%
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
latex.summImpute.wizmiss.wt200d.sbgrpWt.resPop.1 <- latex.summImpute.wizmiss.wt200d.sbgrpWt.resPop %>%
  select(covariate, t_0, round_imputed_0, e_1, t_1, round_imputed_1, e_2, t_2, round_imputed_2, e_3, t_3, round_imputed_3)
latex.summImpute.wizmiss.wt200d.sbgrpWt.resPop.1 <- latex.summImpute.wizmiss.wt200d.sbgrpWt.resPop %>%
  select(covariate, t_0, t_1, t_2, t_3)  
latex.summImpute.wizmiss.wt200d.sbgrpWt.resPop.2 <- latex.summImpute.wizmiss.wt200d.sbgrpWt.resPop %>%
  select(covariate, t_4, round_imputed_4, e_1, t_5, round_imputed_5, e_2, t_6, round_imputed_6, e_3, t_7, round_imputed_7)
print(xtable(latex.summImpute.wizmiss.wt200d.sbgrpWt.resPop.1), include.rownames = FALSE)
print(xtable(latex.summImpute.wizmiss.wt200d.sbgrpWt.resPop.2), include.rownames = FALSE)

# Paper Analysis section
tmp.wizMiss <- subData.wizmiss.wt200d.sbgrpWt.resPop %>%
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
tmp.noMiss <- subData.nomiss.wt200d.sbgrpWt.resPop %>%
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

sum(tmp.wizMiss$artStatus == "naive")
mean(tmp.wizMiss$artStatus == "naive")
# Weight_0 + viralloadImptCtgr_0 +
# SystolicBP_0 + DiastolicBP_0 + Height_0 + tbtx_0 + 
# activeTB_0 + Married_0 + nhif_0,

apply(tmp.wizMiss %>% select(Weight_0, SystolicBP_0:Married_0, glucose_0:nhif_0), 2, function(x) mean(as.numeric(x), na.rm = T))
apply(tmp.wizMiss %>% select(Weight_0, SystolicBP_0:Married_0, glucose_0:nhif_0), 2, function(x) sd(as.numeric(x), na.rm = T))

apply(tmp.noMiss %>% select(Weight_0, SystolicBP_0:Married_0, nhif_0), 2, function(x) mean(as.numeric(x), na.rm = T))
apply(tmp.noMiss %>% select(Weight_0, SystolicBP_0:Married_0, nhif_0), 2, function(x) sd(as.numeric(x), na.rm = T))

apply(tmp.wizMiss %>% select(tbtx_0, activeTB_0, Married_0, nhif_0), 2, function(x) sum(as.numeric(x), na.rm = T))
apply(tmp.noMiss %>% select(tbtx_0, activeTB_0, Married_0, nhif_0), 2, function(x) sum(as.numeric(x)))

mean(tmp.noMiss$activeTB_0)
mean(tmp.wizMiss$tbtx_0, na.rm = T)


# summary(tmp$nhif_0)
# apply(finalData.wizmiss.t0Jul2016.switchDTG.wt200d.resPop %>% select(Weight_0), 2, sd)

table(tmp.noMiss$viralloadImptCtgr_0)
table(tmp.noMiss$viralloadImptCtgr_1)
table(tmp.noMiss$viralloadImptCtgr_2)
table(tmp.noMiss$viralloadImptCtgr_3)

table(tmp.noMiss$viralloadImptCtgr_0) / summCensor.wizmiss.wt200d.sbgrpWt.resPop$n_0[3]
table(tmp.noMiss$viralloadImptCtgr_1) / summCensor.wizmiss.wt200d.sbgrpWt.resPop$n_1[3]
table(tmp.noMiss$viralloadImptCtgr_2) / summCensor.wizmiss.wt200d.sbgrpWt.resPop$n_2[3]
table(tmp.noMiss$viralloadImptCtgr_3) / summCensor.wizmiss.wt200d.sbgrpWt.resPop$n_3[3]

# time 1
apply(tmp.wizMiss %>% select(SystolicBP_1:Married_1, glucose_1:nhif_1), 2, function(x) mean(as.numeric(x), na.rm = T))
apply(tmp.wizMiss %>% select(SystolicBP_1:Married_1, glucose_1:nhif_1), 2, function(x) sd(as.numeric(x), na.rm = T))

apply(tmp.noMiss %>% select(SystolicBP_1:Married_1, nhif_1), 2, function(x) mean(as.numeric(x), na.rm = T))
apply(tmp.noMiss %>% select(SystolicBP_1:Married_1, nhif_1), 2, function(x) sd(as.numeric(x), na.rm = T))

apply(tmp.wizMiss %>% select(tbtx_1, activeTB_1, Married_1, nhif_1), 2, function(x) sum(as.numeric(x), na.rm = T))
apply(tmp.noMiss %>% select(tbtx_1, activeTB_1, Married_1, nhif_1), 2, function(x) sum(as.numeric(x), na.rm = T))

# time 2
apply(tmp.wizMiss %>% select(SystolicBP_2:Married_2, glucose_2:nhif_2), 2, function(x) mean(as.numeric(x), na.rm = T))
apply(tmp.wizMiss %>% select(SystolicBP_2:Married_2, glucose_2:nhif_2), 2, function(x) sd(as.numeric(x), na.rm = T))

apply(tmp.noMiss %>% select(SystolicBP_2:Married_2, nhif_2), 2, function(x) mean(as.numeric(x), na.rm = T))
apply(tmp.noMiss %>% select(SystolicBP_2:Married_2, nhif_2), 2, function(x) sd(as.numeric(x), na.rm = T))

apply(tmp.wizMiss %>% select(tbtx_2, activeTB_2, Married_2, nhif_2), 2, function(x) sum(as.numeric(x), na.rm = T))
apply(tmp.noMiss %>% select(tbtx_2, activeTB_2, Married_2, nhif_2), 2, function(x) sum(as.numeric(x), na.rm = T))

# time 3
apply(tmp.wizMiss %>% select(SystolicBP_3:Married_3, glucose_3:nhif_3), 2, function(x) mean(as.numeric(x), na.rm = T))
apply(tmp.wizMiss %>% select(SystolicBP_3:Married_3, glucose_3:nhif_3), 2, function(x) sd(as.numeric(x), na.rm = T))

apply(tmp.noMiss %>% select(SystolicBP_3:Married_3, nhif_3), 2, function(x) mean(as.numeric(x), na.rm = T))
apply(tmp.noMiss %>% select(SystolicBP_3:Married_3, nhif_3), 2, function(x) sd(as.numeric(x), na.rm = T))

apply(tmp.wizMiss %>% select(tbtx_3, activeTB_3, Married_3, nhif_3), 2, function(x) sum(as.numeric(x), na.rm = T))
apply(tmp.noMiss %>% select(tbtx_3, activeTB_3, Married_3, nhif_3), 2, function(x) sum(as.numeric(x), na.rm = T))



