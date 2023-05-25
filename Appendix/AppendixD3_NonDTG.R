library(dplyr) 
library(tidyr)
library(doParallel)

# registerDoParallel(cores=28)
registerDoParallel(cores=Sys.getenv("SLURM_CPUS_PER_TASK"))

load("../../../Data/Data20210512/Processed/Summ2AllWt20230304.RData")

ampath.visit.wt200d.sbgrpWt.noPrgnt <- ampath.visit.wt200d.sbgrpWt %>%
  filter(patient_id %in% subData.nomiss.wt200d.sbgrpWt.resPop$patient_id)
# dim(ampath.visit.wt200d.sbgrpWt)
# dim(ampath.visit.wt200d.sbgrpWt.noPrgnt)
length(unique(ampath.visit.wt200d.sbgrpWt.noPrgnt$patient_id))

tmp <- 
  foreach(i = 1:nrow(subData.nomiss.wt200d.sbgrpWt.resPop),
          .combine = rbind) %dopar% {
            
            if ((i %% (4*10^3)) == 0) {
              print(i)
            }
            
            tmp.visits <- ampath.visit.wt200d.sbgrpWt.noPrgnt %>%
              filter(patient_id == subData.nomiss.wt200d.sbgrpWt.resPop$patient_id[i])
            tmp.subData.nomiss.wt200d.sbgrpWt.resPop <- subData.nomiss.wt200d.sbgrpWt.resPop[i, ]
            
            tmp.tdf <- NULL
            
            for (t.200d in (0:(maxT.wizmiss.t0Jul2016.switchDTG.wt200d - 2))) {
              
              tmp.visits.200d <- tmp.visits %>%
                filter(apptdate_time0_200d == t.200d)
              nrow.tmp.visits.200d <- nrow(tmp.visits.200d)
              
              # at time 0
              if (t.200d == 0) {
                
                if (nrow.tmp.visits.200d > 0) {
                  
                  tmp.tdf <- c(tmp.tdf, ifelse(!is.na(tmp.visits.200d$TDF[nrow.tmp.visits.200d]), 1, 0))
                  
                } else {
                  
                  cat("no visit at time 0\n")
                }
                
                # after time 0
              } else {
                
                # if censored, add in missing TDF info (even when there is visit info)
                if (tmp.subData.nomiss.wt200d.sbgrpWt.resPop[, paste0("censor_", t.200d - 1)] == 1) {
                  
                  tmp.tdf <- c(tmp.tdf, rep(NA, maxT.wizmiss.t0Jul2016.switchDTG.wt200d - 1 - t.200d))
                  break
                  
                  # not censored
                } else {
                  
                  if (nrow.tmp.visits.200d > 0) {
                    
                    tmp.tdf <- c(tmp.tdf, ifelse(!is.na(tmp.visits.200d$TDF[nrow.tmp.visits.200d]), 1, 0))
                    
                    # carry over in situations where have not censored but no observation in window
                  } else {
                    tmp.tdf <- c(tmp.tdf, tmp.tdf[t.200d])
                  }
                  
                } # not censored
              } # after time 0
              
              
            } # for t.200d
            
            tmp.tdf
            
            # if (length(tmp.tdf) != 8) {
            #   cat(i, "length diff\n")
            # }
          }

# save.image("IntermediateData.RData")
# load("IntermediateData.RData")

TDFOn <- data.frame(tmp)
colnames(TDFOn) <- paste0("TDFOn_", 0:7)
DTGOn <- subData.nomiss.wt200d.sbgrpWt.resPop %>%
  select(starts_with("DTGOn_"))

sum(subData.nomiss.wt200d.sbgrpWt.resPop$censor_6 == 0, na.rm = T)

# time 1
summ.nonDTG <- data.frame(t = 1,
                          uncensored = sum(subData.nomiss.wt200d.sbgrpWt.resPop$censor_0 == 0, na.rm = T),
                          AlwaysDTG  = sum((DTGOn$DTGOn_0 == 1) & 
                                             (subData.nomiss.wt200d.sbgrpWt.resPop$censor_0 == 0), na.rm = T),
                          AlwaysDtgTdf = sum((DTGOn$DTGOn_0 == 1) & 
                                               (TDFOn$TDFOn_0 == 1) &
                                               (subData.nomiss.wt200d.sbgrpWt.resPop$censor_0 == 0), na.rm = T),
                          AlwaysDtgNvrTdf = sum((DTGOn$DTGOn_0 == 1) & 
                                               (TDFOn$TDFOn_0 == 0) &
                                               (subData.nomiss.wt200d.sbgrpWt.resPop$censor_0 == 0), na.rm = T),
                          NeverDTG    = sum((DTGOn$DTGOn_0 == 0) & 
                                              (subData.nomiss.wt200d.sbgrpWt.resPop$censor_0 == 0), na.rm = T),
                          NeverDtgTdf = sum((DTGOn$DTGOn_0 == 0) & 
                                              (TDFOn$TDFOn_0 == 1) &
                                              (subData.nomiss.wt200d.sbgrpWt.resPop$censor_0 == 0), na.rm = T),
                          NeverDtgNvrTdf = sum((DTGOn$DTGOn_0 == 0) & 
                                                 (TDFOn$TDFOn_0 == 0) &
                                                 (subData.nomiss.wt200d.sbgrpWt.resPop$censor_0 == 0), na.rm = T),
                          NeverDtgSwtTdf2 = NA,
                          NeverDtgSwtTdf3 = NA,
                          NeverDtgSwtTdf4 = NA)

# time 2
summ.nonDTG <- rbind(summ.nonDTG, 
                     data.frame(t = 2,
                                uncensored = sum(subData.nomiss.wt200d.sbgrpWt.resPop$censor_1 == 0, na.rm = T),
                                AlwaysDTG  = sum((DTGOn$DTGOn_0 == 1) & 
                                                   (DTGOn$DTGOn_1 == 1) & 
                                                   (subData.nomiss.wt200d.sbgrpWt.resPop$censor_1 == 0), na.rm = T),
                                AlwaysDtgTdf = sum((DTGOn$DTGOn_0 == 1) & 
                                                     (DTGOn$DTGOn_1 == 1) & 
                                                     (TDFOn$TDFOn_0 == 1) &
                                                     (TDFOn$TDFOn_1 == 1) &
                                                     (subData.nomiss.wt200d.sbgrpWt.resPop$censor_1 == 0), na.rm = T),
                                AlwaysDtgNvrTdf = sum((DTGOn$DTGOn_0 == 1) & 
                                                     (DTGOn$DTGOn_1 == 1) & 
                                                     (TDFOn$TDFOn_0 == 0) &
                                                     (TDFOn$TDFOn_1 == 0) &
                                                     (subData.nomiss.wt200d.sbgrpWt.resPop$censor_1 == 0), na.rm = T),
                                NeverDTG    = sum((DTGOn$DTGOn_0 == 0) & 
                                                    (DTGOn$DTGOn_1 == 0) & 
                                                    (subData.nomiss.wt200d.sbgrpWt.resPop$censor_1 == 0), na.rm = T),
                                NeverDtgTdf = sum((DTGOn$DTGOn_0 == 0) & 
                                                    (DTGOn$DTGOn_1 == 0) & 
                                                    (TDFOn$TDFOn_0 == 1) &
                                                    (TDFOn$TDFOn_1 == 1) &
                                                    (subData.nomiss.wt200d.sbgrpWt.resPop$censor_1 == 0), na.rm = T),
                                NeverDtgNvrTdf = sum((DTGOn$DTGOn_0 == 0) & 
                                                       (DTGOn$DTGOn_1 == 0) & 
                                                       (TDFOn$TDFOn_0 == 0) &
                                                       (TDFOn$TDFOn_1 == 0) &
                                                       (subData.nomiss.wt200d.sbgrpWt.resPop$censor_1 == 0), na.rm = T),
                                NeverDtgSwtTdf2 = sum((DTGOn$DTGOn_0 == 0) & 
                                                        (DTGOn$DTGOn_1 == 0) & 
                                                        (TDFOn$TDFOn_0 == 0) &
                                                        (TDFOn$TDFOn_1 == 1) &
                                                        (subData.nomiss.wt200d.sbgrpWt.resPop$censor_1 == 0), na.rm = T),
                                NeverDtgSwtTdf3 = NA,
                                NeverDtgSwtTdf4 = NA))

# time 3
summ.nonDTG <- rbind(summ.nonDTG, 
                     data.frame(t = 3,
                                uncensored = sum(subData.nomiss.wt200d.sbgrpWt.resPop$censor_2 == 0, na.rm = T),
                                AlwaysDTG  = sum((DTGOn$DTGOn_0 == 1) & 
                                                   (DTGOn$DTGOn_1 == 1) & 
                                                   (DTGOn$DTGOn_2 == 1) & 
                                                   (subData.nomiss.wt200d.sbgrpWt.resPop$censor_2 == 0), na.rm = T),
                                AlwaysDtgTdf = sum((DTGOn$DTGOn_0 == 1) & 
                                                     (DTGOn$DTGOn_1 == 1) & 
                                                     (DTGOn$DTGOn_2 == 1) & 
                                                     (TDFOn$TDFOn_0 == 1) &
                                                     (TDFOn$TDFOn_1 == 1) &
                                                     (TDFOn$TDFOn_2 == 1) &
                                                     (subData.nomiss.wt200d.sbgrpWt.resPop$censor_2 == 0), na.rm = T),
                                AlwaysDtgNvrTdf = sum((DTGOn$DTGOn_0 == 1) & 
                                                     (DTGOn$DTGOn_1 == 1) & 
                                                     (DTGOn$DTGOn_2 == 1) & 
                                                     (TDFOn$TDFOn_0 == 0) &
                                                     (TDFOn$TDFOn_1 == 0) &
                                                     (TDFOn$TDFOn_2 == 0) &
                                                     (subData.nomiss.wt200d.sbgrpWt.resPop$censor_2 == 0), na.rm = T),
                                NeverDTG    = sum((DTGOn$DTGOn_0 == 0) & 
                                                    (DTGOn$DTGOn_1 == 0) & 
                                                    (DTGOn$DTGOn_2 == 0) & 
                                                    (subData.nomiss.wt200d.sbgrpWt.resPop$censor_2 == 0), na.rm = T),
                                NeverDtgTdf = sum((DTGOn$DTGOn_0 == 0) & 
                                                    (DTGOn$DTGOn_1 == 0) & 
                                                    (DTGOn$DTGOn_2 == 0) & 
                                                    (TDFOn$TDFOn_0 == 1) &
                                                    (TDFOn$TDFOn_1 == 1) &
                                                    (TDFOn$TDFOn_2 == 1) &
                                                    (subData.nomiss.wt200d.sbgrpWt.resPop$censor_2 == 0), na.rm = T),
                                NeverDtgNvrTdf = sum((DTGOn$DTGOn_0 == 0) & 
                                                       (DTGOn$DTGOn_1 == 0) & 
                                                       (DTGOn$DTGOn_2 == 0) & 
                                                       (TDFOn$TDFOn_0 == 0) &
                                                       (TDFOn$TDFOn_1 == 0) &
                                                       (TDFOn$TDFOn_2 == 0) &
                                                       (subData.nomiss.wt200d.sbgrpWt.resPop$censor_2 == 0), na.rm = T),
                                NeverDtgSwtTdf2 = sum((DTGOn$DTGOn_0 == 0) & 
                                                        (DTGOn$DTGOn_1 == 0) & 
                                                        (DTGOn$DTGOn_2 == 0) & 
                                                        (TDFOn$TDFOn_0 == 0) &
                                                        (TDFOn$TDFOn_1 == 1) &
                                                        (TDFOn$TDFOn_2 == 1) &
                                                        (subData.nomiss.wt200d.sbgrpWt.resPop$censor_2 == 0), na.rm = T),
                                NeverDtgSwtTdf3 = sum((DTGOn$DTGOn_0 == 0) & 
                                                        (DTGOn$DTGOn_1 == 0) & 
                                                        (DTGOn$DTGOn_2 == 0) & 
                                                        (TDFOn$TDFOn_0 == 0) &
                                                        (TDFOn$TDFOn_1 == 0) &
                                                        (TDFOn$TDFOn_2 == 1) &
                                                        (subData.nomiss.wt200d.sbgrpWt.resPop$censor_2 == 0), na.rm = T),
                                NeverDtgSwtTdf4 = NA))

# time 4
summ.nonDTG <- rbind(summ.nonDTG, 
                     data.frame(t = 4,
                                uncensored = sum(subData.nomiss.wt200d.sbgrpWt.resPop$censor_3 == 0, na.rm = T),
                                AlwaysDTG  = sum((DTGOn$DTGOn_0 == 1) & 
                                                   (DTGOn$DTGOn_1 == 1) & 
                                                   (DTGOn$DTGOn_2 == 1) & 
                                                   (DTGOn$DTGOn_3 == 1) & 
                                                   (subData.nomiss.wt200d.sbgrpWt.resPop$censor_3 == 0), na.rm = T),
                                AlwaysDtgTdf = sum((DTGOn$DTGOn_0 == 1) & 
                                                     (DTGOn$DTGOn_1 == 1) & 
                                                     (DTGOn$DTGOn_2 == 1) & 
                                                     (DTGOn$DTGOn_3 == 1) & 
                                                     (TDFOn$TDFOn_0 == 1) &
                                                     (TDFOn$TDFOn_1 == 1) &
                                                     (TDFOn$TDFOn_2 == 1) &
                                                     (TDFOn$TDFOn_3 == 1) &
                                                     (subData.nomiss.wt200d.sbgrpWt.resPop$censor_3 == 0), na.rm = T),
                                AlwaysDtgNvrTdf = sum((DTGOn$DTGOn_0 == 1) & 
                                                     (DTGOn$DTGOn_1 == 1) & 
                                                     (DTGOn$DTGOn_2 == 1) & 
                                                     (DTGOn$DTGOn_3 == 1) & 
                                                     (TDFOn$TDFOn_0 == 0) &
                                                     (TDFOn$TDFOn_1 == 0) &
                                                     (TDFOn$TDFOn_2 == 0) &
                                                     (TDFOn$TDFOn_3 == 0) &
                                                     (subData.nomiss.wt200d.sbgrpWt.resPop$censor_3 == 0), na.rm = T),
                                NeverDTG    = sum((DTGOn$DTGOn_0 == 0) & 
                                                    (DTGOn$DTGOn_1 == 0) & 
                                                    (DTGOn$DTGOn_2 == 0) & 
                                                    (DTGOn$DTGOn_3 == 0) & 
                                                    (subData.nomiss.wt200d.sbgrpWt.resPop$censor_3 == 0), na.rm = T),
                                NeverDtgTdf = sum((DTGOn$DTGOn_0 == 0) & 
                                                    (DTGOn$DTGOn_1 == 0) & 
                                                    (DTGOn$DTGOn_2 == 0) & 
                                                    (DTGOn$DTGOn_3 == 0) & 
                                                    (TDFOn$TDFOn_0 == 1) &
                                                    (TDFOn$TDFOn_1 == 1) &
                                                    (TDFOn$TDFOn_2 == 1) &
                                                    (TDFOn$TDFOn_3 == 1) &
                                                    (subData.nomiss.wt200d.sbgrpWt.resPop$censor_3 == 0), na.rm = T),
                                NeverDtgNvrTdf = sum((DTGOn$DTGOn_0 == 0) & 
                                                       (DTGOn$DTGOn_1 == 0) & 
                                                       (DTGOn$DTGOn_2 == 0) & 
                                                       (DTGOn$DTGOn_3 == 0) & 
                                                       (TDFOn$TDFOn_0 == 0) &
                                                       (TDFOn$TDFOn_1 == 0) &
                                                       (TDFOn$TDFOn_2 == 0) &
                                                       (TDFOn$TDFOn_3 == 0) &
                                                       (subData.nomiss.wt200d.sbgrpWt.resPop$censor_3 == 0), na.rm = T),
                                NeverDtgSwtTdf2 = sum((DTGOn$DTGOn_0 == 0) & 
                                                        (DTGOn$DTGOn_1 == 0) & 
                                                        (DTGOn$DTGOn_2 == 0) & 
                                                        (DTGOn$DTGOn_3 == 0) &
                                                        (TDFOn$TDFOn_0 == 0) &
                                                        (TDFOn$TDFOn_1 == 1) &
                                                        (TDFOn$TDFOn_2 == 1) &
                                                        (TDFOn$TDFOn_3 == 1) &
                                                        (subData.nomiss.wt200d.sbgrpWt.resPop$censor_3 == 0), na.rm = T),
                                NeverDtgSwtTdf3 = sum((DTGOn$DTGOn_0 == 0) & 
                                                        (DTGOn$DTGOn_1 == 0) & 
                                                        (DTGOn$DTGOn_2 == 0) & 
                                                        (DTGOn$DTGOn_3 == 0) &
                                                        (TDFOn$TDFOn_0 == 0) &
                                                        (TDFOn$TDFOn_1 == 0) &
                                                        (TDFOn$TDFOn_2 == 1) &
                                                        (TDFOn$TDFOn_3 == 1) &
                                                        (subData.nomiss.wt200d.sbgrpWt.resPop$censor_3 == 0), na.rm = T),
                                NeverDtgSwtTdf4 = sum((DTGOn$DTGOn_0 == 0) & 
                                                        (DTGOn$DTGOn_1 == 0) & 
                                                        (DTGOn$DTGOn_2 == 0) & 
                                                        (DTGOn$DTGOn_3 == 0) &
                                                        (TDFOn$TDFOn_0 == 0) &
                                                        (TDFOn$TDFOn_1 == 0) &
                                                        (TDFOn$TDFOn_2 == 0) &
                                                        (TDFOn$TDFOn_3 == 1) &
                                                        (subData.nomiss.wt200d.sbgrpWt.resPop$censor_3 == 0), na.rm = T)))

summ.nonDTG

save(TDFOn, summ.nonDTG,
     file = "../../../Data/Data20210512/Processed/NonDTG_20230428.RData")


library(xtable)
latex.summ.nonDTG <- summ.nonDTG %>%
  select(-AlwaysDtgNvrTdf) %>%
  select(t:NeverDtgNvrTdf)

print(xtable(latex.summ.nonDTG), include.rownames = FALSE)

