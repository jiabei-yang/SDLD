#!/usr/bin/env Rscript
# load functions
setwd("../../")
folder <- paste(getwd(), "/Functions/", sep="")
functions <- list.files(folder)
functions.dir <- paste(folder, functions, sep = "")
for (functions.i in functions.dir){
  source(functions.i)
}

# RunRscriptNameSeedParallelLoop.sh
# sbatch -J xxxx --nodelist=compute002 --cpus-per-task=28 .sh .R 1 100

setwd("DataAnalysis/Revision/")

# load data first and then read in from command line
# otherwise opt will be covered
load("../../../Data/Data20210512/Processed/Summ2AllWt20230304.RData")
load("../../Simulation/seed1000.rda")


head(subData.nomiss.wt200d.sbgrpWt.resPop %>%
       select(Weight_0, Height_0))

subData.nomiss.wt200d.sbgrpWt.resPop <- subData.nomiss.wt200d.sbgrpWt.resPop %>%
  mutate(bmi_0 = Weight_0 / (Height_0 / 100)^2,
         bmi_1 = Weight_1 / (Height_1 / 100)^2,
         bmi_2 = Weight_2 / (Height_2 / 100)^2,
         bmi_3 = Weight_3 / (Height_3 / 100)^2)

mean(subData.nomiss.wt200d.sbgrpWt.resPop$bmi_0 < 18.5)
mean(subData.nomiss.wt200d.sbgrpWt.resPop$bmi_0 > 24.9)

mean(subData.nomiss.wt200d.sbgrpWt.resPop$bmi_3 < 18.5, na.rm = T)
mean(subData.nomiss.wt200d.sbgrpWt.resPop$bmi_3 > 24.9, na.rm = T)

summ.bmi <- data.frame(time = 0:3,
                       under = c(mean(subData.nomiss.wt200d.sbgrpWt.resPop$bmi_0 < 18.5),
                                 mean(subData.nomiss.wt200d.sbgrpWt.resPop$bmi_1 < 18.5, na.rm = T),
                                 mean(subData.nomiss.wt200d.sbgrpWt.resPop$bmi_2 < 18.5, na.rm = T),
                                 mean(subData.nomiss.wt200d.sbgrpWt.resPop$bmi_3 < 18.5, na.rm = T)),
                       over = c(mean(subData.nomiss.wt200d.sbgrpWt.resPop$bmi_0 > 24.9),
                                 mean(subData.nomiss.wt200d.sbgrpWt.resPop$bmi_1 > 24.9, na.rm = T),
                                 mean(subData.nomiss.wt200d.sbgrpWt.resPop$bmi_2 > 24.9, na.rm = T),
                                 mean(subData.nomiss.wt200d.sbgrpWt.resPop$bmi_3 > 24.9, na.rm = T)))
summ.bmi <- summ.bmi %>%
  mutate(under = round(under * 100, 1),
         over = round(over * 100, 1))

summ.bmi <- summ.bmi %>%
  mutate(under = paste0(under, "%"),
         over = paste0(over, "%"))

library(xtable)
print(xtable(summ.bmi), include.rownames = FALSE)

summary(subData.nomiss.wt200d.sbgrpWt.resPop$bmi_0)
summary(subData.nomiss.wt200d.sbgrpWt.resPop$bmi_1)
summary(subData.nomiss.wt200d.sbgrpWt.resPop$bmi_2)
summary(subData.nomiss.wt200d.sbgrpWt.resPop$bmi_3)

id.selected <- subData.nomiss.wt200d.sbgrpWt.resPop %>%
  filter(subData.nomiss.wt200d.sbgrpWt.resPop$patient_id %in% c("794076", "826446", "815652", "20978", "869898")) %>%
  select(patient_id, 
         starts_with("bmi_"), 
         paste0("Weight_", 0:3),
         paste0("Height_", 0:3))


pdf("../../../Report/BaselineBMI_20230501.pdf", width = 8, height = 8)
par(mfrow = c(2, 2))
x <- boxplot(subData.nomiss.wt200d.sbgrpWt.resPop[, "bmi_0"], 
             main = "BMI at time 0")
text(rep(1, nrow(id.selected)), id.selected$bmi_0, id.selected$patient_id, pos = 4)  

# hist(subData.nomiss.wt200d.sbgrpWt.resPop[, "bmi_0"], 
#      breaks = 50,
#      main = "BMI at time 0")

x <- boxplot(subData.nomiss.wt200d.sbgrpWt.resPop[, "bmi_1"], 
             main = "BMI at time 1")
text(rep(1, nrow(id.selected)), id.selected$bmi_1, id.selected$patient_id, pos = 4)  

x <- boxplot(subData.nomiss.wt200d.sbgrpWt.resPop[, "bmi_2"], 
             main = "BMI at time 2")
text(rep(1, nrow(id.selected)), id.selected$bmi_2, id.selected$patient_id, pos = 4)  

x <- boxplot(subData.nomiss.wt200d.sbgrpWt.resPop[, "bmi_3"], 
             main = "BMI at time 3")
text(rep(1, nrow(id.selected)), id.selected$bmi_3, id.selected$patient_id, pos = 4)  

# hist(subData.nomiss.wt200d.sbgrpWt.resPop[, "bmi_3"], 
#      breaks = 50,
#      main = "BMI at time 3")
dev.off()

