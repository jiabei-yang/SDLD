#!/usr/bin/env Rscript
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

# load data first and then read in from command line
# otherwise opt will be covered
load("../../Data/Data20210512/Processed/Summ2AllWt20220511.RData")
load("../Simulation/seed1000.rda")

registerDoParallel(cores=Sys.getenv("SLURM_CPUS_PER_TASK"))
# registerDoParallel(cores=28)

# Read in the arguments from command line
option_list <- list(make_option(c("-s", "--seed"), type = "double"),
                    make_option(c("-l", "--windowlength"), type = "double"),
                    make_option(c("-d", "--maxdepth"), type = "double"),
                    make_option(c("-p", "--minsplit"), type = "double"),
                    make_option(c("-b", "--minbucket"), type = "double"),
                    make_option(c("-m", "--minobsytrain"), type = "double"))
# p
opt_parser  <- OptionParser(option_list=option_list)
opt         <- parse_args(opt_parser)

# 50 / (517 / 84445) = 8167
# 20 / (517 / 84445) = 3267
# opt = list(seed = 333, windowlength = 4, maxdepth = 10, minsplit = 2000, minbucket = 1000, minobsytrain = 50)
# print(opt)

ampath.visit.wt200d.wtTime0.noPrgnt <- ampath.visit.wt200d.wtTime0 %>%
  filter(patient_id %in% finalData.noMiss.t0Jul2016.switchDTG.wt200d$patient_id)
dim(ampath.visit.wt200d.wtTime0)
dim(ampath.visit.wt200d.wtTime0.noPrgnt)

head(ampath.visit.wt200d.wtTime0)


# colnames(finalData.noMiss.t0Jul2016.switchDTG.wt200d)
finalData.noMiss.t0Jul2016.switchDTG.wt200d <- finalData.noMiss.t0Jul2016.switchDTG.wt200d %>%
  mutate(censor_0 = BinaryToCensoring(is.censored = censor_0),
         censor_1 = BinaryToCensoring(is.censored = censor_1),
         censor_2 = BinaryToCensoring(is.censored = censor_2),
         censor_3 = BinaryToCensoring(is.censored = censor_3),
         censor_4 = BinaryToCensoring(is.censored = censor_4),
         censor_5 = BinaryToCensoring(is.censored = censor_5),
         censor_6 = BinaryToCensoring(is.censored = censor_6),
         censor_7 = BinaryToCensoring(is.censored = censor_7))

#############################
# scale and numeric columns #
#############################
if (opt$windowlength == 4) {
  
  finalData.noMiss.t0Jul2016.switchDTG.wt200d.scaled <- finalData.noMiss.t0Jul2016.switchDTG.wt200d %>%
    select(male:Weight_4) %>%
    select(-enroldate_dob, -enroldate_arvstart, -time0_enroldate) %>%
    mutate(arvstart_dob = scale(arvstart_dob), 
           # enroldate_dob = scale(enroldate_dob),
           time0_dob     = scale(time0_dob),
           # enroldate_arvstart = scale(enroldate_arvstart),
           time0_arvstart     = scale(time0_arvstart),
           # time0_enroldate    = scale(time0_enroldate),
           # viralload_0        = scale(viralload_0),
           SystolicBP_0       = scale(SystolicBP_0),
           SystolicBP_1       = scale(SystolicBP_1),
           SystolicBP_2       = scale(SystolicBP_2),
           SystolicBP_3       = scale(SystolicBP_3),
           DiastolicBP_0      = scale(DiastolicBP_0),
           DiastolicBP_1      = scale(DiastolicBP_1),
           DiastolicBP_2      = scale(DiastolicBP_2),
           DiastolicBP_3      = scale(DiastolicBP_3),
           Weight_0       = scale(Weight_0),
           # Weight_1       = scale(Weight_1),
           # Weight_2       = scale(Weight_2),
           # Weight_3       = scale(Weight_3),
           Height_0       = scale(Height_0),
           Height_1       = scale(Height_1),
           Height_2       = scale(Height_2),
           Height_3       = scale(Height_3)) %>%
    mutate(male                 = ifelse(male == "Female", 0, 1),
           enroll_after20160701 = ifelse(enroll_after20160701, 1, 0),
           # civilstatus_0        = ifelse(civilstatus_0 == "Living w/Partner", 1, civilstatus_0),
           # civilstatus_0        = ifelse(civilstatus_0 == "Separated", 2, civilstatus_0),
           # civilstatus_0        = ifelse(civilstatus_0 == "Legally Married", 3, civilstatus_0),
           # civilstatus_0        = ifelse(civilstatus_0 == "Widowed", 4, civilstatus_0),
           # civilstatus_0        = ifelse(civilstatus_0 == "Never Married and Not Living w/Partner", 5, civilstatus_0),
           # civilstatus_0        = ifelse(civilstatus_0 == "Divorced", 6, civilstatus_0),
           nhif_0               = ifelse(nhif_0 == "yes", 1, 0),
           # civilstatus_1        = ifelse(civilstatus_1 == "Living w/Partner", 1, civilstatus_1),
           # civilstatus_1        = ifelse(civilstatus_1 == "Separated", 2, civilstatus_1),
           # civilstatus_1        = ifelse(civilstatus_1 == "Legally Married", 3, civilstatus_1),
           # civilstatus_1        = ifelse(civilstatus_1 == "Widowed", 4, civilstatus_1),
           # civilstatus_1        = ifelse(civilstatus_1 == "Never Married and Not Living w/Partner", 5, civilstatus_1),
           # civilstatus_1        = ifelse(civilstatus_1 == "Divorced", 6, civilstatus_1),
           nhif_1               = ifelse(nhif_1 == "yes", 1, 0),
           # civilstatus_2        = ifelse(civilstatus_2 == "Living w/Partner", 1, civilstatus_2),
           # civilstatus_2        = ifelse(civilstatus_2 == "Separated", 2, civilstatus_2),
           # civilstatus_2        = ifelse(civilstatus_2 == "Legally Married", 3, civilstatus_2),
           # civilstatus_2        = ifelse(civilstatus_2 == "Widowed", 4, civilstatus_2),
           # civilstatus_2        = ifelse(civilstatus_2 == "Never Married and Not Living w/Partner", 5, civilstatus_2),
           # civilstatus_2        = ifelse(civilstatus_2 == "Divorced", 6, civilstatus_2),
           nhif_2               = ifelse(nhif_2 == "yes", 1, 0),
           # civilstatus_3        = ifelse(civilstatus_3 == "Living w/Partner", 1, civilstatus_3),
           # civilstatus_3        = ifelse(civilstatus_3 == "Separated", 2, civilstatus_3),
           # civilstatus_3        = ifelse(civilstatus_3 == "Legally Married", 3, civilstatus_3),
           # civilstatus_3        = ifelse(civilstatus_3 == "Widowed", 4, civilstatus_3),
           # civilstatus_3        = ifelse(civilstatus_3 == "Never Married and Not Living w/Partner", 5, civilstatus_3),
           # civilstatus_3        = ifelse(civilstatus_3 == "Divorced", 6, civilstatus_3),
           nhif_3               = ifelse(nhif_3 == "yes", 1, 0))
  
} else if (opt$windowlength == 3) {
  
  finalData.noMiss.t0Jul2016.switchDTG.wt200d.scaled <- finalData.noMiss.t0Jul2016.switchDTG.wt200d %>%
    select(male:Weight_3) %>%
    select(-enroldate_dob, -enroldate_arvstart, -time0_enroldate) %>%
    mutate(arvstart_dob = scale(arvstart_dob), 
           # enroldate_dob = scale(enroldate_dob),
           time0_dob     = scale(time0_dob),
           # enroldate_arvstart = scale(enroldate_arvstart),
           time0_arvstart     = scale(time0_arvstart),
           # time0_enroldate    = scale(time0_enroldate),
           # viralload_0        = scale(viralload_0),
           SystolicBP_0       = scale(SystolicBP_0),
           SystolicBP_1       = scale(SystolicBP_1),
           SystolicBP_2       = scale(SystolicBP_2),
           # SystolicBP_3       = scale(SystolicBP_3),
           DiastolicBP_0      = scale(DiastolicBP_0),
           DiastolicBP_1      = scale(DiastolicBP_1),
           DiastolicBP_2      = scale(DiastolicBP_2),
           # DiastolicBP_3      = scale(DiastolicBP_3),
           Weight_0       = scale(Weight_0),
           # Weight_1       = scale(Weight_1),
           # Weight_2       = scale(Weight_2),
           # Weight_3       = scale(Weight_3),
           Height_0       = scale(Height_0),
           Height_1       = scale(Height_1),
           Height_2       = scale(Height_2)) %>%
    # Height_3       = scale(Height_3)) %>%
    mutate(male                 = ifelse(male == "Female", 0, 1),
           enroll_after20160701 = ifelse(enroll_after20160701, 1, 0),
           # civilstatus_0        = ifelse(civilstatus_0 == "Living w/Partner", 1, civilstatus_0),
           # civilstatus_0        = ifelse(civilstatus_0 == "Separated", 2, civilstatus_0),
           # civilstatus_0        = ifelse(civilstatus_0 == "Legally Married", 3, civilstatus_0),
           # civilstatus_0        = ifelse(civilstatus_0 == "Widowed", 4, civilstatus_0),
           # civilstatus_0        = ifelse(civilstatus_0 == "Never Married and Not Living w/Partner", 5, civilstatus_0),
           # civilstatus_0        = ifelse(civilstatus_0 == "Divorced", 6, civilstatus_0),
           nhif_0               = ifelse(nhif_0 == "yes", 1, 0),
           # civilstatus_1        = ifelse(civilstatus_1 == "Living w/Partner", 1, civilstatus_1),
           # civilstatus_1        = ifelse(civilstatus_1 == "Separated", 2, civilstatus_1),
           # civilstatus_1        = ifelse(civilstatus_1 == "Legally Married", 3, civilstatus_1),
           # civilstatus_1        = ifelse(civilstatus_1 == "Widowed", 4, civilstatus_1),
           # civilstatus_1        = ifelse(civilstatus_1 == "Never Married and Not Living w/Partner", 5, civilstatus_1),
           # civilstatus_1        = ifelse(civilstatus_1 == "Divorced", 6, civilstatus_1),
           nhif_1               = ifelse(nhif_1 == "yes", 1, 0),
           # civilstatus_2        = ifelse(civilstatus_2 == "Living w/Partner", 1, civilstatus_2),
           # civilstatus_2        = ifelse(civilstatus_2 == "Separated", 2, civilstatus_2),
           # civilstatus_2        = ifelse(civilstatus_2 == "Legally Married", 3, civilstatus_2),
           # civilstatus_2        = ifelse(civilstatus_2 == "Widowed", 4, civilstatus_2),
           # civilstatus_2        = ifelse(civilstatus_2 == "Never Married and Not Living w/Partner", 5, civilstatus_2),
           # civilstatus_2        = ifelse(civilstatus_2 == "Divorced", 6, civilstatus_2),
           nhif_2               = ifelse(nhif_2 == "yes", 1, 0))
  
}

finalData.noMiss.t0Jul2016.switchDTG.wt200d.scaled <- finalData.noMiss.t0Jul2016.switchDTG.wt200d.scaled %>%
  select(-starts_with(c("viralloadImptMn", "log10ViralloadImptMn"))) %>%
  select(-starts_with("pregnant")) %>%
  select(-starts_with("civilstatus"))

vary.cov.int.used <- c("viralloadImptCtgr", "SystolicBP", "DiastolicBP", "Height",
                       # c("log10Viralload", "SystolicBP", "DiastolicBP", "Height", "pregnant", 
                       "tbtx", "activeTB", "Married", "nhif")
# "tbtx", "activeTB", "Married", "civilstatus", "nhif")

######################################################################################
# split data into tree building (60%, 48% train, 12% test) and ci construction (40%) #
######################################################################################
set.seed(a[opt$seed])
trt.ind.wt200d <- which((finalData.noMiss.t0Jul2016.switchDTG.wt200d.scaled$DTGOn_0 == 1) & (finalData.noMiss.t0Jul2016.switchDTG.wt200d.scaled$DTGOn_1 == 1) & 
                          (finalData.noMiss.t0Jul2016.switchDTG.wt200d.scaled$DTGOn_2 == 1) & (finalData.noMiss.t0Jul2016.switchDTG.wt200d.scaled$DTGOn_3 == 1) & 
                          (!is.na(finalData.noMiss.t0Jul2016.switchDTG.wt200d.scaled$Weight_4))) 
ctrl.ind.wt200d <- which((finalData.noMiss.t0Jul2016.switchDTG.wt200d.scaled$DTGOn_0 == 0) & (finalData.noMiss.t0Jul2016.switchDTG.wt200d.scaled$DTGOn_1 == 0) & 
                           (finalData.noMiss.t0Jul2016.switchDTG.wt200d.scaled$DTGOn_2 == 0) & (finalData.noMiss.t0Jul2016.switchDTG.wt200d.scaled$DTGOn_3 == 0) & 
                           (!is.na(finalData.noMiss.t0Jul2016.switchDTG.wt200d.scaled$Weight_4))) 
n.pts.wt200d          <- nrow(finalData.noMiss.t0Jul2016.switchDTG.wt200d.scaled)
lft.ind.wt200d <- (1:n.pts.wt200d)[!(1:n.pts.wt200d %in% c(trt.ind.wt200d, ctrl.ind.wt200d))]

# trt.ind.wt200d[1:10]
# ctrl.ind.wt200d[1:10]
# lft.ind.wt200d[1:10]

# stratified sampling
n.trt.ind.wt200d  <- length(trt.ind.wt200d)
n.ctrl.ind.wt200d <- length(ctrl.ind.wt200d)
n.lft.ind.wt200d  <- length(lft.ind.wt200d)

tree.trt.ind.wt200d       <- sample(trt.ind.wt200d, floor(n.trt.ind.wt200d * 0.6), replace = FALSE)
tree.trt.train.ind.wt200d <- sample(tree.trt.ind.wt200d, floor(n.trt.ind.wt200d * 0.6 * 0.8), replace = FALSE)
tree.trt.test.ind.wt200d  <- tree.trt.ind.wt200d[!(tree.trt.ind.wt200d %in% tree.trt.train.ind.wt200d)]

tree.ctrl.ind.wt200d       <- sample(ctrl.ind.wt200d, floor(n.ctrl.ind.wt200d * 0.6), replace = FALSE)
tree.ctrl.train.ind.wt200d <- sample(tree.ctrl.ind.wt200d, floor(n.ctrl.ind.wt200d * 0.6 * 0.8), replace = FALSE)
tree.ctrl.test.ind.wt200d  <- tree.ctrl.ind.wt200d[!(tree.ctrl.ind.wt200d %in% tree.ctrl.train.ind.wt200d)]

tree.lft.ind.wt200d       <- sample(lft.ind.wt200d, floor(n.lft.ind.wt200d * 0.6), replace = FALSE)
tree.lft.train.ind.wt200d <- sample(tree.lft.ind.wt200d, floor(n.lft.ind.wt200d * 0.6 * 0.8), replace = FALSE)
tree.lft.test.ind.wt200d  <- tree.lft.ind.wt200d[!(tree.lft.ind.wt200d %in% tree.lft.train.ind.wt200d)]

tree.train.ind.wt200d <- sort(c(tree.trt.train.ind.wt200d, tree.ctrl.train.ind.wt200d, tree.lft.train.ind.wt200d))
tree.test.ind.wt200d <- sort(c(tree.trt.test.ind.wt200d, tree.ctrl.test.ind.wt200d, tree.lft.test.ind.wt200d))
tree.ind.wt200d <- sort(c(tree.train.ind.wt200d, tree.test.ind.wt200d))

# tree.train.ind.wt200d[1:10]
# tree.test.ind.wt200d[1:10]
# tree.ind.wt200d[1:10]
ci.ind.wt200d         <- (1:n.pts.wt200d)[!((1:n.pts.wt200d) %in% tree.ind.wt200d)]

train.data.wt200d <- finalData.noMiss.t0Jul2016.switchDTG.wt200d.scaled[tree.train.ind.wt200d, ]
data.bl.wiz.rownumb <- data.frame(rownumb = 1:length(tree.train.ind.wt200d),
                                  train.data.wt200d[, c("male", "arvstart_dob", "time0_dob", 
                                                        # "male", "arvstart_dob", "enroldate_dob", "time0_dob", "enroldate_arvstart",
                                                        "time0_arvstart", "enroll_after20160701", "Weight_0", paste0(vary.cov.int.used, "_0"))])

# sum((train.data.wt200d$DTGOn_0 == 0) & (train.data.wt200d$DTGOn_1 == 0) & 
#       (train.data.wt200d$DTGOn_2 == 0) & (train.data.wt200d$DTGOn_3 == 0) & 
#       (!is.na(train.data.wt200d$Weight_4)), na.rm = T) 

#############################################
# find corresponding columns for A, C, L, Y #
#############################################
colnames.finalData.noMiss.t0Jul2016.switchDTG.wt200d.scaled <- colnames(finalData.noMiss.t0Jul2016.switchDTG.wt200d.scaled)
tmp.ind.L <- NULL
for (window.i in 1:(opt$windowlength-1)) {
  tmp.ind.L <- c(tmp.ind.L, paste0(vary.cov.int.used, "_", window.i))
}

parms.used <- list(whole.data   = train.data.wt200d, 
                   ind.A        = which(colnames.finalData.noMiss.t0Jul2016.switchDTG.wt200d.scaled %in% paste0("DTGOn_", 0:(opt$windowlength-1))), 
                   ind.C        = which(colnames.finalData.noMiss.t0Jul2016.switchDTG.wt200d.scaled %in% paste0("censor_", 0:(opt$windowlength-1))),
                   ind.L        = which(colnames.finalData.noMiss.t0Jul2016.switchDTG.wt200d.scaled %in% tmp.ind.L),
                   ind.Y        = which(colnames.finalData.noMiss.t0Jul2016.switchDTG.wt200d.scaled %in% paste0("Weight_", 1:opt$windowlength)), 
                   survivalOutcome = NULL,
                   abar            = list(treatment = rep(1, opt$windowlength), control = rep(0, opt$windowlength)),
                   gbounds         = c(0.025, 0.975),
                   max.lvls        = 15,
                   min.obsY.trt    = opt$minobsytrain,
                   num.truc.obs    = 100,
                   num.goodness.iter = 200,
                   num.optimal       = 4,
                   parallel          = TRUE) 

# summary(data.bl.wiz.rownumb)
########### NEED TO DO UNIQUE 

# x     <- data.bl.wiz.rownumb$civilstatus_0
# y     <- 1:nrow(data.bl.wiz.rownumb)
# x     <- sort(data.bl.wiz.rownumb$male)
# y     <- order(data.bl.wiz.rownumb$male)
# wt    <- rep(1, length(y))
# parms <- parms.used

# x     <- sort(data.bl.wiz.rownumb$Weight_0)
# y     <- order(data.bl.wiz.rownumb$Weight_0)
# wt    <- rep(1, length(y))
# parms <- parms.used

ulist.used <- list(eval = etemp.ltmle, split = stemp.ltmle, init = itemp)
large.tree.wt200d <- rpart(rownumb ~ male + arvstart_dob + time0_dob + 
                             # enroldate_dob + enroldate_arvstart + time0_enroldate + 
                             time0_arvstart + enroll_after20160701 + Weight_0 + viralloadImptCtgr_0 +
                             # log10Viralload_0 + pregnant_0 + 
                             SystolicBP_0 + DiastolicBP_0 + Height_0 + tbtx_0 + 
                             # activeTB_0 + Married_0 + civilstatus_0 + nhif_0,
                             activeTB_0 + Married_0 + nhif_0, 
                           data     = data.bl.wiz.rownumb,
                           method   = ulist.used,
                           parms    = parms.used,
                           control  = rpart.control(cp = 0, minsplit = opt$minsplit, minbucket = opt$minbucket, maxsurrogate = 0, maxcompete = 0, maxdepth = opt$maxdepth))
# large.tree

# warnings() 
# save(large.tree, file = "Tree20210911.RData")
save(finalData.noMiss.t0Jul2016.switchDTG.wt200d.scaled,
     tree.ind.wt200d, tree.train.ind.wt200d, tree.test.ind.wt200d, ci.ind.wt200d,
     large.tree.wt200d, 
     file = paste0("../../Data/Data20210512/Processed/TreeRes20220512/WtSubAnal200dAllStp1_seed", opt$seed, 
                   "_windowlength", opt$windowlength, "_maxdepth", opt$maxdepth, "_minsplit", opt$minsplit, 
                   "_minbucket", opt$minbucket, "_minobsytrain", opt$minobsytrain, "_20220512.RData"))
# load("../../Data/Data20210512/Processed/WtSubAnal200dAll20220402.RData")
# large.tree <- large.tree

# rm(opt)

