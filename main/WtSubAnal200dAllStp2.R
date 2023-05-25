#!/usr/bin/env Rscript
#####################
# read in functions #
#####################
# load functions
setwd("../../")
folder <- paste(getwd(), "/Functions/", sep="")
functions <- list.files(folder)
functions.dir <- paste(folder, functions, sep = "")
for (functions.i in functions.dir){
  source(functions.i)
}

# RunRscriptNameSeedParallel.sh
# sbatch -J xxxx --nodelist=compute002 --array=1-32 .sh .R 
# sbatch -J WtSubAnal200dAllStp2_20230304 --nodelist=compute009 --array=101-150 RunRscriptNameSeedParallel.sh WtSubAnal200dAllStp2_20230304.R

setwd("DataAnalysis/Revision/")

############################################################
# Read in the arguments from command line for read in data # 
############################################################
array.id <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))

# p
# opt_parser  <- OptionParser(option_list=option_list)
# opt         <- parse_args(opt_parser)

# opt = list(seed = 222, windowlength = 4, maxdepth = 10, minsplit = 2000, 
#            minbucket = 1000, minobsytrain = 50, minobsytest = 10)
# opt = list(seed = 1)
# print(opt)

load(paste0("../../../Data/Data20210512/Processed/TreeRes20230304/WtSubAnal200dAllStp1_seed", array.id, 
            "_20230304.RData"))

#####################################
# Creat sequence of candidate trees # 
#####################################
seq.tree.wt200d <- create.sequence(large.tree = large.tree.wt200d)

# print(opt$minobsytest)
train.data.wt200d <- subData.nomiss.wt200d.sbgrpWt.resPop.scaled[tree.train.ind.wt200d, ]
colnames.subData.nomiss.wt200d.sbgrpWt.resPop.scaled <- colnames(subData.nomiss.wt200d.sbgrpWt.resPop.scaled)

vary.cov.int.used <- c("viralloadImptCtgr", "SystolicBP", "DiastolicBP", "Height",
                       # c("log10Viralload", "SystolicBP", "DiastolicBP", "Height", "pregnant", 
                       "tbtx", "activeTB", "Married", "nhif")
                       # "tbtx", "activeTB", "Married", "civilstatus", "nhif")
tmp.ind.L <- NULL
for (window.i in 1:(4-1)) {
  tmp.ind.L <- c(tmp.ind.L, paste0(vary.cov.int.used, "_", window.i))
}

final.tree.list.wt200d <- EstLtmle.CvMethod1.MltLamda(tree.data       = train.data.wt200d, 
                                                      tree.list       = seq.tree.wt200d$tree.list, 
                                                      lambda.used     = c(1:3, qchisq(0.95, 1), 4:40), # 13:26
                                                      test.data       = subData.nomiss.wt200d.sbgrpWt.resPop.scaled[tree.test.ind.wt200d, ],                                             
                                                      ind.A        = which(colnames.subData.nomiss.wt200d.sbgrpWt.resPop.scaled %in% paste0("DTGOn_", 0:(4-1))), 
                                                      ind.C        = which(colnames.subData.nomiss.wt200d.sbgrpWt.resPop.scaled %in% paste0("censor_", 0:(4-1))),
                                                      ind.L        = which(colnames.subData.nomiss.wt200d.sbgrpWt.resPop.scaled %in% tmp.ind.L),
                                                      ind.Y        = which(colnames.subData.nomiss.wt200d.sbgrpWt.resPop.scaled %in% paste0("Weight_", 1:4)), 
                                                      survivalOutcome = F,
                                                      abar            = list(treatment = rep(1, 4), control = rep(0, 4)),
                                                      gbounds         = c(0.025, 0.975),
                                                      min.obsY.trt    = 10)

# tree.data       = train.data.wt200d 
# tree.list       = seq.tree.wt200d$tree.list 
# lambda.used     = c(1:3, qchisq(0.95, 1), 4:40) # 13:26
# test.data       = subData.nomiss.wt200d.sbgrpWt.resPop.scaled[tree.test.ind.wt200d, ]                                             
# ind.A        = which(colnames.subData.nomiss.wt200d.sbgrpWt.resPop.scaled %in% paste0("DTGOn_", 0:(opt$windowlength-1))) 
# ind.C        = which(colnames.subData.nomiss.wt200d.sbgrpWt.resPop.scaled %in% paste0("censor_", 0:(opt$windowlength-1)))
# ind.L        = which(colnames.subData.nomiss.wt200d.sbgrpWt.resPop.scaled %in% tmp.ind.L)
# ind.Y        = which(colnames.subData.nomiss.wt200d.sbgrpWt.resPop.scaled %in% paste0("Weight_", 1:opt$windowlength))
# survivalOutcome = F
# abar            = list(treatment = rep(1, opt$windowlength), control = rep(0, opt$windowlength))
# gbounds         = c(0.025, 0.975)
# min.obsY.trt    = opt$minobsytest

save(seq.tree.wt200d, final.tree.list.wt200d,
     file = paste0("../../../Data/Data20210512/Processed/TreeRes20230304/WtSubAnal200dAllStp2_seed", array.id, 
                  "_20230304.RData"))

