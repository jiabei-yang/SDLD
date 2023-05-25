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

############################################################
# Read in the arguments from command line for read in data # 
############################################################
option_list <- list(make_option(c("-s", "--seed"), type = "double"),
                    make_option(c("-l", "--windowlength"), type = "double"),
                    make_option(c("-d", "--maxdepth"), type = "double"),
                    make_option(c("-p", "--minsplit"), type = "double"),
                    make_option(c("-b", "--minbucket"), type = "double"),
                    make_option(c("-m", "--minobsytrain"), type = "double"),
                    make_option(c("-t", "--minobsytest"), type = "double"))
# p
opt_parser  <- OptionParser(option_list=option_list)
opt         <- parse_args(opt_parser)

# opt = list(seed = 222, windowlength = 4, maxdepth = 10, minsplit = 2000, 
#            minbucket = 1000, minobsytrain = 50, minobsytest = 10)
print(opt)

load(paste0("../../Data/Data20210512/Processed/TreeRes20220512/WtSubAnal200dAllStp1_seed", opt$seed, 
            "_windowlength", opt$windowlength, "_maxdepth", opt$maxdepth, "_minsplit", opt$minsplit, 
            "_minbucket", opt$minbucket, "_minobsytrain", opt$minobsytrain, "_20220512.RData"))

#####################################
# Creat sequence of candidate trees # 
#####################################
seq.tree.wt200d <- create.sequence(large.tree = large.tree.wt200d)

# print(opt$minobsytest)
train.data.wt200d <- finalData.noMiss.t0Jul2016.switchDTG.wt200d.scaled[tree.train.ind.wt200d, ]
colnames.finalData.noMiss.t0Jul2016.switchDTG.wt200d.scaled <- colnames(finalData.noMiss.t0Jul2016.switchDTG.wt200d.scaled)

vary.cov.int.used <- c("viralloadImptCtgr", "SystolicBP", "DiastolicBP", "Height",
                       # c("log10Viralload", "SystolicBP", "DiastolicBP", "Height", "pregnant", 
                       "tbtx", "activeTB", "Married", "nhif")
                       # "tbtx", "activeTB", "Married", "civilstatus", "nhif")
tmp.ind.L <- NULL
for (window.i in 1:(opt$windowlength-1)) {
  tmp.ind.L <- c(tmp.ind.L, paste0(vary.cov.int.used, "_", window.i))
}

final.tree.list.wt200d <- EstLtmle.CvMethod1.MltLamda(tree.data       = train.data.wt200d, 
                                                      tree.list       = seq.tree.wt200d$tree.list, 
                                                      lambda.used     = c(1:3, qchisq(0.95, 1), 4:40), # 13:26
                                                      test.data       = finalData.noMiss.t0Jul2016.switchDTG.wt200d.scaled[tree.test.ind.wt200d, ],                                             
                                                      ind.A        = which(colnames.finalData.noMiss.t0Jul2016.switchDTG.wt200d.scaled %in% paste0("DTGOn_", 0:(opt$windowlength-1))), 
                                                      ind.C        = which(colnames.finalData.noMiss.t0Jul2016.switchDTG.wt200d.scaled %in% paste0("censor_", 0:(opt$windowlength-1))),
                                                      ind.L        = which(colnames.finalData.noMiss.t0Jul2016.switchDTG.wt200d.scaled %in% tmp.ind.L),
                                                      ind.Y        = which(colnames.finalData.noMiss.t0Jul2016.switchDTG.wt200d.scaled %in% paste0("Weight_", 1:opt$windowlength)), 
                                                      survivalOutcome = F,
                                                      abar            = list(treatment = rep(1, opt$windowlength), control = rep(0, opt$windowlength)),
                                                      gbounds         = c(0.025, 0.975),
                                                      min.obsY.trt    = opt$minobsytest)

# tree.data       = train.data.wt200d 
# tree.list       = seq.tree.wt200d$tree.list 
# lambda.used     = c(1:3, qchisq(0.95, 1), 4:40) # 13:26
# test.data       = finalData.noMiss.t0Jul2016.switchDTG.wt200d.scaled[tree.test.ind.wt200d, ]                                             
# ind.A        = which(colnames.finalData.noMiss.t0Jul2016.switchDTG.wt200d.scaled %in% paste0("DTGOn_", 0:(opt$windowlength-1))) 
# ind.C        = which(colnames.finalData.noMiss.t0Jul2016.switchDTG.wt200d.scaled %in% paste0("censor_", 0:(opt$windowlength-1)))
# ind.L        = which(colnames.finalData.noMiss.t0Jul2016.switchDTG.wt200d.scaled %in% tmp.ind.L)
# ind.Y        = which(colnames.finalData.noMiss.t0Jul2016.switchDTG.wt200d.scaled %in% paste0("Weight_", 1:opt$windowlength))
# survivalOutcome = F
# abar            = list(treatment = rep(1, opt$windowlength), control = rep(0, opt$windowlength))
# gbounds         = c(0.025, 0.975)
# min.obsY.trt    = opt$minobsytest

save(seq.tree.wt200d, final.tree.list.wt200d,
     file = paste0("../../Data/Data20210512/Processed/TreeRes20220512/WtSubAnal200dAllStp2_seed", opt$seed, 
                  "_windowlength", opt$windowlength, "_maxdepth", opt$maxdepth, "_minsplit", opt$minsplit, 
                  "_minbucket", opt$minbucket, "_minobsytrain", opt$minobsytrain, "_minobsytest", opt$minobsytest, "_20220512.RData"))

