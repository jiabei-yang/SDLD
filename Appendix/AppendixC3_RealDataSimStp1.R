#!/usr/bin/env Rscript
setwd("../")
folder <- paste(getwd(), "/Functions/", sep="")
functions <- list.files(folder)
functions <- paste(folder, functions, sep = "")
for (i in functions){
  source(i)
}

# sbatch -J SimDataRpart20210507 RunMa20201224.sh SimDataRpart20200422.R 1 100

setwd("Simulation/")
load("seed1000.rda")

# registerDoParallel(cores=2)
registerDoParallel(cores=Sys.getenv("SLURM_CPUS_PER_TASK"))

# Read in the arguments from command line
option_list <- list(make_option(c("-s", "--seed"), type = "double"))
# make_option(c("-l", "--windowlength"), type = "double"),
# make_option(c("-d", "--maxdepth"), type = "double"),
# make_option(c("-p", "--minsplit"), type = "double"),
# make_option(c("-b", "--minbucket"), type = "double"),
# make_option(c("-m", "--minobsytrain"), type = "double"))
# p
opt_parser  <- OptionParser(option_list=option_list)
opt         <- parse_args(opt_parser)

# 50 / (517 / 84445) = 8167
# 20 / (517 / 84445) = 3267
# opt = list(seed = 333, windowlength = 4, maxdepth = 10, minsplit = 2000, minbucket = 1000, minobsytrain = 50)
# opt = list(seed = 333)
print(opt)

all.data.realData <- simData.realData(n.tree = 3*10^4, 
                                      n.test = 3*10^3 / 4, 
                                      seed   = a[opt$seed])

colnames.data.realData <- colnames(all.data.realData$tree.data)
data.bl.wiz.rownumb <- data.frame(rownumb = 1:nrow(all.data.realData$tree.data),
                                  all.data.realData$tree.data[, 1:15])

ulist.used <- list(eval = etemp.ltmle, split = stemp.ltmle, init = itemp)
parms.used <- list(whole.data   = all.data.realData$tree.data, 
                   ind.A        = grep("A_", colnames.data.realData), 
                   ind.C        = grep("C_", colnames.data.realData),
                   ind.L        = 19:26,
                   ind.Y        = grep("Y_", colnames.data.realData), 
                   survivalOutcome = NULL,
                   abar            = list(treatment = c(1, 1), control = c(0, 0)),
                   gbounds         = c(0.025, 0.975),
                   max.lvls        = 15,
                   min.obsY.trt    = 50,
                   num.truc.obs    = 100,
                   num.goodness.iter = 200,
                   num.optimal       = 4,
                   parallel          = TRUE) 

large.tree <- rpart(rownumb ~ W_1_0 + W_2_0 + W_3_0 + W_4_0 + W_5_0 +
                      W_6_0 + W_7_0 + W_8_0 + W_9_0 + W_10_0 +
                      W_11_0 + W_12_0 + W_13_0 + W_14_0 + W_15_0, 
                           data     = data.bl.wiz.rownumb,
                           method   = ulist.used,
                           parms    = parms.used,
                           control  = rpart.control(cp = 0, minsplit = 2000, minbucket = 1000, maxsurrogate = 0, maxcompete = 0, maxdepth = 10))

save(large.tree, 
     all.data.realData,
     file = paste0("../../Data/Simulation/RealDataSim20230517/RealDataSim_seed", opt$seed, 
                   "_20230517.RData"))
