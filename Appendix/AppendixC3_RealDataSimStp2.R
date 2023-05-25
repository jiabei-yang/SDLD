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

# RunRscriptNameSeedParallel.sh
# sbatch -J xxxx --nodelist=compute002 --array=1-32 .sh .R 
# sbatch -J WtSubAnal200dAllStp2_20230304 --nodelist=compute009 --array=101-150 RunRscriptNameSeedParallel.sh WtSubAnal200dAllStp2_20230304.R

setwd("Simulation/")

############################################################
# Read in the arguments from command line for read in data # 
############################################################
load("../../Data/Simulation/RealDataSim20230517/RealDataSim_seed1_20230517.RData")

#####################################
# Creat sequence of candidate trees # 
#####################################
seq.tree <- create.sequence(large.tree = large.tree)

colnames.data.realData <- colnames(all.data.realData$tree.data)
final.tree.list <- EstLtmle.CvMethod1.MltLamda(tree.data       = all.data.realData$tree.data, 
                                               tree.list       = seq.tree$tree.list, 
                                               lambda.used     = c(1:3, qchisq(0.95, 1), 4:40), # 13:26
                                               test.data       = all.data.realData$test.data, 
                                               ind.A           = grep("A_", colnames.data.realData), 
                                               ind.C           = grep("C_", colnames.data.realData),
                                               ind.L           = 19:26,
                                               ind.Y           = grep("Y_", colnames.data.realData), 
                                               survivalOutcome = F,
                                               abar            = list(treatment = c(1, 1), control = c(0, 0)),
                                               gbounds         = c(0.025, 0.975),
                                               min.obsY.trt    = 10)

final.tree.list[[1]][4][[1]]
# obtained the correct tree