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

registerDoParallel(cores=(Sys.getenv("SLURM_NTASKS_PER_NODE")))
# registerDoParallel(cores=2)

# Read in the arguments from command line
option_list = list(
  make_option(c("-s", "--start"), type="integer"),
  make_option(c("-e", "--end"), type = "integer"))

opt_parser <- OptionParser(option_list=option_list)
opt <- parse_args(opt_parser)
start <- opt$start
end   <- opt$end

load("seed1000.rda")

performance.trtDep.csrDep.yCont <- 
  foreach(i = start:end) %dopar% {
    
    if ((i %% 28) == 0) {
      print(i)
    }
    
    all.data.trtDep.csrDep.yCont <- simData.trtDep.csrDep.yCont(n.tree = 10^4, 
                                                                n.test = 2*10^3, 
                                                                seed   = a[i])
    
    colnames.data.trtDep.csrDep.yCont <- colnames(all.data.trtDep.csrDep.yCont$tree.data)
    data.bl.wiz.rownumb <- data.frame(rownumb = 1:nrow(all.data.trtDep.csrDep.yCont$tree.data),
                                      all.data.trtDep.csrDep.yCont$tree.data[, grep("W_[1-5]_0", colnames.data.trtDep.csrDep.yCont)])
    
    ulist.used <- list(eval = etemp.ltmle, split = stemp.ltmle, init = itemp)
    parms.used <- list(whole.data   = all.data.trtDep.csrDep.yCont$tree.data, 
                       ind.A        = grep("A_", colnames.data.trtDep.csrDep.yCont), 
                       ind.C        = grep("C_", colnames.data.trtDep.csrDep.yCont),
                       ind.L        = grep("W_[4-5]_1", colnames.data.trtDep.csrDep.yCont),
                       ind.Y        = grep("Y_", colnames.data.trtDep.csrDep.yCont), 
                       survivalOutcome = NULL,
                       abar            = list(treatment = c(1, 1), control = c(0, 0)),
                       gbounds         = c(0.025, 0.975),
                       max.lvls        = 15,
                       min.obsY.trt    = 50,
                       num.truc.obs    = 100,
                       num.goodness.iter = 200,
                       num.optimal       = 4,
                       parallel          = F) 
    
    # head(all.data.trtDep.csrDep.yCont$tree.data)
    ########### NEED TO DO UNIQUE 
    
    # x     <- sort(data.bl.wiz.rownumb$W_2_0)
    # y     <- order(data.bl.wiz.rownumb$W_2_0)
    # wt    <- rep(1, length(y))
    # parms <- parms.used
    
    large.tree <- rpart(rownumb ~ W_1_0 + W_2_0 + W_3_0 + W_4_0 + W_5_0, 
                        data     = data.bl.wiz.rownumb,
                        method   = ulist.used,
                        parms    = parms.used,
                        control  = rpart.control(cp = 0, minsplit = 500, maxsurrogate = 0, maxcompete = 0, maxdepth = 10))
    # large.tree
    # warnings() 
    # save(large.tree, file = "Tree20210911.RData")
    # load("Tree20210911.RData")

    # large.tree <- large.tree
    
    seq.tree <- create.sequence(large.tree = large.tree)
    final.tree.list <- EstLtmle.CvMethod1.MltLamda(tree.data       = all.data.trtDep.csrDep.yCont$tree.data, 
                                                   tree.list       = seq.tree$tree.list, 
                                                   lambda.used     = c(1:3, qchisq(0.95, 1), 4:40), # 13:26
                                                   test.data       = all.data.trtDep.csrDep.yCont$test.data, 
                                                   ind.A           = grep("A_", colnames.data.trtDep.csrDep.yCont), 
                                                   ind.C           = grep("C_", colnames.data.trtDep.csrDep.yCont),
                                                   ind.L           = grep("W_[4-5]_1", colnames.data.trtDep.csrDep.yCont),
                                                   ind.Y           = grep("Y_", colnames.data.trtDep.csrDep.yCont), 
                                                   survivalOutcome = F,
                                                   abar            = list(treatment = c(1, 1), control = c(0, 0)),
                                                   gbounds         = c(0.025, 0.975),
                                                   min.obsY.trt    = 10)
    
    # load("IntermediateData.RData")

    performance.list <- list()
    for (tree.i in 1:length(final.tree.list[[1]])) {
      
      performance.list <- c(performance.list,
                            list(eval.measures.eff(final.tree       = final.tree.list[[1]][[tree.i]], 
                                                   test.data        = all.data.trtDep.csrDep.yCont$test.data,
                                                   latent.test.data = all.data.trtDep.csrDep.yCont$latent.test.data, 
                                                   noise.var        = all.data.trtDep.csrDep.yCont$noise.var, 
                                                   corr.split       = all.data.trtDep.csrDep.yCont$corr.split, 
                                                   where.split      = all.data.trtDep.csrDep.yCont$where.split, 
                                                   dir.split        = all.data.trtDep.csrDep.yCont$dir.split)))
      
      # final.tree       = large.tree
      # test.data        = all.data.trtDep.csrDep.yCont$test.data
      # latent.test.data = all.data.trtDep.csrDep.yCont$latent.test.data 
      # noise.var        = all.data.trtDep.csrDep.yCont$noise.var 
      # corr.split       = all.data.trtDep.csrDep.yCont$corr.split 
      # where.split      = all.data.trtDep.csrDep.yCont$where.split 
      # dir.split        = all.data.trtDep.csrDep.yCont$dir.split
      
    }
    
    performance.list$complex.test   <- final.tree.list[[2]]
    performance.list$corr.frst.splt <- eval.corr.frst.splts(large.tree = large.tree, 
                                                            corr.split = all.data.trtDep.csrDep.yCont$corr.split)
    
    performance.list
  }

save(performance.trtDep.csrDep.yCont, 
     file = paste0("../../Data/Simulation/FindLambdaTrtDepCsrDepYCont20220714_", start, "_", end, ".RData"))
