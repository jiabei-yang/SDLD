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

# sbatch -J SimDataRpart20210507 RunMa20201224.sh SimDataRpart20200422.R 1 100

setwd("DataAnalysis/Revision/")

load(paste0("../../../Data/Data20210512/Processed/TreeRes20230304/WtSubAnal200dAllStp1_seed45_20230304.RData"))
# large.tree.wt200d.seed45.wdlth4.mxdth10.obsytrn50 <- large.tree.wt200d

# # # Use this one
# load(paste0("../../Data/Data20210512/Processed/TreeRes20220512/WtSubAnal200dAllStp2_seed5_windowlength4_maxdepth10_minsplit2000_minbucket1000_minobsytrain50_minobsytest10_20220512.RData"))
# final.tree.wt200d.seed5.wdlth4.mxdth10.obsytrn50.obsytst10.lbdChi  <- final.tree.list.wt200d[[1]][4][[1]]

# n.iters <- 1:10^3
n.iters <- c(1:453, 551:606, 701:754, 851:905)
# fnlTr.frstSplt.ind <- matrix(0, ncol = 15, nrow = n.iter)
# fnlTr.othrSplt.ind <- matrix(0, ncol = 15, nrow = n.iter)
varnames.l0 <- colnames(subData.nomiss.wt200d.sbgrpWt.resPop.scaled)[1:15]

registerDoParallel(cores=28)

tmp.res <- 
  foreach(n.iters.i = n.iters) %dopar% {

  # iter 6 two splits on same variable
  # iter 3 no split
  if (n.iters.i %% 50 == 0) {
    cat(paste0(n.iters.i, "\n"))
  }

  fnlTr.frstSplt.ind <- rep(0, 15)  
  fnlTr.othrSplt.ind <- rep(0, 15)  

  load(paste0("../../../Data/Data20210512/Processed/TreeRes20230304/WtSubAnal200dAllStp2_seed", n.iters.i, "_20230304.RData"))
  tmp.fnlTr <- final.tree.list.wt200d[[1]][4][[1]]

  fnlTr.frstSplt.ind[which(varnames.l0 == tmp.fnlTr$frame$var[1])] <- 1
  
  tmp.othrSplt <- tmp.fnlTr$frame$var[-1]
  tmp.othrSplt <- tmp.othrSplt[tmp.othrSplt != "<leaf>"]
  tmp.lth.othrSplt <- length(tmp.othrSplt)

  if (tmp.lth.othrSplt > 0) {
    for (othrSplt.i in 1:tmp.lth.othrSplt) {
      tmp.ind.othrSplt <- which(varnames.l0 == tmp.othrSplt[othrSplt.i])
      fnlTr.othrSplt.ind[tmp.ind.othrSplt] <- fnlTr.othrSplt.ind[tmp.ind.othrSplt] + 1

    } # for othrSplt.i
  } # if

  tmp.size.tree <- sum(tmp.fnlTr$frame$var == "<leaf>")

  list(fnlTr.frstSplt.ind = fnlTr.frstSplt.ind, 
       fnlTr.othrSplt.ind = fnlTr.othrSplt.ind, 
       tmp.size.tree      = tmp.size.tree)

} # for n.iter.i

df.fnlTr.frstSplt.ind <- 
  foreach(n.iters.i = 1:length(n.iters),
          .combine = rbind) %dopar% {

    tmp.res[[n.iters.i]]$fnlTr.frstSplt.ind

  }

df.fnlTr.othrSplt.ind <- 
  foreach(n.iters.i = 1:length(n.iters),
          .combine = rbind) %dopar% {

    tmp.res[[n.iters.i]]$fnlTr.othrSplt.ind

  }

size.fnlTr <- 
  foreach(n.iters.i = 1:length(n.iters),
          .combine = c) %dopar% {

    tmp.res[[n.iters.i]]$tmp.size.tree

  }

colnames(df.fnlTr.frstSplt.ind) <- varnames.l0
colnames(df.fnlTr.othrSplt.ind) <- varnames.l0

dim(df.fnlTr.othrSplt.ind)

apply(df.fnlTr.frstSplt.ind, 2, sum)
apply(df.fnlTr.othrSplt.ind, 2, sum)

apply(df.fnlTr.frstSplt.ind, 2, sum) + apply(df.fnlTr.othrSplt.ind, 2, sum)

which.max(apply(df.fnlTr.frstSplt.ind, 2, sum))
which.max(apply(df.fnlTr.othrSplt.ind, 2, sum))
which.max(apply(df.fnlTr.frstSplt.ind, 2, sum) + apply(df.fnlTr.othrSplt.ind, 2, sum))

size.fnlTr[1:20]
table(size.fnlTr)
mean(size.fnlTr)

# trees first split on male
ind.tree.size2 <- which(size.fnlTr == 2)
apply(df.fnlTr.frstSplt.ind[ind.tree.size2, ], 2, sum)
apply(df.fnlTr.othrSplt.ind[ind.tree.size2, ], 2, sum)
ind.tree.size2.male <- which((size.fnlTr == 2) & (df.fnlTr.frstSplt.ind[, "male"] == 1))
length(ind.tree.size2.male)

ind.tree.size3 <- which(size.fnlTr == 3)
apply(df.fnlTr.frstSplt.ind[ind.tree.size3, ], 2, sum)
apply(df.fnlTr.othrSplt.ind[ind.tree.size3, ], 2, sum)

# trees first split on male, then on time0_dob among females
ind.tree.size3.genderDob <- which((size.fnlTr == 3) & (df.fnlTr.frstSplt.ind[, "male"] == 1) & (df.fnlTr.othrSplt.ind[, "time0_dob"] == 1))
lth.ind.tree.size3.genderDob <- length(ind.tree.size3.genderDob)

ind.tree.size3.genderMaleDob <- NULL
ind.tree.size3.genderFemaleDob <- NULL

whrSplt.genderMaleDob <- NULL
whrSplt.genderFemaleDob <- NULL

# see if any size 3 tree make a second split among males
for (n.iters.i in ind.tree.size3.genderDob) {
  
  load(paste0("../../../Data/Data20210512/Processed/TreeRes202304/WtSubAnal200dAllStp2_seed", n.iters.i, "_20230304.RData"))
  tmp.fnlTr <- final.tree.list.wt200d[[1]][4][[1]]

  # need to take care of male to the left or right
  if ((("4" %in% rownames(tmp.fnlTr$frame)) & (tmp.fnlTr$splits[1, "ncat"] == 1)) |
        (("6" %in% rownames(tmp.fnlTr$frame)) & (tmp.fnlTr$splits[1, "ncat"] == -1))) {
    ind.tree.size3.genderMaleDob <- c(ind.tree.size3.genderMaleDob, n.iters.i)
    whrSplt.genderMaleDob        <- c(whrSplt.genderMaleDob, tmp.fnlTr$splits[2, "index"])
  } else {
    ind.tree.size3.genderFemaleDob <- c(ind.tree.size3.genderFemaleDob, n.iters.i)
    whrSplt.genderFemaleDob        <- c(whrSplt.genderFemaleDob, tmp.fnlTr$splits[2, "index"])
  }
}

save(df.fnlTr.frstSplt.ind, df.fnlTr.othrSplt.ind, size.fnlTr, 
     ind.tree.size2, ind.tree.size3, ind.tree.size3.genderDob,
     ind.tree.size3.genderMaleDob, ind.tree.size3.genderFemaleDob,
     whrSplt.genderMaleDob, whrSplt.genderFemaleDob,
     file = "../../../Data/Data20210512/Processed/WtSubAnal200dAllStp3_20230304.RData")
# load("WtSubAnal200dAllStp3_20220512.RData")

res.splits <- data.frame(covariate = varnames.l0, 
                         first = as.integer(apply(df.fnlTr.frstSplt.ind, 2, sum)),
                         other = as.integer(apply(df.fnlTr.othrSplt.ind, 2, sum)),
                         total = as.integer(apply(df.fnlTr.frstSplt.ind, 2, sum) + apply(df.fnlTr.othrSplt.ind, 2, sum)))

library(xtable)
print(xtable(res.splits), include.rownames = FALSE, digits = 0)


n.iters.i <- ind.tree.size3.genderFemaleDob[2]
load(paste0("../../Data/Data20210512/Processed/TreeRes20220512/WtSubAnal200dAllStp2_seed", n.iters.i, "_windowlength4_maxdepth10_minsplit2000_minbucket1000_minobsytrain50_minobsytest10_20220512.RData"))
tmp.fnlTr <- final.tree.list.wt200d[[1]][4][[1]]
tmp.fnlTr


