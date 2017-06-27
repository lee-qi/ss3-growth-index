
#setwd("C:/Users/Lee Qi/Desktop/SS3/Splitnose/Testing")
#setwd("E:/SS3/Splitnose/Testing")
setwd("E:/Splitnose")

dir.check <- getwd()

dir.code <- "C:/Users/Lee Qi/Google Drive/SS3/Splitnose/Testing"
dir.plots <- file.path(dir.code, "Plots")

require(r4ss)

dir.pres <- "OM 5" #Change to whichever model you're working in
dir.rep <- "Reports"

setwd(dir.pres)

Nboot <- 100 #Set number of bootstraps
foreyears <- 50 #Set number of years we want to forecast
par2change <- c("VonBert_K_Fem_GP_1", "VonBert_K_Mal_GP_1", "L_at_Amax_Fem_GP_1", "L_at_Amax_Mal_GP_1")
addormult <- "1" #positive for multiplicative, negative for additive
if(addormult == 1) {envar <- "mult"}
if(addormult == -1) {envar <- "add"}

EMvec <- c(1,2, "5-0.8", "5-0.6", "5-0.4", "5-0.2", 5)
#EMvec <- c(1, 2, 4, 5, 3)
#EMvec <- 1:2
nEMs <- length(EMvec)

pars4table <- c(par2change, paste0(par2change, "_ENV_", envar),
                "CV_young_Fem_GP_1", "CV_young_Mal_GP_1",
                "CV_old_Fem_GP_1", "CV_old_Mal_GP_1",
                "SR_LN(R0)")

setwd(dir.rep)

#########################################
#### Comparing results
#########################################



if(file.exists(paste0("EM_",paste(EMvec, collapse = "_"),".RData"))) {load(paste0("EM_",paste(EMvec, collapse = "_"),".RData"))}
if(!file.exists(paste0("EM_",paste(EMvec, collapse = "_"),".RData"))) {source(file.path(dir.code, "ReadReports.R"))}

source(file.path(dir.code, "Plot_SSBrec.R"))

#source(file.path(dir.code, "Table_MARE_pars.R"))
# EMvals[em,param,boot]
# truevals[em,param,boot]

#source(file.path(dir.code, "Table_Decision.R"))
#
#source(file.path(dir.code, "Table_MARE_dervquants.R"))
#
#source(file.path(dir.code, "Table_MeanErr_dervquants.R"))

source(file.path(dir.code, "Plot_Growth.R"))


#################################################
#### Plotting Estimates of Link Param ###########
#################################################

if(as.numeric(addormult)>0) {envtype <- "mult"}
if(as.numeric(addormult)<0) {envtype <- "add"}
envparnames <- paste(par2change, "_ENV_", envtype, sep = "")
envpar.mat <- matrix(nrow = length(envparnames), ncol = Nboot)

dev.new()
par(mfrow = c(length(par2change),1), mar = c(4,4,2,2))
for(parm in 1:length(envparnames)) {
  trueval <- true.reps[[1]]$parameters[which(true.reps[[1]]$parameters$Label==envparnames[parm]),]$Value
  if(length(trueval)==0) {trueval <- 0}
  for(boot in 1:Nboot) {
    envpar.mat[parm,boot] <- EM2.reps[[boot]]$parameters[which(EM2.reps[[boot]]$parameters$Label==envparnames[parm]),]$Value
  }
  #hist(envpar.mat[parm,], main = paste("Estimates of ", envparnames[parm], sep = ""), xlab = "")
  hist(envpar.mat[parm,], main = "", xlab = "")
  abline(v = trueval, col = "green", lwd = 2)
  abline(v = median(envpar.mat[parm,]), col = "red", lwd = 2)
}
par(mfrow = c(1,1))

#################################################
#### Plotting Estimates of Growth Param #########
#################################################

growthpar.mat <- array(dim = c(length(par2change), Nboot, 2))
growtherr.mat <- array(dim = c(length(par2change), Nboot, 2))
dev.new()
par(mfrow = c(length(par2change),2), mar = c(4,4,2,2))
for(parm in 1:length(par2change)) {
  trueval <- true.reps[[1]]$parameters[which(true.reps[[1]]$parameters$Label==par2change[parm]),]$Value
  if(length(trueval)==0) {trueval <- 0}
  for(boot in 1:Nboot) {
    growthpar.mat[parm,boot,1] <- EM1.reps[[boot]]$parameters[which(EM1.reps[[boot]]$parameters$Label==par2change[parm]),]$Value
    growthpar.mat[parm,boot,2] <- EM2.reps[[boot]]$parameters[which(EM2.reps[[boot]]$parameters$Label==par2change[parm]),]$Value
  }
  hist(growthpar.mat[parm,,1], main = paste("EM1: ", par2change[parm], sep = ""), xlab = "")
  abline(v = trueval, col = "green", lwd = 3)
  abline(v = median(growthpar.mat[parm,,1]), col = "red", lwd = 2)

  hist(growthpar.mat[parm,,2], main = paste("EM2: ", par2change[parm], sep = ""), xlab = "")
  abline(v = trueval, col = "green", lwd = 3)
  abline(v = median(growthpar.mat[parm,,2]), col = "red", lwd = 2)
}
par(mfrow = c(1,1))

dev.new()
par(mfrow = c(length(par2change),2), mar = c(4,4,2,2))
for(parm in 1:length(par2change)) {
  trueval <- true.reps[[1]]$parameters[which(true.reps[[1]]$parameters$Label==par2change[parm]),]$Value
  if(length(trueval)==0) {trueval <- 0}
  for(boot in 1:Nboot) {
    growtherr.mat[parm,boot,1] <- (growthpar.mat[parm,boot,1] - trueval) / trueval
    growtherr.mat[parm,boot,2] <- (growthpar.mat[parm,boot,2] - trueval) / trueval
  }
  hist(growtherr.mat[parm,,1], main = paste("Rel Error EM1: ", par2change[parm], sep = ""), xlab = "", xlim = c(-.5, .5))
  abline(v = 0, col = "green", lwd = 3)
  abline(v = median(growtherr.mat[parm,,1]), col = "red", lwd = 2)

  hist(growtherr.mat[parm,,2], main = paste("Rel Error EM2: ", par2change[parm], sep = ""), xlab = "", xlim = c(-.5, .5))
  abline(v = 0, col = "green", lwd = 3)
  abline(v = median(growtherr.mat[parm,,2]), col = "red", lwd = 2)
}
par(mfrow = c(1,1))



