
true.MSY <- NULL
sim.MSY <- matrix(nrow = nEMs, ncol = Nboot)

RunNos <- 0
for(boot in runvec) {
	RunNos <- RunNos + 1

	MSY.true.rep <- readLines(file.path(dir.check,dir.pres,dir.new,dir.temp, "Report.sso"))
    true.MSY[RunNos] <- as.numeric(unlist(strsplit(MSY.true.rep[grep("TotYield_MSY",MSY.true.rep)], split = " ")))[3]

	for(em in 1:nEMs) {
		dir.new <- paste0("Bootstrap-", boot)
    	dir.temp <- "Temp"
    	dir.bias <- paste0("Bias_Bootstrap-", boot)
      	dir.EM <- paste0("EM-",EMvec[em])

        if(em == 1) {
          sim.MSY[em,RunNos] <- subset(EM.reps[[em]][[RunNos]]$derived_quants, LABEL == "TotYield_MSY")$Value
        }
        else {
          MSY.sim.rep <- readLines(file.path(dir.check, dir.pres, dir.new, dir.EM,dir.bias,dir.temp, "Report.sso"))
          sim.MSY[em,RunNos] <- as.numeric(unlist(strsplit(MSY.sim.rep[grep("TotYield_MSY",MSY.sim.rep)], split = " ")))[3]
        }
  	}
}

sim.MSY <- sim.MSY[,1:RunNos]

MARE2save <- matrix(nrow = nEMs, ncol = 3 + length(pars4table))

for(parm in 1:length(pars4table)) {
	param <- which(colnames(MARE.table)==pars4table[parm])
	if (length(param)==0) {MARE2save[,parm] <- NA}
	else{
		MARE2save[,parm] <- MARE.table[,which(colnames(MARE.table)==pars4table[parm])]
	}
}

colnames(MARE2save) <- c(pars4table, "B0", "B2008", "MSY")

for (em in 1:nEMs) {

	MARE2save[em,(1+length(pars4table))] <- MARE.func(sim.B0[em,], true.B0)
	MARE2save[em,(2+length(pars4table))] <- MARE.func(sim.Bfinal[em,], true.Bfinal)
	MARE2save[em,(3+length(pars4table))] <- MARE.func(sim.MSY[em,], true.MSY)

}

write.csv(MARE2save, file.path(dir.code, paste0(dir.pres, "_MARE_ParamDerv.csv")), row.names = FALSE)

#dev.new()
#par(mfrow = c(3+length(par2change),3), mar = c(2,2,2,2))
#hist(true.MSY, main = "True MSY")
#abline(v = median(true.MSY), col = "green", lwd = 2)
#
#hist(sim1.MSY, main = "EM-1 MSY")
#abline(v = median(true.MSY), col = "green", lwd = 2)
#abline(v = median(sim1.MSY), col = "red", lwd = 1)
#
#hist(sim2.MSY, main = "EM-2 MSY")
#abline(v = median(true.MSY), col = "green", lwd = 2)
#abline(v = median(sim2.MSY), col = "red", lwd = 1)
#
#hist(true.initSSB, main = "True B0")
#abline(v = median(true.initSSB), col = "green", lwd = 2)
#
#hist(sim1.initSSB, main = "EM-1 B0")
#abline(v = median(true.initSSB), col = "green", lwd = 2)
#abline(v = median(sim1.initSSB), col = "red", lwd = 1)
#
#hist(sim2.initSSB, main = "EM-2 B0")
#abline(v = median(true.initSSB), col = "green", lwd = 2)
#abline(v = median(sim2.initSSB), col = "red", lwd = 1)
#
#hist(true.finSSB, main = "True B2008")
#abline(v = median(true.finSSB), col = "green", lwd = 2)
#
#hist(sim1.finSSB, main = "EM-1 B2008")
#abline(v = median(true.finSSB), col = "green", lwd = 2)
#abline(v = median(sim1.finSSB), col = "red", lwd = 1)
#
#hist(sim2.finSSB, main = "EM-2 B2008")
#abline(v = median(true.finSSB), col = "green", lwd = 2)
#abline(v = median(sim2.finSSB), col = "red", lwd = 1)
#
#trueval <- NULL
#for(parm in 1:length(par2change)) {
#
#  if(length(trueval)==0) {trueval <- 0}
#  for(boot in 1:Nboot) {
#    trueval[boot] <- true.reps[[boot]]$parameters[which(true.reps[[1]]$parameters$Label==par2change[parm]),]$Value
#    growthpar.mat[parm,boot,1] <- EM1.reps[[boot]]$parameters[which(EM1.reps[[boot]]$parameters$Label==par2change[parm]),]$Value
#    growthpar.mat[parm,boot,2] <- EM2.reps[[boot]]$parameters[which(EM2.reps[[boot]]$parameters$Label==par2change[parm]),]$Value
#  }
#  hist(rep(trueval,100), main = par2change[parm])
#  abline(v = trueval, col = "green", lwd = 2)
#
#  hist(growthpar.mat[parm,,1], main = par2change[parm], xlab = "")
#  abline(v = trueval, col = "green", lwd = 2)
#  abline(v = median(growthpar.mat[parm,,1]), col = "red", lwd = 1)
#
#  hist(growthpar.mat[parm,,2], main = par2change[parm], xlab = "")
#  abline(v = trueval, col = "green", lwd = 2)
#  abline(v = median(growthpar.mat[parm,,2]), col = "red", lwd = 1)
#}#