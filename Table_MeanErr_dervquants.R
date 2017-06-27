
meanerr2save <- matrix(nrow = nEMs, ncol = 3 + length(pars4table))

for(parm in 1:length(pars4table)) {
	param <- which(colnames(meanerr.table)==pars4table[parm])
	if (length(param)==0) {meanerr2save[,parm] <- NA}
	else{
		meanerr2save[,parm] <- meanerr.table[,which(colnames(meanerr.table)==pars4table[parm])]
	}
}

colnames(meanerr2save) <- c(pars4table, "B0", "B2008", "MSY")

for (em in 1:nEMs) {

	meanerr2save[em,(1+length(pars4table))] <- meanerr.func(sim.B0[em,], true.B0)
	meanerr2save[em,(2+length(pars4table))] <- meanerr.func(sim.Bfinal[em,], true.Bfinal)
	meanerr2save[em,(3+length(pars4table))] <- meanerr.func(sim.MSY[em,], true.MSY)

}

write.csv(meanerr2save, file.path(dir.code, paste0(dir.pres, "_MeanErr_ParamDerv.csv")), row.names = FALSE)

