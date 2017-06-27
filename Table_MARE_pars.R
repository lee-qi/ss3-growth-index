#----------------------------
# Finding MARE for parameters
# Won't include betas if not present in OM
#----------------------------

MARE.func <- function(estvec,truevec) {
  temp <- abs((estvec - truevec) / truevec)
  return(round(median(temp), digits = 2))
}

meanerr.func <- function(estvec,truevec) {
  temp <- (estvec - truevec) / truevec
  return(round(median(temp), digits = 2))
}

Npars <- nrow(true.reps[[1]]$parameters)
em.Npars <- nrow(EM.reps[[2]][[1]]$parameters)

MARE.table <- array(dim = c(nEMs, Npars))
meanerr.table <- array(dim = c(nEMs, Npars))
par.names <- NULL

truevals <- matrix(nrow = nEMs, ncol = Npars)
EMvals <- array(dim = c(nEMs, Npars, Nboot))

if(em.Npars != Npars) {
  MARE.table <- array(dim = c(nEMs, em.Npars))
  meanerr.table <- array(dim = c(nEMs, em.Npars))
  par.names <- NULL
  
  truevals <- matrix(nrow = nEMs, ncol = em.Npars)
  EMvals <- array(dim = c(nEMs, em.Npars, Nboot))
}


for(em in 1:nEMs) {
  for (j in 1:Npars) {

    #----------------------------------------
    # Making sure pars in EM match pars in OM
    #----------------------------------------
    param <- subset(true.reps[[1]]$parameters, Num == j)$Label
    param.sim <- subset(EM.reps[[em]][[1]]$parameters, Label == param)$Label

    if(length(param)==0 | length(param.sim)==0) {next}
    param.sim.phase <- subset(EM.reps[[em]][[1]]$parameters, Label == param)$Phase
    if(param.sim.phase <= 0 | is.na(param.sim.phase)) {next}

    for (i in 1:Nboot) {
      truevals[em,j] <- subset(true.reps[[i]]$parameters, Label == param)$Value
      EMvals[em,j,i] <- subset(EM.reps[[em]][[i]]$parameters, Label == param)$Value
    }

    MARE.table[em,j] <- MARE.func(EMvals[em,j,], truevals[em,j])
    meanerr.table[em,j] <- meanerr.func(EMvals[em,j,], truevals[em,j])
    par.names[j] <- param
  }

  if(em.Npars != Npars) {
  	if(as.numeric(addormult)>0) {envtype <- "mult"}
    if(as.numeric(addormult)<0) {envtype <- "add"}
    envparnames <- paste(par2change, "_ENV_", envtype, sep = "")
  	for(gpar in 1:length(par2change)) {
  		j <- j+1
  		param <- envparnames[gpar]
  		param.sim <- subset(EM.reps[[em]][[1]]$parameters, Label == param)$Label

        if(length(param.sim)==0) {next}
        param.sim.phase <- subset(EM.reps[[em]][[1]]$parameters, Label == param)$Phase
        if(param.sim.phase <= 0 | is.na(param.sim.phase)) {next}
    
        for (i in 1:Nboot) {
          truevals[em,j] <- 0
          EMvals[em,j,i] <- subset(EM.reps[[em]][[i]]$parameters, Label == param)$Value
        }
    
        MARE.table[em,j] <- round(median(abs(EMvals[em,j,])), digits = 2)
        meanerr.table[em,j] <- round(median(EMvals[em,j,], truevals[em,j]), digits = 2)
  	    }
  }
}

if(em.Npars != Npars) {
	par.names <- c(par.names, envparnames)
}
MARE.table <- MARE.table[,unlist(lapply(data.frame(MARE.table), function(x) !all(is.na(x))))]
colnames(MARE.table) <- par.names[!is.na(par.names)]
write.csv(MARE.table, file.path(dir.code, paste0(dir.pres, "_MARE_Params.csv")), row.names = FALSE)

meanerr.table <- meanerr.table[,unlist(lapply(data.frame(meanerr.table), function(x) !all(is.na(x))))]
colnames(meanerr.table) <- par.names[!is.na(par.names)]
write.csv(meanerr.table, file.path(dir.code, paste0(dir.pres, "_MeanErr_Params.csv")), row.names = FALSE)

#  for (i in 1:NROW(MARE.table)) {
#    if (i%%9 == 1) {
#      png(paste0("EM-","Graph-", (i+8)/9, ".png"),600,900)
#      par(mfrow = c(3,3))
#      hist(MARE.table[i,], main = par.names[i],xlim = c(-0.5,0.5))
#      abline(v = 0, col = "green", lwd = 2)
#      abline(v = mean(MARE.table[i,]), col = "red", lwd = 2, lty = 2)
#    }
#    else {
#      hist(MARE.table[i,], main = par.names[i],xlim = c(-0.5,0.5))
#      abline(v = 0, col = "green", lwd = 2)
#      abline(v = mean(MARE.table[i,]), col = "red", lwd = 2, lty = 2)
#    }
#    if (i%%9 == 0 | i == NROW(MARE.table)) {
#      par(mfrow = c(1,1))
#    }
#  }
#}

