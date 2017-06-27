#recdevs made of zeros
dir.check <- getwd()
if (!grepl(basename(dir.check), "splitnose", ignore.case = TRUE)) {
  stop(paste("Change your working directory!"))
}

require(r4ss)
require(foreach)
require(doParallel)

registerDoParallel(4)
dir.base <- "Master"
dir.pres <- "Small Gen Recdevs" #Change to whichever model you're working in
dir.rep <- "Reports"

setwd(dir.pres)
cr.reps <- dir.create(dir.rep, showWarnings = FALSE)

Nboot <- 100 #Set number of bootstraps


#########################################
#### Creating the two different ctl files
#########################################
ctl.1 <- "control-1.ss"

orig.ctl <- readLines("control.ss")

writeLines(orig.ctl, ctl.1)

#########################################
#### Modifying starter file
#########################################
 
start.file <- SS_readstarter("starter.ss_new")
start.file$init_values_src <- 1 #Use par file instead of inits in ctl
start.file$N_bootstraps <- Nboot + 2 #Set Bootstrap number
start.file$last_estimation_phase <- 0
 
SS_writestarter(start.file, file = "starter.ss_new", overwrite = TRUE)

#########################################
#########################################
#### Creating true report files for each
#### run
#########################################
#########################################

dir.new <- "Bootstrap"
 
cr.dir <- dir.create(dir.new, showWarnings = FALSE)
  
file.par <- "ss3.par"
file.start.new <- "starter.ss_new"
file.dat <- "data.ss"
file.ctl <- "control.ss"
 file.fore <- "forecast.ss"

ignore <- mapply(file.copy, to = file.path(dir.new),
                 MoreArgs = list(from = c(
                   file.par, file.start.new, file.dat, file.ctl, file.fore,"SS3.exe"), overwrite = TRUE))
  
setwd(dir.new)

file.dat.new <- "data.ss_new"
file.start <- "starter.ss"

file.rename(from = file.start.new, to = file.start)

#########################################
#### Run Bootstrap simulator to generate
#### files of the "truth"
#########################################
  
system("SS3", show.output.on.console = FALSE)
  
#########################################
#### Creating new dat file
#########################################
  
dat.lines <- readLines(file.dat.new)
end.lines <- which(dat.lines == 999)
check.boot <- length(end.lines) - 2
  
if(check.boot<=0) {
  stop(paste("Not bootstrap simulation, check starter file."))
}
if(check.boot == 1) {
  new.dat <- dat.lines[(end.lines[2] + 1):end.lines[3]]
  writeLines(new.dat, paste("boot-dat",1,".ss",sep=""))
}
if(check.boot > 1) {
  for (ii in 1:Nboot) {
    new.dat <- c(dat.lines[1:3],
                 dat.lines[(end.lines[ii+1] + 1):end.lines[ii+2]])
    boot.name <- paste("boot-dat",ii,".ss",sep="")
    writeLines(new.dat, boot.name)
  }
}

#########################################
#### Creating new directories for runs
#########################################
foreach(i=1:Nboot) %dopar% {
  require(r4ss)
  setwd(file.path(dir.check,dir.pres,dir.new))
  dir.res <- paste("Results",i, sep = "-")
  cr.res <- dir.create(dir.res, showWarnings = FALSE)
    
  dat.name <- paste("boot-dat",i,".ss",sep="")
  ctl.name <- "control-1.ss"
                    
  move.dat <- file.copy(to = dir.res, from = dat.name, overwrite = TRUE)
    
  setwd(dir.res)
  done.orig <- mapply(file.copy, to = getwd(),
                    MoreArgs = list(from = file.path(dir.check,dir.pres,c(
                      file.start, ctl.name, file.par,
                      "forecast.ss", "SS3.exe"))), overwrite = TRUE)
    
  rename.done <- mapply(file.rename, from = c(dat.name, ctl.name),
                               to = c(file.dat,file.ctl))
  
  start.file <- SS_readstarter("starter.ss")
  start.file$init_values_src <-0
  start.file$last_estimation_phase <- 25
  SS_writestarter(start.file, file = "starter.ss", overwrite = TRUE)
  system("SS3.exe", show.output.on.console = FALSE)

#  rep.file <- SS_output(warn=FALSE,checkcor=TRUE,NoCompOK=TRUE,
#                        verbose=FALSE,printstats=FALSE,
#                        covar=TRUE, readwt=FALSE, forecast=FALSE,
#                         dir = file.path(dir.check,dir.pres,dir.new,dir.res))

#  dir.bias <- paste("Bias_Bootstrap-", i, sep = "")
#  dir.create(dir.bias)
#  SS_fitbiasramp(rep.file, plot = FALSE,
#                  oldctl = file.path(dir.check,dir.pres,dir.new, "control.ss"),
#                  newctl = file.path(dir.check, dir.pres,dir.new,dir.res,dir.bias,"control.ss"))
#  mapply(file.copy, from = file.path(dir.check,dir.pres,dir.new,c("data.ss", "SS3.exe", "forecast.ss", "starter.ss")),
#                    to = file.path(dir.check, dir.pres,dir.new,dir.res,dir.bias), overwrite = TRUE)

#  setwd(dir.bias)
#  start.file <- SS_readstarter("starter.ss")
#  start.file$init_values_src <-0
#  start.file$last_estimation_phase <- 25
#  SS_writestarter(start.file, file = "starter.ss", overwrite = TRUE)

#  system("SS3.exe", show.output.on.console = FALSE)

  rep.name <- paste("Boot", i, "-Ctl", 1,".sso", sep = "")
  file.rename(from = "Report.sso", to = rep.name)
  file.copy(from = rep.name, to = file.path(dir.check,dir.pres,dir.rep), overwrite = TRUE)
}

#########################################
#### Comparing results
#########################################
setwd(dir.rep)

reps.ctl1 <- NULL
true.rep <- SS_output(warn=FALSE,checkcor=FALSE,NoCompOK=TRUE,
                              verbose=FALSE,printstats=FALSE,
                              covar=FALSE, readwt=FALSE, forecast=FALSE,
                              dir = file.path(dir.check, dir.pres, "Bootstrap"))

for(i in 1:Nboot) {
  reps.ctl1[[i]] <- SS_output(repfile=paste("Boot", i, "-Ctl", 1,".sso", sep = ""), 
                              warn=FALSE,checkcor=FALSE,NoCompOK=TRUE,
                              verbose=FALSE,printstats=FALSE,
                              covar=FALSE, readwt=FALSE, forecast=FALSE,
                              dir = file.path(dir.check, dir.pres, dir.rep))
}

std.errs <- matrix(nrow=206, ncol = Nboot)
par.names <- vector(length = 1)
for (j in 1:206) {
  sim.values <- rep(NA,length = Nboot)
  std.err.1 <- rep(NA,length = Nboot)
  param <- subset(reps.ctl1[[i]]$parameters, Num == j)$Label
  true.values <- subset(true.rep$parameters, Num ==j)$Value
  for (i in 1:Nboot) {
    sim.values[i] <- subset(reps.ctl1[[i]]$parameters, Num == j)$Value
    std.err.1[i] <- (sim.values[i] - true.values) / true.values
   #std.err.2[i] <- (reps.ctl2[[i]]$SBzero - true.rep$SBzero) / true.rep$SBzero
  }
  if (sum(std.err.1) == 0 | sum(std.err.1) == "NaN") {
    print(paste(param, "no difference.", sep = " "))
  }
  else {
    std.errs[j,] <- std.err.1
    par.names <- c(par.names, param)
  }
}
std.errs <- std.errs[complete.cases(std.errs),]
par.names <- par.names[-1]

for (i in 1:NROW(std.errs)) {
  if (i%%9 == 1) {
    png(paste("Graph-", (i+8)/9 , ".png", sep = ""),600,900)
    par(mfrow = c(3,3))
    hist(std.errs[i,], main = par.names[i],xlim = c(-0.5,0.5))
    abline(v = 0, col = "green", lwd = 2)
    abline(v = mean(std.errs[i,]), col = "red", lwd = 2, lty = 2)
  }
  else {
    hist(std.errs[i,], main = par.names[i],xlim = c(-0.5,0.5))
    abline(v = 0, col = "green", lwd = 2)
    abline(v = mean(std.errs[i,]), col = "red", lwd = 2, lty = 2)
  }
  if (i%%9 == 0 | i == NROW(std.errs)) {
    par(mfrow = c(1,1))
    dev.off()
  }
}

sel.params <- c(107,109,115,128)

#################################################
#### Plotting std errors for SSB ################
#################################################

true.SSB <- true.rep$recruit$spawn_bio[which(true.rep$recruit$spawn_bio!=0)]
SPB.err <- matrix(nrow=Nboot, ncol = length(true.SSB))
med.SSB <- vector(length=length(true.SSB))
CI50.SSB <- vector(length=(length(true.SSB))*2)
CI90.SSB <- vector(length=(length(true.SSB))*2)

for (i in 1:Nboot) {
  sim.values <- reps.ctl1[[i]]$recruit$spawn_bio[1:length(true.SSB)]
  SPB.err[i,] <- (sim.values-true.SSB) / true.SSB
}
for (j in 1:length(true.SSB)) {
  med.SSB[j] <- median(SPB.err[,j])
  CI50.SSB[j] <- quantile(SPB.err[,j],probs=0.25)
  CI50.SSB[(length(true.SSB)*2+1)-j] <- quantile(SPB.err[,j],probs=0.75)
  CI90.SSB[j] <- quantile(SPB.err[,j],probs=0.05)
  CI90.SSB[(length(true.SSB)*2+1)-j] <- quantile(SPB.err[,j],probs=0.95)
}

StartYr <- min(true.rep$recruit$year)
EndYr <- max(true.rep$recruit$year)
plot(0, xlim=c(StartYr, EndYr), ylim=c(-0.5,0.5),xaxs="i",ylab="Error")
polygon(y=CI90.SSB,x=c(StartYr:EndYr,EndYr:StartYr), col = "gray25")
polygon(y=CI50.SSB,x=c(StartYr:EndYr,EndYr:StartYr),col="gray50")
lines(y=med.SSB, x = c(StartYr:EndYr), type = "l", col="white", lwd=2)
abline(h=0, col="red")
mtext("Standardised Errors for SSB", side = 3)

#################################################
#### Plotting std errors for Recruitment ########
#################################################

true.Rec <- true.rep$recruit$exp_recr[which(true.rep$recruit$exp_recr!=0)]
Rec.err <- matrix(nrow=Nboot, ncol = length(true.Rec))
med.Rec <- vector(length=length(true.Rec))
CI50.Rec <- vector(length=(length(true.Rec))*2)
CI90.Rec <- vector(length=(length(true.Rec))*2)

for (i in 1:Nboot) {
  sim.values <- reps.ctl1[[i]]$recruit$exp_recr[1:length(true.Rec)]
  Rec.err[i,] <- (sim.values-true.Rec) / true.Rec
}
for (j in 1:length(true.Rec)) {
  med.Rec[j] <- median(Rec.err[,j])
  CI50.Rec[j] <- quantile(Rec.err[,j],probs=0.25)
  CI50.Rec[(length(true.Rec)*2+1)-j] <- quantile(Rec.err[,j],probs=0.75)
  CI90.Rec[j] <- quantile(Rec.err[,j],probs=0.05)
  CI90.Rec[(length(true.Rec)*2+1)-j] <- quantile(Rec.err[,j],probs=0.95)
}

StartYr <- min(true.rep$recruit$year)
EndYr <- max(true.rep$recruit$year)
plot(0, xlim=c(StartYr, EndYr), ylim=c(-0.5,0.5),xaxs="i")
polygon(y=CI90.Rec,x=c(StartYr:EndYr,EndYr:StartYr), col = "gray25")
polygon(y=CI50.Rec,x=c(StartYr:EndYr,EndYr:StartYr),col="gray50")
lines(y=med.Rec, x = c(StartYr:EndYr), type = "l", col="white", lwd=2)
abline(h=0, col="red")
mtext("Standardised Errors for Recruitment", side = 3)

#################################################
#### Plotting std errors for Rec Devs ###########
#################################################

Nyears <- 2020-1960+1
std.err.1 <- matrix(ncol = Nboot, nrow = Nyears)
med.ts <- vector(length = Nyears)
CI50.ts <- vector(length=(Nyears*2))
CI90.ts <- vector(length=(Nyears*2))
for (j in 1:Nyears) {
  true.value <- subset(true.rep$parameters, Num ==(j+30))$Value
  for (i in 1:Nboot) {
    sim.value <- subset(reps.ctl1[[i]]$parameters, Num == (j+30))$Value
    std.err.1[j,i] <- (sim.value - true.value) / true.value
   }
    med.ts[j] <- median(std.err.1[j,])
    CI50.ts[j] <- quantile(std.err.1[j,],probs=0.25)
    CI50.ts[(Nyears*2+1)-j] <- quantile(std.err.1[j,],probs=0.75)
    CI90.ts[j] <- quantile(std.err.1[j,],probs=0.05)
    CI90.ts[(Nyears*2+1)-j] <- quantile(std.err.1[j,],probs=0.95)
}


#################################################
#### Plotting all three ########
#################################################
par(mfrow=c(2,1), mar = c(2,2,2,1))

plot(0, xlim=c(StartYr, EndYr), ylim=c(-0.6,0.6),xaxs="i")
polygon(y=CI90.SSB,x=c(StartYr:EndYr,EndYr:StartYr), col = "gray25")
polygon(y=CI50.SSB,x=c(StartYr:EndYr,EndYr:StartYr),col="gray50")
lines(y=med.SSB, x = c(StartYr:EndYr), type = "l", col="white", lwd=2)
abline(h=0, col="red")
mtext("Standardised Errors for SSB", side = 3)

plot(0, xlim=c(StartYr, EndYr), ylim=c(-0.6,0.6),xaxs="i")
polygon(y=CI90.Rec,x=c(StartYr:EndYr,EndYr:StartYr), col = "gray25")
polygon(y=CI50.Rec,x=c(StartYr:EndYr,EndYr:StartYr),col="gray50")
lines(y=med.Rec, x = c(StartYr:EndYr), type = "l", col="white", lwd=2)
abline(h=0, col="red")
mtext("Standardised Errors for Recruitment", side = 3)

plot(0, xlim=c(1960,2020), ylim=c(-1.5,1),xaxs="i")
polygon(y=CI90.ts,x=c(1960:2020,2020:1960), col = "gray25")
polygon(y=CI50.ts,x=c(1960:2020,2020:1960),col="gray50")
lines(y=med.ts, x = c(1960:2020), type = "l", col="white", lwd=2)
abline(h=0, col="red")
mtext("Standardised Errors for Recruitment Deviations", side = 3)

par(mfrow=c(1,1))
