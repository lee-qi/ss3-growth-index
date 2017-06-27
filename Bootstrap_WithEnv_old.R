setwd("C:/Users/Lee Qi/Desktop/SS3/Splitnose/Testing")


dir.check <- getwd()
if (!grepl(basename(dir.check), "splitnose", ignore.case = TRUE)) {
  stop(paste("Change your working directory!"))
}

require(r4ss)
require(foreach)
require(doParallel)

registerDoParallel(4)
dir.pres <- "Old_Variance and Forecast" #Change to whichever model you're working in
dir.rep <- "Reports"

file.index <- "splitnose_master_chronology.csv"
env.index <- read.csv(file.index, header = TRUE)
N_envobs <- NROW(env.index)

setwd(dir.pres)
dir.create(dir.rep, showWarnings = FALSE)

Nboot <- 100 #Set number of bootstraps
foreyears <- 30 #Set number of years we want to forecast
par2change <- c("L_at_Amax_Fem_GP_1", "L_at_Amin_Mal_GP_1")
addormult <- "1" #positive for multiplicative, negative for additive

file.dat <- "data.ss" 
new.file.dat <- "data-new.ss"
file.ctl <- "control.ss"
ctl.1 <- "control-1.ss"
ctl.2 <- "control-2.ss"
file.exe <- "SS3.exe"
file.start <- "starter.ss"
file.par <- "ss3.par"
file.start.new <- "starter.ss_new"
file.fore <- "forecast.ss"
file.dat.new <- "data.ss_new"

#########################################
#### Modifying data file
#### Getting rid of ageing error
#########################################

dat.file <- SS_readdat(file.dat)
dat.file$ageerror[2,] <- rep(.001, length = ncol(dat.file$ageerror))
Nfleets <- dat.file$Nfleet
Nareas <- dat.file$N_areas
endyr <- dat.file$endyr

SS_writedat(dat.file, outfile = new.file.dat, overwrite = TRUE)

#########################################
#### Creating the two different ctl files
#### For the two different EMs
#########################################


orig.ctl <- readLines(file.ctl)

#Turn off variance adjustment of comp data
line.1 <- grep("#_mult_by_lencomp_N", orig.ctl)
lencompvar <- strsplit(orig.ctl[line.1], split = " ")[[1]]
parnums <-  which(lencompvar!="")
parnums <- parnums[-length(parnums)]
lencompvar[parnums] <- "1"
orig.ctl[line.1] <- paste(lencompvar, collapse=" ")

line.2 <- grep("#_mult_by_agecomp_N", orig.ctl)
agecompvar <- strsplit(orig.ctl[line.2], split = " ")[[1]]
parnums <-  which(agecompvar!="")
parnums <- parnums[-length(parnums)]
agecompvar[parnums] <- "1"
orig.ctl[line.2] <- paste(agecompvar, collapse=" ")

#Turn off F ballpark year
line.3 <- grep("ballpark", orig.ctl)[2]
orig.ctl[line.3] <- paste("-", orig.ctl[line.3], collapse = " ", sep  = "")

#Changing F method from Pope's approximation to hybrid method
line.4 <- grep("F_Method", orig.ctl)[1]
Fmethod <- strsplit(orig.ctl[line.4], split = " ")[[1]]
Fmethod[1] <- "3"
orig.ctl[line.4] <- paste(Fmethod, collapse = " ")

line.5 <- line.4 + 1
maxF <- strsplit(orig.ctl[line.5], split = " ")[[1]]
maxF[1] <- "4" #per Methot's recommendation in SS3 Manual
orig.ctl[line.5] <- paste(maxF, collapse = " ")

line.6 <- line.5 + 3
NitersF <- strsplit(orig.ctl[line.6], split = " ")[[1]]
orig.ctl[line.6] <- paste("4", orig.ctl[line.6], sep = "  ", collapse = " ")

#Remove prior on F
for(i in 1:Nfleets) {
  line.7 <- grep("#_initial_F_parms", orig.ctl) + (i+1)
  initF <- strsplit(orig.ctl[line.7], split = " ")[[1]]
  parnums <- which(initF!="")
  initF[parnums[5]] <- "-2"
  orig.ctl[line.7] <- paste(initF, collapse = " ")
}

#Remove priors on selectivity
lineNos <- grep("#_size_sel:", orig.ctl)
for(i in 1:length(lineNos)) {
  line.8 <- lineNos[i]+1
  while(length(strsplit(orig.ctl[line.8], split = " ")[[1]]) >= 14) {
    selex <- strsplit(orig.ctl[line.8], split = " ")[[1]]
    parnums <- which(selex!="")
    selex[parnums[5]] <- "-1"
    orig.ctl[line.8] <- paste(selex, collapse = " ")
    line.8 <- line.8+1
  }
}

#Env-start

#Finding growth parameter
for (param in par2change) {
  line.param <- grep(param, orig.ctl)
  growth <- strsplit(orig.ctl[line.param], split = " ")[[1]]
  parnums <- which(growth!="")
  growth[parnums[8]] <- addormult        #env-var column
  orig.ctl[line.param] <- paste(growth, collapse = " ")
}


#Finding link parameter
line.beta <- grep("#custom_MG-env_setup", orig.ctl)
EnvSetup <- strsplit(orig.ctl[line.beta], split = " ")[[1]]
EnvSetup <- EnvSetup[-(1:2)]
orig.ctl[line.beta] <- paste(EnvSetup, collapse = " ")

line.betasetup <- line.beta+1
betasetup <- strsplit(orig.ctl[line.betasetup], split = " ")[[1]]
betasetup <- betasetup[-(1:2)]

#For beta == 0
collapse.beta <- paste(betasetup, collapse = " ")
beta.table <- collapse.beta

#if(length(par2change) > 1) {
#  for(i in 2:length(par2change)){
#    beta.table <- c(beta.table, collapse.beta)
#  }
#}

orig.ctl.1 <- c(orig.ctl[1:(line.betasetup - 1)], beta.table, orig.ctl[(line.betasetup + 1):length(orig.ctl)])

writeLines(orig.ctl.1, ctl.1)

#For beta == estimated
parnums <- which(betasetup!="")
betasetup[parnums[7]] <- "5"            #Phase of beta estimation
collapse.beta <- paste(betasetup, collapse = " ")
beta.table <- collapse.beta

#if(length(par2change) > 1) {
#  for(i in 2:length(par2change)){
#    beta.table <- c(beta.table, collapse.beta)
#  }
#}

orig.ctl.2 <- c(orig.ctl[1:(line.betasetup - 1)], beta.table, orig.ctl[(line.betasetup + 1):length(orig.ctl)])

writeLines(orig.ctl.2, ctl.2)

#Env-end


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

foreach(i=1:Nboot) %dopar% {
  setwd(file.path(dir.check,dir.pres))
  seed <- 5000*i
  set.seed(seed)
  require(r4ss)

  dir.new <- paste("Bootstrap",i, sep = "-")
  
  cr.dir <- dir.create(dir.new, showWarnings = FALSE)

  ignore <- mapply(file.copy, to = file.path(dir.new),
                   MoreArgs = list(from = c(
                    file.par, file.start.new, new.file.dat, file.ctl, file.fore,file.exe), overwrite = TRUE))
  
  setwd(dir.new)

  file.rename(from = file.start.new, to = file.start)
  file.rename(from = new.file.dat, to = file.dat)

#Env-start
  #########################################
  #### Modifying dat file
  #########################################

  dat.file$N_environ_variables <- 1
  dat.file$N_environ_obs <- N_envobs
  SS3.index <- cbind(env.index$year, 1, rnorm(N_envobs, 0, 1))

  colnames(SS3.index) <- c("Year", "Variable", "Value")
  dat.file$envdat <- data.frame(SS3.index)

  SS_writedat(dat.file, file.dat, overwrite = TRUE)
#Env-end

  #########################################
  #### Modifying par file
  #########################################
  
  par.lines <- readLines(file.par)

  #Finding sigma R
  sigR.line <- grep("SR_parm", par.lines)[3] +1
  sigR <- as.numeric(par.lines[sigR.line])
  meanR <- -sigR^2/2
  
  #Finding and replacing the recdevs
  recdevs.line <- grep("recdev", par.lines) +1
  recdevs <- strsplit(par.lines[recdevs.line], split = " ")
  recdevs <- as.vector(recdevs[[1]])
  recdevs.length <- length(recdevs) - 1
  
  gen.recdevs <- rnorm(recdevs.length, meanR, sigR)
  gen.recdevs.str <- paste(gen.recdevs, collapse = " ")
  gen.recdevs.str <- paste(" ", gen.recdevs.str, sep = " ")
  par.lines[recdevs.line] <- gen.recdevs.str
  
  writeLines(par.lines, file.par)

  #########################################
  #### Modifying control file
  #########################################

  ctl.lines <- readLines(file.ctl)
  max_adj.line <- grep("#_max_bias_adj_in_MPD", ctl.lines)
  max_adj.str <- strsplit(ctl.lines[max_adj.line], split = " ")
  max_adj.str[[1]][2] <- 0
  max_adj.str <- paste(max_adj.str[[1]], collapse = " ")
  ctl.lines[max_adj.line] <- max_adj.str

  writeLines(ctl.lines, file.ctl)

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
  for(j in 1:2) {
    setwd(file.path(dir.check,dir.pres,dir.new))
    dir.res <- paste("Results",j, sep = "-")
    cr.res <- dir.create(dir.res, showWarnings = FALSE)
    
    dat.name <- paste("boot-dat",i,".ss",sep="")
    ctl.name <- paste("control-",j,".ss",sep="")
                    
    move.dat <- file.copy(to = dir.res, from = dat.name, overwrite = TRUE)
    
    setwd(dir.res)
    done.orig <- mapply(file.copy, to = getwd(),
                    MoreArgs = list(from = file.path(dir.check,dir.pres,c(
                      file.start, ctl.name,
                      file.fore, "SS3.exe"))), overwrite = TRUE)
    
    rename.done <- mapply(file.rename, from = c(dat.name, ctl.name),
                               to = c(file.dat,file.ctl))

    start.file <- SS_readstarter(file.start)
    start.file$init_values_src <- 0
    start.file$last_estimation_phase <- 6
    SS_writestarter(start.file, file = file.start, overwrite = TRUE)

    system("SS3.exe", show.output.on.console = FALSE)

    rep.file <- SS_output(warn=FALSE,checkcor=TRUE,NoCompOK=TRUE,
                        verbose=FALSE,printstats=FALSE,
                        covar=TRUE, readwt=FALSE, forecast=FALSE,
                         dir = file.path(dir.check,dir.pres,dir.new,dir.res))

    dir.bias <- paste("Bias_Bootstrap-", i, sep = "")
    dir.create(dir.bias)
    SS_fitbiasramp(rep.file, plot = FALSE,
                    oldctl = file.path(dir.check,dir.pres,dir.new, "control.ss"),
                   newctl = file.path(dir.check, dir.pres,dir.new,dir.res,dir.bias,"control.ss"))
    mapply(file.copy, to = file.path(dir.check, dir.pres,dir.new,dir.res,dir.bias), overwrite = TRUE,
            MoreArgs = list(from = file.path(dir.check,dir.pres,dir.new,dir.res,c("data.ss", "SS3.exe", "forecast.ss", "starter.ss"))))

    setwd(dir.bias)
    start.file <- SS_readstarter("starter.ss")
    start.file$init_values_src <- 0
    start.file$last_estimation_phase <- 6
    SS_writestarter(start.file, file = "starter.ss", overwrite = TRUE)

    system("SS3.exe", show.output.on.console = FALSE)

#Env-start
    ###########################################
    #### Retrieving Fmsy from rep file
    #### ?Not sure how to deal with allocations?
    #### ?Not sure if using the right Fmsy
    ###########################################

    rep.file <- SS_output(dir = getwd(), verbose = F)
    Fmsy <- rep.file$derived_quants[rep.file$derived_quants[,1] == "Fstd_MSY", 2]
    Years <- seq(from = (endyr+1), length = foreyears, by = 1) #?Find start year for forecasting
    catch.table <- cbind(Years, rep(1, foreyears), rep(1, foreyears), Fmsy)
    colnames(catch.table) <- c("Year", "Seas", "Fleet", "Catch_or_F")
    
    ##########################################
    ### New OM Folder
    ##########################################

    dir.fin <- paste("Projection-", j, sep = "")
    dir.create(dir.fin)
    mapply(file.copy, to = dir.fin, overwrite = TRUE,
                 MoreArgs = list(from = c(file.path(getwd(), c(file.fore, file.start, file.dat, "SS3.exe")),
                                       file.path(dir.check, dir.pres, dir.new, c(file.ctl, file.par)))))

    setwd(dir.fin)

    ##########################################
    ### Change Forecast file
    ##########################################

    fore.file <- SS_readforecast(file.fore, Nfleets = Nfleets, Nareas = Nareas)
    fore.file$Ncatch <- foreyears * Nfleets
    fore.file$InputBasis <- 99
    fore.file$Forecast <- 5       #Set to input catch
    fore.file$ForeCatch <- data.frame(catch.table)

    SS_writeforecast(fore.file, overwrite = TRUE)

    ##########################################
    ### Run OM with new Fmsy
    ##########################################

    system("SS3.exe", show.output.on.console = FALSE)

#Env-end


    ###########################################
    ### Retrieving final report file
    ###########################################

    rep.name <- paste("Boot", i, "-Ctl", j,".sso", sep = "")
    file.rename(from = "Report.sso", to = rep.name)
    file.copy(from = rep.name, to = file.path(dir.check,dir.pres,dir.rep), overwrite = TRUE)

  }
}


#########################################
#### Comparing results
#########################################
setwd(dir.rep)

reps.ctl1 <- NULL
true.reps <- NULL
for(i in 1:Nboot) {
  reps.ctl1[[i]] <- SS_output(repfile=paste("Boot", i, "-Ctl", 1,".sso", sep = ""),
                              warn=FALSE,checkcor=TRUE,NoCompOK=TRUE,
                              verbose=FALSE,printstats=FALSE,
                              covar=FALSE, readwt=FALSE, forecast=FALSE,
                              dir = file.path(dir.check,dir.pres,dir.rep))
  true.dir <- paste("Bootstrap-",i, sep = "")
  true.reps[[i]] <- SS_output(warn=FALSE,checkcor=FALSE,NoCompOK=TRUE,
                              verbose=FALSE,printstats=FALSE,
                              covar=FALSE, readwt=FALSE, forecast=FALSE,
                              dir = file.path(dir.check, dir.pres, true.dir))
}



std.errs <- matrix(nrow=149, ncol = Nboot)
par.names <- vector(length = 1)
for (j in 1:149) {
  sim.values <- rep(NA,length = Nboot)
  std.err.1 <- rep(NA,length = Nboot)
  param <- subset(reps.ctl1[[i]]$parameters, Num == j)$Label
  true.values <- rep(NA,length = Nboot)
  for (i in 1:Nboot) {
    true.values[i] <- subset(true.reps[[i]]$parameters, Num ==j)$Value
    sim.values[i] <- subset(reps.ctl1[[i]]$parameters, Num == j)$Value
    std.err.1[i] <- (sim.values[i] - true.values[i]) / true.values[i]
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
    png(paste("Graph-", (i+8)/9, ".png", sep = ""),600,900)
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

#################################################
#### Plotting std errors for SSB ################
#################################################

Nyears <- length(true.reps[[1]]$recruit$spawn_bio[which(true.reps[[1]]$recruit$spawn_bio!=0)])
SPB.err <- matrix(nrow=Nboot, ncol = Nyears)
med.SSB <- vector(length=Nyears)
CI50.SSB <- vector(length=(Nyears*2))
CI90.SSB <- vector(length=(Nyears*2))

for (i in 1:Nboot) {
  true.SSB <- true.reps[[i]]$recruit$spawn_bio[which(true.reps[[i]]$recruit$spawn_bio!=0)]
  sim.values <- reps.ctl1[[i]]$recruit$spawn_bio[1:Nyears]
  SPB.err[i,] <- (sim.values-true.SSB) / true.SSB
}
for (j in 1:Nyears) {
  med.SSB[j] <- median(SPB.err[,j])
  CI50.SSB[j] <- quantile(SPB.err[,j],probs=0.25)
  CI50.SSB[(Nyears*2+1)-j] <- quantile(SPB.err[,j],probs=0.75)
  CI90.SSB[j] <- quantile(SPB.err[,j],probs=0.05)
  CI90.SSB[(Nyears*2+1)-j] <- quantile(SPB.err[,j],probs=0.95)
}

StartYr <- min(true.reps[[1]]$recruit$year)
EndYr <- max(true.reps[[1]]$recruit$year)
plot(0, xlim=c(StartYr, EndYr), ylim=c(-0.5,0.5),xaxs="i",ylab="Error")
polygon(y=CI90.SSB,x=c(StartYr:EndYr,EndYr:StartYr), col = "gray25")
polygon(y=CI50.SSB,x=c(StartYr:EndYr,EndYr:StartYr),col="gray50")
lines(y=med.SSB, x = c(StartYr:EndYr), type = "l", col="white", lwd=2)
abline(h=0, col="red")
mtext("Standardised Errors for SSB", side = 3)
dev.off()

#################################################
#### Plotting std errors for Recruitment ########
#################################################

Nyears <- length(true.reps[[1]]$recruit$exp_recr[which(true.reps[[1]]$recruit$exp_recr!=0)])
Rec.err <- matrix(nrow=Nboot, ncol = Nyears)
med.Rec <- vector(length=Nyears)
CI50.Rec <- vector(length=(Nyears*2))
CI90.Rec <- vector(length=(Nyears*2))

for (i in 1:Nboot) {
  true.Rec <- true.reps[[i]]$recruit$exp_recr[which(true.reps[[i]]$recruit$exp_recr!=0)]
  sim.values <- reps.ctl1[[i]]$recruit$exp_recr[1:length(true.Rec)]
  Rec.err[i,] <- (sim.values-true.Rec) / true.Rec
}
for (j in 1:length(true.Rec)) {
  med.Rec[j] <- median(Rec.err[,j])
  CI50.Rec[j] <- quantile(Rec.err[,j],probs=0.25)
  CI50.Rec[(Nyears*2+1)-j] <- quantile(Rec.err[,j],probs=0.75)
  CI90.Rec[j] <- quantile(Rec.err[,j],probs=0.05)
  CI90.Rec[(Nyears*2+1)-j] <- quantile(Rec.err[,j],probs=0.95)
}

StartYr <- min(true.reps[[1]]$recruit$year)
EndYr <- max(true.reps[[1]]$recruit$year)
plot(0, xlim=c(StartYr, EndYr), ylim=c(-0.5,0.5),xaxs="i")
polygon(y=CI90.Rec,x=c(StartYr:EndYr,EndYr:StartYr), col = "gray25")
polygon(y=CI50.Rec,x=c(StartYr:EndYr,EndYr:StartYr),col="gray50")
lines(y=med.Rec, x = c(StartYr:EndYr), type = "l", col="white", lwd=2)
abline(h=0, col="red")
mtext("Standardised Errors for Recruitment", side = 3)
dev.off()


#################################################
#### Plotting all three ########
#################################################
par(mfrow=c(2,1), mar = c(2,2,2,1))

plot(0, xlim=c(StartYr, EndYr), ylim=c(-0.5,0.5),xaxs="i")
polygon(y=CI90.SSB,x=c(StartYr:EndYr,EndYr:StartYr), col = "gray25")
polygon(y=CI50.SSB,x=c(StartYr:EndYr,EndYr:StartYr),col="gray50")
lines(y=med.SSB, x = c(StartYr:EndYr), type = "l", col="white", lwd=2)
abline(h=0, col="red")
mtext("Standardised Errors for SSB", side = 3)

plot(0, xlim=c(StartYr, EndYr), ylim=c(-0.5,0.5),xaxs="i")
polygon(y=CI90.Rec,x=c(StartYr:EndYr,EndYr:StartYr), col = "gray25")
polygon(y=CI50.Rec,x=c(StartYr:EndYr,EndYr:StartYr),col="gray50")
lines(y=med.Rec, x = c(StartYr:EndYr), type = "l", col="white", lwd=2)
abline(h=0, col="red")
mtext("Standardised Errors for Recruitment", side = 3)

par(mfrow=c(1,1))
