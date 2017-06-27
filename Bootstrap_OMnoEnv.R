#setwd("/Users/LeeQi/Google Drive/SS3/Splitnose/Testing")
setwd("C:/Users/Lee Qi/Desktop/SS3/Splitnose/Testing")

dir.check <- getwd()
if (!grepl(basename(dir.check), "splitnose", ignore.case = TRUE)) {
  stop(paste("Change your working directory!"))
}

require(r4ss)
require(foreach)
require(doParallel)

registerDoParallel(7)
dir.pres <- "OM 1 Stochastic" #Change to whichever model you're working in
dir.rep <- "Reports"

file.index <- "splitnose_master_chronology.csv"
env.index <- read.csv(file.index, header = TRUE)
N_envobs <- NROW(env.index)
Gparams <- arima(ts(env.index$splitnose_master_chronology_normalized), order = c(1,0,0))
sigG <- Gparams$sigma2
G.AR <- as.numeric(Gparams$coef[1])

setwd(dir.pres)
dir.create(dir.rep, showWarnings = FALSE)

Nboot <- 100 #Set number of bootstraps
foreyears <- 50 #Set number of years we want to forecast
par2change <- c("VonBert_K_Fem_GP_1", "VonBert_K_Mal_GP_1", "L_at_Amax_Fem_GP_1", "L_at_Amax_Mal_GP_1")
addormult <- "1" #positive for multiplicative, negative for additive

file.dat <- "data.ss" 
file.dat.new <- "data-new.ss"

file.ctl <- "control.ss"
ctl.1 <- "control-1.ss"
ctl.2 <- "control-2.ss"

file.fore <- "forecast.ss"
file.fore.new <- "forecast-new.ss"

file.start <- "starter.ss"
file.start.new <- "starter.ss_new"

file.par <- "ss3.par"

file.exe <- "SS3.exe"
exe.cmd <- paste(file.exe, " -cbs 2000000000", sep = "")

#########################################
#### Modifying data file
#### Getting rid of ageing error
#########################################

dat.file <- SS_readdat(file.dat)
dat.file$ageerror[2,] <- rep(.001, length = ncol(dat.file$ageerror))
Nfleets <- dat.file$Nfleet
Nareas <- dat.file$N_areas
endyr <- dat.file$endyr
styr <- dat.file$styr

SS_writedat(dat.file, outfile = file.dat.new, overwrite = TRUE)

#########################################
#### Modifying forecast file
#### Changing Nforecast years
#########################################

#fore.file <- SS_readforecast(file.fore, Nfleets, Nareas)
##fore.file$Nforecastyrs <- foreyears
##fore.file$FirstYear_for_caps_and_allocations <- endyr + foreyears
#fore.file$Forecast <- 2
#
#SS_writeforecast(fore.file, file = file.fore.new, dir = getwd(), overwrite = TRUE)

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
#line.4 <- grep("F_Method", orig.ctl)[1]
#Fmethod <- strsplit(orig.ctl[line.4], split = " ")[[1]]
#Fmethod[1] <- "3"
#orig.ctl[line.4] <- paste(Fmethod, collapse = " ")
#
#line.5 <- line.4 + 1
#maxF <- strsplit(orig.ctl[line.5], split = " ")[[1]]
#maxF[1] <- "4" #per Methot's recommendation in SS3 Manual
#orig.ctl[line.5] <- paste(maxF, collapse = " ")
#
#line.6 <- line.5 + 3
#NitersF <- strsplit(orig.ctl[line.6], split = " ")[[1]]
#orig.ctl[line.6] <- paste("4", orig.ctl[line.6], sep = "  ", collapse = " ")

#Remove prior on F
for(i in 1:Nfleets) {
  line.7 <- grep("#_initial_F_parms", orig.ctl) + (i+1)
  initF <- strsplit(orig.ctl[line.7], split = " ")[[1]]
  parnums <- which(initF!="")
  initF[parnums[5]] <- "-2"
  orig.ctl[line.7] <- paste(initF, collapse = " ")
}

#Remove priors on selectivity
#Need to figure out how to deal with retention fishery
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

lineNos <- grep("#_Retention", orig.ctl)
for(i in 1:length(lineNos)) {
  line.9 <- lineNos[i]+1
  while(length(strsplit(orig.ctl[line.9], split = " ")[[1]]) >= 14) {
    selex <- strsplit(orig.ctl[line.9], split = " ")[[1]]
    parnums <- which(selex!="")
    selex[parnums[5]] <- "-1"
    orig.ctl[line.9] <- paste(selex, collapse = " ")
    line.9 <- line.9+1
  }
}

#Env-start

orig.ctl.1 <- orig.ctl
writeLines(orig.ctl.1, ctl.1)

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

#orig.ctl.1 <- c(orig.ctl[1:(line.betasetup - 1)], beta.table, orig.ctl[(line.betasetup + 1):length(orig.ctl)])

#writeLines(orig.ctl.1, ctl.1)

#For beta == estimated
parnums <- which(betasetup!="")
betasetup[parnums[7]] <- "3"            #Phase of beta estimation
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
 
start.file <- SS_readstarter(file.start)
start.file$init_values_src <- 1 #Use par file instead of inits in ctl
start.file$N_bootstraps <- Nboot + 2 #Set Bootstrap number
start.file$last_estimation_phase <- 0
 
SS_writestarter(start.file, file = file.start.new, overwrite = TRUE)

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

  file.start.new <- "starter.ss_new"

  ignore <- mapply(file.copy, to = file.path(dir.new),
                   MoreArgs = list(from = c(
                    file.par, file.start.new, file.dat.new, file.ctl, file.fore, file.exe), overwrite = TRUE))
  
  setwd(dir.new)
  
  file.start <- "starter.ss"

  file.rename(from = file.start.new, to = file.start)
  file.rename(from = file.dat.new, to = file.dat)

  #########################################
  #### Modifying dat file
  #########################################

  dat.file$N_environ_variables <- 1
  EnvYrs <- seq(from = (styr+1), to = (endyr + foreyears), by = 1)
  N_envobs <- length(EnvYrs)
  dat.file$N_environ_obs <- N_envobs
  
  AR.index <- rnorm(N_envobs, mean = ((-sigG/2)*(1-G.AR)/sqrt(1-G.AR^2)), sd = sqrt(sigG))
  for(t in 2:N_envobs) {
    AR.index[t] <- G.AR*AR.index[t-1] + AR.index[t]*sqrt(1-(G.AR^2))
  }
  
  AR.index <- AR.index - mean(AR.index)

  SS3.index <- cbind(EnvYrs, 1, AR.index)
  colnames(SS3.index) <- c("Year", "Variable", "Value")
  dat.file$envdat <- data.frame(SS3.index)

  SS_writedat(dat.file, file.dat, overwrite = TRUE)

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

  fore.recdevs.line <- grep("Fcast_recruitments", par.lines) + 1
  fore.recdevs.length <- strsplit(par.lines[fore.recdevs.line], split = " ")
  fore.recdevs.length <- as.vector(fore.recdevs.length[[1]])
  fore.recdevs.length <- length(fore.recdevs.length) - 1
  fore.recdevs <- rnorm(fore.recdevs.length, meanR, sigR)
  fore.recdevs.str <- paste(fore.recdevs, collapse = " ")
  fore.recdevs.str <- paste(" ", fore.recdevs.str, sep = " ")
  par.lines[fore.recdevs.line] <- fore.recdevs.str

  writeLines(par.lines, file.par)

  #########################################
  #### Modifying control file
  #### Get rid of bias adjustment
  #########################################

  ctl.lines <- readLines(file.ctl)
  max_adj.line <- grep("#_max_bias_adj_in_MPD", ctl.lines)
  max_adj.str <- strsplit(ctl.lines[max_adj.line], split = " ")[[1]]
  max_adj.str[2] <- 0
  max_adj.str <- paste(max_adj.str, collapse = " ")
  ctl.lines[max_adj.line] <- max_adj.str

  writeLines(ctl.lines, file.ctl)

  #########################################
  #### Run Bootstrap simulator to generate
  #### files of the "truth"
  #########################################
  
  system(paste(exe.cmd, " -nohess", sep = ""), show.output.on.console = FALSE)
  #system(exe.cmd)

  #########################################
  #### Projecting the truth
  #########################################
  dir.OM <- "OM Proj-Catch"
  dir.create(dir.OM)

  Years <- seq(from = (endyr+1), length = foreyears, by = 1) #?Find start year for forecasting
  repfile <- readLines(file.path(getwd(), "Report.sso"))

#  Fmsynum <- grep("Fstd_MSY", repfile)
#  Fmsy <- as.numeric(strsplit(repfile[Fmsynum], split = " ")[[1]])[3]
#
#  Biosmrynum <- grep("Bio_smry",repfile)
#  Bio_smry.col <- which(strsplit(repfile[Biosmrynum], split = " ")[[1]]=="Bio_smry")
#
#  finyr.row <- grep(paste(endyr," TIME", sep = ""), repfile)[1]
#  fin.SSB <- as.numeric(strsplit(repfile[finyr.row], split = " ")[[1]][Bio_smry.col])
#
#  proj.catch <- fin.SSB * Fmsy

  MSYnum <- grep("RetYield_MSY", repfile)
  proj.catch <- as.numeric(strsplit(repfile[MSYnum], split = " ")[[1]][3])

  catch.table <- cbind(Years, rep(1, foreyears), rep(1, foreyears), proj.catch)
  colnames(catch.table) <- c("Year", "Seas", "Fleet", "Catch_or_F")

  ignore <- mapply(file.copy, to = dir.OM,
                   MoreArgs = list(from = file.path(dir.check,dir.pres,dir.new,c(
                  file.par, file.start, file.dat, file.ctl, file.fore, file.exe)), overwrite = TRUE))
  
  setwd(dir.OM)

  fore.file <- SS_readforecast(file.fore, Nfleets = Nfleets, Nareas = Nareas)
  fore.file$Ncatch <- foreyears * Nfleets
  fore.file$InputBasis <- 3
  fore.file$Forecast <- 5       #Set to input catch
  fore.file$ForeCatch <- data.frame(catch.table)
  SS_writeforecast(fore.file, overwrite = TRUE)

  start.file <- SS_readstarter(file.start)
  start.file$init_values_src <- 1 #Use par file instead of inits in ctl
  start.file$N_bootstraps <- 2 #Set Bootstrap number
  start.file$last_estimation_phase <- 0
  SS_writestarter(start.file, file = file.start, overwrite = TRUE)

  system(paste(exe.cmd, " -nohess", sep = ""), show.output.on.console = FALSE)

  #########################################
  #### Creating new dat file
  #########################################
  setwd(file.path(".."))
  file.dat.new <- "data.ss_new"
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
    dir.res <- paste("EM",j, sep = "-")
    cr.res <- dir.create(dir.res, showWarnings = FALSE)
    
    dat.name <- paste("boot-dat",i,".ss",sep="")
    ctl.name <- paste("control-",j,".ss",sep="")
                    
    move.dat <- file.copy(to = dir.res, from = dat.name, overwrite = TRUE)

  ##########################################
  ### Running the EMs
  ##########################################    
    setwd(dir.res)
    done.orig <- mapply(file.copy, to = getwd(),
                    MoreArgs = list(from = file.path(dir.check,dir.pres,c(
                      file.start, ctl.name,
                      file.fore, file.exe))), overwrite = TRUE)
    
    rename.done <- mapply(file.rename, from = c(dat.name, ctl.name),
                               to = c(file.dat,file.ctl))

    start.file <- SS_readstarter(file.start)
    start.file$init_values_src <- 0
    start.file$last_estimation_phase <- 7
    SS_writestarter(start.file, file = file.start, overwrite = TRUE)

    system(exe.cmd, show.output.on.console = FALSE)

    rep.file <- SS_output(dir = getwd())
    endyr <- rep.file$endyr

    dir.bias <- paste("Bias_Bootstrap-", i, sep = "")
    dir.create(dir.bias)
    SS_fitbiasramp(rep.file, plot = FALSE,
                    oldctl = file.path(getwd(), file.ctl),
                   newctl = file.path(getwd(),dir.bias,file.ctl))
    mapply(file.copy, to = file.path(getwd(),dir.bias), overwrite = TRUE,
            MoreArgs = list(from = file.path(getwd(),
                            c(file.dat, file.exe, file.fore, file.start, file.par))))

    setwd(dir.bias)
    start.file$init_values_src <- 1
    SS_writestarter(start.file, file = file.start, overwrite = TRUE)
    #Reasonable to fix selectivity and whatnot here? How would the bias ramp affect it? Look it up!

    system(exe.cmd, show.output.on.console = FALSE)

#Env-start
    ###########################################
    #### Retrieving Fmsy from rep file
    ###########################################

    repfile <- readLines(file.path(getwd(), "Report.sso"))
    #Fmsynum <- grep("Fstd_MSY", repfile)
    #Fmsy <- as.numeric(strsplit(repfile[Fmsynum], split = " ")[[1]])[3]
    #
    #Biosmrynum <- grep("Bio_smry",repfile)
    #Bio_smry.col <- which(strsplit(repfile[Biosmrynum], split = " ")[[1]]=="Bio_smry")
    #
    #finyr.row <- grep(paste(endyr," TIME", sep = ""), repfile)[1]
    #fin.SSB <- as.numeric(strsplit(repfile[finyr.row], split = " ")[[1]][Bio_smry.col])
    #
    #proj.catch <- fin.SSB * Fmsy

     
    ##########################################
    ### New OM Folder
    ##########################################

    dir.fin <- paste("Projection-", j, sep = "")
    dir.create(dir.fin)

    if(j == 2) {
      mapply(file.copy, to = dir.fin, overwrite = TRUE,
             MoreArgs = list(from = file.path(getwd(),
              c(file.ctl, file.dat, file.fore, file.start, file.par, file.exe))))

      setwd(dir.fin)
      par.file <- readLines(file.par)
      for(Gpar in 1:length(par2change)) {
        Gparam.line <- grep("MGparm", par.file)[24+Gpar]+1
        par.file[Gparam.line] <- "0"
      }
  
    writeLines(par.file, file.par)
      
    start.file <- SS_readstarter(file.start)
    start.file$init_values_src <- 1 #Use par file instead of inits in ctl
    start.file$N_bootstraps <- 2 #Set Bootstrap number
    start.file$last_estimation_phase <- 0
    SS_writestarter(start.file, file = file.start, overwrite = TRUE)

    system(paste(exe.cmd, " -nohess", sep = ""), show.output.on.console = FALSE)
    }

    ###########################################
    #### Retrieving Fmsy from rep file
    ###########################################

    repfile <- readLines(file.path(getwd(), "Report.sso"))

    MSYnum <- grep("RetYield_MSY", repfile)
    proj.catch <- as.numeric(strsplit(repfile[MSYnum], split = " ")[[1]][3])
  
    new.catch.table <- cbind(Years, rep(1, foreyears), rep(1, foreyears), proj.catch)

    if(j == 1) {setwd(dir.fin)}
    if(j == 2) {unlink(list.files(getwd()))}

    mapply(file.copy, to = getwd(), overwrite = TRUE,
                 MoreArgs = list(from = file.path(dir.check, dir.pres, dir.new, dir.OM,
                  c(file.ctl, file.dat, file.fore, file.start, file.par, file.exe))))


    ##########################################
    ### Change Forecast file
    ##########################################

    fore.file <- SS_readforecast(file.fore, Nfleets = Nfleets, Nareas = Nareas)
    fore.file$ForeCatch <- data.frame(new.catch.table)

    SS_writeforecast(fore.file, overwrite = TRUE)

    ##########################################
    ### Run OM with new Fmsy
    ##########################################

    system(paste(exe.cmd, " -nohess", sep = ""), show.output.on.console = FALSE)
    #system(exe.cmd, show.output.on.console = FALSE)

#Env-end


    ###########################################
    ### Retrieving final report file
    ###########################################

    rep.name <- paste("Boot", i, "-Ctl", j,".sso", sep = "")
    file.copy(from = "Report.sso", to = file.path(dir.check,dir.pres,dir.rep,rep.name))

  }
}


#########################################
#### Comparing results
#########################################
setwd(dir.rep)

source("C:/Users/Lee Qi/Google Drive/SS3/Splitnose/Testing/Plots.R")

#reps.ctl1 <- NULL
#reps.ctl2 <- NULL
#true.reps <- NULL
#EM1.reps <- NULL
#EM2.reps <- NULL
#RunNos <- 0
#for(i in 1:Nboot) {
#  rep1name <- paste("Boot", i, "-Ctl", 1,".sso", sep = "")
#  rep2name <- paste("Boot", i, "-Ctl", 2,".sso", sep = "")
#
#  if(!file.exists(rep1name) | !file.exists(rep2name)) {next}
#  else {
#    RunNos <- RunNos+1
#    reps.ctl1[[RunNos]] <- SS_output(repfile=rep1name,
#                                warn=FALSE,checkcor=TRUE,NoCompOK=TRUE,
#                                verbose=FALSE,printstats=FALSE,
#                                covar=FALSE, readwt=FALSE, forecast=FALSE,
#                                dir = file.path(dir.check,dir.pres,dir.rep),ncols = 113)
#    reps.ctl2[[RunNos]] <- SS_output(repfile=rep2name,
#                                warn=FALSE,checkcor=TRUE,NoCompOK=TRUE,
#                                verbose=FALSE,printstats=FALSE,
#                                covar=FALSE, readwt=FALSE, forecast=FALSE,
#                                dir = file.path(dir.check,dir.pres,dir.rep),ncols = 113)
#    true.dir <- paste("Bootstrap-",i, sep = "")
#    EM1.reps[[RunNos]] <- SS_output(warn=FALSE,checkcor=TRUE,NoCompOK=TRUE,
#                                verbose=FALSE,printstats=FALSE,
#                                covar=FALSE, readwt=FALSE, forecast=FALSE,
#                                dir = file.path(dir.check,dir.pres,true.dir,"EM-1",
#                                  paste("Bias_Bootstrap-",i,sep = "")),ncols = 113)
#    EM2.reps[[RunNos]] <- SS_output(warn=FALSE,checkcor=TRUE,NoCompOK=TRUE,
#                                verbose=FALSE,printstats=FALSE,
#                                covar=FALSE, readwt=FALSE, forecast=FALSE,
#                                dir = file.path(dir.check,dir.pres,true.dir,"EM-2",
#                                  paste("Bias_Bootstrap-",i,sep = "")),ncols = 113)
#    true.reps[[RunNos]] <- SS_output(verbose = FALSE, printstats = FALSE, covar = FALSE,
#                                dir = file.path(dir.check, dir.pres, true.dir, "OM Proj-Catch"),ncols = 113)
#
#  }
#}
#
#Nboot <- RunNos
#
#
#std.errs <- matrix(nrow=200, ncol = Nboot)
#par.names <- vector(length = 1)
#med.sims <- matrix(nrow = 200, ncol = 2)
#for (j in 1:200) {
#  sim.values <- rep(NA,length = Nboot)
#  std.err.1 <- rep(NA,length = Nboot)
#  param <- subset(true.reps[[1]]$parameters, Num == j)$Label
#  param.sim <- subset(EM1.reps[[1]]$parameters, Label == param)$Label
#  if(length(param)==0 | length(param.sim)==0) {next}
#  true.values <- rep(NA,length = Nboot)
#  for (i in 1:Nboot) {
#    true.values[i] <- subset(true.reps[[i]]$parameters, Num ==j)$Value
#    sim.values[i] <- subset(EM1.reps[[i]]$parameters, Label == param)$Value
#    std.err.1[i] <- (sim.values[i] - true.values[i]) / true.values[i]
#  }
#  med.sims[j,] <- c(param, median(sim.values))
#  if (sum(std.err.1) == 0 | sum(std.err.1) == "NaN") {next}
#  else {
#    std.errs[j,] <- std.err.1
#    par.names <- c(par.names, param)
#  }
#}
#std.errs <- std.errs[complete.cases(std.errs),]
#par.names <- par.names[-1]
#
#for (i in 1:NROW(std.errs)) {
#  if (i%%9 == 1) {
#    png(paste("EM1-Graph-", (i+8)/9, ".png", sep = ""),600,900)
#    par(mfrow = c(3,3))
#    hist(std.errs[i,], main = par.names[i],xlim = c(-0.5,0.5))
#    abline(v = 0, col = "green", lwd = 2)
#    abline(v = mean(std.errs[i,]), col = "red", lwd = 2, lty = 2)
#  }
#  else {
#    hist(std.errs[i,], main = par.names[i],xlim = c(-0.5,0.5))
#    abline(v = 0, col = "green", lwd = 2)
#    abline(v = mean(std.errs[i,]), col = "red", lwd = 2, lty = 2)
#  }
#  if (i%%9 == 0 | i == NROW(std.errs)) {
#    par(mfrow = c(1,1))
#    dev.off()
#  }
#}
#
#std.errs2 <- matrix(nrow=200, ncol = Nboot)
#par.names <- vector(length = 1)
#med.sims <- matrix(nrow = 200, ncol = 2)
#for (j in 1:200) {
#  sim.values <- rep(NA,length = Nboot)
#  std.err.2 <- rep(NA,length = Nboot)
#  param <- subset(true.reps[[1]]$parameters, Num == j)$Label
#  if(length(param)==0) {next}
#  true.values <- rep(NA,length = Nboot)
#  for (i in 1:Nboot) {
#    true.values[i] <- subset(true.reps[[i]]$parameters, Label == param)$Value
#    sim.values[i] <- subset(EM2.reps[[i]]$parameters, Label == param)$Value
#    std.err.2[i] <- (sim.values[i] - true.values[i]) / true.values[i]
#  }
#  med.sims[j,] <- c(param, median(sim.values))
#  if (sum(std.err.2) == 0 | sum(std.err.2) == "NaN") {next}
#  else {
#    std.errs2[j,] <- std.err.2
#    par.names <- c(par.names, param)
#  }
#}
#std.errs2 <- std.errs2[complete.cases(std.errs2),]
#par.names <- par.names[-1]
#
#for (i in 1:NROW(std.errs2)) {
#  if (i%%9 == 1) {
#    png(paste("EM2-Graph-", (i+8)/9, ".png", sep = ""),600,900)
#    par(mfrow = c(3,3))
#    hist(std.errs2[i,], main = par.names[i],xlim = c(-0.5,0.5))
#    abline(v = 0, col = "green", lwd = 2)
#    abline(v = median(std.errs2[i,]), col = "red", lwd = 2, lty = 2)
#  }
#  else {
#    hist(std.errs[i,], main = par.names[i],xlim = c(-0.5,0.5))
#    abline(v = 0, col = "green", lwd = 2)
#    abline(v = median(std.errs2[i,]), col = "red", lwd = 2, lty = 2)
#  }
#  if (i%%9 == 0 | i == NROW(std.errs2)) {
#    par(mfrow = c(1,1))
#    dev.off()
#  }
#}
#
##################################################
##### Plotting std errors for SSB ################
##################################################
#
#Nyears <- length(true.reps[[1]]$recruit$spawn_bio[which(true.reps[[1]]$recruit$spawn_bio!=0)])-foreyears
#SPB.err <- array(dim = c(Nboot,Nyears,2))
#med.SSB1 <- vector(length=Nyears)
#CI50.SSB1 <- vector(length=(Nyears*2))
#CI90.SSB1 <- vector(length=(Nyears*2))
#med.SSB2 <- vector(length=Nyears)
#CI50.SSB2 <- vector(length=(Nyears*2))
#CI90.SSB2 <- vector(length=(Nyears*2))
#
#for (i in 1:Nboot) {
#  true.SSB <- true.reps[[i]]$recruit$spawn_bio[which(true.reps[[i]]$recruit$spawn_bio!=0)][1:Nyears]
#  sim.SSB1 <- EM1.reps[[i]]$recruit$spawn_bio[1:Nyears]
#  sim.SSB2 <- EM2.reps[[i]]$recruit$spawn_bio[1:Nyears]
#  SPB.err[i,,1] <- (sim.SSB1-true.SSB) / true.SSB
#  SPB.err[i,,2] <- (sim.SSB2-true.SSB) / true.SSB
#}
#for (j in 1:Nyears) {
#  med.SSB1[j] <- median(SPB.err[,j,1])
#  CI50.SSB1[j] <- quantile(SPB.err[,j,1],probs=0.25)
#  CI50.SSB1[(Nyears*2+1)-j] <- quantile(SPB.err[,j,1],probs=0.75)
#  CI90.SSB1[j] <- quantile(SPB.err[,j,1],probs=0.05)
#  CI90.SSB1[(Nyears*2+1)-j] <- quantile(SPB.err[,j,1],probs=0.95)
#
#  med.SSB2[j] <- median(SPB.err[,j,2])
#  CI50.SSB2[j] <- quantile(SPB.err[,j,2],probs=0.25)
#  CI50.SSB2[(Nyears*2+1)-j] <- quantile(SPB.err[,j,2],probs=0.75)
#  CI90.SSB2[j] <- quantile(SPB.err[,j,2],probs=0.05)
#  CI90.SSB2[(Nyears*2+1)-j] <- quantile(SPB.err[,j,2],probs=0.95)
#}
#
#StartYr <- min(true.reps[[1]]$recruit$year)
#EndYr <- max(true.reps[[1]]$recruit$year)-foreyears
#
##################################################
##### Plotting std errors for Recruitment ########
##################################################
#
#Nyears <- length(true.reps[[1]]$recruit$exp_recr[which(true.reps[[1]]$recruit$exp_recr!=0)])-foreyears
#Rec.err <- array(dim=c(Nboot, Nyears, 2))
#med.Rec1 <- vector(length=Nyears)
#CI50.Rec1 <- vector(length=(Nyears*2))
#CI90.Rec1 <- vector(length=(Nyears*2))
#
#med.Rec2 <- vector(length=Nyears)
#CI50.Rec2 <- vector(length=(Nyears*2))
#CI90.Rec2 <- vector(length=(Nyears*2))
#
#for (i in 1:Nboot) {
#  true.Rec <- true.reps[[i]]$recruit$exp_recr[which(true.reps[[i]]$recruit$exp_recr!=0)][1:Nyears]
#  sim.Rec1 <- EM1.reps[[i]]$recruit$exp_recr[1:length(true.Rec)]
#  sim.Rec2 <- EM2.reps[[i]]$recruit$exp_recr[1:length(true.Rec)]
#  Rec.err[i,,1] <- (sim.Rec1-true.Rec) / true.Rec
#  Rec.err[i,,2] <- (sim.Rec2-true.Rec) / true.Rec
#}
#for (j in 1:length(true.Rec)) {
#  med.Rec1[j] <- median(Rec.err[,j,1])
#  CI50.Rec1[j] <- quantile(Rec.err[,j,1],probs=0.25)
#  CI50.Rec1[(Nyears*2+1)-j] <- quantile(Rec.err[,j,1],probs=0.75)
#  CI90.Rec1[j] <- quantile(Rec.err[,j,1],probs=0.05)
#  CI90.Rec1[(Nyears*2+1)-j] <- quantile(Rec.err[,j,1],probs=0.95)
#
#  med.Rec2[j] <- median(Rec.err[,j,2])
#  CI50.Rec2[j] <- quantile(Rec.err[,j,2],probs=0.25)
#  CI50.Rec2[(Nyears*2+1)-j] <- quantile(Rec.err[,j,2],probs=0.75)
#  CI90.Rec2[j] <- quantile(Rec.err[,j,2],probs=0.05)
#  CI90.Rec2[(Nyears*2+1)-j] <- quantile(Rec.err[,j,2],probs=0.95)
#}
#
#StartYr <- min(true.reps[[1]]$recruit$year)
#EndYr <- max(true.reps[[1]]$recruit$year)-foreyears
#
#
##################################################
##### Plotting all SSB and Recruitment ########
##################################################
#dev.new()
#par(mfrow=c(2,2), mar = c(2,2,2,1), oma = c(1,1,1,1))
#
#plot(0, xlim=c(StartYr, EndYr), ylim=c(-1,1),xaxs="i", ylab = "Relative Error", xlab = "")
#polygon(y=CI90.SSB1,x=c(StartYr:EndYr,EndYr:StartYr), col = "gray60")
#polygon(y=CI50.SSB1,x=c(StartYr:EndYr,EndYr:StartYr),col="gray75")
#lines(y=med.SSB1, x = c(StartYr:EndYr), type = "l", col="black", lwd=2)
#abline(h=0, col="red")
#abline(v = 2009, col = "blue", lty = 2)
##mtext(paste("EM1 SSB Mean Relative Error = ", round(mean(med.SSB1), digits = 3), sep = ""), side = 3)
##mtext("Spawning Stock Biomass", side = 3, padj = -3)
##mtext("Scenario 3", side = 2, padj = -6, xpd = NA)
#
#plot(0, xlim=c(StartYr, EndYr), ylim=c(-1,1),xaxs="i", ylab = "Relative Error", xlab = "")
#polygon(y=CI90.SSB2,x=c(StartYr:EndYr,EndYr:StartYr), col = "gray60")
#polygon(y=CI50.SSB2,x=c(StartYr:EndYr,EndYr:StartYr),col="gray75")
#lines(y=med.SSB2, x = c(StartYr:EndYr), type = "l", col="black", lwd=2)
#abline(h=0, col="red")
#abline(v = 2009, col = "blue", lty = 2)
##mtext(paste("EM2 SSB Mean Relative Error = ", round(mean(med.SSB2), digits = 3), sep = ""), side = 3)
##mtext("Scenario 4", side = 2, padj = -6, xpd = NA)
#
#plot(0, xlim=c(StartYr, EndYr), ylim=c(-1,1),xaxs="i", ylab = "Relative Error", xlab = "")
#polygon(y=CI90.Rec1,x=c(StartYr:EndYr,EndYr:StartYr), col = "gray60")
#polygon(y=CI50.Rec1,x=c(StartYr:EndYr,EndYr:StartYr),col="gray75")
#lines(y=med.Rec1, x = c(StartYr:EndYr), type = "l", col="black", lwd=2)
#abline(h=0, col="red")
#abline(v = 2009, col = "blue", lty = 2)
##mtext(paste("EM1 Rec Mean Relative Error = ", round(mean(med.Rec1), digits = 3), sep = ""), side = 3)
##mtext("Recruitment", side = 3, padj = -3)
#
#plot(0, xlim=c(StartYr, EndYr), ylim=c(-1,1),xaxs="i", ylab = "Relative Error", xlab = "")
#polygon(y=CI90.Rec2,x=c(StartYr:EndYr,EndYr:StartYr), col = "gray60")
#polygon(y=CI50.Rec2,x=c(StartYr:EndYr,EndYr:StartYr),col="gray75")
#lines(y=med.Rec2, x = c(StartYr:EndYr), type = "l", col="black", lwd=2)
#abline(h=0, col="red")
#abline(v = 2009, col = "blue", lty = 2)
##mtext(paste("EM2 Rec Mean Relative Error = ", round(mean(med.Rec2), digits = 3), sep = ""), side = 3)
#
#par(mfcol=c(1,1))
#
##################################################
##### Plotting std errors for Growth Param #######
##################################################
#
#modNyears <- length(true.reps[[1]]$MGparmAdj$Year)
#modStartYr <- min(true.reps[[1]]$MGparmAdj$Year)
#modEndYr <- max(true.reps[[1]]$MGparmAdj$Year)
#sim.pars <- array(dim = c(Nboot,modNyears,length(par2change),2))
#true.pars <- array(dim = c(Nboot,modNyears,length(par2change)))
#med.sim.pars <- array(dim = c(length(par2change),modNyears,2))
#med.true.pars <- matrix(nrow = length(par2change), ncol = modNyears)
#
#parnames <- unlist(strsplit(par2change, split = "_GP_1"))
#
#dev.new()
#par(mfrow = c(length(par2change),2), mar = c(2,2,2,2))
#for(parm in 1:length(par2change)) {
#  StartYr <- min(true.reps[[1]]$recruit$year)
#  EndYr <- max(true.reps[[1]]$recruit$year)
#  par.err <- matrix(nrow=Nboot, ncol = modNyears)
#  med.par <- vector(length=modNyears)
#  CI50.par <- vector(length=(modNyears*2))
#  CI90.par <- vector(length=(modNyears*2))
#  for(run in 1:2) {
#    for (i in 1:Nboot) {
#      true.pars[i,,parm] <- eval(parse(text=paste("true.reps[[i]]$MGparmAdj$",par2change,sep = "")[parm]))
#      sim.pars[i,,parm,run] <- eval(parse(text=paste("EM",run,".reps[[i]]$MGparmAdj$",par2change,sep = "")[parm]))
#      par.err[i,] <- sim.pars[i,,parm,run]-true.pars[i,,parm]
#    }
#    for (j in 1:modNyears) {
#      med.par[j] <- median(par.err[,j])
#      CI50.par[j] <- quantile(par.err[,j],probs=0.25)
#      CI50.par[(modNyears*2+1)-j] <- quantile(par.err[,j],probs=0.75)
#      CI90.par[j] <- quantile(par.err[,j],probs=0.05)
#      CI90.par[(modNyears*2+1)-j] <- quantile(par.err[,j],probs=0.95)
#    }
#    #plot(0, xlim=c(modStartYr, modEndYr), ylim=c(-0.5,0.5),xaxs="i",ylab="Absolute Error", xlab = "Year")
#  if(parm %in% grep("K", par2change)) {plot(0, xlim=c(modStartYr, modEndYr), ylim=c(-0.1,0.1),xaxs="i",ylab="", xlab = "")}
#  if(parm %in% grep("L_at_Amax", par2change)) {plot(0, xlim=c(modStartYr, modEndYr), ylim=c(-2.5,2.5),xaxs="i",ylab="", xlab = "")}
#  polygon(y=CI90.par,x=c(modStartYr:modEndYr,modEndYr:modStartYr), col = "gray25")
#  polygon(y=CI50.par,x=c(modStartYr:modEndYr,modEndYr:modStartYr),col="gray50")
#  lines(y=med.par, x = c(modStartYr:modEndYr), type = "l", col="white", lwd=2)
#  abline(h=0, col="red")
#  #mtext(paste("EM ", run,": ", parnames[parm], sep = ""), side = 3)
#
#  for(year in 1:modNyears) {
#    med.sim.pars[parm,year,run] <- median(sim.pars[,year,parm,run])
#    med.true.pars[parm,year] <- median(true.pars[,year,parm])
#  }
#  }
#}
#par(mfcol = c(1,1))
#
#
##################################################
##### Plotting Estimates of Growth Param #########
##################################################
##dev.new()
##par(mfrow = c(length(par2change),3))
##for(parm in 1:length(par2change)) {
## ylims <- range(c(med.true.pars[parm,],med.sim.pars[parm,,1],med.sim.pars[parm,,2]))
## ylims[1] <- ylims[1]-.05
## ylims[2] <- ylims[2]+.05
## plot(med.true.pars[parm,], x = true.reps[[1]]$MGparmAdj$Year, xlab = "Year", type = "l", main = paste("OM Values of ", parnames[parm]),
##   ylim = ylims)
## plot(med.sim.pars[parm,,1], x = true.reps[[1]]$MGparmAdj$Year, xlab = "Year", type = "l", main = paste("EM 1 Median Values of ", parnames[parm]),
##   ylim = ylims)
## plot(med.sim.pars[parm,,2], x = true.reps[[1]]$MGparmAdj$Year, xlab = "Year", type = "l", main = paste("EM 2 Median Values of ", parnames[parm]),
##   ylim = ylims)
##}
##par(mfrow = c(1,1))
#
##################################################
##### Plotting Estimates of Link Param ###########
##################################################
#
#if(as.numeric(addormult)>0) {envtype <- "mult"}
#if(as.numeric(addormult)<0) {envtype <- "add"}
#envparnames <- paste(par2change, "_ENV_", envtype, sep = "")
#envpar.mat <- matrix(nrow = length(envparnames), ncol = Nboot)
#
#dev.new()
#par(mfrow = c(length(par2change),1), mar = c(4,4,2,2))
#for(parm in 1:length(envparnames)) {
#  trueval <- true.reps[[1]]$parameters[which(true.reps[[1]]$parameters$Label==envparnames[parm]),]$Value
#  if(length(trueval)==0) {trueval <- 0}
#  for(boot in 1:Nboot) {
#    envpar.mat[parm,boot] <- EM2.reps[[boot]]$parameters[which(EM2.reps[[boot]]$parameters$Label==envparnames[parm]),]$Value
#  }
#  #hist(envpar.mat[parm,], main = paste("Estimates of ", envparnames[parm], sep = ""), xlab = "")
#  hist(envpar.mat[parm,], main = "", xlab = "")
#  abline(v = trueval, col = "green", lwd = 2)
#  abline(v = median(envpar.mat[parm,]), col = "red", lwd = 2)
#}
#par(mfrow = c(1,1))
#
##################################################
##### Plotting Estimates of Growth Param ###########
##################################################
#
#growthpar.mat <- array(dim = c(length(par2change), Nboot, 2))
#growtherr.mat <- array(dim = c(length(par2change), Nboot, 2))
#dev.new()
#par(mfrow = c(length(par2change),2), mar = c(4,4,2,2))
#for(parm in 1:length(par2change)) {
#  trueval <- true.reps[[1]]$parameters[which(true.reps[[1]]$parameters$Label==par2change[parm]),]$Value
#  if(length(trueval)==0) {trueval <- 0}
#  for(boot in 1:Nboot) {
#    growthpar.mat[parm,boot,1] <- EM1.reps[[boot]]$parameters[which(EM1.reps[[boot]]$parameters$Label==par2change[parm]),]$Value
#    growthpar.mat[parm,boot,2] <- EM2.reps[[boot]]$parameters[which(EM2.reps[[boot]]$parameters$Label==par2change[parm]),]$Value
#  }
#  hist(growthpar.mat[parm,,1], main = paste("EM1: ", par2change[parm], sep = ""), xlab = "")
#  abline(v = trueval, col = "green", lwd = 3)
#  abline(v = median(growthpar.mat[parm,,1]), col = "red", lwd = 2)
#
#  hist(growthpar.mat[parm,,2], main = paste("EM2: ", par2change[parm], sep = ""), xlab = "")
#  abline(v = trueval, col = "green", lwd = 3)
#  abline(v = median(growthpar.mat[parm,,2]), col = "red", lwd = 2)
#}
#par(mfrow = c(1,1))
#
#dev.new()
#par(mfrow = c(length(par2change),2), mar = c(4,4,2,2))
#for(parm in 1:length(par2change)) {
#  trueval <- true.reps[[1]]$parameters[which(true.reps[[1]]$parameters$Label==par2change[parm]),]$Value
#  if(length(trueval)==0) {trueval <- 0}
#  for(boot in 1:Nboot) {
#    growtherr.mat[parm,boot,1] <- (growthpar.mat[parm,boot,1] - trueval) / trueval
#    growtherr.mat[parm,boot,2] <- (growthpar.mat[parm,boot,2] - trueval) / trueval
#  }
#  hist(growtherr.mat[parm,,1], main = paste("Rel Error EM1: ", par2change[parm], sep = ""), xlab = "", xlim = c(-.5, .5))
#  abline(v = 0, col = "green", lwd = 3)
#  abline(v = median(growtherr.mat[parm,,1]), col = "red", lwd = 2)
#
#  hist(growtherr.mat[parm,,2], main = paste("Rel Error EM2: ", par2change[parm], sep = ""), xlab = "", xlim = c(-.5, .5))
#  abline(v = 0, col = "green", lwd = 3)
#  abline(v = median(growtherr.mat[parm,,2]), col = "red", lwd = 2)
#}
#par(mfrow = c(1,1))
#
#
##################################################
##### Plotting single run
##################################################
#dev.new()
#EndYr <- max(true.reps[[1]]$recruit$year)-foreyears
#par(mar = c(5,5,2,1))
#
#plot(0, xlim=c(StartYr, EndYr), ylim=c(0, (max(true.SSB, sim.SSB1, sim.SSB2) / 1E9)+.1), 
#  ylab = expression(paste("Spawning Stock Biomass (", "10"^"8", "mt)", sep = "")),
#  xlab = "Year")
#lines(y=true.SSB / 1E9, x = c(StartYr:EndYr), type = "l", col="blue", lwd=2)
#lines(y=sim.SSB1 / 1E9, x = c(StartYr:EndYr), type = "l", col="red", lwd=2)
#lines(y=sim.SSB2 / 1E9, x = c(StartYr:EndYr), type = "l", col="black", lwd=2)
#abline(h = 0, col = "grey", lwd = .5)
#legend(x = 1905, y = 0.7, lty = 1, col = c("blue","red", "black"),
#  legend = c("OM", "EM-1", "EM-2"), lwd = 2)
#
#
##################################################
##### Calculating values for decision table
##################################################
#
#lost.yield.1 <- NULL
#lost.yield.2 <- NULL
#
#yield.prop.1 <- NULL
#yield.prop.2 <- NULL
#
#true.Bratio <- NULL
#Bratio.1 <- NULL
#Bratio.2 <- NULL
#
#Bfinal.1 <- NULL
#Bfinal.2 <- NULL
#B.diff.1 <- NULL
#B.diff.2 <- NULL
#
#sumtrueyield <- 0
#for(i in 1:Nboot) {
#  true.yield <- true.reps[[i]]$derived_quant[grep(paste("ForeCatchret_", modEndYr+1, sep = ""), true.reps[[i]]$derived_quant[,1]),2]
#  yield.1 <- reps.ctl1[[i]]$derived_quant[grep(paste("ForeCatchret_", modEndYr+1, sep = ""), reps.ctl1[[i]]$derived_quant[,1]),2]
#  yield.2 <- reps.ctl2[[i]]$derived_quant[grep(paste("ForeCatchret_", modEndYr+1, sep = ""), reps.ctl2[[i]]$derived_quant[,1]),2]
#
#  lost.yield.1[i] <- yield.1 - true.yield
#  lost.yield.2[i] <- yield.2 - true.yield
#  sumtrueyield <- sumtrueyield + true.yield
#  yield.prop.1[i] <- lost.yield.1[i] / true.yield
#  yield.prop.2[i] <- lost.yield.2[i] / true.yield
#
#  true.Bratio[i] <- rev(true.reps[[i]]$derived_quant[grep("Bratio", true.reps[[i]]$derived_quant[,1]),2])[1]
#  Bratio.1[i] <- rev(reps.ctl1[[i]]$derived_quant[grep("Bratio", reps.ctl1[[i]]$derived_quant[,1]),2])[1]
#  Bratio.2[i] <- rev(reps.ctl2[[i]]$derived_quant[grep("Bratio", reps.ctl2[[i]]$derived_quant[,1]),2])[1]
#
#  true.B <- rev(true.reps[[i]]$derived_quant[grep("SPB",true.reps[[i]]$derived_quants$LABEL),2])[1]
#  Bfinal.1[i] <- rev(reps.ctl1[[i]]$derived_quant[grep("SPB",reps.ctl1[[i]]$derived_quants$LABEL),2])[1]
#  Bfinal.2[i] <- rev(reps.ctl2[[i]]$derived_quant[grep("SPB",reps.ctl2[[i]]$derived_quants$LABEL),2])[1]
#
#  B.diff.1[i] <- Bfinal.1[i] - true.B
#  B.diff.2[i] <- Bfinal.2[i] - true.B
#}
#
#median(lost.yield.1)
#median(lost.yield.2)
#
#median(yield.prop.1)
#median(yield.prop.2)
#
#range(yield.prop.1)
#range(yield.prop.2)
#
#sd(yield.prop.1)
#sd(yield.prop.2)
#
#median(Bratio.1)
#median(Bratio.2)
#
#length(which(true.Bratio <= 0.25)) / Nboot
#length(which(Bratio.1 <= 0.25)) / Nboot
#length(which(Bratio.2 <= 0.25)) / Nboot
#
#median(B.diff.1)
#median(B.diff.2)
#
#median(true.Bratio)
#
#