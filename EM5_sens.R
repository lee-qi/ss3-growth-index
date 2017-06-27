#setwd("/Users/LeeQi/Google Drive/SS3/Splitnose/Testing")
setwd("C:/Users/Lee Qi/Desktop/SS3/Splitnose/Testing")
#setwd("E:/SS3/Splitnose/Testing")
#setwd("E:/Splitnose")

dir.check <- getwd()
if (!grepl(basename(dir.check), "splitnose", ignore.case = TRUE)) {
  stop(paste("Change your working directory!"))
}

require(r4ss)
require(foreach)
require(doParallel)

registerDoParallel(7)
dir.pres <- "OM 5" #Change to whichever model you're working in
dir.rep <- "Reports"

file.index <- "splitnose_master_chronology.csv"
env.index <- read.csv(file.index, header = TRUE)
#N_envobs <- NROW(env.index)
Gparams <- arima(ts(env.index$splitnose_master_chronology_normalized), order = c(1,0,0))
sigG <- Gparams$sigma2
G.AR <- as.numeric(Gparams$coef[1])
G.AR <- 0.95

errvec <- c(0.2, 0.4, 0.6, 0.8)

setwd(dir.pres)
#dir.create(dir.rep, showWarnings = FALSE)

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

runvec <- 0
for(i in 1:Nboot) {
  repname <- paste0("Boot", i, "-EM", 2,".sso")
  if(!file.exists(file.path("E:/Splitnose/OM 5 Beta 0.3 AR 0.95", dir.rep,repname))) {next}
  else{
    runvec <- c(runvec, i)
  }
}
runvec <- runvec[-1]

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

#SS_writedat(dat.file, outfile = file.dat.new, overwrite = TRUE)

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

orig.ctl.2 <- orig.ctl

#Finding growth parameter
for (param in par2change) {
  line.param <- grep(param, orig.ctl)
  growth <- strsplit(orig.ctl[line.param], split = " ")[[1]]
  parnums <- which(growth!="")
  growth[parnums[8]] <- 0        #env-var column
  orig.ctl[line.param] <- paste(growth, collapse = " ")
}

#Finding link parameter
line.beta <- grep("#custom_MG-env_setup", orig.ctl)
#EnvSetup <- strsplit(orig.ctl[line.beta], split = " ")[[1]]
orig.ctl[line.beta] <- paste("#", orig.ctl[line.beta], collapse = " ")

line.betasetup <- line.beta+1
orig.ctl[line.betasetup] <- paste("#", orig.ctl[line.betasetup], collapse = " ")

#Env-end


#########################################
#### Modifying starter file
#########################################
 
start.file <- SS_readstarter(file.start)
start.file$init_values_src <- 1 #Use par file instead of inits in ctl
start.file$N_bootstraps <- Nboot + 2 #Set Bootstrap number
start.file$last_estimation_phase <- 0
 
#SS_writestarter(start.file, file = file.start.new, overwrite = TRUE)

#for(err in length(errvec):1) {
err <- 2
foreach(i=1:Nboot) %dopar% {
  setwd(file.path(dir.check,dir.pres))
  seed <- 12345*i
  set.seed(seed)
  require(r4ss)

  dir.new <- paste("Bootstrap",i, sep = "-")
  dir.OM <- "OM Proj-Catch"
  setwd(dir.new)
#  if(!dir.exists("EM-2")) {next}

  ##########################################
  ### Changing env index to white noise
  ##########################################

  j <- paste(5, errvec[err], sep = "-")
  dir.res <- paste("EM",j, sep = "-")
  if(dir.exists(dir.res)) {
    unlink(dir.res, recursive = T)
    unlink(file.path(dir.check, dir.pres, dir.rep, paste("Boot", i, "-EM", j,".sso", sep = "")))}
  cr.res <- dir.create(dir.res, showWarnings = FALSE)
  
  dat.name <- paste("boot-dat",i,".ss",sep="")
  ctl.name <- "control-2.ss"
                  
  move.dat <- file.copy(to = dir.res, from = dat.name, overwrite = TRUE)

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

  #########################################
  #### Modifying dat file
  #########################################

  dat.file <- SS_readdat(file.dat)
  dat.file$N_environ_variables <- 1
  EnvYrs <- seq(from = (styr+1), to = (endyr + foreyears), by = 1)
  N_envobs <- length(EnvYrs)
  dat.file$N_environ_obs <- N_envobs
  
  AR.index <- rnorm(N_envobs, mean = ((-sigG/2)*(1-G.AR)/sqrt(1-G.AR^2)), sd = sqrt(sigG))
  for(t in 2:N_envobs) {
    AR.index[t] <- G.AR*AR.index[t-1] + AR.index[t]*sqrt(1-(G.AR^2))
  }

  new.AR.index <- (dat.file$envdat$Value * sqrt(errvec[err])) + (sqrt(1-errvec[err]) * AR.index)
  new.AR.index <- new.AR.index - mean(new.AR.index)

  dat.file$envdat$Value <- new.AR.index

  SS_writedat(dat.file, file.dat, overwrite = TRUE)

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

   ##########################################
    ### New OM Folder
    ##########################################

    dir.fin <- paste("Projection-", j, sep = "")
    dir.create(dir.fin)

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

    ###########################################
    #### Retrieving MSY from rep file
    ###########################################

    repfile <- readLines(file.path(getwd(), "Report.sso"))

    MSYnum <- grep("RetYield_MSY", repfile)
    proj.catch <- as.numeric(strsplit(repfile[MSYnum], split = " ")[[1]][3])
    Years <- seq(from = (endyr+1), length = foreyears, by = 1) #?Find start year for forecasting
  
    new.catch.table <- cbind(Years, rep(1, foreyears), rep(1, foreyears), proj.catch)

    dir.temp <- file.path(dir.check, dir.pres, dir.new, dir.res, dir.bias, "Temp")
    dir.create(dir.temp)
    mapply(file.copy, to = dir.temp, overwrite = TRUE,
               MoreArgs = list(from = list.files(getwd())))

    unlink(list.files(getwd()))

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


    ###########################################
    ### Retrieving final report file
    ###########################################

    rep.name <- paste("Boot", i, "-EM", j,".sso", sep = "")
    file.copy(from = "Report.sso", to = file.path(dir.check,dir.pres,dir.rep,rep.name))

}

}
stopCluster(7)