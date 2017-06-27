#setwd("/Users/LeeQi/Google Drive/SS3/Splitnose/Testing")
setwd("C:/Users/Lee Qi/Desktop/SS3/Splitnose/Testing")
#setwd("E:/SS3/Splitnose/Testing")

dir.check <- getwd()
if (!grepl(basename(dir.check), "splitnose", ignore.case = TRUE)) {
  stop(paste("Change your working directory!"))
}

require(r4ss)
require(foreach)
require(doParallel)

registerDoParallel(7)
dir.pres <- "AR" #Change to whichever model you're working in
dir.rep <- "Reports"

file.index <- "splitnose_master_chronology.csv"
env.index <- read.csv(file.index, header = TRUE)
N_envobs <- NROW(env.index)
Gparams <- arima(ts(env.index$splitnose_master_chronology_normalized), order = c(1,0,0))
sigG <- Gparams$sigma2
#G.AR <- as.numeric(Gparams$coef[1])
G.AR <- 0.95

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

orig.ctl.2 <- orig.ctl
writeLines(orig.ctl.2, ctl.2)

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
#betasetup <- strsplit(orig.ctl[line.betasetup], split = " ")[[1]]
#betasetup <- betasetup[-(1:2)]

#For beta == 0
#collapse.beta <- paste(betasetup, collapse = " ")
#beta.table <- collapse.beta

#if(length(par2change) > 1) {
#  for(i in 2:length(par2change)){
#    beta.table <- c(beta.table, collapse.beta)
#  }
#}

#orig.ctl.1 <- c(orig.ctl[1:(line.betasetup - 1)], beta.table, orig.ctl[(line.betasetup + 1):length(orig.ctl)])

writeLines(orig.ctl, ctl.1)

#For beta == estimated
#parnums <- which(betasetup!="")
#betasetup[parnums[7]] <- "5"            #Phase of beta estimation
#collapse.beta <- paste(betasetup, collapse = " ")
#beta.table <- collapse.beta
#
##if(length(par2change) > 1) {
##  for(i in 2:length(par2change)){
##    beta.table <- c(beta.table, collapse.beta)
##  }
##}
#
#orig.ctl.2 <- c(orig.ctl[1:(line.betasetup - 1)], beta.table, orig.ctl[(line.betasetup + 1):length(orig.ctl)])
#
#writeLines(orig.ctl.2, ctl.2)

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
  
  #########################################
  #### Projecting the truth
  #########################################
  dir.OM <- "OM Proj-Catch"
  dir.create(dir.OM)

  ignore <- mapply(file.copy, to = dir.OM,
                   MoreArgs = list(from = c(
                  file.par, file.start, file.dat, file.ctl, file.fore, file.exe), overwrite = TRUE))
  
  setwd(dir.OM)
  par.file <- readLines(file.par)
  for(Gpar in 1:length(par2change)) {
    Gparam.line <- grep("MGparm", par.file)[24+Gpar]+1
    par.file[Gparam.line] <- "0"
  }

  writeLines(par.file, file.par)

  system(paste(exe.cmd, " -nohess", sep = ""), show.output.on.console = FALSE)
  
  Years <- seq(from = (endyr+1), length = foreyears, by = 1) #?Find start year for forecasting
  repfile <- readLines(file.path(getwd(), "Report.sso"))

  MSYnum <- grep("RetYield_MSY", repfile)
  proj.catch <- as.numeric(strsplit(repfile[MSYnum], split = " ")[[1]][3])

  catch.table <- cbind(Years, rep(1, foreyears), rep(1, foreyears), proj.catch)
  colnames(catch.table) <- c("Year", "Seas", "Fleet", "Catch_or_F")

  dir.temp <- file.path(dir.check,dir.pres,dir.new, "Temp")
  dir.create(dir.temp)

  mapply(file.copy, to = dir.temp, overwrite = TRUE,
               MoreArgs = list(from = list.files(getwd())))

  unlink(list.files(getwd()))
  ignore <- mapply(file.copy, to = getwd(),
                   MoreArgs = list(from = file.path(dir.check, dir.pres, dir.new, c(
                  file.par, file.start, file.dat, file.ctl, file.fore, file.exe)), overwrite = TRUE))

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
#    ###########################################
#    #### Retrieving Fmsy from rep file
#    #### ?Not sure how to deal with allocations?
#    ############################################

#    #rep.file <- SS_output(dir = getwd(), verbose = F)
#    #Fmsy <- rep.file$derived_quants[rep.file$derived_quants[,1] == "Fstd_MSY", 2]
#    Years <- seq(from = (endyr+1), length = foreyears, by = 1) #?Find start year for forecasting
#    forefile <- readLines(file.path(getwd(), "Forecast-report.sso"))
#    Fnums <- grep("Fmult", forefile)
#    Fmult_MSY <- as.numeric(strsplit(forefile[Fnums[6]], split = " ")[[1]][2])
#    catch.table <- cbind(Years, rep(1, foreyears), rep(1, foreyears), Fmult_MSY)
#    colnames(catch.table) <- c("Year", "Seas", "Fleet", "Catch_or_F")
#    
#    ##########################################
#    ### New OM Folder
#    ###########################################

#    dir.fin <- paste("Projection-", j, sep = "")
#    dir.create(dir.fin)
#    mapply(file.copy, to = dir.fin, overwrite = TRUE,
#                 MoreArgs = list(from = file.path(dir.check, dir.pres, dir.new, 
#                  c(file.ctl, file.dat, file.fore, file.start, file.par, file.exe)#)))

#    setwd(dir.f#in)

#    ##########################################
#    ### Change Forecast file
#    ###########################################

#    fore.file <- SS_readforecast(file.fore, Nfleets = Nfleets, Nareas = Nareas)
#    fore.file$Ncatch <- foreyears * Nfleets
#    fore.file$InputBasis <- 99
#    fore.file$Forecast <- 5       #Set to input catch
#    fore.file$ForeCatch <- data.frame(catch.tab#le)

#    SS_writeforecast(fore.file, overwrite = TRUE)

     
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
    else {
      dir.temp <- file.path(dir.check, dir.pres, dir.new, dir.res, dir.bias, "Temp")
      dir.create(dir.temp)
      mapply(file.copy, to = dir.temp, overwrite = TRUE,
                 MoreArgs = list(from = list.files(getwd())))

      unlink(list.files(getwd()))
    }

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

#Env-end


    ###########################################
    ### Retrieving final report file
    ###########################################

    rep.name <- paste("Boot", i, "-EM", j,".sso", sep = "")
    file.copy(from = "Report.sso", to = file.path(dir.check,dir.pres,dir.rep,rep.name))

  }
}


#########################################
#### Comparing results
#########################################

setwd(dir.rep)

source("C:/Users/Lee Qi/Google Drive/SS3/Splitnose/Testing/Plots.R")

