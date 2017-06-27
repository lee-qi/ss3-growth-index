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
dir.pres <- "OM 1 No Env Hybrid par from Linf Mult" #Change to whichever model you're working in
dir.rep <- "Reports"


setwd(dir.pres)

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

#########################################
#### Modifying data file
#### Getting rid of ageing error
#########################################

dat.file <- SS_readdat(file.dat)
Nfleets <- dat.file$Nfleet
Nareas <- dat.file$N_areas
endyr <- dat.file$endyr

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
  file.start.new <- "starter.ss_new"
  setwd(dir.new)

  file.start <- "starter.ss"
  
  
  #########################################
  #### Creating new directories for runs
  #########################################
  for(j in 1:2) {
    dir.res <- paste("EM",j, sep = "-")
    dir.bias <- paste("Bias_Bootstrap-", i, sep = "")
    setwd(file.path(dir.check,dir.pres,dir.new, dir.res, dir.bias))

    ###########################################
    #### Retrieving MSY from rep file
    #### ?Not sure how to deal with allocations?
    ###########################################

    Years <- seq(from = (endyr+1), length = foreyears, by = 1) #?Find start year for forecasting
    forefile <- readLines(file.path(getwd(), "Forecast-report.sso"))
    MSYnum <- grep("MSY_retain", forefile)
    MSY <- as.numeric(strsplit(forefile[MSYnum], split = " ")[[1]][2])
    catch.table <- cbind(Years, rep(1, foreyears), rep(1, foreyears), MSY)
    colnames(catch.table) <- c("Year", "Seas", "Fleet", "Catch_or_F")
    
    ##########################################
    ### New OM Folder
    ##########################################

    dir.fin <- paste("ProjectCatch-", j, sep = "")
    dir.create(dir.fin)
    mapply(file.copy, to = dir.fin, overwrite = TRUE,
                 MoreArgs = list(from = file.path(dir.check, dir.pres, dir.new, 
                  c(file.ctl, file.dat, file.fore, file.start, file.par, file.exe))))

    setwd(dir.fin)

    ##########################################
    ### Change Forecast file
    ##########################################

    fore.file <- SS_readforecast(file.fore, Nfleets = Nfleets, Nareas = Nareas)
    fore.file$Ncatch <- foreyears * Nfleets
    fore.file$InputBasis <- 3
    fore.file$Forecast <- 5       #Set to input catch
    fore.file$ForeCatch <- data.frame(catch.table)

    SS_writeforecast(fore.file, overwrite = TRUE)

    ##########################################
    ### Run OM with new Fmsy
    ##########################################

    system(file.exe, show.output.on.console = FALSE)

#Env-end

  }
}


