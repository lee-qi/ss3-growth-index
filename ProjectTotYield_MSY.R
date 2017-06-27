setwd("C:/Users/Lee Qi/Desktop/SS3/Splitnose/Testing")
dir.check <- getwd()

require(r4ss)
require(foreach)
require(doParallel)

registerDoParallel(4)
dir.pres <- "New Master" #Change to whichever model you're working in
dir.rep <- "Reports-TotYield_MSY"

setwd(dir.pres)
dir.create(dir.rep)
Nboot <- 56 #Set number of bootstraps
foreyears <- 50 #Set number of years we want to forecast
file.dat <- "data.ss" 
file.dat.new <- "data.ss_new"

file.fore <- "forecast.ss"
file.fore.new <- "forecast-new.ss"

file.ctl <- "control.ss"

file.par <- "ss3.par"
file.start.new <- "starter.ss_new"

file.start <- "starter.ss"

dat.file <- SS_readdat(file.dat)
Nfleets <- dat.file$Nfleet
Nareas <- dat.file$N_areas
endyr <- dat.file$endyr

foreach(i=1:Nboot) %dopar% {
  setwd(file.path(dir.check,dir.pres))

  require(r4ss)

  dir.new <- paste("Bootstrap",i, sep = "-")

  setwd(dir.new)

  for(j in 1:2) {
    setwd(file.path(dir.check,dir.pres, dir.new))
  	dir.res <- paste("Results",j, sep = "-")
  	setwd(dir.res)

  	dir.bias <- paste("Bias_Bootstrap-", i, sep = "")
  	setwd(dir.bias)

	  dir.fin <- paste("Projection-TotYield_MSY-", j, sep = "")

  	dir.create(dir.fin)
  	mapply(file.copy, to = dir.fin, overwrite = TRUE,
            MoreArgs = list(from = c(file.path(getwd(), c(file.fore, file.start, "SS3.exe")),
                                     file.path(dir.check, dir.pres, dir.new, c(file.ctl, file.dat)))))

	  rep.file <- SS_output(dir = getwd(), verbose = F, printstats = FALSE)
    fore.catch <- rep.file$derived_quants[rep.file$derived_quants[,1] == "TotYield_MSY",2]
    Years <- seq(from = (endyr+1), length = foreyears, by = 1) #?Find start year for forecasting
   	
    #OMrep <- SS_output(dir = file.path(dir.check, dir.pres, dir.new), covar = FALSE)
    #CurrSPB <- OMrep$derived_quants[OMrep$derived_quants[,1] == paste("SPB_", endyr, sep = ""),2]

    #fore.catch <- Fmsy * CurrSPB
    catch.table <- cbind(Years, rep(1, foreyears), rep(1, foreyears), fore.catch)
   	colnames(catch.table) <- c("Year", "Seas", "Fleet", "Catch_or_F")
    
    setwd(dir.fin)

    ##########################################
    ### Change Forecast file
    ### Can explore different options here
    ##########################################

    fore.file <- SS_readforecast(file.fore, Nfleets = Nfleets, Nareas = Nareas)
    fore.file$Ncatch <- foreyears * Nfleets
    fore.file$InputBasis <- 3
    fore.file$Forecast <- 5       #Set to input catch
    fore.file$ForeCatch <- data.frame(catch.table)

    SS_writeforecast(fore.file, overwrite = TRUE)

    #########################################
    #### Modifying control file
    #### Get rid of bias adjustment
    #########################################

    ctl.lines <- readLines(file.ctl)
    max_adj.line <- grep("#_max_bias_adj_in_MPD", ctl.lines)
    max_adj.str <- strsplit(ctl.lines[max_adj.line], split = " ")[[1]]
    max_adj.str[2] <- 1
    max_adj.str <- paste(max_adj.str, collapse = " ")
    ctl.lines[max_adj.line] <- max_adj.str

    writeLines(ctl.lines, file.ctl)

    start.file <- SS_readstarter(file.start)
    start.file$init_values_src <- 0
    start.file$last_estimation_phase <- 7
    SS_writestarter(start.file, file = file.start, overwrite = TRUE)

    ##########################################
    ### Run OM with new Fmsy
    ##########################################

    system("SS3.exe", show.output.on.console = FALSE)

    ###########################################
    ### Retrieving final report file
    ###########################################

    rep.name <- paste("Boot", i, "-Ctl", j,".sso", sep = "")
    file.copy(from = "Report.sso", to = file.path(dir.check,dir.pres,dir.rep,rep.name))

  }
}