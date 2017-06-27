setwd("C:/Users/Lee Qi/Desktop/SS3/Splitnose/Testing")
dir.check <- getwd()

require(r4ss)
require(foreach)
require(doParallel)

registerDoParallel(4)
dir.pres <- "Test Additive" #Change to whichever model you're working in
dir.rep <- "Reports"

setwd(dir.pres)
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

  for(j in 1:2) {
  	setwd(file.path(dir.check,dir.pres, dir.new))
  	dir.res <- paste("Results",j, sep = "-")
  	setwd(dir.res)

  	dir.bias <- paste("Bias_Bootstrap-", i, sep = "")
  	setwd(dir.bias)

	  dir.fin <- paste("Projection-", j, sep = "")
  	unlink(dir.fin, recursive = TRUE)

  	dir.create(dir.fin)
  	mapply(file.copy, to = dir.fin, overwrite = TRUE,
            MoreArgs = list(from = c(file.path(getwd(), c(file.fore, file.start, "SS3.exe")),
                                     file.path(dir.check, dir.pres, dir.new, c(file.ctl, file.dat, file.par)))))

	  rep.file <- SS_output(dir = getwd(), verbose = F, printstats = FALSE)
  	Fmsy <- rep.file$derived_quants[rep.file$derived_quants[,1] == "Fstd_MSY", 2]
   	Years <- seq(from = (endyr+1), length = foreyears, by = 1) #?Find start year for forecasting
   	catch.table <- cbind(Years, rep(1, foreyears), rep(1, foreyears), Fmsy)
   	colnames(catch.table) <- c("Year", "Seas", "Fleet", "Catch_or_F")
    
    setwd(dir.fin)

    ##########################################
    ### Change Forecast file
    ### Can explore different options here
    ##########################################

    fore.file <- SS_readforecast(file.fore, Nfleets = Nfleets, Nareas = Nareas)
    fore.file$Ncatch <- foreyears * Nfleets
    fore.file$InputBasis <- 99
    fore.file$Forecast <- 5       #Set to input catch
    fore.file$ForeCatch <- data.frame(catch.table)

    SS_writeforecast(fore.file, overwrite = TRUE)

    #########################################
    #### Modifying starter file
    #########################################
 
    start.file <- SS_readstarter(file.start)
    start.file$init_values_src <- 1 #Use par file instead of inits in ctl
    start.file$N_bootstraps <- 2 #Set Bootstrap number
    start.file$last_estimation_phase <- 0
 
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