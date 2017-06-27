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
dir.pres <- "OM 5 Beta 0.3 AR 0.95" #Change to whichever model you're working in
dir.rep <- "Reports"

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


foreach(i=1:Nboot) %dopar% {
  seed <- 5000*i
  set.seed(seed)
  require(r4ss)

  dir.new <- paste("Bootstrap",i, sep = "-")

  setwd(file.path(dir.check,dir.pres,dir.new, "Temp"))

  dir.temp <- file.path(dir.check,dir.pres,dir.new, "Temp")
  dir.create(dir.temp)

  mapply(file.copy, to = dir.temp, overwrite = TRUE,
               MoreArgs = list(from = list.files(getwd())))
  setwd(dir.temp)
  par.file <- readLines(file.par)
  for(Gpar in 1:length(par2change)) {
    Gparam.line <- grep("MGparm", par.file)[24+Gpar]+1
    par.file[Gparam.line] <- "0"
  }

  writeLines(par.file, file.par)

  system(paste(exe.cmd, " -nohess", sep = ""), show.output.on.console = FALSE)

  dir.res <- "EM-2"
  dir.bias <- paste("Bias_Bootstrap-", i, sep = "")

  setwd(file.path(dir.check, dir.pres, dir.new, dir.res,dir.bias))
  dir.create(dir.temp)

  mapply(file.copy, to = dir.temp, overwrite = TRUE,
             MoreArgs = list(from = file.path(getwd(),
              c(file.ctl, file.dat, file.fore, file.start, file.par, file.exe))))

  setwd(dir.temp)
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

