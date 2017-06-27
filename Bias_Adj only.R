dir.check <- getwd()
if (!grepl(basename(dir.check), "splitnose", ignore.case = TRUE)) {
  stop(paste("Change your working directory!"))
}

require(r4ss)
require(foreach)
require(doParallel)

registerDoParallel(4)
dir.base <- "Est only R0 and recdevs"
dir.pres <- "New Bootstrap Change Recdevs" #Change to whichever model you're working in
dir.rep <- "Reports"

setwd(dir.pres)
dir.create(dir.rep, showWarnings = FALSE)

Nboot <- 100 #Set number of bootstraps


foreach(i=1:Nboot) %dopar% {
  require(r4ss)
  dir.bias <- paste("Bias_Bootstrap-", i, sep = "")
  dir.new <- paste("Bootstrap",i, sep = "-")
  dir.res <- paste("Results",j, sep = "-")
  setwd(file.path(dir.check,dir.pres,dir.new,dir.res,dir.bias))

  file.remove(list.files(getwd())[-which(list.files(getwd())=="control.ss")])
  mapply(file.copy, to = file.path(dir.check, dir.pres,dir.new,dir.res,dir.bias), overwrite = TRUE,
            MoreArgs = list(from = file.path(dir.check,dir.pres,dir.new,dir.res,c("data.ss", "SS3.exe", "forecast.ss", "starter.ss"))))
    start.file <- SS_readstarter("starter.ss")
    start.file$init_values_src <-0
    SS_writestarter(start.file, file = "starter.ss", overwrite = TRUE)

    system("SS3.exe", show.output.on.console = FALSE)

    rep.name <- paste("Boot", i, "-Ctl", j,".sso", sep = "")
    file.rename(from = "Report.sso", to = rep.name)
    file.copy(from = rep.name, to = file.path(dir.check,dir.pres,dir.rep), overwrite = TRUE)

}