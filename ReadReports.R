#------------------------
# Reading in report files
#------------------------

fore.reps <- vector("list", nEMs)
EM.reps <- vector("list", nEMs)

true.reps <- NULL

RunNos <- 0
runvec <- 0

for(i in 1:Nboot) {
  repnames <- paste0("Boot", i, "-EM", EMvec,".sso")

  #------------------------------------
  # Change previous runs using old code
  #------------------------------------
  wrongreps <- paste0("Boot", i, "-Ctl", 1:2,".sso")
  if(sum(file.exists(wrongreps))==2) {
    do.rename <- mapply(file.rename, from = wrongreps, to = repnames[1:2])
  }

  #---------------------------------------------------
  # Reading in report files
  # If EMs don't all exist for one OM, OM is discarded
  # Can also consider using lapply on vector of directories
  #---------------------------------------------------
  if(sum(file.exists(repnames))!=nEMs) {next}
  else {
    RunNos <- RunNos+1
    runvec <- c(runvec, i)

    dir.new <- paste0("Bootstrap-", i)
    true.reps[[RunNos]] <- SS_output(verbose = FALSE, printstats = FALSE, covar = FALSE,
                                      dir = file.path(dir.check, dir.pres, dir.new, "OM Proj-Catch"),ncols = 113)

    dir.temp <- "Temp"
    dir.bias <- paste0("Bias_Bootstrap-", i)


    for(em in 1:nEMs) {
      fore.reps[[em]][[RunNos]] <- SS_output(repfile=repnames[em],
                                  warn=FALSE,checkcor=TRUE,NoCompOK=TRUE,
                                  verbose=FALSE,printstats=FALSE, hidewarn=TRUE,
                                  covar=FALSE, readwt=FALSE, forecast=FALSE,
                                  dir = file.path(dir.check,dir.pres,dir.rep),ncols = 113)
      dir.EM <- paste0("EM-",EMvec[em])
      EM.reps[[em]][[RunNos]] <- SS_output(warn=FALSE,checkcor=TRUE,NoCompOK=TRUE,
                                  verbose=FALSE,printstats=FALSE,
                                  covar=FALSE, readwt=FALSE, forecast=FALSE,
                                  dir = file.path(dir.check,dir.pres,dir.new,dir.EM,
                                                  paste0("Bias_Bootstrap-",i)),ncols = 113)
    }
  }
}


runvec <- runvec[-1]
Nboot <- RunNos

save.image(file = paste0("EM_",paste(EMvec, collapse = "_"),".RData"))