#####################################
### Tuning the model
#####################################
dir.tune <- paste("Tune-", i, sep = "")
dir.create(dir.tune)

mapply(file.copy, to = file.path(getwd(), dir.tune), overwrite = TRUE,
        MoreArgs = list(from = file.path(getwd(),c(file.dat, "SS3.exe", file.fore, file.start, file.par, file.ctl))))

tune.rep <- SS_output(dir = getwd())
tune.rep$Length_comp_Eff_N_tuning_check[,9]

setwd(dir.tune)
orig.ctl <- readLines(file.ctl)

lencompvar <- strsplit(orig.ctl[line.1], split = " ")[[1]]
parnums <-  which(lencompvar!="")
parnums <- parnums[-length(parnums)]
lencompvar[parnums] <- "1"
orig.ctl[line.1] <- paste(lencompvar, collapse=" ")

agecompvar <- strsplit(orig.ctl[line.2], split = " ")[[1]]
parnums <-  which(agecompvar!="")
parnums <- parnums[-length(parnums)]
agecompvar[parnums] <- "1"
orig.ctl[line.2] <- paste(agecompvar, collapse=" ")