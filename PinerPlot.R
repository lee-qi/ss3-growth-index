setwd("C:/Users/Lee Qi/Desktop/Male Lmax Mult")

dir.check <- getwd()
Nruns <- 30

require(r4ss)
require(doParallel)
require(foreach)
registerDoParallel(3)

Fbetas <- seq(from = 0, to = 3*0.108278, length = Nruns)
Mbetas <- seq(from = 0, to = 3*-0.0481775, length = Nruns)

file.dat <- "data.ss" 
file.ctl <- "control.ss"
file.fore <- "forecast.ss"
file.exe <- "SS3.exe"
file.start <- "starter.ss"

foreach(i = 1:Nruns) %dopar% {
	dir.run <- paste("Run-", i, sep = "")
	setwd(dir.check)
	dir.create(dir.run)
	mapply(file.copy, to = dir.run,
		MoreArgs = list(from = c(file.dat, file.ctl, file.fore, file.exe, file.start)),
		overwrite = TRUE)

	setwd(dir.run)
	ctl.file <- readLines(file.ctl)
	line.setup <- grep("#custom_MG-env_setup", ctl.file)
	setup.str <- unlist(strsplit(ctl.file[line.setup], split = " "))
	setup.str[1] <- 1
	setup.str <- paste(setup.str, collapse = " ")
	ctl.file[line.setup] <- setup.str

	line.beta <- line.setup + 1
	beta.str <- unlist(strsplit(ctl.file[line.beta], split = " "))
	beta.str[which(beta.str!="")][7] <- 6
	#beta.str[which(beta.str!="")][3:4] <- Fbetas[i]
	beta.str.M <- beta.str

	beta.str <- paste(beta.str, collapse = " ")
	ctl.file[line.beta] <- beta.str

	beta.str.M[which(beta.str.M!="")][3:4] <- Mbetas[i]
	beta.str.M[which(beta.str.M!="")][7] <- -1
	beta.str.M <- paste(beta.str.M, collapse = " ")

	ctl.new <- c(ctl.file[1:line.beta], beta.str.M, ctl.file[(line.beta+1):length(ctl.file)])
	writeLines(ctl.new, file.ctl)

	system(file.exe, show.output.on.console = FALSE)
}

dir.vec <- NULL
for (i in 1:Nruns) {dir.vec[i] <- paste("Run-",i,sep = "")}

reps <- SSgetoutput(dirvec = dir.vec)

PinerPlot(SSsummarize(reps),profile.string="L_at_Amax_Mal_GP_1_ENV_mult",profile.label = "Male Lmax Beta")

#Histogram of betas for opposite sex

fem.beta <- NULL
mal.beta <- NULL
for(i in 1:Nruns) {
	fem.beta[i] <- reps[[i]]$parameters[25,]$Value
	mal.beta[i] <- reps[[i]]$parameters[26,]$Value
}

par(mfrow = c(2,1))
hist(fem.beta)
hist(mal.beta)
par(mfrow=c(1,1))
