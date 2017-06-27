setwd("E:/Splitnose")

require(r4ss)


OM.names <- list.files()[grep("OM", list.files())]
nOMs <- length(OM.names)
dir.boot <- "Bootstrap-1"

truerep <- SS_output(file.path(getwd(),OM.names[1]), covar = F)

reps <- lapply(file.path(getwd(), OM.names, dir.boot), SS_output, covar = F)



Nyears <- length(truerep$recruit$spawn_bio[which(truerep$recruit$spawn_bio!=0)])
StartYr <- min(truerep$recruit$year)
EndYr <- max(truerep$recruit$year)

true.SSB <- truerep$recruit$spawn_bio[which(truerep$recruit$spawn_bio!=0)][1:Nyears]

sim.SSB <- matrix(nrow = nOMs, ncol = Nyears)
for(om in 1:nOMs) {
	sim.SSB[om,] <- reps[[om]]$recruit$spawn_bio[1:Nyears]
}

dev.new()
SSplotCatch(truerep, subplots = 1)

dev.new()
par(mfrow=c(1,1), mar = c(5,5,2,1))
plot(0, xlim=c(StartYr, EndYr), ylim=c(0, (max(sim.SSB) / 1E9)+.1), 
	ylab = expression(paste("Spawning Stock Biomass (", "10"^"8", "mt)", sep = "")),
	xlab = "Year")

cls <- rainbow(nOMs)
for(om in 1:nOMs) {
	lines(sim.SSB[om,] / 1E9, x = StartYr:EndYr, type = "l", col = cls[om], lwd = 2)
}
#lines(true.SSB/1E9, x = StartYr:EndYr, type = "l", col = "black", lwd = 3)
abline(v = 2008, lty = 2, col = "black")
abline(h = 0, col = "grey", lwd = .5)
legend("topleft", lty = 1, col = cls,
	legend = paste("OM", 1:nOMs, sep = " "), lwd = 2)

