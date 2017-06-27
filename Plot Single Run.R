setwd("C:/Users/Lee Qi/Desktop/Test")

require(r4ss)

dir.check <- getwd()
dir.pres <- "Fixed EM"

OMrep <- SS_output(dir = file.path(dir.check, dir.pres, "OM"), covar = FALSE,
					verbose=FALSE,printstats=FALSE,)


EMrep <- SS_output(dir = file.path(dir.check, dir.pres, "EM"), covar = FALSE,
					verbose=FALSE,printstats=FALSE,)

StartYr <- min(OMrep$recruit$year)
EndYr <- max(OMrep$recruit$year)
Nyears <- length(OMrep$recruit$spawn_bio[which(OMrep$recruit$spawn_bio!=0)])

OM.Rec <- OMrep$recruit$exp_recr[which(OMrep$recruit$exp_recr!=0)]
EM.Rec <- EMrep$recruit$exp_recr[1:length(OM.Rec)]


OM.SSB <- OMrep$recruit$spawn_bio[which(OMrep$recruit$spawn_bio!=0)]
EM.SSB <- EMrep$recruit$spawn_bio[1:Nyears]

par(mfrow=c(2,1), mar = c(2,4,2,1))

plot(0, xlim=c(StartYr, EndYr), ylim=c(min(OM.SSB, EM.SSB), max(OM.SSB, EM.SSB)),xaxs="i", main = dir.pres, ylab = "SSB")
lines(y=OM.SSB, x = c(StartYr:EndYr), type = "l", col="black", lwd=2)
lines(y=EM.SSB, x = c(StartYr:EndYr), type = "l", col="red", lwd=2)
abline(v = 2008, lty = 2, col = "blue")
legend(x = 1905, y = 1.3*min(OM.SSB, EM.SSB), lty = 1, col = c("black", "red"), legend = c("OM", "EM"), lwd = 2)


plot(0, xlim=c(StartYr, EndYr), ylim=c(min(OM.Rec, EM.Rec), max(OM.Rec, EM.Rec)),xaxs="i", ylab = "Recruitment")
lines(y=OM.Rec, x = c(StartYr:EndYr), type = "l", col="black", lwd=2)
lines(y=EM.Rec, x = c(StartYr:EndYr), type = "l", col="red", lwd=2)
abline(v = 2008, lty = 2, col = "blue")

par(mfrow=c(1,1))

###############################
####
###############################
par(mfrow=c(2,1), mar = c(2,4,2,1))

plot(0, xlim=c(StartYr, EndYr), ylim=c(min(true.SSB, sim.SSB), max(true.SSB, sim.SSB)),xaxs="i", main = dir.pres, ylab = "SSB")
lines(y=true.SSB, x = c(StartYr:EndYr), type = "l", col="black", lwd=2)
lines(y=sim.SSB, x = c(StartYr:EndYr), type = "l", col="red", lwd=2)
abline(v = 2008, lty = 2, col = "blue")
legend(x = 1905, y = 1.3*min(true.SSB, sim.SSB), lty = 1, col = c("black", "red"), legend = c("OM", "EM"), lwd = 2)


plot(0, xlim=c(StartYr, EndYr), ylim=c(min(true.Rec, sim.Rec), max(true.Rec, sim.Rec)),xaxs="i", ylab = "Recruitment")
lines(y=true.Rec, x = c(StartYr:EndYr), type = "l", col="black", lwd=2)
lines(y=sim.Rec, x = c(StartYr:EndYr), type = "l", col="red", lwd=2)
abline(v = 2008, lty = 2, col = "blue")

par(mfrow=c(1,1))
