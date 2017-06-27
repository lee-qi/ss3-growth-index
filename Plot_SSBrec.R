#################################################
#### Plotting std errors for SSB and Recruitment 
#################################################

Nyears <- length(true.reps[[1]]$recruit$spawn_bio[which(true.reps[[1]]$recruit$spawn_bio!=0)])-foreyears
StartYr <- min(true.reps[[1]]$recruit$year)
EndYr <- max(true.reps[[1]]$recruit$year)-foreyears

SSB.err <- array(dim = c(nEMs,Nboot,Nyears))
med.SSB <- matrix(nrow = nEMs, ncol = Nyears)
SI50.SSB <- matrix(nrow = nEMs, ncol = Nyears*2)
SI90.SSB <- matrix(nrow = nEMs, ncol = Nyears*2)

Rec.err <- array(dim = c(nEMs,Nboot,Nyears))
med.Rec <- matrix(nrow = nEMs, ncol = Nyears)
SI50.Rec <- matrix(nrow = nEMs, ncol = Nyears*2)
SI90.Rec <- matrix(nrow = nEMs, ncol = Nyears*2)

sim.SSB <- matrix(nrow = nEMs, ncol = Nyears)
sim.Rec <- matrix(nrow = nEMs, ncol = Nyears)

for (boot in 1:Nboot) {
  true.SSB <- true.reps[[boot]]$recruit$spawn_bio[which(true.reps[[boot]]$recruit$spawn_bio!=0)][1:Nyears]
  true.Rec <- true.reps[[boot]]$recruit$exp_recr[which(true.reps[[boot]]$recruit$exp_recr!=0)][1:Nyears]
  
  for (em in 1:nEMs) {
    sim.SSB[em,] <- EM.reps[[em]][[boot]]$recruit$spawn_bio[1:Nyears]
    SSB.err[em,boot,] <- (sim.SSB[em,]-true.SSB) / true.SSB
  
    sim.Rec <- EM.reps[[em]][[boot]]$recruit$exp_recr[1:length(true.Rec)]
    Rec.err[em,boot,] <- (sim.Rec-true.Rec) / true.Rec
  }
}

for(em in 1:nEMs) {
  for(yr in 1:Nyears) {
    med.SSB[em, yr] <- median(SSB.err[em,,yr])
    SI50.SSB[em, yr] <- quantile(SSB.err[em,,yr],probs=0.25)
    SI50.SSB[em, (Nyears*2+1)-yr] <- quantile(SSB.err[em,,yr],probs=0.75)
    SI90.SSB[em, yr] <- quantile(SSB.err[em,,yr],probs=0.05)
    SI90.SSB[em, (Nyears*2+1)-yr] <- quantile(SSB.err[em,,yr],probs=0.95)
  
    med.Rec[em, yr] <- median(Rec.err[em,,yr])
    SI50.Rec[em, yr] <- quantile(Rec.err[em,,yr],probs=0.25)
    SI50.Rec[em, (Nyears*2+1)-yr] <- quantile(Rec.err[em,,yr],probs=0.75)
    SI90.Rec[em, yr] <- quantile(Rec.err[em,,yr],probs=0.05)
    SI90.Rec[em, (Nyears*2+1)-yr] <- quantile(Rec.err[em,,yr],probs=0.95)
  }
}


#################################################
#### Plotting single run
#################################################
png(filename = file.path(dir.plots, paste0(dir.pres, "_SingleRun_", nEMs, ".png")), width = 5, height = 4, units = "in", res = 600)

cls <- rainbow(nEMs)
par(mar = c(4,4,1,1))
plot(0, xlim=c(StartYr, EndYr), ylim=c(0, (max(true.SSB, sim.SSB) / 1E9)+.1), 
  ylab = "",
#  ylab = expression(paste("Spawning Stock Biomass (", "10"^"8", "mt)", sep = "")),
  xlab = "")
for(em in 1:nEMs) {
  lines(y=sim.SSB[em,] / 1E9, x = c(StartYr:EndYr), type = "l", col=cls[em], lwd=2)
}
lines(y=true.SSB / 1E9, x = c(StartYr:EndYr), type = "l", col="black", lwd=2)
abline(h = 0, col = "grey", lwd = .5)
legend("bottomleft", lty = 1, col = c("black",cls),
  legend = c("OM", paste0("EM-",EMvec)), lwd = 2)
title(ylab = expression(paste("Spawning Stock Biomass (", "10"^"8", "mt)", sep = "")), line = 2)
title(xlab = "Year", line = 2)

dev.off()


#################################################
#### Plotting all SSB and Recruitment ########
#################################################
#png(filename = file.path(dir.plots, paste0(dir.pres, "_SSBrec_", nEMs, ".png")), width = 600, height = 400)
##dev.new()
#par(mfcol=c(2,nEMs), mar = c(.3,.3,.3,.3), oma = c(2,2,1,1))
#
#for(em in 1:nEMs) {
#
#  plot(0, xlim=c(StartYr, EndYr), ylim=c(-1,1),xaxs="i", ylab = "", xlab = "", ann = , xaxt = 'n', yaxt = 'n')
#  polygon(y=SI90.SSB[em,],x=c(StartYr:EndYr,EndYr:StartYr), col = "gray60", border = NA)
#  polygon(y=SI50.SSB[em,],x=c(StartYr:EndYr,EndYr:StartYr),col="gray75", border = NA)
#  lines(y=med.SSB[em,], x = c(StartYr:EndYr), type = "l", col="black", lwd=2)
#  abline(h = 0, col = "black", lwd = 0.5, lty = 2)
#  if(em == 1) {axis(side = 2)}
#
#  plot(0, xlim=c(StartYr, EndYr), ylim=c(-1,1),xaxs="i", ylab = "", xlab = "", ann = , xaxt = 'n', yaxt = 'n')
#  polygon(y=SI90.Rec[em,],x=c(StartYr:EndYr,EndYr:StartYr), col = "gray60", border = NA)
#  polygon(y=SI50.Rec[em,],x=c(StartYr:EndYr,EndYr:StartYr),col="gray75", border = NA)
#  lines(y=med.Rec[em,], x = c(StartYr:EndYr), type = "l", col="black", lwd=2)
#  abline(h = 0, col = "black", lwd = 0.5, lty = 2)
#  axis(side = 1)
#  if(em == 1) {axis(side = 2)}
#  
#}
#
#par(mfcol=c(1,1))
#
#dev.off()


#################################################
#### Plotting all SSB and Recruitment ########
#################################################
png(filename = file.path(dir.plots, paste0(dir.pres, "_SSB_", nEMs, ".png")), width = 6, height = 2, units = "in", res = 600)
par(mfcol=c(1,nEMs), mar = c(.3,.3,.3,.3), oma = c(2,2,1,1))

for(em in 1:nEMs) {
  plot(0, xlim=c(StartYr, EndYr), ylim=c(-1,1),xaxs="i", ylab = "", xlab = "", ann = , xaxt = 'n', yaxt = 'n')
  polygon(y=SI90.SSB[em,],x=c(StartYr:EndYr,EndYr:StartYr), col = "gray60", border = NA)
  polygon(y=SI50.SSB[em,],x=c(StartYr:EndYr,EndYr:StartYr),col="gray75", border = NA)
  lines(y=med.SSB[em,], x = c(StartYr:EndYr), type = "l", col="black", lwd=2)
  abline(h = 0, col = "black", lwd = 0.5, lty = 2)
#  axis(side = 1)
  if(em == 1) {axis(side = 2)}
}

par(mfcol=c(1,1))
dev.off()


#dev.new()
png(filename = file.path(dir.plots, paste0(dir.pres, "_Rec_", nEMs, ".png")), width = 6, height = 2, units = "in", res = 600)
par(mfcol=c(1,nEMs), mar = c(.3,.3,.3,.3), oma = c(2,2,1,1))
for(em in 1:nEMs) {
  plot(0, xlim=c(StartYr, EndYr), ylim=c(-1,1),xaxs="i", ylab = "", xlab = "", ann = , xaxt = 'n', yaxt = 'n')
  polygon(y=SI90.Rec[em,],x=c(StartYr:EndYr,EndYr:StartYr), col = "gray60", border = NA)
  polygon(y=SI50.Rec[em,],x=c(StartYr:EndYr,EndYr:StartYr),col="gray75", border = NA)
  lines(y=med.Rec[em,], x = c(StartYr:EndYr), type = "l", col="black", lwd=2)
  abline(h = 0, col = "black", lwd = 0.5, lty = 2)
  axis(side = 1)
  if(em == 1) {axis(side = 2)}
}

par(mfcol=c(1,1))
dev.off()


#################################################
#### Plotting all SSB and Recruitment ########
#################################################


for(em in 1:nEMs) {
#  dev.new()
  png(filename = file.path(dir.plots, paste0(dir.pres, "_SSB_EM_", em, ".png")), width = 4, height = 4, units = "in", res = 600)
  par(mar = c(.1,.1,.1,.1), oma = c(2.2,2.2,1,1))
  plot(0, xlim=c(StartYr, EndYr), ylim=c(-1,1),xaxs="i", ylab = "", xlab = "", ann = , xaxt = 'n', yaxt = 'n')
  polygon(y=SI90.SSB[em,],x=c(StartYr:EndYr,EndYr:StartYr), col = "gray60", border = NA)
  polygon(y=SI50.SSB[em,],x=c(StartYr:EndYr,EndYr:StartYr),col="gray75", border = NA)
  lines(y=med.SSB[em,], x = c(StartYr:EndYr), type = "l", col="black", lwd=2)
  abline(h = 0, col = "black", lwd = 0.5, lty = 2)
#  axis(side = 1, cex.axis = 1.5)
  if(sum(grep(1,dir.pres), grep(2,dir.pres))>0) {
    if(em==2 | em == 1) {axis(side = 2, cex.axis = 1.5)}}
  dev.off()
}



#dev.new()
for(em in 1:nEMs) {
#  dev.new()
  png(filename = file.path(dir.plots, paste0(dir.pres, "_Rec_EM_", em, ".png")), width = 4, height = 4, units = "in", res = 600)
  par(mar = c(.1,.1,.1,.1), oma = c(2.2,2.2,1,1))
  plot(0, xlim=c(StartYr, EndYr), ylim=c(-1,1),xaxs="i", ylab = "", xlab = "", ann = , xaxt = 'n', yaxt = 'n')
  polygon(y=SI90.Rec[em,],x=c(StartYr:EndYr,EndYr:StartYr), col = "gray60", border = NA)
  polygon(y=SI50.Rec[em,],x=c(StartYr:EndYr,EndYr:StartYr),col="gray75", border = NA)
  lines(y=med.Rec[em,], x = c(StartYr:EndYr), type = "l", col="black", lwd=2)
  abline(h = 0, col = "black", lwd = 0.5, lty = 2)
  axis(side = 1, cex.axis = 1.5)
  if(sum(grep(1,dir.pres), grep(2,dir.pres))>0) {
    if(em ==2 | em == 1) {axis(side = 2, cex.axis = 1.5)}}
  dev.off()
}


