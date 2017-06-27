#---------------------------------
# Plot std errors for growh params
#---------------------------------

gr.pars <- array(dim = c(nEMs, length(par2change), Nyears))
gr.std.errs <- array(dim = c(nEMs, length(par2change), Nboot, Nyears))

med.par <-  array(dim = c(nEMs, length(par2change), Nyears))
SI50.par <- array(dim = c(nEMs, length(par2change), Nyears*2))
SI90.par <- array(dim = c(nEMs, length(par2change), Nyears*2))

for(parm in 1:length(par2change)) {
	for (boot in 1:Nboot) {
		true.pars <- eval(parse(text=paste0("true.reps[[boot]]$MGparmAdj$",par2change[parm])))
		for(em in 1:nEMs) {
			gr.pars[em,parm,] <- eval(parse(text=paste0("EM.reps[[em]][[boot]]$MGparmAdj$",par2change[parm])))
			gr.std.errs[em, parm, boot,] <- (gr.pars[em,parm,] - true.pars) / true.pars
		}
	}
}

png(filename = file.path(dir.plots, paste0(dir.pres, "_GrowthParms_", nEMs, ".png")), width = 6, height = 4, units = "in", res = 600)
par(mfrow = c(length(par2change), nEMs), mar = c(.3,.3,.3,.3), oma = c(2,2,1,1))
for(parm in 1:length(par2change)) {
	for(em in 1:nEMs) {
		for(yr in 1:Nyears) {
			med.par[em, parm, yr] <- median(gr.std.errs[em,parm, ,yr])
			SI50.par[em, parm, yr] <- quantile(gr.std.errs[em,parm, ,yr], probs = 0.25)
			SI50.par[em, parm,(Nyears*2+1)-yr] <- quantile(gr.std.errs[em,parm, ,yr], probs = 0.75)
			SI90.par[em, parm, yr] <- quantile(gr.std.errs[em,parm, ,yr], probs = 0.05)
			SI90.par[em, parm,(Nyears*2+1)-yr] <- quantile(gr.std.errs[em,parm, ,yr], probs = 0.95)
		}

	if(parm %in% grep("K", par2change)) {plot(0, xlim=c(StartYr, EndYr), ylim=c(-.7,.7),xaxs="i", ylab = "", xlab = "", ann = , xaxt = 'n', yaxt = 'n')}
  	if(parm %in% grep("L_at_Amax", par2change)) {plot(0, xlim=c(StartYr, EndYr), ylim=c(-1.5,1.5),xaxs="i", ylab = "", xlab = "", ann = , xaxt = 'n', yaxt = 'n')}
  
  	polygon(y=SI90.par[em,parm,],x=c(StartYr:EndYr,EndYr:StartYr), col = "gray60", border = NA)
  	polygon(y=SI50.par[em,parm,],x=c(StartYr:EndYr,EndYr:StartYr),col="gray75", border = NA)
  	lines(y=med.par[em,parm,], x = c(StartYr:EndYr), type = "l", col="black", lwd=2)
  	abline(h = 0, col = "black", lwd = 0.5, lty = 2)
#  	axis(side = 1)
  	if(em == 1) {axis(side = 2)}
  	if(parm == length(par2change)) {axis(side = 1)}
  }

}
par(mfcol = c(1,1))

dev.off()

