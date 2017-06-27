#################################################
#### Calculating values for decision table
#################################################

lost.yield <- matrix(nrow = nEMs, ncol = Nboot)

yield.prop <- matrix(nrow = nEMs, ncol = Nboot)

# For overfishing component
true.B0 <- NULL
sim.B0 <- matrix(nrow = nEMs, ncol = Nboot)
simB.trueB0 <- matrix(nrow = nEMs, ncol = Nboot)

true.Bfinal <- NULL
sim.Bfinal <- matrix(nrow = nEMs, ncol = Nboot)
simB.trueB <- matrix(nrow = nEMs, ncol = Nboot)

true.Bprojfinal <- NULL
sim.Bprojfinal <- matrix(nrow = nEMs, ncol = Nboot)

true.B.Bmsy <- NULL # Not sure if this is needed
simB.Bmsy <- matrix(nrow = nEMs, ncol = Nboot)

sumtrueyield <- 0

for(boot in 1:Nboot) {
  true.yield <- true.reps[[boot]]$derived_quant[grep(paste("ForeCatchret_", EndYr+1, sep = ""), true.reps[[boot]]$derived_quant[,1]),2]

  true.Bprojfinal[boot] <- rev(true.reps[[boot]]$derived_quant[grep("SPB",true.reps[[boot]]$derived_quants$LABEL),2])[1] # B2058

  true.B0[boot] <- true.reps[[boot]]$derived_quants$Value[1]

  true.Bmsy <- subset(true.reps[[boot]]$derived_quants, LABEL == "SSB_MSY")$Value

  true.B.Bmsy[boot] <- true.Bfinal[boot] / true.Bmsy

  true.Bfinal[boot] <- true.reps[[boot]]$recruit$spawn_bio[Nyears] # B2008

  for(em in 1:nEMs) {
    sim.yield <- fore.reps[[em]][[boot]]$derived_quant[grep(paste("ForeCatchret_", EndYr+1, sep = ""), fore.reps[[em]][[boot]]$derived_quant[,1]),2]
    lost.yield[em, boot] <- sim.yield - true.yield
    yield.prop[em, boot] <- lost.yield[em,boot] / true.yield

    sim.Bprojfinal[em, boot] <- rev(fore.reps[[em]][[boot]]$derived_quant[grep("SPB",fore.reps[[em]][[boot]]$derived_quants$LABEL),2])[1]
    simB.trueB0[em, boot] <- sim.Bprojfinal[em, boot] / true.B0[boot]
    simB.trueB[em, boot] <- sim.Bprojfinal[em,boot] / true.Bprojfinal[boot]

    simB.Bmsy[boot] <- sim.Bprojfinal[em,boot] / true.Bmsy

    sim.Bfinal[em, boot] <- EM.reps[[em]][[boot]]$recruit$spawn_bio[Nyears]

    sim.B0[em, boot] <- EM.reps[[em]][[boot]]$derived_quants$Value[1]

  }
}

dec.table <- matrix(ncol = 3, nrow = nEMs)

for(em in 1:nEMs) {
#  dec.table[em, 1] <- paste0(signif(median(lost.yield[em,]),3), " (", signif(IQR(lost.yield[em,]),3), ")")
  dec.table[em, 1] <- paste0(signif(median(yield.prop[em,]),3), " (", signif(IQR(yield.prop[em,]),3), ")")
  dec.table[em, 2] <- paste0(signif(median(simB.trueB[em,]),3), " (", signif(IQR(simB.trueB[em,]),3), ")")
  dec.table[em, 3] <- round(length(which(simB.trueB0[em,] <= 0.25)) / Nboot, digits = 2) 
}

write.csv(dec.table, file = file.path(dir.code,paste(dir.pres, "_DecisionTable.csv", sep = "")), row.names = F)
