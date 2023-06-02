nevsor <- read.csv2("nevsor.csv", strings = F)

for(ttnev in 1:nrow(nevsor)) {
    StudentFilename <- sub(" ", "",nevsor[ttnev, "Név"])
    set.seed(nevsor[ttnev, "seed"])
    eszak  <- sample(450:650,1)
    szeles <- sample(270:360,1)
    magas <- sample((19:30)*10,1)
    ut.tavs <- sample((3/8*szeles):((5*szeles/8)),1)
    topo <- genertopo(length = eszak, height = magas, width = szeles,
                      perc=3, east.dist = ut.tavs)
    topo.loess <- loess(z ~ x * y, topo,span=0.5, normalize=F)
    topo.ma <- list(x=seq(0,szeles,length.out=21),y=seq(0,eszak,length.out = 21))
    topo.lo <- predict(topo.loess, expand.grid(topo.ma), se=T)
    ## Traverse lengths
    (ut.neutr.dist <- (3/4 *szeles) - ut.tavs)
    short.edge <- sample(50:70,1)
    long.edge <- eszak - 50
    tteszt <- gener(fulldist = long.edge, angledist = short.edge, ox=szeles/4,
                    oy=nevsor[ttnev, "Ycorrect"])
    tteszt[2:(nrow(tteszt)-1), "z"] <- predict(topo.loess, data.frame(x=tteszt$x, y=tteszt$y))[-c(1,nrow(tteszt))]
    ## Topo point generation
    felmer.df <- expand.grid(x = seq(0, szeles - ut.tavs + 50, by = 20),
                             y = seq(long.edge - 80, eszak, by = 20)
                             )
    felmer.df$x <- felmer.df$x + runif(nrow(felmer.df), min = .5, max = 8)
    felmer.df$y <- felmer.df$y - runif(nrow(felmer.df), min = .5, max = 8)
    Bose.max <- min(felmer.df$y) - 10
    Bose.nr <- round(Bose.max / 20)
    Bose.df <- data.frame(x = rep(szeles / 4, Bose.nr),
                          y = seq(0.2, Bose.max, length.out = Bose.nr)
                          )
    Bose.df$x <- Bose.df$x + rnorm(nrow(Bose.df), sd = 2)
    Bose.df$y <- Bose.df$y + rnorm(nrow(Bose.df), sd = 1)
    Bose.df <- cbind(Bose.df, z = predict(topo.loess, Bose.df))
    topo.eov <- topo
    topo.eov$x <- round(topo.eov$x,3) + nevsor[ttnev, "easting"]
    topo.eov$y <- round(topo.eov$y,3) + nevsor[ttnev, "northing"]
    topo.eov$z <- round(topo.eov$z,3)
    ## Rotation
    act.ang <- nevsor[ttnev, "Angle"]
    toporot.eov <- eovrotate(topo.eov, act.ang)
    datgen(toporot.eov[toporot.eov$dat,], StudentFilename,
           settlement = nevsor[ttnev, "Telep"],
           student = nevsor[ttnev, "Név"])
    ######################################################################
    ## Measurement generation
    ## Without orientation, old instrument
    trav.noorient <- tteszt[tteszt$k == "ap" | tteszt$k == "sp",]
    plot.traverse(trav.noorient, tofile = paste0(StudentFilename,".pdf"))
    ## Traverse transformation
    travnoo.eov <- trav.noorient
    travnoo.eov$x <- round(travnoo.eov$x,3) + nevsor[ttnev, "easting"]
    travnoo.eov$y <- round(travnoo.eov$y,3) + nevsor[ttnev, "northing"]
    travnoo.eov$z <- round(travnoo.eov$z,3)
    ## Rotate
    travnoorot.eov <- eovrotate(travnoo.eov, act.ang)
##    write(export.coo.gizi(travnoo.eov[travnoo.eov$k == "ap",]), paste0(StudentFilename,".coo"), sep="\n")
    ttres <- meascalc.ordered(travnoorot.eov, orient = FALSE, generror = TRUE, topo = FALSE)
##    write(export.geo.gizi(ttres), paste0(StudentFilename,".geo"), sep="\n")
##    write.csv2(travnoo.eov[travnoo.eov$k == "ap",], paste0(StudentFilename,"coo.csv"), row.names = FALSE)
    if(nevsor[ttnev, "Meastype"] == "sdr") {
        ttres.degree <- twoface(ttres)
        ttres.degree$h <- angleconv(ttres.degree$h, output = "degree")
        ttres.degree$z <- angleconv(ttres.degree$z, output = "degree")
        write(paste0(export.sdr(angle =ttres.degree,coor = travnoorot.eov),"\r"), paste0(StudentFilename,".sdr"), sep="\n")
    } else {
        ttres.degree.dot <- twoface(ttres)
        ttres.degree.dot$h <- angleconv(ttres.degree.dot$h, format = "dot", round.sec = 1)
        ttres.degree.dot$z <- angleconv(ttres.degree.dot$z, format = "dot", round.sec = 1)
    write(paste0(export.m5(paste0(StudentFilename,Sys.Date()), angle =ttres.degree.dot, coor = travnoorot.eov),"\r"), paste0(StudentFilename,".m5"), sep="\n")
    }
##        ttres.degree <- twoface(ttres)
##        ttres.degree$h <- angleconv(ttres.degree$h)
##        ttres.degree$z <- angleconv(ttres.degree$z)
##    write.csv2(ttres.degree, paste0(StudentFilename,"geo.csv"), row.names = FALSE, quot = FALSE)
######################################################################
### Orientation
    ## Additional point plotted
    addpt.nr <- which(tteszt$k == "spp")
    tteszt.first <- tteszt[-addpt.nr, ]
    ## Another additional point
    addpt.df <- tteszt[addpt.nr, ]
    addpt.df <- rbind(addpt.df, addpt.df)
    addpt.df[2, "n"] <- addpt.df[2, "n"] +1
    addpt.df$y <- tteszt.first[4, "y"] + c(2, 1) *  diff(tteszt.first[4:3, "y"])/3
    addpt.df$x <- tteszt.first[3, "x"] + c(4, 2) *  diff(tteszt.first[3:2, "x"])/6
    addpt.df$z <- round(predict(topo.loess, addpt.df[,1:3]),3)
    tteszt.addpt <- rbind(tteszt[1:2,], addpt.df, tteszt[4,],
                          tteszt[c(nrow(tteszt)-2,nrow(tteszt)),])
    ## Generated instrument heights
    plusinsh <- sample(1390:1550, nrow(tteszt.addpt))/1000
    ## Search identical instrument heights
    for(ttpont in 1:nrow(tteszt.addpt)) {
        ttaktpontnr <- tteszt.addpt[ttpont, "n"]
        if(any(ttres$ns == ttaktpontnr)) {
            ## Select ihs and unique in the case of two faces
            plusinsh[ttpont] <- unique(ttres[ttres$ns == ttaktpontnr, "ihs"])
        }
    }
    ttres.addpt <- meascalc(tteszt.addpt, ins.height.range = plusinsh
                            )
    ttres.full <- rbind(ttres.addpt[1:2,], # AP1 -> orient & AP1 -> add1
                        ttres[1:3,], # AP1 -> 100 ... to SP110
                        ttres.addpt[c(3:6,8),], # additional points
                        ttres[4:nrow(ttres),], # AP1 -> 100 ... to SP110
                        ttres.addpt[nrow(ttres.addpt),] # AP2 -> orient
                        )
    ttres.full[nrow(ttres.full), "ihs"] <- ttres.full[nrow(ttres.full) - 1, "ihs"]
    ## Plot traverse with orientation and node
    pdf(paste0(StudentFilename,".pdf"))
    plot.traverse(tteszt.first, north = 0)
    ## Additional point plotted
    lines(tteszt.addpt[-c(1,nrow(tteszt.addpt)-c(1,0)), c("x","y")])
    points(tteszt.addpt[3:4, c("x","y")], pch = 4)
    text(tteszt.addpt[3:4, c("x","y")], lab=tteszt.addpt[3:4, "n"], adj=c(-0.2,0))
    text(tteszt.addpt[3:4, c("x","y")], lab=tteszt.addpt[3:4, "k"], adj=c(-0.2,1.2))
    dev.off()
    ## Export.csv
    trav.eov <- tteszt.addpt[tteszt.addpt$k == "ap" | tteszt.addpt$k == "op",]
    trav.eov$x <- round(trav.eov$x,3) + nevsor[ttnev, "easting"]
    trav.eov$y <- round(trav.eov$y,3) + nevsor[ttnev, "northing"]
    trav.eov$z <- round(trav.eov$z,3)
##    write.csv2(trav.eov, paste0(StudentFilename,"coo.csv"), row.names = FALSE)
    ttres.full.degree <- ttres.full
    ttres.full.degree$h <- angleconv(ttres.full.degree$h)
    ttres.full.degree$z <- angleconv(ttres.full.degree$z)
##    write.csv2(ttres.full.degree, paste0(StudentFilename,"geo.csv"), row.names = FALSE, quot = FALSE)
### Bose plus points
    Bose.df$y <- abs(Bose.df$y)
    Bose.df$z <- Bose.df$z + rnorm(nrow(Bose.df), sd = 0.04)
    BoseW1 <- Bose.df
    BoseW1$x <- BoseW1$x - rnorm(nrow(BoseW1), 10, sd = 1)
    BoseW1$y <- abs(BoseW1$y - rnorm(nrow(BoseW1), sd = 1))
    BoseW1$z <- predict(topo.loess, BoseW1[, 1:2]) + rnorm(nrow(BoseW1), sd = 0.05)
    BoseW2 <- Bose.df
    BoseW2$x <- BoseW2$x - rnorm(nrow(BoseW2), 20, sd = 2)
    BoseW2$y <- abs(BoseW2$y - rnorm(nrow(BoseW2), sd = 2))
    BoseW2$z <- predict(topo.loess, BoseW2[, 1:2]) + rnorm(nrow(BoseW2), sd = 0.1)
    BoseE1 <- Bose.df
    BoseE1$x <- BoseE1$x + rnorm(nrow(BoseE1), 10, sd = 1)
    BoseE1$y <- BoseE1$y + rnorm(nrow(BoseE1), sd = 1)
    BoseE1$z <- predict(topo.loess, BoseE1[, 1:2]) + rnorm(nrow(BoseE1), sd = 0.05)
    BoseE2 <- Bose.df
    BoseE2$x <- BoseE2$x + rnorm(nrow(BoseE1), 20, sd = 2)
    BoseE2$y <- BoseE2$y + abs(rnorm(nrow(BoseE1), sd = 2))
    BoseE2$z <- predict(topo.loess, BoseE2[, 1:2]) + rnorm(nrow(BoseE2), sd = 0.1)
    ## Topo point generation south
    felmerS.df <- expand.grid(x = seq(0, max(BoseW2$x), by = 20),
                             y = seq(0, 70, by = 20)
                             )
    felmerS.df$x <- felmerS.df$x + runif(nrow(felmerS.df), min = .5, max = 2)
    felmerS.df$y <- felmerS.df$y + runif(nrow(felmerS.df), min = .5, max = 2)
    felmerS.df$z <- predict(topo.loess, felmerS.df[, 1:2]) + rnorm(nrow(felmerS.df), sd = 0.1)
    felmer.df$z <- predict(topo.loess, felmer.df[, 1:2]) + rnorm(nrow(felmer.df), sd = 0.1)
    felmertopo.df <- rbind(felmer.df,
                          BoseW1,
                          BoseW2,
                          BoseE1,
                          BoseE2,
                          felmerS.df)
    Bose.df$k <- "s"
    felmertopo.df$k  <- "t"
    felmerall.df <- rbind(felmertopo.df, Bose.df)
    felmerall.df$x <- round(felmerall.df$x,2) + nevsor[ttnev, "easting"]
    felmerall.df$y <- round(felmerall.df$y,2) + nevsor[ttnev, "northing"]
    felmerall.df$z <- round(felmerall.df$z,2)
### Generation of point numbers
    felmer.nr <- nrow(felmer.df)
    felmerS.nr <- nrow(felmerS.df)
    Bose.nr <- nrow(Bose.df)
    fullbose.nr <- Bose.nr * 5
    fullbose.mat <- matrix(seq((felmer.nr + 1), by = 1, length.out = fullbose.nr),
                           nrow = 5)
    for(ttcol in 1:ncol(fullbose.mat))
        fullbose.mat[, ttcol] <- fullbose.mat[sample(1:5,5), ttcol]
    felmerS.first.nr <- felmer.nr + fullbose.nr + 1
    full.nr <- c(sample(1:felmer.nr, felmer.nr),
                 as.vector(fullbose.mat[-3,]),
                 sample(seq(felmerS.first.nr, by = +1, length.out = felmerS.nr),
                        felmerS.nr),
                 as.vector(fullbose.mat[3,]) # same order as above
                 )
    felmerall.df <- cbind(full.nr, felmerall.df)
    colnames(felmerall.df) <- c("nr", "EOV.Y", "EOV.X", "Z", "k")
    felmerall.df <- felmerall.df[order(felmerall.df$nr),]
    felmerall.df$nr <- felmerall.df$nr + 2000
    felmerall.df <- felmerall.df[!is.na(felmerall.df$Z),]
    write.csv2(felmerall.df, paste0(StudentFilename,"points.csv"), row.names = FALSE, quot = FALSE)
    ttres.degree.dot <- ttres
    ttres.degree.dot$h <- angleconv(ttres.degree.dot$h, format = "dot", round.sec = 1)
    ttres.degree.dot$z <- angleconv(ttres.degree.dot$z, format = "dot", round.sec = 1)
    write(paste0(export.m5(paste0(StudentFilename,Sys.Date()), angle =ttres.degree.dot, coor = travnoo.eov[travnoo.eov$k == "ap",]),"\r"), paste0(StudentFilename,".m5"), sep="\n")
}

### Export to gizi
trav.eov <- tteszt[tteszt$k == "ap" | tteszt$k == "op",]
trav.eov$x <- round(trav.eov$x,3) + nevsor[ttnev, "easting"]
trav.eov$y <- round(trav.eov$y,3) + nevsor[ttnev, "northing"]
trav.eov$z <- round(trav.eov$z,3)
write(export.coo.gizi(trav.eov), paste0(StudentFilename,".coo"), sep="\n")
write(export.geo.gizi(ttres.full), paste0(StudentFilename,".geo"), sep="\n")



## PLOT
library(MASS)
eqscplot(topo.ma, typ="n")
contour(topo.ma$x, topo.ma$y, topo.lo$fit, add=T)
points(topo[,1:2])
text(topo[,"x"], topo[,"y"], round(topo[,"z"]), adj=c(0,1))
points(topo[topo$dat, c("x","y")], pch = 4)
## Points on topo
points(tteszt[, c("x","y")], col=4)
points(felmer.df[,1:2], col = 2)
points(felmerS.df[,1:2], col = 2)
points(Bose.df[,1:2], col =6)
points(BoseW1[,1:2], col = 6)
points(BoseW2[,1:2], col = 6)
points(BoseE1[,1:2], col = 6)
points(BoseE2[,1:2], col = 6)
dev.set(3)
plot(Bose.df[,2:3])
dev.set(2)

ttres.degree <- ttres
ttres.degree$h <- angleconv(ttres.degree$h, format = "dot", round.sec = 1)
ttres.degree$z <- angleconv(ttres.degree$z, format = "dot", round.sec = 1)

write(paste0(export.m5(paste0("KaliczPéter",Sys.Date()), angle =ttres.degree, coor = travnoo.eov[travnoo.eov$k == "ap",]),"\r"), "data.m5", sep="\n")

## Csomópont nem OK
write(paste0(export.m5(paste0("KaliczPéter",Sys.Date()), angle =ttres.full.degree, coor = trav.eov),"\r"), "data.m5", sep="\n")

export.m5(paste0("KaliczPéter",Sys.Date()), angle = ttres.degree, coor = travnoo.eov[travnoo.eov$k == "ap",])

ttfelmerall.df <- felmerall.df[,c(2:5,1),]
names(ttfelmerall.df) <- c("x", "y", "z", "k", "n")
write(paste0(export.m5(paste0("KaliczPéter",Sys.Date()), coor = ttfelmerall.df),"\r"), "data.m5", sep="\n")
