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
                          y = seq(0, Bose.max, length.out = Bose.nr)
                          )
    Bose.df$x <- Bose.df$x + rnorm(nrow(Bose.df), sd = 2)
    Bose.df$y <- Bose.df$y + rnorm(nrow(Bose.df), sd = 1)
    Bose.df <- cbind(Bose.df, z = predict(topo.loess, Bose.df))
    topo.eov <- topo
    topo.eov$x <- round(topo.eov$x,3) + nevsor[ttnev, "easting"]
    topo.eov$y <- round(topo.eov$y,3) + nevsor[ttnev, "northing"]
    topo.eov$z <- round(topo.eov$z,3)
    ## act.ang <- nevsor[ttnev, "Angle"] * pi /180
    ## topo.eov$x <- topo.eov$x * cos(act.ang) -
    ##     topo.eov$y * sin(act.ang)
    ## topo.eov$y <- topo.eov$x * sin(act.ang) +
    ##     topo.eov$y * cos(act.ang)
    datgen(topo.eov[topo.eov$dat,], StudentFilename,
           settlement = nevsor[ttnev, "Telep"],
           student = nevsor[ttnev, "Név"])
    ######################################################################
    ## Measurement generation
    ## Without orientation, old instrument
    trav.noorient <- tteszt[tteszt$k == "AP" | tteszt$k == "SP",]
    plot.traverse(trav.noorient, tofile = paste0(StudentFilename,".pdf"))
    ## Traverse transformation
    travnoo.eov <- trav.noorient
    travnoo.eov$x <- round(travnoo.eov$x,3) + nevsor[ttnev, "easting"]
    travnoo.eov$y <- round(travnoo.eov$y,3) + nevsor[ttnev, "northing"]
    travnoo.eov$z <- round(travnoo.eov$z,3)
    ## travnoo.eov$x <- travnoo.eov$x * cos(act.ang) -
    ##     travnoo.eov$y * sin(act.ang)
    ## travnoo.eov$y <- travnoo.eov$x * sin(act.ang) +
    ##     travnoo.eov$y * cos(act.ang)
    ## travnoo.eov$x <- round(travnoo.eov$x,3)
    ## travnoo.eov$y <- round(travnoo.eov$y,3)
    write(export.coo.gizi(travnoo.eov[travnoo.eov$k == "AP",]), paste0(StudentFilename,".coo"), sep="\n")
    ttres <- meascalc(travnoo.eov, orient = FALSE, generror = TRUE)
    write(export.geo.gizi(ttres), paste0(StudentFilename,".geo"), sep="\n")
    write.csv2(travnoo.eov[travnoo.eov$k == "AP",], paste0(StudentFilename,"coo.csv"), row.names = FALSE)
    ttres.degree <- ttres
    ttres.degree$h <- angleconv(ttres.degree$h)
    ttres.degree$z <- angleconv(ttres.degree$z)
    write.csv2(ttres.degree, paste0(StudentFilename,"geo.csv"), row.names = FALSE, quot = FALSE)
######################################################################
### Orientation
    ## Additional point plotted
    addpt.nr <- which(tteszt$k == "SPP")
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
    ttres.addpt <- meascalc(tteszt.addpt, ins.height.range = c(
                                              ttres[1, "ihs"],
                                              sample(1500:1600, 2)/1000,
                                              ttres[c(4,nrow(ttres)), "ihs"]
                                          )
                            )
    ttres.full <- rbind(ttres.addpt[1:2,], # AP1 -> orient & AP1 -> add1
                        ttres[1:3,], # AP1 -> 100 ... to SP110
                        ttres.addpt[c(3:6,8),], # additional points
                        ttres[4:nrow(ttres),], # AP1 -> 100 ... to SP110
                        ttres.addpt[nrow(ttres.addpt),] # AP2 -> orient
                        )
    ttres.full[nrow(ttres.full), "ihs"] <- ttres.full[nrow(ttres.full) - 1, "ihs"]
    ## Plot traverse with orientation and node
    pdf(paste0(StudentFilename,".pdf"), width = 3)
    plot.traverse(tteszt.first, north = 0)
    ## Additional point plotted
    lines(tteszt.addpt[-c(1,nrow(tteszt.addpt)), c("x","y")])
    points(tteszt.addpt[3:4, c("x","y")], pch = 4)
    text(tteszt.addpt[3:4, c("x","y")], lab=tteszt.addpt[3:4, "n"], adj=c(-0.2,0))
    text(tteszt.addpt[3:4, c("x","y")], lab=tteszt.addpt[3:4, "k"], adj=c(-0.2,1.2))
    dev.off()
    ## Export.csv
    write.csv2(trav.eov, paste0(StudentFilename,"coo.csv"), row.names = FALSE)
    ttres.full.degree <- ttres.full
    ttres.full.degree$h <- angleconv(ttres.full.degree$h)
    ttres.full.degree$z <- angleconv(ttres.full.degree$z)
    write.csv2(ttres.full.degree, paste0(StudentFilename,"geo.csv"), row.names = FALSE, quot = FALSE)
}

### Export to gizi
trav.eov <- tteszt[tteszt$k == "AP" | tteszt$k == "OP",]
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
points(felmer.df, col = 2)
points(Bose.df[,1:2], col =6)
dev.set(3)
plot(Bose.df[,2:3])
dev.set(2)

