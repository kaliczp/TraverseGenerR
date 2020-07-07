nevsor <- read.csv2("nevsor.csv", strings = F)

for(ttnev in 1:nrow(nevsor)) {
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
    tteszt <- gener(fulldist = long.edge, angledist = short.edge, ox=szeles/4, oy=0)
    tteszt[2:(nrow(tteszt)-1), "z"] <- predict(topo.loess, data.frame(x=tteszt$x, y=tteszt$y))[-c(1,nrow(tteszt))]
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
    act.ang <- nevsor[ttnev, "Angle"] * pi /180
    topo.eov$x <- topo.eov$x * cos(act.ang) -
        topo.eov$y * sin(act.ang)
    topo.eov$y <- topo.eov$x * sin(act.ang) +
        topo.eov$y * cos(act.ang)
    datgen(topo.eov[topo.eov$dat,], sub(" ", "",nevsor[ttnev, "Név"]),
           settlement = nevsor[ttnev, "Telep"],
           student = nevsor[ttnev, "Név"])
}

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

