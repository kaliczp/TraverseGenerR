nevsor <- read.csv2("nevsor.csv", strings = F)

## Szomszeddal sdr-be átírva
szomszed <- TRUE
nevsor$seed <- nevsor$seed + 1
nevsor$easting <- nevsor$easting + 50
nevsor$northing <- nevsor$northing + 50

haz.lst <- list()
for(ttnev in 1:nrow(nevsor)) {
    muszer <- nevsor[ttnev, "Meastype"]
    StudentFilename <- sub(" ", "",nevsor[ttnev, "Név"])
    if(muszer == "sokkia") {
        set.seed(nevsor[ttnev, "seed"] + 30)
    } else {
        set.seed(nevsor[ttnev, "seed"] + 20)
    }
    ## Alap magasság
    magas <- sample((9:13)*10,1)
    ## Telek hossza, széle
    hosszu  <- sample(80:200,1)
    szeles <- sample(25:45,1)
    haz <- data.frame(x=rep(0,4), y=rep(0,4), z=rep(magas,4))
    haz[2:3,"x"] <- szeles
    haz[3:4,"y"] <- hosszu
    ## Szomszéd telkek
    if(szomszed) {
        haz[5,"x"] <- round(haz[4,"x"] - szeles + rnorm(1),2)
        haz[6,"x"] <- round(haz[3,"x"] + szeles + rnorm(1),2)
        haz[5:6,"y"] <- hosszu
        haz[5:6,"z"] <- magas
    }
    haz$k  <- "t"
    ## z random
    haz[,"z"] <- round(haz[,"z"] + rnorm(nrow(haz), sd=.1), 3)
    ## x, y random
    haz[,"x"] <- round(haz[,"x"] + rnorm(nrow(haz), sd=.005), 3)
    haz[,"y"] <- round(haz[,"y"] + rnorm(nrow(haz), sd=.005), 3)
    ## Sokszög
    ## Ház előtti és ház mögötti pont
    travhaz <- data.frame(x = szeles/2 + rnorm(2),
                          y = c(hosszu + sample(2:10,1), # Utca
                              hosszu/2 - sample(0:(hosszu/4),1)  # Udvar
                              ) + rnorm(2),
                          z = magas,
                          k = "sp",
                          n = c(110, 120)
                          )
    travhaz[,"x"] <- round(travhaz[,"x"], 3)
    travhaz[,"y"] <- round(travhaz[,"y"], 3)
    ## Tájékozó pontok
    travorient <- travhaz
    travorient[1, "k"] <- "op"
    travorient[, "n"] <- c(sample(1:9,1)*10, 100)
    travorient[2, "y"]  <- travorient[1, "y"] + sample(2:10,1) #Utca vége y
    travorient[2, "x"]  <- travorient[1, "x"] + sample(50:100,1) #Utca vége x
    ## Tájékozó távoli
    travorient[1, "y"] <- sample(-1500:1500,1)
    travorient[1, "x"] <- sample(200:1500,1)
    ## Randomizáció
    travorient[,"x"] <- round(travorient[,"x"] + rnorm(2), 3)
    travorient[,"y"] <- round(travorient[,"y"] + rnorm(2), 3)
    travorient[,"z"] <- round(travorient[,"z"] + rnorm(2, sd=3), 3)
    ## Felmérő, tájékozó SP egyesítése
    travfull <- rbind(travorient, travhaz)
    ## Eltolás
    travfull[,"x"] <- travfull[,"x"] + nevsor[ttnev, "easting"]
    travfull[,"y"] <- travfull[,"y"] + nevsor[ttnev, "northing"]
    haz[,"x"] <- haz[,"x"] + nevsor[ttnev, "easting"]
    haz[,"y"] <- haz[,"y"] + nevsor[ttnev, "northing"]
    ## Északról délre növekvő a pontszám miatt
    haz.pnum <- haz[order(haz$y, decreasing = TRUE),]
    haz.pnum$n <- 1001:(1001 + nrow(haz.pnum) - 1)
    if(szomszed){
        travfulltopo <- rbind(travfull[1:3,], haz.pnum[1:4,], travfull[4,],haz.pnum[5:6,])
    } else {
    travfulltopo <- rbind(travfull[1:3,], haz.pnum[1:2,], travfull[4,],haz.pnum[3:4,])
    }
    row.names(travfulltopo) <- NULL
    ## Forgatás
    forgscale <- ifelse(muszer == "sokkia", -1.1, 1)
    travfulltopo.rot <- eovrotate(travfulltopo, forgscale * nevsor[ttnev, "Angle"])
    travfull.rot <- travfulltopo.rot[travfulltopo.rot$n < 1000,]
    haz.lst[[ttnev]] <- travfulltopo.rot[travfulltopo.rot$n > 1000,1:2]
    ## 1 tájékozó és három SP
    ttres <- meascalc(travfulltopo.rot, orient = TRUE, generror = TRUE, topo = TRUE)
    ttres <- ttres[-nrow(ttres),]
    ## Szög konvertálás
    ttres.degree <- ttres
    if(muszer == "sokkia") {
        ttres.degree <- twoface(ttres.degree)
        ttres.degree$h <- angleconv(ttres.degree$h, output = "degree", round.sec = 1)
        ttres.degree[-1,"z"] <- angleconv(ttres.degree[-1,"z"], output = "degree", round.sec = 1)
    } else {
    ttres.degree$h <- angleconv(ttres.degree$h, format = "dot", round.sec = 1)
    ttres.degree[-1,"z"] <- angleconv(ttres.degree[-1,"z"], format = "dot", round.sec = 1)
    }
    ttres.degree[1,"z"] <- NA
    ## Magassági hiba
    travfull$z <- round(travfull.rot$z + rnorm(nrow(travfull.rot), sd = 0.007),3)
    ## Exportálás
    if(muszer == "sokkia") {
        meres <- export.sdr(angle =ttres.degree,coor = travfull.rot[c(1:2,4),])
        kiterj <- ".sdr"
    } else {
        meres <- export.m5(paste0("Kalicz",Sys.Date()), angle =ttres.degree, coor = travfull.rot[c(1:2,4),])
        kiterj <- ".m5"
        }
    write(paste0(meres,"\r"), paste0(StudentFilename, kiterj), sep="\n")
    write.table(haz.lst[[ttnev]], paste0(StudentFilename, ".csv"), eol= "\r\n", quot = FALSE, row=FALSE, sep = ";", dec = ",")
}

plot(travfull.rot[,1:2],asp=T, ylim = quantile(travfulltopo.rot$y, probs = c(0,1)))
points(travfulltopo.rot[travfulltopo.rot$n > 1000,1:2], col = 2)

pdf(paper = "a4")
par(mfrow = c(3,6), mar = c(0,0,0,0))
for(ttnev in 1:length(haz.lst)) {
    plot(haz.lst[[ttnev]][c(1:2,4:3,1),], type = "l", axes = FALSE, asp = TRUE, xlab = "", ylab = "")
    legend("bottomleft", legend = nevsor[ttnev, "Név"], bty = "n")
}
dev.off()
