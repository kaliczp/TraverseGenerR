nevsor <- read.csv2("nevsor.csv", strings = F)

for(ttnev in 1:nrow(nevsor)) {
    StudentFilename <- sub(" ", "",nevsor[ttnev, "Név"])
    set.seed(nevsor[ttnev, "seed"])
    ## Alap magasság
    magas <- sample((9:13)*10,1)
    ## Telek hossza, széle
    hosszu  <- sample(80:200,1)
    szeles <- sample(25:45,1)
    haz <- data.frame(x=rep(0,4), y=rep(0,4), z=rep(magas,4))
    haz[2:3,"x"] <- szeles
    haz[3:4,"y"] <- hosszu
    ## Szomszéd telkek
    haz[5,"x"] <- round(haz[4,"x"] - szeles + rnorm(1),2)
    haz[6,"x"] <- round(haz[3,"x"] + szeles + rnorm(1),2)
    haz[5:6,"y"] <- hosszu
    haz[5:6,"z"] <- magas
    ## Épület 10x10m-es
    elokert <- hosszu - sample(2:10, 1)
    haz[7:10, "y"] <- elokert - c(0,0,10,10)
    haz[7:10, "x"] <- szeles - c(0,10,0,10)
    haz[7:10, "z"] <- magas
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
                          k = "SP",
                          n = c(110, 120)
                          )
    travhaz[,"x"] <- round(travhaz[,"x"], 3)
    travhaz[,"y"] <- round(travhaz[,"y"], 3)
    ## Tájékozó pontok
    travorient <- travhaz
    travorient[1, "k"] <- "OP"
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
    ## 1 tájékozó és három SP
    ttres <- meascalc(travfull, orient = TRUE, generror = TRUE)
    ttres <- ttres[-nrow(ttres),]
    ## Szög konvertálás
    ttres.degree <- ttres
    ttres.degree$h <- angleconv(ttres.degree$h, format = "dot", round.sec = 1)
    ttres.degree[-1,"z"] <- angleconv(ttres.degree[-1,"z"], format = "dot", round.sec = 1)
    ttres.degree[1,"z"] <- NA
    ## Magassági hiba
    travfull$z <- round(travfull$z + rnorm(nrow(travfull), sd = 0.007),3)
    ## Exportálás
    write(paste0(export.m5(paste0("Kalicz",Sys.Date()), angle =ttres.degree, coor = travfull[c(1:2,4),]),"\r"), paste0(StudentFilename,".m5"), sep="\n")
}

plot(haz[,1:2], asp=T, xlim = c(-60, 150), ylim=c(0,hosszu+20), ask = TRUE)
text(x = haz[,"x"], y = haz[,"y"], lab=row.names(haz), adj=c(0,1))
points(travhaz[,1:2])
points(travorient[,1:2])

plot(travfull[,1:2],asp=T)
points(haz[,1:2])

plot(haz[,1:2], asp=T)

## GeoEasy teszt
write(export.coo.gizi(travhaz[1:2,]), paste0(StudentFilename,".coo"), sep="\n")
write(export.geo.gizi(ttres), paste0(StudentFilename,".geo"), sep="\n")
