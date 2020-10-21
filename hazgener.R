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
}

plot(haz[,1:2], asp=T)
text(x = haz[,"x"], y = haz[,"y"], lab=row.names(haz), adj=c(0,1))

## 1 tájékozó és három SP
travhaz <- travnoo.eov[1:4,]
travhaz[1,"k"] <- "OP"
ttres <- meascalc(travhaz, orient = TRUE, generror = TRUE)
ttres <- ttres[-nrow(ttres),]

## GeoEasy teszt
write(export.coo.gizi(travhaz[1:2,]), paste0(StudentFilename,".coo"), sep="\n")
write(export.geo.gizi(ttres), paste0(StudentFilename,".geo"), sep="\n")

ttres.degree <- ttres
ttres.degree$h <- angleconv(ttres.degree$h, format = "dot", round.sec = 1)
ttres.degree$z <- angleconv(ttres.degree$z, format = "dot", round.sec = 1)
ttres.degree[1,"z"] <- NA

write(paste0(export.m5(paste0("KaliczPéter",Sys.Date()), coor = travhaz.eov),"\r"), "data.m5", sep="\n")

write(paste0(export.m5(paste0("Kalicz",Sys.Date()), angle =ttres.degree, coor = travhaz),"\r"), "data.m5", sep="\n")

