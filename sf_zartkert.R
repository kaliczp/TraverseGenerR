nevsor <- read.csv2("nevsor.csv", strings = F)

parc.gen <- function(kezd, veg) {
    parcalj <- 0
    parctetej <- round(runif(1,30,50),2)
    pollist <- list(rbind(c(kezd,parcalj),
                          c(veg,parcalj),
                          c(veg, parctetej),
                          c(kezd ,parctetej),
                          c(kezd,parcalj))
                    )
    while(parctetej <= (parcellamag - 50)) {
        parcalj <- parctetej
        parctetej <- parcalj + round(runif(1,10,40),2)
        pollist[[length(pollist) + 1]]  <- rbind(c(kezd,parcalj),
                                                 c(veg,parcalj),
                                                 c(veg, parctetej),
                                                 c(kezd ,parctetej),
                                                 c(kezd,parcalj)
                                                 )
    }
    parcalj <- parctetej
    pollist[[length(pollist) + 1]]  <- rbind(c(kezd,parcalj),
                                             c(veg,parcalj),
                                             c(veg, parcellamag),
                                             c(kezd ,parcellamag),
                                             c(kezd,parcalj)
                                         )
    st_polygon(pollist)
}

for(ttnev in 1:nrow(nevsor)) {

StudentFilename <- sub(" ", "",nevsor[ttnev, "Név"])
set.seed(nevsor[ttnev, "seed"])


utszel1 <- 6.5
parcellamag <- 800
parcellaszel1 <- 77
parcellaszel2 <- 77
p1 <- rbind(c(0,0), c(utszel1,0), c(utszel1,parcellamag), c(0,parcellamag), c(0,0))
polut1 <-st_polygon(list(p1))
aktkezd <- utszel1
aktveg <- utszel1 + parcellaszel1
poltomb1.1 <- parc.gen(kezd = aktkezd, veg = aktveg)
aktkezd <- aktveg
aktveg <- aktkezd + parcellaszel1
poltomb1.2 <- parc.gen(kezd = aktkezd, veg = aktveg)
aktkezd <- aktveg
aktveg <- aktkezd + utszel1
p1 <- rbind(c(aktkezd,0), c(aktveg,0), c(aktveg, parcellamag), c(aktkezd ,parcellamag), c(aktkezd,0))
polut2 <-st_polygon(list(p1))
aktkezd <- aktveg
aktveg <- aktkezd + parcellaszel2
poltomb2.1 <- parc.gen(kezd = aktkezd, veg = aktveg)
aktkezd <- aktveg
aktveg <- aktkezd + parcellaszel2
poltomb2.2 <- parc.gen(kezd = aktkezd, veg = aktveg)
aktkezd <- aktveg
aktveg <- aktkezd + utszel1
p1 <- rbind(c(aktkezd,0), c(aktveg,0), c(aktveg, parcellamag), c(aktkezd ,parcellamag), c(aktkezd,0))
polut3 <-st_polygon(list(p1))
zkall <- st_multipolygon(list(polut1, poltomb1.1, poltomb1.2, polut2, poltomb2.1, poltomb2.2,polut3))

pdf(paste0(StudentFilename, "par.pdf"))
plot(zkall)
dev.off()

coor <- as.data.frame(st_coordinates(zkall))
names(coor) <- tolower(names(coor))

## Sokszög
travzky <- -5
while(travzky[length(travzky)] <= (parcellamag - 120)) {
    travzky <- c(travzky, travzky[length(travzky)] + runif(1, 60 ,80))
}
travzky <- c(travzky, parcellamag + 5)
travzk <- data.frame(x = round(rep(c(min(coor[coor$l2 == 4,"x"] + 0.2),
                                     max(coor[coor$l2 == 4,"x"] - 0.2)
                                     ), length.out = length(travzky)) + rnorm(length(travzky), sd = 0.01)
                             , 3),
                     y = round(travzky, 3),
                     z = round(travzky * 0.001 + rnorm(length(travzky), sd = 0.01), 3) + 80,
                     k = "sp",
                     n = 10:(9 + length(travzky)) * 10
                     )

## Tájékozó pontok
travorientzk <- travzk[c(1,nrow(travzk)),]
travorientzk[1, "k"] <- "op"
travorientzk[1, "n"] <- sample(1:8,1)*10
travorientzk[1, "x"] <- sample(-1500:1500,1)
travorientzk[1, "y"] <- sample(-1500:-2600,1)
travorientzk[1, "z"] <- travorientzk[1, "z"] + sample(10:30, 1)
travorientzk[2, "k"] <- "op"
travorientzk[2, "n"] <- travorientzk[1, "n"] + 10
travorientzk[2, "x"] <- sample(-1500:1500,1)
travorientzk[2, "y"] <- sample(1500:2600,1)
travorientzk[2, "z"] <- travorientzk[2, "z"] + sample(10:30, 1)
travorientzk[, "x"] <- travorientzk[, "x"] + round(rnorm(2),3)
travorientzk[, "y"] <- travorientzk[, "y"] + round(rnorm(2),3)
travorientzk[, "z"] <- travorientzk[, "z"] + round(rnorm(2),3)

## A középső út körüli
coor.csak <- coor[coor$l2 == 3 | coor$l2 == 5 ,c("x","y")]
coor.csak <- unique(coor.csak[order(coor.csak$y),])
## Terep
coor.csak$z <- round(coor.csak$y * 0.001 + rnorm(nrow(coor.csak), sd = 0.01), 3) + 80

## Selection to traverse points
tthatarprev <- min(travzk[, "y"]) - 1
ttnumprev <- 2001
travzkmeas <- travzk[1,]
travzkmeas <- rbind(travzkmeas, travorientzk[1,])
for(tti in 1:(nrow(travzk)-1)) {
    tthatar <- mean(c(travzk[tti, "y"], travzk[tti+1, "y"]))
    ## Pont válogatás
    ttselect <- coor.csak[coor.csak$y > tthatarprev & coor.csak$y < tthatar, ]
    ttselect$k <- "f"
    ttnumend <- ttnumprev + (nrow(ttselect) - 1)
    ttselect$n <- ttnumprev:ttnumend
    ttnumprev <- ttnumend + 1
    travzkmeas <- rbind(travzkmeas, ttselect, travzk[tti + 1,])
    tthatarprev <- tthatar
}
travzkmeas <- rbind(travzkmeas, travorientzk[2,])
ttselect <- coor.csak[coor.csak$y > tthatarprev,]
ttselect$k <- "f"
ttnumend <- ttnumprev + (nrow(ttselect) - 1)
ttselect$n <- ttnumprev:ttnumend
travzkmeas.norot <- rbind(travzkmeas, ttselect)
row.names(travzkmeas.norot) <- NULL

## Rotate
travzkmeas <- eovrotate(travzkmeas.norot, nevsor[ttnev, "Angle"])

## Shift
travzkmeas[,"x"] <- travzkmeas[,"x"] + nevsor[ttnev, "easting"]
travzkmeas[,"y"] <- travzkmeas[,"y"] + nevsor[ttnev, "northing"]

## Traverse generation
ttres <- meascalc.ordered(travzkmeas, generror = TRUE)
ttres.degree <- twoface(ttres)
ttres.degree$h <- angleconv(ttres.degree$h, format = "dot", round.sec = 1)
ttres.degree$z <- angleconv(ttres.degree$z, format = "dot", round.sec = 1)
ttres.degree[,"d"] <- round(ttres.degree[,"d"], 3)
## Export
write(paste0(export.m5(paste0("Kalicz",Sys.Date()), angle =ttres.degree, coor = travzkmeas),"\r"), paste0(StudentFilename,".m5"), sep="\n")

plot.traverse(travzkmeas[travzkmeas$k == "sp" | travzkmeas$k == "op", ], tofile = paste0(StudentFilename, "trv.pdf"))
}
