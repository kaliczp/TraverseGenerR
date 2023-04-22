parc.gen <- function(kezd, veg) {
    parcalj <- 0
    parctetej <- round(runif(1,30,50),2)
    pollist <- list(rbind(c(kezd,parcalj),
                          c(veg,parcalj),
                          c(veg, parctetej),
                          c(kezd ,parctetej),
                          c(kezd,parcalj))
                    )
    while(parctetej <= (parcellamag - 30)) {
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
zkall <- st_multipolygon(list(polut1, poltomb1.1, poltomb1.2, polut2, poltomb2.1, poltomb2.2))
plot(zkall)
