utszel1 <- 6.5
parcellamag <- 800
parcellaszel1 <- 77
parcellaszel2 <- 77
p1 <- rbind(c(0,0), c(utszel1,0), c(utszel1,parcellamag), c(0,parcellamag), c(0,0))
polut1 <-st_polygon(list(p1))
aktkezd <- utszel1
aktveg <- utszel1 + parcellaszel1
p1 <- rbind(c(aktkezd,0), c(aktveg,0), c(aktveg, parcellamag), c(aktkezd ,parcellamag), c(aktkezd,0))
poltomb1.1 <-st_polygon(list(p1))
aktkezd <- aktveg
aktveg <- aktkezd + parcellaszel1
p1 <- rbind(c(aktkezd,0), c(aktveg,0), c(aktveg, parcellamag), c(aktkezd ,parcellamag), c(aktkezd,0))
poltomb1.2 <-st_polygon(list(p1))
aktkezd <- aktveg
aktveg <- aktkezd + utszel1
p1 <- rbind(c(aktkezd,0), c(aktveg,0), c(aktveg, parcellamag), c(aktkezd ,parcellamag), c(aktkezd,0))
polut2 <-st_polygon(list(p1))
aktkezd <- aktveg
aktveg <- aktkezd + parcellaszel2
p1 <- rbind(c(aktkezd,0), c(aktveg,0), c(aktveg, parcellamag), c(aktkezd ,parcellamag), c(aktkezd,0))
poltomb2.1 <-st_polygon(list(p1))
zkall <- st_multipolygon(list(polut1, poltomb1.1, poltomb1.2, polut2, poltomb2.1))
plot(zkall)
