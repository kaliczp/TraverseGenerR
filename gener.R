newcoo <- coo[10:9,]

gener <- function(slope=6, firstnr = 100, ox=464800,oy=259400, oz=250){
    fulldist <- sample(400:500,1)
    angledist <- sample(50:100,1)
    segments.nr <- sample(5:7, 1)
    segment.length <- fulldist/segments.nr
    frame <- data.frame(x=c(angledist,rep(0,segments.nr+1)),
                        y=c(fulldist,
                            seq(fulldist, 0, -segment.length))
                        )
    frame$x <- frame$x + rnorm(nrow(frame), sd = 20)
    frame$y <- frame$y + rnorm(nrow(frame), sd = 5)
    frame$z <- -slope/100*frame$y
    frame$k <- c("AP", rep("SP", segments.nr), "AP")
    fixpoints <- sort(sample(1:9,2))*10
    frame$n <- c(fixpoints[1], seq(firstnr, by = 10, length = segments.nr),
                 fixpoints[2])
    ## Translate
    frame$x <- frame$x + ox
    frame$y <- frame$y + oy
    frame$z <- frame$z + oz
    frame
}

tteszt <- gener()
plot(tteszt[,1:2], asp=TRUE)

write.csv(tteszt, "newteszt.csv", row.names = FALSE, quote = FALSE)

## Without instument hight!
meascalc <- function(coord) {
    slop.dist <- sqrt(diff(coord$x)^2 + diff(coord$y)^2 + diff(coord$z)^2)
    hor.angle <- -atan2(diff(coord$y),diff(coord$x))
    hor.angle <- hor.angle*180/pi
    hor.angle.back <- 180-hor.angle
    zenit.for <- 90-asin(diff(coord$z)/slop.dist)*180/pi
    zenit.back <- 90+asin(diff(coord$z)/slop.dist)*180/pi
    fore <- data.frame(ns = coord$n[-nrow(coord)],
                      nf = coord$n[-1],
                      h = hor.angle,
                      z = zenit.for,
                      d = slop.dist
                      )
    back <- data.frame(ns = coord$n[-1],
                      nf = coord$n[-nrow(coord)],
                      h = hor.angle.back,
                      z = zenit.back,
                      d = slop.dist
                      )
    result <- rbind(fore, back)
    result[order(result$ns, result$nf),]
}

ttres <- meascalc(tteszt)
ttres$h <- angleconv(ttres$h)
ttres$z <- angleconv(ttres$z)

angleconv <- function(angle, round.sec = 0) {
    angle.trunc <- trunc(angle)
    mins <- (angle - angle.trunc)*60
    mins.trunc <- trunc(mins)
    secs <- (mins - mins.trunc)*60
    paste(angle.trunc, mins.trunc, round(secs, round.sec), sep="-")
}
