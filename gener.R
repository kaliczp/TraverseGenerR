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
tttavs <- sqrt(diff(tteszt$x)^2 + diff(tteszt$y)^2 + diff(tteszt$z)^2)
ttvszog <- -atan2(diff(tteszt$y),diff(tteszt$x))*180/pi
(ttvszog-trunc(ttvszog))*60
180-ttvszog
ttmszogoda <- 90-asin(diff(tteszt$z)/tttavs)*180/pi
(ttmszogoda-trunc(ttmszogoda))*60
ttmszogvissza <- 90+asin(diff(tteszt$z)/tttavs)*180/pi

angleconv <- function(angle, round.sec = 0) {
    angle.trunc <- trunc(angle)
    mins <- (angle - angle.trunc)*60
    mins.trunc <- trunc(mins)
    secs <- (mins - mins.trunc)*60
    paste(angle.trunc, mins.trunc, round(secs, round.sec), sep="-")
}
