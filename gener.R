newcoo <- coo[10:9,]

gener <- function(slope=6, firstnr = 100, ox=464800,oy=259400, oz=250, orient = c(1,1)){
    fulldist <- sample(400:500,1)
    angledist <- sample(50:100,1)
    segments.nr <- sample(5:7, 1)
    segment.length <- fulldist/segments.nr
    frame <- data.frame(x=c(angledist,rep(0,segments.nr+1)),
                        y=c(fulldist,
                            seq(fulldist, 0, -segment.length))
                        )
    frame$x <- frame$x + rnorm(nrow(frame), sd = 10)
    frame$y <- frame$y + rnorm(nrow(frame), sd = 5)
    frame$z <- -slope/100*frame$y
    frame$k <- c("AP", rep("SP", segments.nr), "AP")
    fixpoints <- sort(sample(1:9,2))*10
    frame$n <- c(fixpoints[1], seq(firstnr, by = 10, length = segments.nr),
                 fixpoints[2])
    orient.nr  <- sum(orient)
    if(orient.nr > 0) {
        orie.x <- sample(700:1000, orient.nr)
        orie.y <- c(rep(fulldist,orient[1]),rep(0,orient[2])) +
            rnorm(orient.nr, sd = 50)
        orie.z <- mean(frame$z) + rnorm(orient.nr, 20, 1)
        orient.df <- data.frame(x = orie.x, y = orie.y, z = orie.z,
                                k = rep("OP", orient.nr),
                                n = 1:orient.nr)
        if(orient[1] > 0)
            frame <- rbind(orient.df[1:orient[1],], frame)
        if(orient[2] > 0)
            orient.row.num <- (nrow(orient.df) - orient[2] + 1):nrow(orient.df) 
            frame <- rbind(frame, orient.df[orient.row.num,])
    }
    ## Translate
    frame$x <- frame$x + ox
    frame$y <- frame$y + oy
    frame$z <- frame$z + oz
    frame
}

tteszt <- gener()
plot(tteszt[,1:2], asp=TRUE)


write.csv(tteszt, "newteszt.csv", row.names = FALSE, quote = FALSE)

meascalc <- function(coord, ins.height.range = c(1.450, 1.620), orient = TRUE) {
    if(orient) {
        orient.idx <- coord$k == "OP"
        orient <- coord[orient.idx, ]
        coord <- coord[!orient.idx, ]
    }
    ins.height  <- sample(seq(ins.height.range[1],
                              ins.height.range[2], by=.001),
                          nrow(coord))
    coord$z <- coord$z + ins.height
    slop.dist <- sqrt(diff(coord$x)^2 + diff(coord$y)^2 + diff(coord$z)^2)
    slop.dist <- round(slop.dist,3)
    hor.angle <- -atan2(diff(coord$y),diff(coord$x))
    hor.angle.back <- hor.angle + pi
    zenit.for <- pi/2 - asin(diff(coord$z)/slop.dist)
    zenit.back <- pi/2 + asin(diff(coord$z)/slop.dist)
    fore <- data.frame(ns = coord$n[-nrow(coord)],
                      ihs = ins.height[-length(ins.height)],
                      nfb = coord$n[-1],
                      ihfb = ins.height[-1],
                      h = hor.angle,
                      z = zenit.for,
                      d = slop.dist
                      )
    back <- data.frame(ns = coord$n[-1],
                      ihs = ins.height[-1],
                      nfb = coord$n[-nrow(coord)],
                      ihfb = ins.height[-length(ins.height)],
                      h = hor.angle.back,
                      z = zenit.back,
                      d = slop.dist
                      )
    result <- rbind(fore, back[-nrow(back),])
    result.ord <- result[order(result$ns, result$nf),]
    result.ok <- rbind(result.ord, back[nrow(back), ])
    ## Are there any negative angle?
    negh.row <- result.ok$h < 0
    if(any(negh.row)) {
        ## Correct negative angles
        result.ok[negh.row, "h"]  <- result.ok[negh.row, "h"] + 2*pi
    }
    result.ok
}

(ttres <- meascalc(tteszt))
ttres$h <- angleconv(ttres$h)
ttres$z <- angleconv(ttres$z)

angleconv <- function(angle, round.sec = 0, input = "radian") {
    if(input == "radian") {
        angle  <-  angle * 180 / pi
    }
    angle.trunc <- trunc(angle)
    mins <- (angle - angle.trunc)*60
    mins.trunc <- trunc(mins)
    secs <- (mins - mins.trunc)*60
    paste(angle.trunc, mins.trunc, round(secs, round.sec), sep="-")
}

export.geo.gizi <- function(traverse) {
    ## Empty result
    result <- character()
    last.station <- 0
    ## Processing row-by-row
    for(row.num in 1:nrow(traverse)){
        curr.station <- traverse[row.num, "ns"]
        if(last.station != curr.station) {
            result <- c(result,
                        paste0("{2 ",
                               curr.station,
                               "} {6 ",
                               traverse[row.num, "ihs"],
                               "}"
                               )
                        )
        }
        target.id <- traverse[row.num, "nfb"]
        result <- c(result,
                    paste0("{5 ",
                           target.id,
                           "} {6 ", # Instrument height of target
                           traverse[row.num, "ihfb"],
                           "} {7 ",
                           traverse[row.num, "h"],
                           "} {8 ",
                           traverse[row.num, "z"],
                           "} {9 ",
                           round(traverse[row.num, "d"], 5),
                           "}"
                           )
                    )
        last.station <- curr.station
    }
    result
}

export.coo.gizi <- function(coordinates) {
    ## Empty result
    result <- character()
    last.station <- 0
    ## Processing row-by-row
    for(row.num in 1:nrow(coordinates)){
        curr.station <- coordinates[row.num, "n"]
        result <- c(result,
                    paste0("{5 ",
                           curr.station,
                           "} {37 ",
                           round(coordinates[row.num, "y"], 4),
                           "} {38 ",
                           round(coordinates[row.num, "x"], 4),
                           "} {39 ",
                           round(coordinates[row.num, "z"], 4),
                           "}"
                           )
                    )
    }
    result
}

## Full process
tteszt <- gener()
## pdf(width = 2.5)
par(mar=c(0,0,0,0))
plot(tteszt[tteszt$k != "OP",1:2], asp=TRUE, axes = FALSE, xlab="", ylab="", pch=4)
lines(tteszt[tteszt$k != "OP",1:2])
text(tteszt[,c("x","y")], lab=tteszt$n, adj=c(1.2,0))
text(tteszt[,c("x","y")], lab=tteszt$k, adj=c(1.2,1.2))
ttres <- meascalc(tteszt)
write(export.coo.gizi(tteszt[tteszt$k == "AP",]), "newteszt.coo", sep="\n")
write(export.geo.gizi(ttres), "newteszt.geo", sep="\n")
## Compare angles
ttres$h <- angleconv(ttres$h)
ttres$z <- angleconv(ttres$z)
