newcoo <- coo[10:9,]

gener <- function(slope=6, firstnr = 100, angledist = NULL, fulldist = NULL, ox=464800,oy=259400, oz=250, orient = c(1,1), additional = TRUE){
    if(is.null(fulldist))
        fulldist <- sample(400:500,1)
    if(is.null(angledist))
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
    if(additional) {
        ## Additional point generation mean of first two points
        addtnl <- colMeans(frame[c(1,3), c("x", "y", "z")])
        addtnl.df <- data.frame(x = addtnl["x"] + rnorm(1,sd = 5),
                                y = addtnl["y"] + rnorm(1,sd = 5),
                                z = addtnl["z"] + rnorm(1,sd = 5),
                                k = "SPP",
                                n = firstnr + 1)
        frame <- rbind(frame, addtnl.df)
    }
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
        if(orient[2] > 0) {
            orient.row.num <- (nrow(orient.df) - orient[2] + 1):nrow(orient.df) 
            frame <- rbind(frame, orient.df[orient.row.num,])
        }
        row.names(frame) <- NULL
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

meascalc <- function(coord, ins.height.range = c(1.450, 1.620), orient = TRUE, generror = FALSE) {
    if(orient) {
        ## Are there orient really?
        orient.idx <- coord$k == "OP"
        if(!any(orient.idx)) {
            orient = FALSE
        }
    }
    if(orient) {
        ## Separate orientation and standard measurenets
        orient.idx <- which(orient.idx)
        orient.df <- coord[orient.idx, ]
        coord <- coord[-orient.idx, ]
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
    if(orient) {
        ## Are multiple points orientated?
        orient.sep <- diff(orient.idx) > 1
        ## More points oriented?
        if(any(orient.sep)) {
            ## Separate start and end
            orient.sep <- which(diff(orient.idx) > 1)
            ## from start point
            first.orient.idx <- row.names(orient.df)[1:orient.sep]
            first.orient.df <- rbind(orient.df[first.orient.idx,],
                                     coord[1,])
            ## 2pi - angle + pi
            first.hor.angle.ori <- pi - atan2(diff(first.orient.df$y),
                                         diff(first.orient.df$x))
            ## from end point
            last.orient.idx <- row.names(orient.df)[(orient.sep+1):nrow(orient.df)]
            last.orient.df <- rbind(orient.df[last.orient.idx,],
                                    coord[nrow(coord),])
            last.hor.angle.ori <- pi - atan2(diff(last.orient.df$y),
                                         diff(last.orient.df$x))
            ori.fin <- data.frame(ns = c(first.orient.df[nrow(first.orient.df), "n"],
                                         last.orient.df[nrow(first.orient.df), "n"]),
                                  ihs = ins.height[1],
                                  nfb = c(first.orient.df[1, "n"],
                                          last.orient.df[1, "n"]),
                                  ihfb = 0,
                                  h = c(first.hor.angle.ori,last.hor.angle.ori),
                                  z = NA,
                                  d = NA
                      )
        } else {
            ## If only one point oriented first or last?
        }
    }
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
    if(orient) {
        result <- rbind(result, ori.fin[1,])
    }
    result.ord <- result[order(result$ns, result$nf),]
    result.ok <- rbind(result.ord, back[nrow(back), ])
    if(orient) {
        result.ok <- rbind(result.ok, ori.fin[nrow(ori.fin),])
    }
    ## Are there any negative angle?
    negh.row <- result.ok$h < 0
    if(any(negh.row)) {
        ## Correct negative angles
        result.ok[negh.row, "h"]  <- result.ok[negh.row, "h"] + 2*pi
    }
    if(generror) {
        sec.rad <- (1/60/60)*pi/180 # one second in rad
        result.ok$h <- result.ok$h + rnorm(nrow(result.ok), sd = 3 * sec.rad)
        result.ok$z <- result.ok$z + rnorm(nrow(result.ok), sd = 3 * sec.rad)
        result.ok$z <- result.ok$z + rnorm(nrow(result.ok), sd = 0.0004)
    }
    result.ok
}

(ttres <- meascalc(tteszt))

twoface <- function(measdata) {
    meas.face <- cbind(measdata[,1:4], fce = NA, measdata[, 5:ncol(measdata)])
    meas.face$fce <- factor(rep(c("I"), nrow(meas.face)), levels = c("I","II"))
    meas.colnames <- colnames(meas.face)
    result <- as.data.frame(matrix(ncol = length(meas.colnames)))
    colnames(result) <- meas.colnames
    for(row.num in 1:nrow(meas.face)){
        ## Angles
        act.error <- abs(rnorm(2,sd=20))*10^(-6)
        actualrow <- meas.face[row.num, ]
        actualrow$h  <- actualrow$h + act.error[1]
        nextrow <- actualrow
        nextrow$h  <- nextrow$h + pi + act.error[1]
        actualrow$z  <- actualrow$z + act.error[2]
        nextrow$z  <- 2*pi - nextrow$z + act.error[2]
        ## Distance
        dist.err <- sample(c(-0.001, 0, 0, 0, 0.001), 2, replace = TRUE)
        actualrow$d <- actualrow$d + dist.err[1]
        nextrow$d <- nextrow$d + dist.err[2]
        nextrow$fce <- "II"
        result <- rbind(result, actualrow, nextrow)
    }
    result <- result[-1,]
    bigangle <- function(x) {
        bigger.nr <- which(x >= 2*pi)
        if(length(bigger.nr) > 0) {
            x[bigger.nr] <- x[bigger.nr] - 2 * pi
        }
        x
    }
    result$h  <- bigangle(result$h)
    result$z  <- bigangle(result$z)
    result
}
ttface <- twoface(ttres)

ttres$h <- angleconv(ttres$h)
ttres$z <- angleconv(ttres$z)

angleconv <- function(angle, round.sec = 0, input = "radian", output = "sexagesimal") {
    if(input == "radian") {
        angle  <-  angle * 180 / pi
    }
    if(output == "sexagesimal") {
        angle.trunc <- trunc(angle)
        mins <- (angle - angle.trunc)*60
        mins.trunc <- trunc(mins)
        secs <- (mins - mins.trunc)*60
        return(paste(angle.trunc, mins.trunc, round(secs, round.sec), sep="-"))
    } else {
        return(angle)
    }
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
        if(is.na(traverse[row.num, "z"])) {
            zenit.dist <- "}"
        } else {
            zenit.dist <- paste0("} {8 ",
                                 traverse[row.num, "z"],
                                 "} {9 ",
                                 round(traverse[row.num, "d"], 5),
                                 "}")
        }
        result <- c(result,
                    paste0("{5 ",
                           target.id,
                           "} {6 ", # Instrument height of target
                           traverse[row.num, "ihfb"],
                           "} {7 ",
                           traverse[row.num, "h"],
                           zenit.dist
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

plot.traverse <- function(traverse, tofile = NULL, north = NULL) {
    if(!is.null(tofile))
        pdf(file = tofile, width = 3)
    par(mar=c(0,0,0,0))
    ## Mean points for plots
    x.mean <- mean(traverse$x)
    y.mean <- mean(traverse$y)
    ## Filter orientation points out
    traverse.xy <- traverse[traverse$k != "OP", 1:2]
    traverse.lab <- traverse[traverse$k != "OP", 4:5]
    ## Extend traverse with mean and lower then x minima.
    trav.extend <- data.frame(x = c(x.mean, min(traverse.xy$x - 30)),
                                    y = rep(y.mean, 2))
    ## Plot traverse point with line
    plot(rbind(traverse.xy, trav.extend),
         asp=TRUE, axes = FALSE, xlab="", ylab="", type = "n")
    points(traverse.xy, pch=4)
    lines(traverse.xy)
    ## Point numbers
    text(traverse.xy, lab=traverse.lab$n, adj=c(1.2,0))
    ## Point codes
    text(traverse.xy, lab=traverse.lab$k, adj=c(1.2,1.2))
    ## First orientation arrow
    arrows(traverse[2,1], traverse[2,2], # from
           traverse[2,1]+40, traverse[2,2], # to
           lty="dashed", angle = 15)
    ## Text for first
    ## number
    text(traverse[2, 1] + 40, traverse[2, 2],
         lab=traverse[1, "n"], adj=c(0,0))
    ## code
    text(traverse[2,1] + 40, traverse[2, 2],
         lab=traverse[1, "k"], adj=c(0,1.2))
    ## Second orientation arrow
    arrows(traverse[nrow(traverse)-1,1], traverse[nrow(traverse)-1,2], #from
           traverse[nrow(traverse)-1,1]+40, traverse[nrow(traverse)-1,2], #to
           lty="dashed", angle = 15)
    ## Text for second
    ## number
    text(traverse[nrow(traverse)-1,1] + 40, traverse[nrow(traverse)-1,2],
         lab=traverse[nrow(traverse),"n"], adj=c(0,0))
    ## code
    text(traverse[nrow(traverse)-1,1] + 40, traverse[nrow(traverse)-1,2],
         lab=traverse[nrow(traverse),"k"], adj=c(0,1.2))
    if(!is.null(north)) {
        x.arr.end <- x.mean - 20
        y.arr.end <- y.mean + 30
        arrows(x.mean - 20, y.mean, x.arr.end, y.arr.end)
        text(x.arr.end, y.arr.end, "N", adj = c(0.5, -0.5))
        lines(x = c(rep(x.mean - 50, 2), rep(x.mean, 2)),
              y = c(y.mean - 35, rep(y.mean - 40, 2), y.mean - 35))
        text(x = x.mean - 25, y = y.mean - 35, "50 m", adj = c(0.5, 0))
    }
    ## Close file
    if(!is.null(tofile))
        dev.off()
}

## Initial traverse
tteszt <- gener(orient=c(0,0),firstnr=200,additional=FALSE)
ttres <- meascalc(tteszt)
write(export.coo.gizi(tteszt), "newteszt.coo", sep="\n")
write(export.geo.gizi(ttres), "newteszt.geo", sep="\n")
pdf(width=2)
plot.traverse(tteszt, north = 0)
dev.off()

## Full process
tteszt <- gener(ox=80, oy=0)
tteszt[2:(nrow(tteszt)-1), "z"] <- predict(topo.loess, data.frame(x=tteszt$x, y=tteszt$y))[-c(1,nrow(tteszt))]
## Points on topo
points(tteszt[, c("x","y")], col=4)
tteszt$x <- tteszt$x + 464800
tteszt$y <- tteszt$y + 259400
addpt.nr <- which(tteszt$k == "SPP")
tteszt.first <- tteszt[-addpt.nr, ]
ttres <- meascalc(tteszt.first)
tteszt.addpt <- rbind(tteszt[1:2,],tteszt[addpt.nr, ], tteszt[4,])
ttres.addpt <- meascalc(tteszt.addpt)
write(export.coo.gizi(tteszt[tteszt$k == "AP" | tteszt$k == "OP" ,]), "newteszt.coo", sep="\n")
write(export.geo.gizi(ttres), "newteszt.geo", sep="\n")
write(export.geo.gizi(ttres.addpt), "newtesztadd.geo", sep="\n")
plot.traverse(tteszt.first, north = 0)
## Additional point plotted
lines(tteszt.addpt[-1, c("x","y")])
points(tteszt.addpt[3, c("x","y")], pch = 4)
text(tteszt.addpt[3, c("x","y")], lab=tteszt.addpt[3, "n"], adj=c(1.2,0))
text(tteszt.addpt[3, c("x","y")], lab=tteszt.addpt[3, "k"], adj=c(1.2,1.2))
## two faces
ttface <- twoface(ttres)
## Compare angles
ttres$h <- angleconv(ttres$h)
ttres$z <- angleconv(ttres$z)

## DAT file preparation and generation
topo.eov <- topo
topo.eov$x <- round(topo.eov$x,3) + 464800
topo.eov$y <- round(topo.eov$y,3) + 259400
topo.eov$z <- round(topo.eov$z,3)
datgen(topo.eov[topo.eov$dat,], "tesztfile")
