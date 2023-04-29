meascalc.ordered <- function(coord, ins.height.range = c(1.450, 1.620), orient = TRUE, generror = FALSE, topo = TRUE, topo.target.height = 1.7) {
    oricoord <- coord
    if(orient) {
        ## Are there orient really?
        orient.idx <- coord$k == "op"
        if(!any(orient.idx)) {
            orient = FALSE
            warning("There are not any orientation points!")
        }
    }
    if(orient) {
        ## Separate orientation and standard measurements
        orient.idx <- which(orient.idx)
        orient.df <- coord[orient.idx, ]
        coord <- coord[-orient.idx, ]
    }
    if(topo) {
        ## Are there topo points really?
        topo.idx <- coord$n > 1000
        if(!any(topo.idx)) {
            topo = FALSE
            warning("There are not any topo points!")
        }
    }
    if(topo) {
        ## Separate topo points and standard measurements
        topo.idx <- which(topo.idx)
        ## Select station numbers for topo points
        new.topostation <- topo.idx[diff(topo.idx) > 1]
        if(length(new.topostation) > 0) {
            ## Two or more stations with topo points
            new.topostation.loc <- c(topo.idx[1], new.topostation + 2)
        } else {
            ## Only one station for topo points need correction
            new.topostation.loc <- topo.idx[1]
        }
        topo.station.df <- coord[new.topostation.loc - 1, ]
        topo.list <- list()
        for(topo.list.idx in 1:(length(new.topostation.loc)-1)) {
            topo.select.idx <- new.topostation.loc[topo.list.idx]:(new.topostation.loc[topo.list.idx+1]-2)
            topo.list[[topo.list.idx]] <- coord[topo.select.idx, ]
        }
        topo.list.idx  <- length(new.topostation.loc)
        topo.select.idx <- new.topostation.loc[topo.list.idx]:nrow(coord)
        topo.list[[topo.list.idx]] <- coord[topo.select.idx, ]
        coord <- coord[-topo.idx, ]
        topo.fore <- data.frame(ns = 0,
                           ihs = 1.7,
                           nfb = 0,
                           ihfb = topo.target.height,
                           h = 0,
                           z = 0,
                           d = 0,
                           k = "t"
                           )
        ## Topo measurements calculation
        for(act.topo.station in 1:nrow(topo.station.df)) {
        slop.dist <- sqrt((topo.list[[act.topo.station]][,"x"] - topo.station.df[act.topo.station, "x"])^2 +
                          (topo.list[[act.topo.station]][,"y"] - topo.station.df[act.topo.station, "y"])^2 +
                          (topo.list[[act.topo.station]][,"z"] - topo.station.df[act.topo.station, "z"])^2)
        hor.angle <- -atan2(topo.list[[act.topo.station]][,"y"] - topo.station.df[act.topo.station, "y"],
                            topo.list[[act.topo.station]][,"x"] - topo.station.df[act.topo.station, "x"])
        zenit.for <- pi/2 - asin((topo.list[[act.topo.station]][,"z"] - topo.station.df[act.topo.station, "z"]) / slop.dist)
        topo.fore <- rbind(topo.fore, data.frame(ns = topo.station.df[act.topo.station, "n"],
                                                 ihs = 1.7,
                                                 nfb = topo.list[[act.topo.station]][,"n"],
                                                 ihfb = topo.target.height,
                                                 h = hor.angle,
                                                 z = zenit.for,
                                                 d = slop.dist,
                                                 k = topo.list[[act.topo.station]][,"k"]
                                                 )
                           )
        }
        topo.fore <- topo.fore[-1,]
    }
### Traverse generation
    ## Instrument height added
    if(length(ins.height.range) == nrow(coord)) {
        ins.height <- ins.height.range
    } else {
        warning("Instrument height generated!")
        ins.height  <- sample(seq(ins.height.range[1],
                                  ins.height.range[2], by=.001),
                              nrow(coord), replace = TRUE)
    }
    coord$z <- coord$z + ins.height
    ## Sloped distance generation
    slop.dist <- sqrt(diff(coord$x)^2 + diff(coord$y)^2 + diff(coord$z)^2)
    slop.dist <- round(slop.dist,3)
    ## Horizontal angle generation forward and backward
    hor.angle <- -atan2(diff(coord$y),diff(coord$x))
    hor.angle.back <- hor.angle + pi
    ## Zenit angle
    zenit.for <- pi/2 - asin(diff(coord$z)/slop.dist)
    zenit.back <- pi/2 + asin(diff(coord$z)/slop.dist)
### Orientation point
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
                                  d = NA,
                                  k = c(first.orient.df[1, "k"],
                                        last.orient.df[1, "k"])
                      )
        } else {
            ## If only one point oriented first or last?
            ## Currently only first implemented
            ## from start point
            orient.df <- rbind(orient.df[as.character(orient.idx),],
                                     coord[1,])
            ## 2pi - angle + pi
            hor.angle.ori <- pi - atan2(diff(orient.df$y),
                                         diff(orient.df$x))
            ori.fin <- data.frame(ns = orient.df[nrow(orient.df), "n"],
                                  ihs = ins.height[1],
                                  nfb = orient.df[1, "n"],
                                  ihfb = 0,
                                  h = hor.angle.ori,
                                  z = NA,
                                  d = NA,
                                  k = orient.df[1, "k"]
                      )
        }
    }
### Assemble traverse from calculated values
    fore <- data.frame(ns = coord$n[-nrow(coord)],
                      ihs = ins.height[-length(ins.height)],
                      nfb = coord$n[-1],
                      ihfb = ins.height[-1],
                      h = hor.angle,
                      z = zenit.for,
                      d = slop.dist,
                      k = coord$k[-1]
                      )
    back <- data.frame(ns = coord$n[-1],
                      ihs = ins.height[-1],
                      nfb = coord$n[-nrow(coord)],
                      ihfb = ins.height[-length(ins.height)],
                      h = hor.angle.back,
                      z = zenit.back,
                      d = slop.dist,
                      k = coord$k[-nrow(coord)]
                      )
    result <- rbind(fore, back)
    ## Orientation at the beginning
    if(orient) {
        result <- rbind(result, ori.fin[1,])
    }
    ## Topo points if available
    if(topo) {
        result <- rbind(result, topo.fore)
    }
    ## Ordering data frame station number (ns) and measured point number (nfb)
    order.res <- order(result$ns, result$nfb)
    result.ok <- result[order.res,]
    ## Put the final orientation
    if(orient) {
        if(nrow(ori.fin) > 1) {
            result.ok <- rbind(result.ok, ori.fin[nrow(ori.fin),])
            }
    }
    ## Are there any negative angle?
    negh.row <- result.ok$h < 0
    if(any(negh.row)) {
        ## Correct negative angles
        result.ok[negh.row, "h"]  <- result.ok[negh.row, "h"] + 2*pi
    }
    ## Generate errors
    if(generror) {
        sec.rad <- (1/60/60)*pi/180 # one second in rad
        result.ok$h <- result.ok$h + rnorm(nrow(result.ok), sd = 12 * sec.rad)
        result.ok$z <- result.ok$z + rnorm(nrow(result.ok), sd = 12 * sec.rad)
        result.ok$d <- round(result.ok$d + rnorm(nrow(result.ok), sd = 0.05), 3)
    }
    result.ok
}
