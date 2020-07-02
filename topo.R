genertopo <- function(length = 500, width=300, height=200, perc=4, east.dist = 50) {
    road.end <- height + perc * length/100
    ## Corner points: lowerleft, lower right, upper left, upper right.
    corner.df  <-  data.frame(x = c(0, width, 0, width),
                              y = c(0, 0, length, length),
                              z = c(road.end + perc * (width/4)/100,
                                    height + rnorm(1, sd = 2),
                                    height + rnorm(1, sd = 3), height
                                    )
                              )
    ## Neutral line at quarter of width from bottom to top
    neutr.points = 6
    neutr.df <- data.frame(x = rep(width/4, neutr.points),
                           y = seq(0, length, length.out = neutr.points),
                           z = seq(road.end, height, length.out = neutr.points)
                           )
    ## Edge points
    edge.points <- 5
    corner.idx  <- c(1, edge.points + 2)# Corner index to remove
    ## y from south to north
    edge.y  <- seq(0,
                   length,
                   length.out = edge.points + 2)[-corner.idx]
    ## East edge moves easter only southest point removed
    east.x  <- seq(width,
                   width - east.dist,
                   length.out = edge.points + 2)[-corner.idx[1]]
    ## Only south east corner removed from z
    east.df <- data.frame(x = east.x,
                          y = c(edge.y, length), # Northern edge added
                          z = seq(corner.df[2, "z"],
                                  height,
                                  length.out = edge.points + 2)[-corner.idx[1]]
                          )
    ## West edge
    west.df <- data.frame(x = rep(0, edge.points),
                          y = edge.y,
                          z = seq(corner.df[1, "z"],
                                  corner.df[3, "z"],
                                  length.out = edge.points + 2)[-corner.idx]
                          )
    ## Divider between planned and ori road
    div.points = 3
    divider.df <- data.frame(x = rep(2 * width/3, div.points),
                             y = seq(0, length/3, length.out = div.points),
                             z = rep(height - perc * length/100, div.points)
                             )
    rbind(corner.df, neutr.df, east.df, west.df, divider.df)
}

topo <- genertopo(perc=3, east.dist = 150)

topo.loess <- loess(z ~ x * y, topo,span=0.5, normalize=F)
topo.ma <- list(x=seq(0,300,length.out=21),y=seq(0,500,length.out = 21))
topo.lo <- predict(topo.loess, expand.grid(topo.ma), se=T)
library(MASS)
eqscplot(topo.ma, typ="n")
contour(topo.ma$x, topo.ma$y, topo.lo$fit, add=T)
points(topo[,1:2])
text(topo[,"x"], topo[,"y"], round(topo[,"z"]), adj=c(0,1))
