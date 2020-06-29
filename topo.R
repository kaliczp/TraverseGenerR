genertopo <- function(length = 500, width=200, height=200, perc=4) {
    road.end <- height + perc * length/100
    ## Corner points: lowerleft, lower right, upper left, upper right.
    corner.df  <-  data.frame(x = c(0, width, 0, width),
                              y = c(0, 0, length, length),
                              z = c(road.end + perc * (width/4)/100,
                                    height + rnorm(1, sd = 2),
                                    height + rnorm(1, sd = 3), height
                                    )
                              )
    divid.df <- c()
    ## Neutral line at quarter of width from bottom to top
    neutr.points = 6
    neutr.df <- data.frame(x = rep(width/4, neutr.points),
                           y = seq(0, length, length.out = neutr.points),
                           z = seq(road.end, height, length.out = neutr.points)
                           )
    ## East edge
    east.points <- 5
    east.y  <- seq(0, length, length.out = east.points + 2)[-c(1, east.points + 2)]
    east.df <- data.frame(x = rep(width, east.points),
                          y = east.y,
                          z = seq(corner.df[2, "z"], height, length.out = east.points)
                          )
    ## Divider between planned and ori road
    div.points = 3
    divider.df <- data.frame(x = rep(2 * width/3, div.points),
                             y = seq(0, length/3, length.out = div.points),
                             z = rep(height - perc * length/100, div.points)
                             )
    rbind(corner.df, neutr.df, east.df, divider.df)
}

topo <- genertopo()

topo.loess <- loess(z ~ x * y, topo,span=0.25, normalize=F)
topo.ma <- list(x=seq(0,200,10),y=seq(0,500,length.out = 21))
topo.lo <- predict(topo.loess, expand.grid(topo.ma), se=T)
library(MASS)
eqscplot(topo.ma, typ="n")
contour(topo.ma$x, topo.ma$y, topo.lo$fit, add=T)
points(topo[,1:2])
