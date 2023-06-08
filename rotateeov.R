## https://geocompr.robinlovelace.net/geometric-operations.html#affine-transformations
## More simpler solution
eovrotate <- function(x, degree = 10, shiftcoo = NULL) {
    radian.angle <- degree * (pi / 180)
    ## Calculate means
    col.means <- colMeans(x[,c("x","y")])
    ## Translate to origin
    shifted.coo <- x[,c("x","y")]
    if(is.null(shiftcoo)) {
    shifted.coo$x <- shifted.coo$x - col.means["x"]
    shifted.coo$y <- shifted.coo$y - col.means["y"]
    } else {
        shifted.coo$x <- shifted.coo$x - shiftcoo[,"x"]
        shifted.coo$y <- shifted.coo$y - shiftcoo[,"y"]
    }
    temporary <- shifted.coo
    shifted.coo$x <- (temporary$x * cos(radian.angle)) -
        (temporary$y * sin(radian.angle))
    shifted.coo$y <- (temporary$x * sin(radian.angle)) +
        (temporary$y * cos(radian.angle))
    ## Translate back
    x$x <- round(shifted.coo$x + col.means["x"], 3)
    x$y <- round(shifted.coo$y + col.means["y"], 3)
    x
}
