eovrotate <- function(x, degree = 10) {
    radian.angle <- degree * (pi / 180)
    ## Calculate means
    col.means <- colMeans(x[,c("x","y")])
    ## Translate to origin
    shifted.coo <- x[,c("x","y")]
    shifted.coo$x <- shifted.coo$x - col.means["x"]
    shifted.coo$y <- shifted.coo$y - col.means["y"]
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
