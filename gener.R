newcoo <- coo[10:9,]

gener <- function(ox=464800,oy=259400){
    fulldist <- sample(400:500,1)
    angledist <- sample(50:100,1)
    frame <- data.frame(x=c(angledist,0,0),
                        y=c(fulldist,fulldist,0))
    ## Translate
    frame$x <- frame$x + ox
    frame$y <- frame$y + oy
    frame
}

plot(gener(), asp=TRUE)
