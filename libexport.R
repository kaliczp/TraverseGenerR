select.coordinates <- function(allcordinates) {
    ## Select op-s and first and last sp-s 
    orientation.rows  <- which(allcordinates$k == "op") # Orientation point row(s)
    ## Select first and last from sp rows
    sp.rows <- which(allcordinates$k == "sp")
    sp.firstlast <- sp.rows[c(1,length(sp.rows))]
    if(length(orientation.rows) > 0) {
        ## There are orientation point(s)
        selected.points <- c(orientation.rows,
                             orientation.rows - 1, # Station before orientation
                             sp.firstlast) # First and last station
        selected.points <- unique(selected.points)
    } else {
        warning("No op codes! Only first and last sp are selected!")
        selected.points <- sp.firstlast
    }
    allcordinates[selected.points, ]
}

align.field <- function(fieldtext, width, alignment = "left") {
    if(is.numeric(fieldtext)) {
        fieldtext <- round(fieldtext, 8)
    }
    spaces <- paste0(rep(" ", width), collapse = "")
    if(alignment == "left") {
        textwithspace <- paste0(fieldtext, spaces)
        aligned <- substr(textwithspace, 1, width)
    } else {
        textwithspace <- paste0(spaces, fieldtext)
        length.txtwspc <- nchar(textwithspace)
        first.char <- (length.txtwspc - width) + 1
        aligned <- substr(textwithspace, first.char, length.txtwspc)
    }
    aligned
}
