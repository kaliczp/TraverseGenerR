export.m5 <- function(projectname = "Default", angle = NULL, coordinates = NULL) {
    align.field <- function(fieldtext, width, alignment = "left") {
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
    make.paramcode <- function(code = "TI", num = "") {
        align.field(paste0(code, num), width = 3)
    }
    make.row <- function(code1 = make.paramcode(), # default param
                         field1 = "",
                         code2 = "",
                         field2 = "",
                         code3 = "",
                         field3 = "",
                         code4 = "",
                         field4 = "",
                         closing = " ") {
        paste(align.field(fieldtext = row.num, width = 5, alignment = "right"),
              align.field(paste(code1, field1), width = 30),
              align.field(paste(code2, field2), width = 22),
              align.field(paste(code3, field3), width = 22),
              align.field(paste(code4, field4), width = 22),
              closing,
              sep = "|"
              )
    }
    ## Empty result
    result <- character()
    ## Create header
    row.num <- 1
    result <- make.row(field1 = paste("Project",
                                      align.field(projectname, width = 19)
                                      )
                       )
    row.num <- 2
    paste("For M5|Adr", result)
}
