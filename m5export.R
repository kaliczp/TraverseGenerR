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
                         f1align = "left",
                         code2 = "",
                         field2 = "",
                         unit2 = "m",
                         code3 = "",
                         field3 = "",
                         unit3 = "m",
                         code4 = "",
                         field4 = "",
                         unit4 = "m",
                         closing = " ") {
        paste(align.field(fieldtext = row.num, width = 5, alignment = "right"),
              paste(code1,
                    align.field(field1, width = 27, alignment = f1align)),
              paste(align.field(code2, width = 2),
                    align.field(field2, width = 14, alignment = "right"),
                    align.field(unit2, width = 4)
                    ),
              paste(align.field(code3, width = 2),
                    align.field(field3, width = 14, alignment = "right"),
                    align.field(unit3, width = 4)
                    ),
              paste(align.field(code4, width = 2),
                    align.field(field4, width = 14, alignment = "right"),
                    align.field(unit4, width = 4)
                    ),
              closing,
              sep = "|"
              )
    }
    ## Empty result
    result <- character()
    ## Create header
    row.num <- 1
    result <- make.row(field1 = paste("Project",
                                      align.field(projectname, width = 20)
                                      ), unit2 = "", unit3 = "", unit4 = ""
                       )
    row.num <- 2
    result <- c(result, make.row(code2 = "05", field2 = 1, unit2 = "",
                                 code3 = "06", field3 = 1, unit3 = "",
                                 code4 = "49", field4 = 0, unit4 = ""
                                 )
                )
    ## Processing coordinates
    if(!is.null(coordinates)) {
        for(coordrow.num in 1:nrow(coordinates)) {
            row.num <- row.num + 1
            act.field1 <- paste0(align.field(coordinates[coordrow.num, "k"],
                                             width = 5,
                                             alignment = "right"
                                             ),
                                 align.field(coordinates[coordrow.num, "n"],
                                             width = 12,
                                             alignment = "right"
                                             )
                                 )
            result <- c(result,
                        make.row(make.paramcode("PI", 1),
                                 field1 = act.field1,
                                 f1align = "right",
                                 code2 = "Y",
                                 field2 = coordinates[coordrow.num, "x"],
                                 code3 = "X",
                                 field3 = coordinates[coordrow.num, "y"],
                                 code4 = "Z",
                                 field4 = coordinates[coordrow.num, "z"],
                                 closing = "I"
                                 )
                        )
        }
    }
    if(!is.null(angle)) {
        station.nr <- -1
        for(anglerow.num in 1:nrow(angle)) {
            row.num <- row.num + 1
            if(station.nr != angle[anglerow.num, "ns"]) {
                ## New station
                station.nr <- angle[anglerow.num, "ns"]
                act.field1 <- paste0(align.field("S",
                                                 width = 5,
                                                 alignment = "right"
                                                 ),
                                     align.field(station.nr,
                                                 width = 12,
                                                 alignment = "right"
                                                 )
                                     )
                result <- c(result,
                            make.row(make.paramcode("PI", 1),
                                     field1 = act.field1,
                                     f1align = "right",
                                     code2 = "s",
                                     field2 = "1.000",
                                     code3 = "Om",
                                     field3 = "90.000",
                                     unit3 = "DMS",
                                     code4 = "ih",
                                     field4 = angle[anglerow.num, "ihs"]
                                     )
                            )
                ## Increase row number for the first input of the station
                row.num <- row.num +1
            }
            ## New point in actual station
            code.act <- "SP"
            ## Target height for actual measurement
            result <- c(result,
                        make.row(field1 = "INPUT",
                                 f1align = "left",
                                 code2 = "s",
                                 field2 = "1.000",
                                 unit2 = "",
                                 code3 = "th",
                                 field3 = angle[anglerow.num, "ihfb"],
                                 code4 = "ih",
                                 field4 = angle[anglerow.num, "ihs"]
                                 )
                        )
            ## Increase row number for the next measurement of the station
            row.num <- row.num +1
            act.field1 <- paste0(align.field(code.act,
                                             width = 5,
                                             alignment = "right"
                                             ),
                                 align.field(angle[anglerow.num, "nfb"],
                                             width = 12,
                                             alignment = "right"
                                             )
                                 )
            ## Field 2 NA?
            if(is.na(angle[anglerow.num, "d"])) {
                act.code2 <- "  "
                act.field2 <- "  "
                act.unit2 <- " "
            } else {
                act.code2 <- "SD"
                act.field2 <- angle[anglerow.num, "d"]
                act.unit2 <- "m"
            }
            ## Field 4 NA?
            if(is.na(angle[anglerow.num, "z"])) {
                act.code4 <- "  "
                act.field4 <- "  "
                act.unit4 <- " "
            } else {
                act.code4 <- "V1"
                act.field4 <- angle[anglerow.num, "z"]
                act.unit4 <- "DMS"
            }
            result <- c(result,
                        make.row(make.paramcode("PI", 1),
                                 field1 = act.field1,
                                 f1align = "right",
                                 code2 = act.code2,
                                 field2 = act.field2,
                                 unit2 = act.unit2,
                                 code3 = "Hz",
                                 field3 = angle[anglerow.num, "h"],
                                 unit3 = "DMS",
                                 code4 = act.code4,
                                 field4 = act.field4,
                                 unit4 = act.unit4,
                                 closing = "M"
                                 )
                        )
        }
    }
    paste("For M5|Adr", result)
}
