export.m5 <- function(projectname = "Default", angle = NULL, coordinates = NULL) {
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
        paste(
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
    result <- make.row(field1 = paste("Project",
                                      align.field(projectname, width = 20)
                                      ), unit2 = "", unit3 = "", unit4 = ""
                       )
    result <- c(result, make.row(code2 = "05", field2 = 1, unit2 = "",
                                 code3 = "06", field3 = 1, unit3 = "",
                                 code4 = "49", field4 = 0, unit4 = ""
                                 )
                )
    ## Processing coordinates
    if(!is.null(coordinates)) {
        coo.only <- select.coordinates(coordinates)
        for(coordrow.num in 1:nrow(coo.only)) {
            act.field1 <- paste0(align.field(coo.only[coordrow.num, "k"],
                                             width = 5,
                                             alignment = "right"
                                             ),
                                 align.field(coo.only[coordrow.num, "n"],
                                             width = 12,
                                             alignment = "right"
                                             )
                                 )
            result <- c(result,
                        make.row(make.paramcode("PI", 1),
                                 field1 = act.field1,
                                 f1align = "right",
                                 code2 = "Y",
                                 field2 = coo.only[coordrow.num, "x"],
                                 code3 = "X",
                                 field3 = coo.only[coordrow.num, "y"],
                                 code4 = "Z",
                                 field4 = coo.only[coordrow.num, "z"],
                                 closing = "I"
                                 )
                        )
        }
    }
    ## Insert instrument type
    result <- c(result, make.row(field1 = "Instr.type Trimble5600",
                                 unit2 = "", unit3 = "", unit4 = ""
                                 )
                )
    ## Processing measurements
    if(!is.null(angle)) {
        station.nr <- -1
        last.target <- -1
        for(anglerow.num in 1:nrow(angle)) {
            if(station.nr != angle[anglerow.num, "ns"]) {
                ## New station
                result <- c(result, make.row(field1 = "STAT. KNWN. PNT.",
                                             unit2 = "", unit3 = "", unit4 = ""
                                             )
                            )
                ## Insert temperature and pressure?
                ## Insert prism constant in the first station
                if(anglerow.num == 1) {
                    result <- c(result, make.row(field1 = "REFLECTOR",
                                                 unit2 = "", unit3 = "",
                                                 code4 = "PC",
                                                 field4 = "-0.0300"
                                                 )
                                )
                }
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
                                     unit2 = "",
                                     code3 = "Om",
                                     field3 = paste0("0.000",sample(0:8,1),sample(1:9,1)),
                                     unit3 = "DMS",
                                     code4 = "ih",
                                     field4 = angle[anglerow.num, "ihs"]
                                     ),
                            make.row(make.paramcode("PI", 1),
                                     field1 = station.nr,
                                     f1align = "right",
                                     unit2 = "",
                                     code3 = "so",
                                     field3 = "0.00000",
                                     unit3 = "DMS",
                                     unit4 = ""
                                     ),
                            make.row(field1 = "COORDINATES/DETAIL PNTS/",
                                     unit2 = "", unit3 = "", unit4 = ""
                                     )
                            )
            }
            ## New point in actual station
            code.act <- angle[anglerow.num, "k"]
            ## Target height for actual measurement
            if(!last.target == angle[anglerow.num, "ihfb"]) {
                ## If target height changed
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
                last.target <- angle[anglerow.num, "ihfb"]
            }
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
    ## Line address number generation
    lineno <- 1:length(result)
    lineno.field <- align.field(fieldtext = lineno, width = 5, alignment = "right")
    paste("For M5", paste("Adr", lineno.field), result, sep ="|")
}
