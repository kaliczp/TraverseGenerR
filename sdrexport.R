export.sdr <- function(projectname = "Default", angle = NULL, coordinates = NULL) {
    ## Initial character STX
    result <- c("\002")
    ## Header
    result <- c(result,
                paste(c("00", # Type
                        "NM", # Derv
                        align.field("SDR33  V04-04.26", width = 16), # Ver
                        align.field("", width = 4), # serial number
                        align.field("11-Jul-22 05:40", width = 16), # date-time
                        "111121" # options
                        ),
                      collapse = ""
                      ),
                paste(c("10",
                        "NM",
                        align.field(projectname, width = 16),
                        "122111" # options
                        ),
                      collapse = ""
                      ),
                paste(c("06", # Scale
                        "NM",
                        "1.00000000"
                        ),
                      collapse = ""
                      )
                )
### Coordinates
    if(!is.null(coordinates)) {
        ## Coordinate header
        result <- c(result,
                    "13OOPOZ,Mind POZ")
        coo.only <- select.coordinates(coordinates)
        for(coordrow.num in 1:nrow(coo.only)) {
            result <- c(result,
                        paste(c("08", # Type
                                "KI", # Derv
                                align.field(coo.only[coordrow.num, "n"], width = 16, alignment = "right"),
                                align.field(coo.only[coordrow.num, "x"], width = 16),
                                align.field(coo.only[coordrow.num, "y"], width = 16),
                                align.field(coo.only[coordrow.num, "z"], width = 16),
                                align.field(coo.only[coordrow.num, "k"], width = 16)
                                ),
                              collapse = ""
                              )
                        )
        }
    }
    ## Tail
    spec.C <- "\003"
    result <- c(result,
                paste0(spec.C,"00000"))
    result
}
