export.sdr <- function(projectname = "Default", angle = NULL, coordinates = NULL, allcoords = FALSE) {
    ## Measurement processing function
    meas.row <- function(x) {
        ## Determine face
        face  <-  ifelse(x$fce == "I", "F1", "F2")
        ## Slope sist if NA
        sl.dist <- ifelse(is.na(x$d), "", x$d)
        ## If vertical observation is NA
        vert.angle <- ifelse(is.na(x$z), "", x$z)
        ## Assemble row
        paste(c("09", # Type
                face, # Derv
                align.field(station.nr, # Source point ID
                            width = 16,
                            alignment = "right"),
                align.field(x$nfb, # Target point ID
                            width = 16,
                            alignment = "right"),
                align.field(sl.dist, # Slope distance
                            width = 16), 
                align.field(vert.angle, # Vertical observation
                            width = 16),
                align.field(x$h, # Horizontal observation
                            width = 16),
                align.field(x$k, # Description
                            width = 16)
                ),
              collapse = ""
              )
    }
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
        coo.only <- select.coordinates(coordinates, all = allcoords)
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
    ## Instrument type
    result <- c(result,
                paste(c("01", # Type
                        "NM", # Derv
                        ":", # EDM type?
                        align.field("SET3000", width = 16), # EDM Description
                        align.field("015737", width = 6), # EDM serial number
                        align.field("", width = 16), # Theodolite descr
                        "000000", # Theodolite serial number
                        "3", # Mounting type
                        "1", # Vertical angle
                        align.field("", width = 16), # EDM offset
                        align.field("", width = 16), # Reflector offset
                        align.field("0.00000000", width = 16) # Prism constant
                        ),
                      collapse = ""
                      )
                )
### Processing measurements
    if(!is.null(angle)) {
        station.nr <- -1
        last.target <- -1
        ## Processing measurement data frame
        for(anglerow.num in 1:nrow(angle)) {
            if(station.nr != angle[anglerow.num, "ns"]) {
                ## New station STN
                station.nr <- angle[anglerow.num, "ns"]
                result <- c(result,
                            paste(c("02", # Type
                                    "SC", # Derv
                                    align.field(station.nr, width = 16,alignment = "right"), # Point ID
                                    align.field("", width = 16), # Northing
                                    align.field("", width = 16), # Easting
                                    align.field("", width = 16), # Elev
                                    align.field(angle[anglerow.num, "ihs"], width = 16), # Theodolite height
                                    align.field(angle[anglerow.num, "k"], width = 16) # Station description
                                    ),
                                  collapse = ""
                                  ),
                            paste(c("13", # Type
                                    "PT", # Derv
                                    "Atmoszf. korr. Alkalmazva: Nyomas= 1022.0 Homerseklet= 25"
                                    ),
                                  collapse = ""
                                  )
                            )
                ## In the case of two face measurement, probably Set of observations
                if(angle[anglerow.num, "nfb"] == angle[anglerow.num + 1, "nfb"]) {
                    result <- c(result,
                                paste(c("12", # Type
                                        "SC", # Derv
                                        align.field(station.nr, # Source point
                                                    width = 16, alignment = "right"),
                                        "004", # Count of observations
                                        "001", # Number of set
                                        "1", # Bad marker
                                        "1", # Return sight made
                                        "1" # Prompt order
                                        ),
                                      collapse = ""
                                      )
                                )
                }
                ## Save target height
                result <- c(result,
                            paste(c("03", # Type
                                    "NM", # Derv
                                    align.field(angle[anglerow.num, "ihfb"], width = 16)
                                    ),
                                  collapse = ""
                                  )
                            )
                last.target <- angle[anglerow.num, "ihfb"]
                ## Save orientation
                result <- c(result,
                            meas.row(angle[anglerow.num,])
                            )
            } else {
                ## If there is no station change first check the target height change
                if(last.target != angle[anglerow.num, "ihfb"]) {
                    ## Save change of target height
                    result <- c(result,
                                paste(c("03", # Type
                                        "NM", # Derv
                                        align.field(angle[anglerow.num, "ihfb"], width = 16)
                                        ),
                                      collapse = ""
                                      )
                                )
                    last.target <- angle[anglerow.num, "ihfb"]
                }
                ## Orientation point measurement
                if(angle[anglerow.num, "nfb"] < 1000) {
                ## Case of set of observation for traverse
                    result <- c(result,
                                meas.row(angle[anglerow.num,])
                                )
                } else {
                    ## Topo point measurement
                    result <- c(result,
                                meas.row(angle[anglerow.num,])
                                )
                }
            }
        }
    }
    ## Tail
    spec.C <- "\003"
    result <- c(result,
                paste0(spec.C,"00000")) # Save 0-s for disable checksum controll
    result
}
