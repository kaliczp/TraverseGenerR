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
                "10NMMunkanev        122111")
    ## Tail
    spec.C <- "\003"
    result <- c(result,
                paste0(spec.C,"00000"))
    result
}
