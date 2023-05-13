export.sdr <- function(projectname = "Default", angle = NULL, coordinates = NULL) {
    ## Header
    result <- c("\002",
                "00NMSDR33  V04-04.26    11-Jul-22 05:40 111121",
                "10NMMunkanev        122111")
    ## Tail
    spec.C <- "\003"
    result <- c(result,
                paste0(spec.C,"00000"))
    result
}
