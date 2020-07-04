datgen <- function(x, file, settlement = "Sehonna", objkod = 105201, student = "Kis Pista", teacher = "Kalicz Péter") {
    filefull <- paste0(file, ".dat")
    WriteDATRow <- function(x, append = TRUE)
        cat(paste0(x,"*\r"), file = fileConn, sep = "\n", append = append)
    fileConn <- file(filefull, "w")
    init <- paste(file, # filename without extension
                  paste0(settlement,"_jogerős"), # some title
                  "Soproni Egyetem", # source organisation
                  teacher, # resp. person
                  "-", # target organisation
                  student,
                  format(Sys.Date(), "%Y%m%d"), # Actual date
                  "DATR 4.4.0", # software version
                  "2015.07.01 1.1", # file version
                  sep = "*")
    WriteDATRow(init, append = FALSE)
    ## Points
    WriteDATRow("T_PONT")
    sorsz <- 1:nrow(x)
    WriteDATRow(paste(sorsz,
                      x$x,
                      x$y,
                      x$z,
                      "0*0",
                      sep="*")
                )
    ## Points attributes
    WriteDATRow("T_OBJ_ATTRAC")
    objkod.pt <- seq(objkod, by = 1, length.out = length(sorsz))
    WriteDATRow(paste(sorsz,
                      "AC04",
                      objkod.pt,
                      sorsz,
                      "1*3*0*1*0*0**4315",
                      sep = "*")
                )
    close(fileConn)
}
