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
    ## Borders
    WriteDATRow("T_HATARVONAL")
    WriteDATRow("1*1*1*2*0*")
    WriteDATRow("2*1*1*3*0*")
    WriteDATRow("3*1*3*5*0*")
    WriteDATRow("4*1*5*2*0*")
    WriteDATRow("5*1*5*4*0*")
    WriteDATRow("6*1*4*2*0*")
    WriteDATRow("T_HATAR")
    WriteDATRow("1*1*1*-")
    WriteDATRow("1*2*2*+")
    WriteDATRow("1*3*3*+")
    WriteDATRow("1*4*4*+")
    WriteDATRow("2*1*5*+")
    WriteDATRow("2*2*6*+")
    WriteDATRow("2*3*4*-")
    WriteDATRow("T_FELULET")
    WriteDATRow("1*1*1*+")
    WriteDATRow("2*1*2*+")
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
