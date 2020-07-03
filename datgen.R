datgen <- function(x, y, file, settlement = "Sehonna") {
    filefull <- paste0(file, ".dat")
    WriteDATRow <- function(x, append = TRUE)
        cat(paste0(x,"*\r"), file = fileConn, sep = "\n", append = append)
    fileConn <- file(filefull, "w")
    init <- paste(file,
                  paste0(settlement,"_jogerős_2/623"),
                  "FÖMI, földhivatalok*-*-*-",
                  format(Sys.Date(), "%Y%m%d"), # Actual date
                  "DATR 4.4.0*2015.07.01 1.1",
                  sep = "*")
    WriteDATRow(init, append = FALSE)
    ## Point codes
    WriteDATRow("T_PONT")
    sorsz <- 1
    WriteDATRow(paste(sorsz, x, y,"*0*0*0", sep="*"))
    WriteDATRow("T_OBJ_ATTRAC")
    WriteDATRow("1*AC03*105203*1*1*3*0*1*0*0**4315")
    close(fileConn)
}
