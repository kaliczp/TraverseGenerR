datgen <- function(x, file, settlement = "Sehonna", objkod = 105201, student = "Kis Pista", teacher = "Kalicz Péter") {
    act.date <- Sys.Date()
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
                  format(act.date, "%Y%m%d"), # Actual date
                  "DATR 4.4.0", # software version
                  "2015.07.01 1.1", # file version
                  sep = "*")
    WriteDATRow(init, append = FALSE)
    ## Points
    WriteDATRow("T_PONT")
    sorsz <- 1:nrow(x)
    WriteDATRow(paste(sorsz,
                      x$y,
                      x$x,
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
    WriteDATRow("T_OBJ_ATTRBD")
    HRSZ1 <- paste0("0", sample(10:100, 1))
    area1 <- 1245
    mod.Dat <- act.date - sample(900:4100, 1)
    mod.date  <- format(mod.Dat, "%Y%m%d")
    law.code <- paste(sample(2:20,1),sample(300:500,1),format(mod.Dat, "%Y"), sep="/")
    WriteDATRow(paste(1,
                      "BD02",
                      1, # area id
                      HRSZ1,
                      "",
                      2, # location rural
                      area1,
                      "********2*1*487",
                      mod.date,
                      law.code, # Changing law
                      "***0*114*",
                      "", # codepoint
                      sep = "*")
                )
    WriteDATRow("T_OBJ_ATTRBE")
    WriteDATRow(paste(1,
                      "BE02",
                      1, # area id
                      "a",
                      HRSZ1,
                      area1,
                      0, # value
                      8, # Forest
                      2, # location rural
                      0,
                      1269, # prev. valid rec.
                      mod.date,
                      "***0*120*1",
                      1, # codepoint
                      sep = "*")
                )
    WriteDATRow("T_OBJ_ATTRBF")
    WriteDATRow(paste(1,
                      "BF01",
                      1, # area id
                      4, # quality code
                      8, # forest
                      2, # rural
                      0,
                      5743, # last valid
                      0,
                      118,
                      1,
                      1,
                      HRSZ1,
                      area1,
                      sep = "*")
                )
    WriteDATRow("T_FELIRAT")
    WriteDATRow(paste(1,
                      HRSZ1,
                      1, # lower left corner point id
                      60, # orientation
                      6, # font size
                      0, # valid date
                      "T_OBJ_ATTRBD",
                      1, # row id
                      11, # property HRSZ
                      sep = "*")
                )
    WriteDATRow("2*zkmk*2*77.1*2*0*T_OBJ_ATTRBD*1*14")
    close(fileConn)
}
