datgen <- function(x, file, settlement = "Sehonna", objkod = 105201, student = "Kis Pista", teacher = "Kalicz Péter") {
    act.date <- Sys.Date()
    ## Generate additional points
    ## Two points for road
    road.pt <- x[c(2,5),]
    road.pt$x <- road.pt$x + 12
    road.pt$y <- road.pt$y + 12
    southeast.corner <- road.pt[1,]
    se.diff.x <- road.pt[1,"x"]- x[2, "x"]
    se.diff.y <- road.pt[1,"y"] - x[2, "y"]
    southeast.corner$x <- southeast.corner$x + sample(5:10,1) * se.diff.x
    southeast.corner$y <- southeast.corner$y + sample(5:10,1) * se.diff.y
    southeast.corner$z <- southeast.corner$z + sample(5:15,1)
    x <- rbind(x, road.pt, southeast.corner)
    ## Write row wiht cat
    WriteDATRow <- function(x, append = TRUE)
        cat(paste0(x,"*\r"), file = fileConn, sep = "\n", append = append)
    ## File initialisation
    filefull <- paste0(file, ".dat")
    fileConn <- file(filefull, "w")
    ## Header line
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
    ## First poly edges
    WriteDATRow("1*1*1*2*0*") # south border
    WriteDATRow("2*1*1*3*0*") # west border
    WriteDATRow("3*1*3*5*0*") # north border
    WriteDATRow("4*1*5*2*0*") # east border
    ## Second poly plus
    WriteDATRow("5*1*7*4*0*") # north border
    WriteDATRow("6*1*4*8*0*") # east line
    WriteDATRow("7*1*8*6*0*") # south line
    ## Road end
    WriteDATRow("8*1*6*2*0*") # south short
    WriteDATRow("9*1*5*7*0*") # north short
    WriteDATRow("10*1*7*6*0*") # est edge
    WriteDATRow("T_HATAR")
    ## First poly
    WriteDATRow("1*1*1*-")
    WriteDATRow("1*2*2*+")
    WriteDATRow("1*3*3*+")
    WriteDATRow("1*4*4*+")
    ## Second poly
    WriteDATRow("2*1*5*+")
    WriteDATRow("2*2*6*+")
    WriteDATRow("2*3*7*+")
    WriteDATRow("2*4*10*-")
    ## Road
    WriteDATRow("3*1*4*-")
    WriteDATRow("3*2*9*+")
    WriteDATRow("3*3*10*+")
    WriteDATRow("3*4*8*+")
    WriteDATRow("T_FELULET")
    WriteDATRow("1*1*1*+")
    WriteDATRow("2*1*2*+")
    WriteDATRow("3*1*3*+")
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
    ## Area attributes
    WriteDATRow("T_OBJ_ATTRBD")
    ## Generate data
    hrsz1 <- sample(10:100, 1)
    hrsz2 <- hrsz1 + sample(1:5, 1)
    HRSZ1 <- paste0("0", hrsz1)
    HRSZ2 <- paste0("0", hrsz2)
    HRSZ3 <- paste0("0", hrsz2 + sample(1:5, 1))
    area1 <- 1245
    area2 <- 1150
    area3 <- 1000
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
    WriteDATRow(paste(2,
                      "BD02",
                      2, # area id
                      HRSZ2,
                      "",
                      2, # location rural
                      area2,
                      "********2*1*487",
                      mod.date,
                      law.code, # Changing law
                      "***0*114*",
                      "", # codepoint
                      sep = "*")
                )
    WriteDATRow(paste(3,
                      "BD02",
                      3, # area id
                      HRSZ3,
                      "",
                      2, # location rural
                      area3,
                      "********2*1*487",
                      mod.date,
                      law.code, # Changing law
                      "***0*114*",
                      "", # codepoint
                      sep = "*")
                )
    WriteDATRow("T_OBJ_ATTRBE")
    WriteDATRow(paste(1,
                      "BE03",
                      1, # area id
                      "-",
                      HRSZ1,
                      area1,
                      "", # value
                      8, # Forest
                      2, # location rural
                      1,
                      1, # prev. valid rec.
                      mod.date,
                      "***0*120*",
                      "", # codepoint
                      sep = "*")
                )
    WriteDATRow(paste(2,
                      "BE03",
                      2, # area id
                      "-",
                      HRSZ2,
                      area2,
                      "", # value
                      8, # Forest
                      2, # location rural
                      1,
                      1, # prev. valid rec.
                      mod.date,
                      "***0*120*",
                      "", # codepoint
                      sep = "*")
                )
    WriteDATRow("T_OBJ_ATTRBF")
    WriteDATRow(paste(1,
                      "BF01",
                      1, # area id
                      4, # quality code
                      8, # forest
                      1, # rural
                      1,
                      1, # last valid
                      0,
                      118,
                      "",
                      "",
                      HRSZ1,
                      area1,
                      sep = "*")
                )
    WriteDATRow(paste(1,
                      "BF01",
                      2, # area id
                      5, # quality code
                      8, # forest
                      1, # rural
                      1,
                      1, # last valid
                      0,
                      118,
                      "",
                      "",
                      HRSZ2,
                      area2,
                      sep = "*")
                )
    WriteDATRow("T_OBJ_ATTRDC")
    WriteDATRow(paste(1,
                      "DC07",
                      3, # area id
                      1, # length id
                      0, # szam
                      12, # pavement
                      "**", # Additional data
                      "*", #
                      1,
                      0,
                      114,
                      "",
                      "",
                      sep = "*")
                )
    WriteDATRow("T_FELIRAT")
    WriteDATRow(paste(1,
                      HRSZ1,
                      1, # lower left corner point id
                      90, # orientation
                      6, # font size
                      0, # valid date
                      "T_OBJ_ATTRBD",
                      1, # row id
                      11, # property HRSZ
                      sep = "*")
                )
    WriteDATRow(paste(2,
                      "E",
                      1, # lower left corner point id
                      90, # orientation
                      2, # font size
                      0, # valid date
                      "T_OBJ_ATTRBD",
                      1, # row id
                      14, # property HRSZ
                      sep = "*")
                )
    WriteDATRow(paste(3,
                      HRSZ2,
                      5, # lower left corner point id
                      90, # orientation
                      6, # font size
                      0, # valid date
                      "T_OBJ_ATTRBD",
                      2, # row id
                      11, # property HRSZ
                      sep = "*")
                )
    WriteDATRow(paste(4,
                      "E",
                      5, # lower left corner point id
                      90, # orientation
                      2, # font size
                      0, # valid date
                      "T_OBJ_ATTRBD",
                      2, # row id
                      14, # property HRSZ
                      sep = "*")
                )
    close(fileConn)
}

hazdatgen <- function(x, file, settlement = "Sehonna", objkod = 105201, student = "Kis Pista", teacher = "Kalicz Péter") {
    act.date <- Sys.Date()
    ## Write row wiht cat
    WriteDATRow <- function(x, append = TRUE)
        cat(paste0(x,"*\r"), file = fileConn, sep = "\n", append = append)
    ## File initialisation
    filefull <- paste0(file, ".dat")
    fileConn <- file(filefull, "w")
    ## Header line
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
    ## Border lines
    WriteDATRow("T_HATARVONAL")
    ## Central poly edges
    WriteDATRow("1*1*1*2*0*") # south border
    WriteDATRow("2*1*2*3*0*") # east border
    WriteDATRow("3*1*3*4*0*") # north border
    WriteDATRow("4*1*4*1*0*") # west border
    ## Left poly plus
    WriteDATRow("5*1*4*7*0*") # north border
    WriteDATRow("6*1*7*5*0*") # west line
    WriteDATRow("7*1*5*1*0*") # south line
    ## Right poly plus
    WriteDATRow("8*1*2*6*0*") # south short
    WriteDATRow("9*1*6*8*0*") # east edge
    WriteDATRow("10*1*8*3*0*") # north edge
    ## Boundaries
    WriteDATRow("T_HATAR")
    ## First poly
    WriteDATRow("1*1*1*+")
    WriteDATRow("1*2*2*+")
    WriteDATRow("1*3*3*+")
    WriteDATRow("1*4*4*+")
    ## Second poly
    WriteDATRow("2*1*5*+")
    WriteDATRow("2*2*6*+")
    WriteDATRow("2*3*7*+")
    WriteDATRow("2*4*4*-")
    ## Third
    WriteDATRow("3*1*8*+")
    WriteDATRow("3*2*9*+")
    WriteDATRow("3*3*10*+")
    WriteDATRow("3*4*2*-")
    ## Polys
    WriteDATRow("T_FELULET")
    WriteDATRow("1*1*1*+")
    WriteDATRow("2*1*2*+")
    WriteDATRow("3*1*3*+")
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
    ## Area attributes
    WriteDATRow("T_OBJ_ATTRBD")
    ## Generate data
    HRSZ1 <- sample(10:100, 1)
    HRSZ2 <- HRSZ1 + sample(1:2, 1)
    HRSZ3 <- HRSZ2 + sample(1:2, 1)
    area1 <- (x[2, "x"] - x[1, "x"]) * (x[3, "y"] - x[2, "y"])
    area2 <- (x[1, "x"] - x[5, "x"]) * (x[3, "y"] - x[2, "y"])
    area3 <- (x[6, "x"] - x[2, "x"]) * (x[3, "y"] - x[2, "y"])
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
    WriteDATRow(paste(2,
                      "BD02",
                      2, # area id
                      HRSZ2,
                      "",
                      2, # location rural
                      area2,
                      "********2*1*487",
                      mod.date,
                      law.code, # Changing law
                      "***0*114*",
                      "", # codepoint
                      sep = "*")
                )
    WriteDATRow(paste(3,
                      "BD02",
                      3, # area id
                      HRSZ3,
                      "",
                      2, # location rural
                      area3,
                      "********2*1*487",
                      mod.date,
                      law.code, # Changing law
                      "***0*114*",
                      "", # codepoint
                      sep = "*")
                )
    WriteDATRow("T_OBJ_ATTRBE")
    WriteDATRow(paste(1,
                      "BE03",
                      1, # area id
                      "-",
                      HRSZ1,
                      area1,
                      "", # value
                      8, # Forest
                      2, # location rural
                      1,
                      1, # prev. valid rec.
                      mod.date,
                      "***0*120*",
                      "", # codepoint
                      sep = "*")
                )
    WriteDATRow(paste(2,
                      "BE03",
                      2, # area id
                      "-",
                      HRSZ2,
                      area2,
                      "", # value
                      8, # Forest
                      2, # location rural
                      1,
                      1, # prev. valid rec.
                      mod.date,
                      "***0*120*",
                      "", # codepoint
                      sep = "*")
                )
    WriteDATRow(paste(3,
                      "BE03",
                      3, # area id
                      "-",
                      HRSZ3,
                      area3,
                      "", # value
                      8, # Forest
                      2, # location rural
                      1,
                      1, # prev. valid rec.
                      mod.date,
                      "***0*120*",
                      "", # codepoint
                      sep = "*")
                )
    WriteDATRow("T_OBJ_ATTRBF")
    WriteDATRow(paste(1,
                      "BF01",
                      1, # area id
                      4, # quality code
                      8, # forest
                      1, # rural
                      1,
                      1, # last valid
                      0,
                      118,
                      "",
                      "",
                      HRSZ1,
                      area1,
                      sep = "*")
                )
    WriteDATRow(paste(2,
                      "BF01",
                      2, # area id
                      5, # quality code
                      8, # forest
                      1, # rural
                      1,
                      1, # last valid
                      0,
                      118,
                      "",
                      "",
                      HRSZ2,
                      area2,
                      sep = "*")
                )
    WriteDATRow(paste(3,
                      "BF01",
                      3, # area id
                      5, # quality code
                      8, # forest
                      1, # rural
                      1,
                      1, # last valid
                      0,
                      118,
                      "",
                      "",
                      HRSZ3,
                      area3,
                      sep = "*")
                )
    WriteDATRow("T_FELIRAT")
    WriteDATRow(paste(1,
                      HRSZ1,
                      1, # lower left corner point id
                      90, # orientation
                      6, # font size
                      0, # valid date
                      "T_OBJ_ATTRBD",
                      1, # row id
                      11, # property HRSZ
                      sep = "*")
                )
    WriteDATRow(paste(2,
                      "E",
                      1, # lower left corner point id
                      90, # orientation
                      2, # font size
                      0, # valid date
                      "T_OBJ_ATTRBD",
                      1, # row id
                      14, # property HRSZ
                      sep = "*")
                )
    WriteDATRow(paste(3,
                      HRSZ2,
                      5, # lower left corner point id
                      90, # orientation
                      6, # font size
                      0, # valid date
                      "T_OBJ_ATTRBD",
                      2, # row id
                      11, # property HRSZ
                      sep = "*")
                )
    WriteDATRow(paste(4,
                      "E",
                      5, # lower left corner point id
                      90, # orientation
                      2, # font size
                      0, # valid date
                      "T_OBJ_ATTRBD",
                      2, # row id
                      14, # property HRSZ
                      sep = "*")
                )
    WriteDATRow(paste(5,
                      HRSZ3,
                      2, # lower left corner point id
                      90, # orientation
                      6, # font size
                      0, # valid date
                      "T_OBJ_ATTRBD",
                      2, # row id
                      11, # property HRSZ
                      sep = "*")
                )
    close(fileConn)
}
