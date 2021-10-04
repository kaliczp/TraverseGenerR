haztobbdatgen <- function(x, file, settlement = "Sehonna", objkod = 105201, student = "Kis Pista", teacher = "Kalicz Péter", parcels = 3, addcoo = TRUE) {
    act.date <- Sys.Date()
    ## Data preparation
    ## Create sf
    elso.mat <- as.matrix(rbind(x[1:4,1:3], x[1,1:3]))
    row.names(elso.mat) <- c((1:4),11)
    elso.poly <- st_polygon(list(elso.mat))
    masodik.poly <- st_polygon(list(as.matrix(rbind(x[c(5,1,4,7),1:3], x[5,1:3]))))
    harmadik.poly <- st_polygon(list(as.matrix(rbind(x[c(2,6,8,3),1:3], x[2,1:3]))))
    x_sfc <- st_sfc(elso.poly,masodik.poly, harmadik.poly, crs=23700)
    ## calculate centroids
    x.centr <- st_coordinates(st_centroid(x_sfc))
    x.centr <- cbind(x.centr, x[1:3,3])
    x.centr <- round(x.centr, 3)
    ## Points for address-coordinates m rounded centroids
    if(addcoo) {
        x.addrcoo <- round(x.centr)
        x.addrcoo[,3] <- 0
        x.centr <- rbind(x.centr, x.addrcoo)
    }
    colnames(x.centr) <- c("x","y","z")
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
    xfull <- rbind(x[,1:3], x.centr)
    sorsz.full <- 1:nrow(xfull)
    WriteDATRow(paste(sorsz.full,
                      xfull$y,
                      xfull$x,
                      xfull$z,
                      "0*0",
                      sep="*")
                )
    sorsz <- 1:nrow(x)
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
                      "AC02",
                      objkod.pt,
                      sorsz,
                      "1*3*0*1*0*0**3215",
                      sep = "*")
                )
    ## Address-coordinates attributes
    if(addcoo) {
        sorsz.addrcoo <- 1:parcels
        ## ID of last points
        max.point.id <- sorsz.full[length(sorsz.full)]
        pointid.addrcoo <- (max.point.id - parcels + 1):max.point.id
        WriteDATRow("T_OBJ_ATTRAD")
        WriteDATRow(paste(sorsz.addrcoo,
                          "AD01",
                          1, # Name of the point
                          "5411", # Addr-coo (54), not-signed (1), generated (1)
                          pointid.addrcoo,
                          "", # date of invalidity
                          195, # Graphical representation
                          "", # Description optional
                          "", # parcel_id1
                          sorsz.addrcoo, # parcel_id2 number!
                          "", # building id
                          "", # other prop id
                          sorsz.addrcoo, # last valid id,
                          "", # survey id
                          sep = "*")
                    )
    }
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
                      "BD01",
                      1, # area id
                      HRSZ1,
                      "",
                      1, # location
                      area1,
                      "********2*1*487",
                      mod.date,
                      law.code, # Changing law
                      "***0*114*",
                      "", # codepoint
                      sep = "*")
                )
    WriteDATRow(paste(2,
                      "BD01",
                      2, # area id
                      HRSZ2,
                      "",
                      1, # location
                      area2,
                      "********2*1*487",
                      mod.date,
                      law.code, # Changing law
                      "***0*114*",
                      "", # codepoint
                      sep = "*")
                )
    WriteDATRow(paste(3,
                      "BD01",
                      3, # area id
                      HRSZ3,
                      "",
                      1, # location
                      area3,
                      "********2*1*487",
                      mod.date,
                      law.code, # Changing law
                      "***0*114*",
                      "", # codepoint
                      sep = "*")
                )
    WriteDATRow("T_OBJ_ATTRBF")
    WriteDATRow(paste(1,
                      "BF01",
                      1, # area id
                      0, # quality code
                      9, # kivett
                      1, # elhat elozetes
                      1, # módja
                      1, # last valid
                      0,
                      118,
                      1,
                      "",
                      HRSZ1,
                      area1,
                      sep = "*")
                )
    WriteDATRow(paste(2,
                      "BF01",
                      2, # area id
                      5, # quality code
                      9, # kivett
                      1, # rural
                      1,
                      1, # last valid
                      0,
                      118,
                      1,
                      "",
                      HRSZ2,
                      area2,
                      sep = "*")
                )
    WriteDATRow(paste(3,
                      "BF01",
                      3, # area id
                      5, # quality code
                      9, # kivett
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
                      9, # lower left corner point id
                      90, # orientation
                      6, # font size
                      0, # valid date
                      "T_OBJ_ATTRBD",
                      1, # row id
                      11, # property HRSZ
                      sep = "*")
                )
    WriteDATRow(paste(3,
                      HRSZ2,
                      10, # lower left corner point id
                      90, # orientation
                      6, # font size
                      0, # valid date
                      "T_OBJ_ATTRBD",
                      2, # row id
                      11, # property HRSZ
                      sep = "*")
                )
    WriteDATRow(paste(5,
                      HRSZ3,
                      11, # lower left corner point id
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
