## Practice stakeout
## Before use import sdrexport.R and libexport.R scripts!
## Required list
##        x        y      z  k    n
## 460975.9 253431.6 79.995 sp  100

kituz <- read.csv("NagyGyak/1_CS_kituz.txt", head = FALSE)
kituzok <- kituz[, c(2,3,4,5,1)]
names(kituzok) <- c("x", "y", "z", "k", "n")
Projectname <- "2CSKIT"
write(paste0(export.sdr(Projectname, coor = kituzok, allcoords = TRUE),"\r"), paste0(Projectname,".sdr"), sep="\n")
