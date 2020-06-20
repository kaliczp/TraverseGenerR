## Read in coordinates
coo <- gizi.read("teszt.coo")
coo[7, 1] <- "SP"
coo[,"X37"] <- as.numeric(as.character(coo[,"X37"]))
coo[,"X38"] <- as.numeric(as.character(coo[,"X38"]))
coo[,"X39"] <- as.numeric(as.character(coo[,"X39"]))

## Plot coordinates
plot(X37 ~ X38, coo, asp=T)
text(coo$X38, coo$X37, lab=coo$X5, adj=c(1,0))
text(coo$X38, coo$X37, lab=coo$X4, adj=c(1,1))

## Read in measurements
##   Station data
gizi.read("teszt.geo")
##   Initial measurement
gizi.read("teszt.geo", 2)
##   Traverse
gizi.read("teszt.geo", 4)

