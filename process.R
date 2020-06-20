## Read in coordinates
coo <- gizi.read("teszt.coo")
coo[7, 1] <- "SP"
coo[,"X37"] <- as.numeric(as.character(coo[,"X37"]))
coo[,"X38"] <- as.numeric(as.character(coo[,"X38"]))
coo[,"X39"] <- as.numeric(as.character(coo[,"X39"]))

## Plot coordinates
plot(X37 ~ X38, coo, asp=T)
lines(X37 ~ X38, coo[c(nrow(coo),1:(nrow(coo)-1)),])
text(coo$X38, coo$X37, lab=coo$X5, adj=c(1,0))
text(coo$X38, coo$X37, lab=coo$X4, adj=c(1,1))


text(coo$X38, coo$X37, lab=1:nrow(coo), adj=c(1,0))

## Read in measurements
##   Station data
Stat <- gizi.read("teszt.geo")
##   Initial measurement
ttinit <- gizi.read("teszt.geo", 2)
##   Traverse
Traverse <- gizi.read("teszt.geo", 4)

## Merge Traverse measurements
for(tti in 3:ncol(Traverse))
    Traverse[,tti] <- as.numeric(Traverse[,tti])
Traverse[2,1:2] <- na.omit(ttinit)[,1:2]
Traverse[2,3:ncol(ttinit)] <- as.numeric(na.omit(ttinit)[,3:ncol(ttinit)])
rm(ttinit)
