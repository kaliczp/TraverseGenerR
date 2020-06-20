coo <- gizi.read("teszt.coo")
coo[7, 1] <- "SP"
coo[,"X37"] <- as.numeric(as.character(coo[,"X37"]))
coo[,"X38"] <- as.numeric(as.character(coo[,"X38"]))
coo[,"X39"] <- as.numeric(as.character(coo[,"X39"]))

plot(X37 ~ X38, coo, asp=T)
text(coo$X38, coo$X37, lab=coo$X5, adj=c(1,0))
text(coo$X38, coo$X37, lab=coo$X4, adj=c(1,1))

gizi.read("teszt.geo")
gizi.read("teszt.geo", 2)
gizi.read("teszt.geo", 4)
