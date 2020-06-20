coo <- gizi.read("teszt.coo")
coo[7, 1] <- "SP"
coo[,"X37"] <- as.numeric(as.character(coo[,"X37"]))
coo[,"X38"] <- as.numeric(as.character(coo[,"X38"]))
coo[,"X39"] <- as.numeric(as.character(coo[,"X39"]))
plot(X37 ~ X38, coo)

gizi.read("teszt.geo")
gizi.read("teszt.geo", 2)
gizi.read("teszt.geo", 4)
