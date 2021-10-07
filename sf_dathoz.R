## Reorder hazdat
hazdatr <- hazdat[c(5,7,4,1,3,2,8,6),]
rownames(hazdatr) <- NULL

## Az első négy pont és a záró
elso.mat <- as.matrix(rbind(hazdatr[1:4,1:3], hazdatr[1,1:3]))
row.names(elso.mat) <- c(1:4,11)
elso.pl <- st_polygon(list(elso.mat))

## Szomszédok generálása
masodik.pl <- elso.pl + c(diff(as.numeric(elso.mat[c(1,3),"x"])),0,0)
elozo.pl <- elso.pl - c(diff(as.numeric(elso.mat[c(1,3),"x"])),0,0)

## Földrészletek
telek.df <- st_sf(data.frame(HRSZ = c(10,11,12), st_sfc(elozo.pl, elso.pl,masodik.pl, crs=23700)))


### Kérdések:
## Pontkód hogyan?

