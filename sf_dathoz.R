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

## Alsó szomszédok
eltol <- c(0,diff(as.numeric(elso.mat[c(1,2),"y"])),0)
elozoalatt.pl <- elozo.pl - eltol
elsoalatt.pl <- elso.pl - eltol
masodikalatt.pl <- masodik.pl - eltol

## Közterület

## Lista
telkek.list <- list(elozo.pl,
                    elso.pl,
                    masodik.pl,
                    elozoalatt.pl,
                    elsoalatt.pl,
                    masodikalatt.pl)

## Földrészletek
telek.df <- st_sf(data.frame(HRSZ = c(10,11,12,20,21,22),
                             st_sfc(telkek.list,
                                    crs=23700)))

## Adatkinyerés
st_coordinates(st_cast(telek.df[telek.df$HRSZ == 11,], to="LINESTRING"))
st_coordinates(st_cast(telek.df[telek.df$HRSZ == 11,], to="MULTIPOINT"))

### Kérdések:
## Pontkód hogyan?

