## Reorder hazdat
hazdatr <- hazdat[c(5,7,4,1,3,2,8,6),]
rownames(hazdatr) <- NULL

## Az első négy pont és a záró
elso.mat <- as.matrix(rbind(hazdatr[1:4,1:3], hazdatr[1,1:3]))
row.names(elso.mat) <- c(1:4,11)
elso.pl <- st_polygon(list(elso.mat))
telkek.list <- list(elso.pl)
telek.df <- st_sf(data.frame(HRSZ = 11,
                             st_sfc(telkek.list,
                                    crs=23700)))

## Szomszédok generálása
parc.gener <- function(x, dir, pid, newpid) {
    curr.geom <- x[x$HRSZ == pid, "geometry"]
    curr.pl <- st_cast(curr.geom[["geometry"]], to="POLYGON")
    coord.mat <- st_coordinates(st_cast(curr.pl, to="MULTIPOINT"))
    new.mat <- switch(dir,
                     up = coord.mat + c(0,diff(as.numeric(coord.mat[c(1,2),"Y"])),0,0),
                     down = coord.mat - c(0,diff(as.numeric(coord.mat[c(1,2),"Y"])),0,0),
                     right = coord.mat + c(diff(as.numeric(coord.mat[c(1,3),"X"])),0,0,0),
                     left = coord.mat - c(diff(as.numeric(coord.mat[c(1,3),"X"])),0,0,0)
                     )
    new.pl <- st_polygon(list(new.mat))
    out.list <- list(new.pl)
    out.df <- st_sf(data.frame(HRSZ = newpid,
                             st_sfc(out.list,
                                    crs=23700)))
    rbind(out.df, x)
}

telek.df <- parc.gener(telek.df, "left", 11, 10)
telek.df <- parc.gener(telek.df, "right", 11, 12)

## Alsó szomszédok
elozoalatt.pl <- parc.gener(telek.df, "down",10,13)
elsoalatt.pl <- parc.gener(elso.pl, "down")
masodikalatt.pl <- parc.gener(masodik.pl, "down")

## Közterület
kozter.list <- list(parc.gener(elozo.pl, "up"),
                    parc.gener(elso.pl, "up"),
                    parc.gener(masodik.pl, "up"))
kozter.df <- st_sf(data.frame(HRSZ = c(9, 9, 9),
                             st_sfc(kozter.list,
                                    crs=23700)))
st_coordinates(st_cast(kozter.df, to="MULTIPOINT"))

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

full.df <- rbind(telek.df, kozter.df)

## Adatkinyerés
st_coordinates(st_cast(telek.df[telek.df$HRSZ == 11,], to="LINESTRING"))
st_coordinates(st_cast(telek.df[telek.df$HRSZ == 11,], to="MULTIPOINT"))

## HRSZ csere
telek.df$HRSZ <- telek.df$HRSZ + 10

### Kérdések:
## Pontkód hogyan?

