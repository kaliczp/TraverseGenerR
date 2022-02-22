## Reorder hazdat
hazdatr <- hazdat[c(5,7,4,1,3,2,8,6),]
rownames(hazdatr) <- NULL

## Az első négy pont és a záró
elso.mat <- as.matrix(rbind(hazdatr[1:4,1:3], hazdatr[1,1:3]))
row.names(elso.mat) <- c(5001:5004,5001)
elso.pl <- st_polygon(list(elso.mat))
telkek.list <- list(elso.pl)
telek.df <- st_sf(data.frame(HRSZ = 11,
                             st_sfc(telkek.list,
                                    crs=23700)))

## Szomszédok generálása
parc.gener <- function(x, dir, pid, newpid) {
    curr.geom <- x[x$HRSZ == pid, "geometry"]
    curr.pl <- st_cast(curr.geom[["geometry"]], to="POLYGON")
    coord.mat <- st_coordinates(st_cast(curr.pl, to="MULTIPOINT"))[,1:3]
    new.mat <- switch(dir,
                     up = coord.mat + matrix(c(0,diff(as.numeric(coord.mat[c(1,2),"Y"])),0), nr =5, nc = 3, byrow = TRUE),
                     down = coord.mat - matrix(c(0,diff(as.numeric(coord.mat[c(1,2),"Y"])),0), nr =5, nc = 3, byrow = TRUE),
                     right = coord.mat + matrix(c(diff(as.numeric(coord.mat[c(1,3),"X"])),0,0), nr =5, nc = 3, byrow = TRUE),
                     left = coord.mat - matrix(c(diff(as.numeric(coord.mat[c(1,3),"X"])),0,0), nr =5, nc = 3, byrow = TRUE)
                     )
    row.names(new.mat) <- switch(dir,
                                 up = as.numeric(row.names(new.mat)) + 4,
                                 down = as.numeric(row.names(new.mat)) - 4,
                                 right = as.numeric(row.names(new.mat)) + 4,
                                 left = as.numeric(row.names(new.mat)) - 4
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
telek.df <- parc.gener(telek.df, "down",10,13)
telek.df <- parc.gener(telek.df, "down", 11, 14)
telek.df <- parc.gener(telek.df, "down", 12, 15)

## Közterület
telek.df <- parc.gener(telek.df, "up",10,9)
telek.df <- parc.gener(telek.df, "up", 11, 9)
telek.df <- parc.gener(telek.df, "up", 12, 9)

st_coordinates(st_cast(telek.df[telek.df$HRSZ == 9,], to="MULTIPOINT"))

## Adatkinyerés
st_coordinates(st_cast(telek.df[telek.df$HRSZ == 11,], to="LINESTRING"))
st_coordinates(st_cast(telek.df[telek.df$HRSZ == 11,], to="MULTIPOINT"))

## HRSZ csere
telek.df$HRSZ <- telek.df$HRSZ + 10

### Kérdések:
## Pontkód hogyan?

