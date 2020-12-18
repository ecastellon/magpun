## base de datos departamentos municipios
library(DBI)
library(RSQLite)

sq <- "c:/encuestas/marco/depmun.sqlite"
ff <- Sys.getenv("DEPMUN")

mydb <- dbConnect(RSQLite::SQLite(),
                  "c:/encuestas/marco/depmun.sqlite")

dbExecute(mydb,
          "create table departamento(dpt int primary key, departamento text);")

dbExecute(mydb, paste0("create table municipio(mun int primary key,",
                       "dpt int foreignkey references departamento(dpt),",
                       "municipio text);"))

ww <- foobj::get_dff(ddpto, file = ff)

ww %<>% filter(dpto != 45)
ww[ww$dpto == 25, "departamento"] <- "Estelí"
ww[ww$dpto == 35, "departamento"] <- "León"
ww[ww$dpto == 85, "departamento"] <- "Río San Juan"
ww[ww$dpto == 91, "departamento"] <- "RACCN"
ww[ww$dpto == 93, "departamento"] <- "RACCS"

cc <- "insert into departamento(dpt, departamento) values("
for (kk in seq_len(nrow(ww))) {
    xx <- ww[kk,,drop = TRUE]
    dbExecute(mydb, paste0(cc, xx$dpto, ",'", xx$departamento, "');"))
}

## iso https://es.wikipedia.org/wiki/ISO_3166-2:NI
## iso https://www.iso.org/obp/ui/#iso:code:3166:NI
ww["iso"] <- character()
ww["abr"] <- character()
ww <- edit()

dbExecute(mydb, "alter table departamento add column iso text")
dbExecute(mydb, "alter table departamento add column abr text")

cc <- "update departamento set "
for (kk in seq_len(nrow(ww))) {
    xx <- ww[kk,,drop = TRUE]
    dbExecute(mydb, paste0(cc, "iso='", xx$iso, "', abr='", xx$abr,
                           "' where dpt=", xx$dpto, ";"))
}

ww <- foobj::get_dff(dmuni, file = ff)
ww <- edit(ww) #corregir acentos

ww["dpt"] <- ww$muni %/% 100L

cc <- "insert into municipio(mun, dpt, municipio) values("
for (kk in seq_len(nrow(ww))) {
    xx <- ww[kk,,drop = TRUE]
    dbExecute(mydb, paste0(cc, xx$muni, ",", xx$dpt, ",'",
                           xx$municipio, "');"))
}

dbExecute(mydb,
          "update municipio set municipio='San Nicolás' where mun=2530")

dbDisconnect(mydb)

## los mismos datos con la geometría
library(sf)

db <- dbConnect(RSQLite::SQLite(),
                "c:/encuestas/marco/depmun.sqlite")
dp <- dbGetQuery(db, "select * from departamento")
mu <- dbGetQuery(db, "select * from municipio")
dbDisconnect(db)

tt <- read_sf("c:/sig/limites/municipios.shp")
uu <- read_sf("c:/sig/limites/departamentos2009.shp")
st_crs(uu) <- st_crs(tt)

gg <- st_geometry(uu)
dd <- st_drop_geometry(uu)
names(dd) <- tolower(names(dd))

dp %<>% rename(cod_dpto = dpt, depto = departamento) %>%
    select(id, dpt, departamento, km2)
