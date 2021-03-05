# -*- coding: utf-8 -*-
##--- selección muestra de puntos ---
## v.0.0 202011

#' Archivos-.shp
#' @description Devuelve lista de los archivos con extensión shp y un
#'     prefijo determinado
#' @param pre character: letras iniciales (prefijo) del nombre
#' @param rut character: el directorio; por omisión, el directorio de
#'     trabajo
#' @return character
#' @export
shp_list <- function(pre = character(), rut = character()) {
    list_ar(pre, ext = "shp", rut)
}

#' Leer-shp
#' @description lee el archivo de una cobertura shape
#' @param shp character: ruta del archivo shape
#' @return objeto de clase "simple feature" o NULL
#' @export
shp_read <- function(shp = character()) {
    stopifnot("arg. shp inadmisible" = filled_char(shp))
    sh <- tryCatch(sf::read_sf(shp),
                   error = function(e){
                       message("\n... error lectura !!!")
                       return(NULL)
                   })
    invisible(sh)
}

#' Bloques-dpt
#' @description Extrae la cobertura de un departamento dado, de la
#'     cobertura de bloques de muestreo del país
#' @details Supone que la cobertura de bloques contiene un atributo
#'     con el código (id) del departamento al cual está asignado cada
#'     bloque
#' @param cod integer: código del departamento
#' @param cob character: ruta a la cobertura de bloques del país
#' @param dpt character: atributo con el código de los departamento;
#'     por omisión, "dpto"
#' @return objeto clase "sf"
#' @export
shp_blo_dpt <- function(cod = integer(), cob = character(),
                        dpt = "dpto") {
    stopifnot("arg. cod inadmisible" = filled_num(cod),
              "arg. cob inadmisible" = filled_char(cob),
              "arg. dpt inadmisible" = filled_char(dpt))
    
    cb <- shp_read(cob)
    if (!is.null(cb)) {
        if (is.element(dpt, names(cb))) {
            ii <- cb[[dpt]] == cod
            if (any(ii)) {
                cb <- cb[ii, ]
            } else {
                cb <- NULL
                warning("\n... departamento ", cod,
                        "no existe en cobertura")
            }
        } else {
            warning("\n... falta atributo ", dpt)
            cb <- NULL
        }
    }

    invisible(cb)
}

#' Shape-save
#' @description Guarda una cobertura de una sola capa como un archivo
#'     tipo "shape". !ÔjÔ! : suplanta el archivo si este ya existe. 
#' @param x objeto clase sf: cobertura
#' @param shp character: directorio y nombre del archivo
#' @return logical: TRUE si no se produce un error
#' @export
#' @examples
#' ## cob <- shp_read("c:/filepath/cob.shp")
#' ## shp_save(cob, "c:/file/path/file.shp")
shp_save <- function(x, shp = character()) {
    stopifnot("arg. shp inadmisible" = filled_char(shp) &&
                  nzchar(shp),
              "arg. x falta" = !missing(x))

    er <- try(sf::st_write(x, shp, driver = "ESRI Shapefile",
                           quiet = TRUE, append = FALSE),
              silent = TRUE)

    nok <- inherits(er, "try-error")
    if (nok) {
        warning("\n... no guardado")
    }

    !nok
}

#' Shape data
#' @description Devuelve el data.frame con los atributos de la
#'     cobertura
#' @param x character: nombre del archivo shape
#' @return data.frame o NULL
#' @export
shp_data_read <- function(x = character()) {
    stopifnot("arg. x inadmisible" = filled_char(x) && nzchar(x),
              "archivo no existe" = file.exists(x))

    ss <- basename(x) %>% rm_ext() %>% paste0(".dbf") %>%
        file.path(dirname(x), .)

    dt <- try(foreign::read.dbf(ss, as.is = TRUE), silent = TRUE)

    if (inherits(dt, "try-error")) {
        warning("\n... error de lectura !!!")
        dt <- NULL
    }

    invisible(dt)
}

#' shp_data_save
#' @description Guarda data frame como la tabla de atributos de un shape
#'     que ya existe
#' @param x el data.frame
#' @param shp nombre del archivo con extensión "shp"
#' @return logical
#' @export
#' @examples
#' ## shp_data_save(dfx, "c:/pathshp/file.shp")
shp_data_save <- function(x, shp = character()) {
    stopifnot(exprs = {
        filled_char(shp)
        nzchar(shp)
        file.exists(shp)
        inherits(x, "data.frame")})

    nf <- file.path(dirname(shp),p
                    sub("\\.shp$", ".dbf", basename(shp)))
    
    nok <- try(foreign::write.dbf(x, nf), silent = TRUE) %>%
        inherits("try-error")

    if (nok) {
        warning("\n... error al guardar !!!")
    }

    !nok
}

#' Atributos
#' @description Devuelve los nombres de las columnas (variables) de la
#'      tabla de atributos de la cobertura
#' @param x character: nombre del archivo shape
#' @return character o NA
#' @export
shp_atributos <- function(x = character()) {
    stopifnot("arg. x inadmisible" = filled_char && nzchar,
              "shp no existe" = file.exists(x))
    
    ww <- shp_data(x)
    na <- NA_character_
    if (is.data.frame(ww)){
        na <- names(ww)
    }

    na
}

#' KML
#' @description Guarda objeto de clase \code{sf} en archivo de formato
#'     kml.
#' @details Tome en cuenta que todos los atributos de la cobertura
#'     "shape" pasan como atributos del archivo "kml", y que por lo
#'     tanto es conveniente, si el kml se visualizará en google-earth,
#'     dejar en la cobertura sólo los atributos de interés.
#' @param shp character o sf: nombre (character) de un archivo tipo
#'     "shape" u objeto de clase \code{sf}
#' @param kml character: nombre del archivo kml
#' @return NULL si error o argumento kml
shp_save_kml <- function(shp, kml) {
    stopifnot("arg. kml inváliso" = ok_fname(kml))

    si <- inherits(shp, "sf")
    if (!si) {
        shp <- shp_read(shp)
        si <- !is.null(shp)
    }

    epsg_google_maps <- 3857
    if (si) {
        ## si no proj. no es lat-lon -> transformar
        if (sf::st_crs(shp)$epsg != epsg_google_maps) {
          shp <- sf::st_transform(shp, crs = epsg_google_maps)
        }

        w <- try(sf::st_write(shp, kml, driver = "kml"),
                 silent = TRUE)
        si <- !inherits(w, "try-error")
    }

    ifelse(si, kml, NULL)
}




## kml
## función para cambiar el pushpin, el color, según id de punto
library(xml2)

fx <- file.path("c:/encuestas/marco/bloquespuntos/sel2021", "tst.kml")
xm <- read_xml(fx)


length(xm)
length(xml_children(xm))
xml_type(xm)
xml_structure(xm)

z <- xml_root(xm)
xml_length(z)
## [1] 1
length(z)
##> [1] 2
xml_attr(xml_children(z), "id")
##> [1] "root_doc"

u <- xml_child(z, 1) %>% xml_children() %>% extract(2)
xml_length(u)
##> + > [1] 669
length(u)
##[1] 1

xml_name(xml_child(u, 1))
##> [1] "name"
xml_path(xml_child(u, 1))
##> [1] "/*/*/*[2]/*[1]"
xml_name(xml_child(u, 4))
##> [1] "Placemark"
xml_path(xml_child(u, 4))
##> + > [1] "/*/*/*[2]/*[4]"

xml_contents(xml_child(u, 4))

t <- xml_find_all(xml_child(u, 4), "*[2]/*[1]")
xml_length(t)
length(t)
## coordenadas
## xml_contents(t)

xml_children(u) %>% length()
xml_contents(u[[1]])

y <- xml_children(z)
xml_length(y)
## > [1] 2

xml_child(y, 1) %>% xml_name()
## "Schema"
xml_child(y, 2) %>% xml_name()
## "Folder"

xml_attr(xml_children(y)[[1]], "id")
## [1] "tst"

xml_attrs(xml_children(y)[[1]])
## >  name    id 
## "tst" "tst" 

xml_contents(xml_children(y)[[1]])
## > {xml_nodeset (2)}
## [1] <SimpleField name="upm" type="string"/>
## [2] <SimpleField name="punto" type="int"/>

xml_child(xml_children(y)[[1]], 1)
## > {xml_node}
## <SimpleField name="upm" type="string">

w <- xml_children(y)[[2]]
xml_length(w)
## [1] 669
length(w)
##> [1] 2

xml_child(w, 1) %>% xml_name()
## [1] "name"
xml_child(w, 1) %>% xml_contents()
## > {xml_nodeset (1)}
## [1] tst

xml_child(w, 2) %>% xml_name()
## [1] "name"

xml_child(w, 2) %>% xml_contents()
## {xml_nodeset (2)}
## <ExtendedData>\n  <SchemaData schemaUrl="#tst">\n    <SimpleData name="up ...
## <Point>\n  <coordinates>-86.6682069572039,13.4442752797853</coordinates>\ ...


v <- xml_child(w, 2)
xml_length(v)
## > + [1] 2
xml_child(w, 2) %>% xml_name()
## > [1] "Placemark"

u <- xml_find_all(z, "/Folder")
xml_length(u)
length(u)
u

xml_length(z)
w <- xml_find_all(z, ".//Schema")
xml_length(w)

class(w)
w


z[[0]]
names(xm)
xm[[1]]
