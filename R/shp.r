## -*- coding: utf-8 -*-
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

    nf <- file.path(dirname(shp),
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

## -- archivos KML --

#' KML write
#' @description Guarda objeto de clase \code{sf} en archivo de formato
#'     KML.
#' @details Tome en cuenta que todos los atributos de la cobertura
#'     "shape" pasan como datos en el archivo "kml", y que por lo
#'     tanto es conveniente, si el kml se visualizará en google-earth,
#'     dejar en la cobertura sólo los atributos que interesa presentar
#'     en el mapa.
#' @param shp character o sf: nombre (character) de un archivo tipo
#'     "shape" u objeto de clase \code{sf}
#' @param kml character: nombre del archivo kml
#' @return NULL si error o argumento kml
shp_save_kml <- function(shp, kml) {
    stopifnot("arg. kml inválido" = ok_fname(kml))

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
