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

#' Ícono GE
#' @description url de los íconos más comunes de google-earth
#' @details Hay muchos íconos que ofrece GE para señalar la posición
#'     de los puntos
#'     (\code{http://kml4earth.appspot.com/icons.html}). Los tipos que
#'     más se utilizan son las tachuelas (pushspin), raquetas (paddle)
#'     y cuadrados a diferentes colores. La función devuelve la
#'     dirección url de los íconos de esos 3 tipos, a 5 colores, junto
#'     con las coordenadas que determinan la ubicación del ícono con
#'     respecto a las del punto. Estos dos elementos se utilizan para
#'     construir los elementos "href" y "hotSpot" de los nodos
#'     IconStyle de los archivos KML.
#' 
#'     El parámetro "ico" es para indicar el tipo (tachuela, raqueta,
#'     cuadro) y "col" para el color del ícono (rojo, verde, azul,
#'     amarillo, blanco). Es suficiente pasar como argumento la
#'     primera letra de las opciones, excepto para los colores
#'     amarillo y azul que son necesarias las dos primeras para evitar
#'     la ambigüedad. Si cualquiera de los dos no es una opción
#'     valida, se devuelve la url de un círculo a color blanco.
#' @param ico character: el tipo de ícono
#' @param col character: el color del ícono
#' @return list con "url" y "pos" para construir los elementos href y
#'     hotSpot de los nodos StyleIcon de los archivos KML
#' @keywords internal
#' @examples
#' icon_def(col = "az") #tachuela color azul
#' icon_def("r", "am")  #raqueta color amarillo
#' icon_def("")         #círculo blanco
icon_def <- function(ico = "tachuela", col = "blanco") {
    stopifnot(exprs = {
        "arg. ico" = filled_char(ico)
        "arg. col" = filled_char(col)
    })

    tico <- c(t = "tachuela", g = "raqueta", c = "cuadro")
    cico <- c(r = "rojo", g = "verde", b = "azul", a = "amarillo",
              w = "blanco")

    ## default: círculo color blanco con centro negro
    urlb <- "http://maps.google.com/mapfiles/kml/"
    icon <- "shapes/placemark_circle.png"
    hspt <- c(x = "0.5", y = "0.5", xunits = "fraction",
              yunits = "fraction")

    ico <- pmatch(ico, tico)
    col <- pmatch(col, cico)
    ## el default si no especifica tipo ícono y su color
    mx <- paste("pushpin", c("red", "grn", "blue", "ylw", "wht"),
                sep = "/") %>%
        paste("pushpin.png", sep = "-") %>%
        c(paste("paddle", c("red", "grn", "blu", "ylw", "wht"),
                sep = "/") %>%
          paste("blank.png", sep = "-")) %>%
        c(paste("paddle", c("red", "grn", "blu", "ylw", "wht"),
                sep = "/") %>%
          paste("blank-lv.png", sep = "-")) %>%
        matrix(nrow = 3, byrow = TRUE)
    
    hs <- list(c(x = "20",  xunits = "pixels",
                 y = "1",   yunits = "pixels"),
               c(x = "0.5", xunits = "fraction",
                 y = "1",   yunits = "pixels"),
               c(x = "0.5", xunits = "fraction",
                 y = "0.5", yunits = "fraction"))
    
    if (!(is.na(ico) || is.na(col))) {
        icon <- mx[ico, col]
        hspt <- hs[[ico]]
    }
    
    list(url = paste0(urlb, icon),
         pos = hspt)
}

#' IconStyle
#' @description Produce un nodo IconStyle de documento KML
#' @details (\code{https://developers.google.com/kml/documentation/})
#' @param as_xml logical: devuelve documento xml o lista; FALSE por
#'     omisión
#' @param ... admisibles: id, ico, col, color, escala, rumbo, url, pos
#' @return nodo IconStyle
#' @seealso \code{icon_def}
#' @export
#' @examples
#' sty_ico()
#' sty_ico(ico = "tach", col = "rojo")
#' sty_ico(ico = "")
sty_ico <- function(as_xml = FALSE, ...) {
    ## TODO: chk. args

    ## -- especificaciones --
    ## default
    w <- icon_def() # default tachuela blanca
    z <- list(color     = "ff0000ff", # rojo
              colorMode = "normal",
              escala    = 0.7,
              rumbo     = 0,
              url       = w$url,
              pos       = w$pos)

    ## cambios por argumentos en ...
    y <- eval(substitute(alist(...)))
    if (filled(y)) {
        x <- c("ico", "col")
        if (all(is.element(x, names(y)))) { # calculada
            w <- do.call("icon_def", y[x])
            y[["url"]] <- w$url
            y[["pos"]] <- w$pos
        }
        x <- intersect(names(y), names(z))
        if (filled_char(x)) {
            z[x] <- y[x]
        }
    }

    ## nodo a partir de lista de spec
    id = ""
    if (is.element(id, names(y))) {
        id <- y[["id"]]
    }
    w <- list(IconStyle = structure(
                  list(
                      list(color   = list(z$color),
                           colorMode = list(z$colorMode),
                           scale   = list(z$escala),
                           heading = list(z$rumbo),
                           Icon    = list(href = list(z$url)),
                           hotSpot = structure(list(),
                                               x = z$pos[["x"]],
                                               y = z$pos[["y"]],
                                               xunits = z$pos[["xunits"]],
                                               yunits = z$pos[["yunits"]])
                           )), id = id))
    
    if (as_xml) w <- as_xml_document(w)
    invisible(w)
}

#' LabelStyle
#' @description Nodo LabelStyle documento KML
#' @details Estilo para el nombre del punto en el mapa
#' @param as_xml logical: devuelve documento xml o lista; FALSE por
#'     omisión
#' @param ... admisibles: id, color, colorMode, escala
#' @return nodo LabelStyle
#' @export
#' @examples
#' sty_lab()
#' sty_lab(id = "labRojo", color = "ffffff00", escala = 1.2)
sty_lab <- function(as_xml = FALSE, ...) {
    ## default
    z <- list(color     = "ff000000", # negro
              colorMode = "normal",
              escala    = 0.7)

    y <- eval(substitute(alist(...)))
    if (filled(y)) {
        x <- intersect(names(y), names(z))
        if (filled_char(x)) {
            z[x] <- y[x]
        }
    }

    ## nodo a partir de lista de spec
    id = ""
    if (is.element(id, names(y))) {
        id <- y[["id"]]
    }

    w <- list(LabelStyle = structure(
                  list(list(color = list(z$color),
                            colorMode = list(z$colorMode),
                            scale   = list(z$escala))
                       ), id = id))

    if (as_xml) w <- as_xml_document(w)
    invisible(w)
}

## Kindle sepia color code reading
## background:#FBF0D9;color:#5F4B32;

#' BalloonStyle
#' @description Nodo BalloonStyle documento KML
#' @details Estilo del cuadro que emerge cuando "click" el ícono del
#'     punto
#' @param as_xml logical: devuelve documento xml o lista; FALSE por
#'     omisión
#' @param ... admisibles: bgcol, txtcol, texto, modo
#' @return nodo BalloonStyle
#' @export
#' @examples
#' sty_bal()
sty_bal <- function(as_xml = FALSE, ...) {
    ## default
    z <- list(bgcol  = "ffffffff", # blanco
              txtcol = "ff000000", # negro
              texto  = "$[name]",
              modo   = "default")  # hide

    y <- eval(substitute(alist(...)))
    if (filled(y)) {
        x <- intersect(names(y), names(z))
        if (filled_char(x)) {
            z[x] <- y[x]
        }
    }

    ## nodo a partir de lista de spec
    id = ""
    if (is.element(id, names(y))) {
        id <- y[["id"]]
    }

    w <- list(BalloonStyle = structure(
                  list(list(bgColor     = list(z[[1]]),
                            textColor   = list(z[[2]]),
                            text        = list(z[[3]]),
                            displayMode = list(z[[4]]))
                       ), id = id))

    if (as_xml) w <- as_xml_document(w)
    invisible(w)
}

## produce StyleMap
## normal, destacado: id del estilo para cada caso

#' StyleMap
#' @description Nodo StyleMap documento KML
#' @details ver especificaciones GE
#' @param as_xml logical: devuelve documento xml o lista; FALSE por
#'     omisión
#' @param id character: atributo id del estilo
#' @param normal character: atributo id del estilo normal
#' @param destacado character: atributo id del estilo alterno
#' @return nodo StyleMap
#' @export
#' @examples
#' sty_map(id = "estilo", "estilo1", "estilo2")
sty_map <- function(as_xml = FALSE, id = character(),
                    normal = character(),
                    destacado = character()) {
    stopifnot(exprs = {
        "arg. id" = filled_char(id) && is_scalar(id)
        "arg. norm." = filled_char(normal) && is_scalar(normal)
        "arg. dest." = filled_char(destacado) && is_scalar(destacado)
    })

    ## valida inicial
    normal <- paste0("#", sub("[^[:alnum:]]", "", normal))
    destacado <- paste0("#", sub("[^[:alnum:]]", "", destacado))
                      
    w <- list(StyleMap = structure(
                  list(Pair = list(
                           list(key = list("normal"),
                                styleUrl = list(normal))),
                       Pair = list(
                           list(key = list("highlight"),
                                styleUrl = list(destacado))
                       )), id = id))

    if (as_xml) w <- as_xml_document(w)
    invisible(w)
}

#' Style
#' @description Nodo Style documento KML
#' @details ver especificaciones
#' @param id character: atributo id del nodo Style
#' @param icon nodo IconStyle
#' @param label nodo LabelStyle
#' @param ball nodo BalloonStyle
#' @return nodo Style
#' @export
#' @examples
#' sty_sty("estilo")
#' sty_sty("est", sty_ico())
sty_sty <- function(id = character(), icon = NULL,
                    label = NULL, ball = NULL) {
    stopifnot(exprs = {
        "arg. id" = filled_char(id) && is_scalar(id)
        "arg. icon" = is.null(icon) ||
            (inherits(icon, "xml_node") &&
             tolower(xml_name(icon)) == "iconstyle")
        "arg. label" = is.null(label) ||
            (inherits(label, "xml_node") &&
             tolower(xml_name(label)) == "labelstyle")
        
        "arg. ball" = is.null(ball) ||
            (inherits(ball, "xml_node") &&
             tolower(xml_name(ball)) == "balloonstyle") })

    ## nodo base
    w <- read_xml(paste0("<Style id = '", id, "'/>"))

    if (!is.null(icon)) {#chk válido
        xml_add_child(w, icon)
    }
    
    if (!is.null(label)) {
        xml_add_child(w, label)
    }

    if (!is.null(ball)) {
        xml_add_child(w, ball)
    }

    invisible(w)
}

## ...: name, Snippet, visibility, open
kml_root <- function(..., as_xml = TRUE) {

    nodes <- c("name", "Snippet", "visibility", "open")
    z <- list(list(name = list("root")),
              list(Snippet = list("root")),
              list(visibility = list(0)),
              list(open = list(0)))
    
    y <- dots_values_as_list(...)
    if (filled(y)) {
        ny <- names(y)
        iy <- which(ny %in% nodes)
        if (filled(iy)) {
            y <- lapply(iy, function(x) y[x])
            iz <- which(nodes %in% ny)
            z[iz] <- y
        }
    }

    w <- list(Document = structure(z, id = "id_root"))
    if (as_xml) w <- as_xml_document(w)
    invisible(w)
}


node_folder <- function(...) {
}

node_placemark <- function(...) {
}

node_point <- function(x, y, as_xml = TRUE) {
    x <- make_node("coordinates", paste(x, y, sep = ","),
                   as_xml = FALSE)
    w <- list(Point = x)
    if (as_xml) w <- as_xml_document(w)
    invisible(w)
}

aa <- node_point(-86.6582466955103,13.4430675626383)
as.character(aa)

node_coordinates <- function(x, y) {
}

node_snippet <- function(x) {
}

node_data <- function(...) {
}

node_visibility <- function(x) {
}

node_open <- function(x) {
}

node_doc <- function(...) {
}

node_name <- function(x) {
}


#' @return lista de nodos xml o lista de listas
make_node_set <- function(tag, value, atr = list(), as_xml = TRUE) {
    ## validar: tiene sentido si value un vector con más de uno
    ## length(value) == length(atr)
    nv <- length(value)
    ni <- length(atr)
    if (nv > ni) {
        atr <- rep_len(atr, nv)
    } else {
        if (nv < ni) {
            value <- rep_len(value, ni)
        }
    }

    Map(function(x, y) make_node(tag = tag, x, y, as_xml), value, atr,
        USE.NAMES = FALSE)
}

bb <- make_node_set("name", letters[1:3])
as.character(bb[[1]])
as.character(bb[[3]])

bb <- make_node_set("name", letters[1:3], as_xml = FALSE)
dd <- list(names = bb)
ee <- as_xml_document(dd)
as.character(ee)

#' @return nodo xml o list
make_node <- function(tag, value = "", atr = list(), as_xml = TRUE) {
    x <- list(value)
    if (filled_list(atr)) attributes(x) <- atr

    w <- list(x)
    names(w) <- tag
    
    if (as_xml) w <- as_xml_document(w)
    
    invisible(w)
}

bb <- make_node("name", 5001, atr = list(id = "5001", bb = "no"))
aa <- make_node("Placemark", "")
xml_add_child(aa, bb)

kml_doc <- function(root) {
    x <- xml_new_root("kml",
                      xmlns = "http://www.opengis.net/kml/2.2") %>%
        xml_add_child(root)
    
    invisible(x)
}

## especificaciones
## - Document
##   name: nombre de encuesta; e.g. "Seguimiento, marzo"
##   Snippet: fecha reporte; e.g. "15 de marzo"
##   visibility: 0
##   open: 0
## - Folder delegaciones
##   name: "delegaciones"
##   Snippet: nada
##   open: 0
##   visibility: 0
##   - Placemark
##     - ExtendedData
##       - Data
##         puntos asignados
##         puntos visitados
##         porcentaje visitados
##         técnico: puntos visitados
##     - description
##       - CDATA
##         los datos en nodo Data
##         gráfica de código de control
##         gráfica maptree(?)
##         foto tropa
## - Folder departamento
##   name: nombre departamento
##   Snippet: nada
##   open: 0
##   visibility: 0
## - Folder municipio
##   name: nombre municipio
##   Snippet: nada
##   open: 0
##   visibility: 0
##   - Placemark
##     name: punto
##     Snippet: nombre del técnico
##     visibility: 1
##     open: 1
##     - ExtendedData
##       - Data
##         técnico
##         departamento
##         municipio
##         localidad
##         nombre finca
##         dirección finca
##     - description
##       - CDATA
##         los datos en nodos Data
##         foto
##     - Point
##       - coordinates

#' mapa-avance
#' @description Mapa kml estado puntos encuesta
#' @details
#' @param xy matrix: coordenadas lat-lon de los puntos
#' @param dfe data frame código de control leídos de la base de datos
#' @param dfp data frame de los puntos de la encuesta del mes
#' @return character
#' @examples
kml_estado_puntos <- function(xy, dfe, dfp) {
    
}

test_file(RD$find_file("tests/testthat/test-kml.r"))
