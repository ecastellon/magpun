# -*- coding: utf-8 -*-

## funciones muestreo, factores expansión y relacionadas

#' Puntos polígonos
#' @description Selección aleatoria de puntos dentro de polígonos
#' @details Después de verificar que los argumentos satisfagan las
#'     especificaciones de los parámetros, se procede a ordenar los
#'     polígonos (si el argumento de «orden» no es vacío) con el fin
#'     de que los puntos aparezcan en ese orden en el resultado. (Si
#'     el orden se especifica con un vector de enteros, este debe ser
#'     producto de la función "order"; la función no verifica la
#'     validez del vector.) Después se hace la muestra y a
#'     continuación se da un nombre a los puntos con la función
#'     "n_ids". Como esta función produce una "secuencia" de nombres
#'     (vector tipo "character"), resulta que los nombres de los
#'     puntos van en la secuencia del orden de los polígonos. Por
#'     ejemplo, si se ordena de manera creciente por el atributo
#'     «municipio», los nombres de los puntos en los polígonos del
#'     municipio con código 10, serán lexicográficamente más grandes
#'     que los nombres de los puntos de los polígonos del municipio
#'     con código 5, y por consiguiente aparecerán, en el resultado,
#'     en filas posteriores.
#' @param x obj. clase "sf": cobertura de los polígonos.
#' @param npun character o numeric: atributo de la cobertura
#'     (character) o vector de enteros (numeric) con el número de
#'     puntos asignados a cada polígono; por omisión, se asume que es
#'     un atributo con nombre "puntos"
#' @param cols character o numeric: nombre (character) o posición
#'     (numeric) de las columnas ('atributos') en la cobertura de
#'     polígonos, que se trasladan a la de los puntos
#'     seleccionados. Por omisión, no se trasladan atributos.
#' @param orden character o integer: nombre (character) de las
#'     columnas por las que se ordenan los polígonos antes de obtener
#'     la muestra, o el vector (integer, obtenido con la función
#'     "order") con el orden deseado. Los puntos siguen el orden de
#'     los polígonos. Por omisión, no se reordena el resultado
#' @param pref character: prefijo del nombre (id) que se dará a los
#'     puntos seleccionados; por omisión, "pt". Los nombres se
#'     obtienen con la función "n_ids". Son devueltos en el resultado
#'     en la columna "idpt".
#' @param mxch numeric: número de caracteres (incluyendo los del
#'     prefijo) que tendrá el nombre del punto; por omisión, 5
#' @param ruta character: nombre del archivo donde guardar el
#'     resultado.
#' @return objeto de clase "sf" o NULL
#' @seealso n_ids
#' @examples
#' nc <- st_read(system.file("shape/nc.shp", package="sf"),
#'               quiet = TRUE) %>%
#'       extract(1:4,) %>%
#'       st_transform(crs = 32617) #coordenadas planas
#' pp <- muestra_puntos(nc, c(2, 1, 2, 1))
#' pp <- muestra_puntos(nc, c(2, 1, 2, 1), c("FIPSNO", "NAME"))
#' pp <- muestra_puntos(nc, c(2, 1, 2, 1), orden = c("NAME", "FIPS"))
#' pp <- muestra_puntos(nc, c(2, 1, 2, 1), orden = order(nc$NAME))
#' @export
muestra_puntos <- function(x, npun = "puntos", cols = character(),
                           orden = character(), pref = "pt", mxch = 5,
                           ruta = character()) {
    
    stopifnot("arg. x inadmisible" = inherits(x, "sf"),
              "arg. npun inadmisible" = filled_char(npun) ||
                  filled_num(npun),
              "arg. cols inadmisible" = is.character(cols) ||
                  is.numeric(cols),
              "arg. cols inadmisible" = is.character(orden) ||
                  is.integer(orden),
              "arg. pref inadmisible" = filled_char(pref),
              "arg. mxch inadmisible" = filled_num(mxch),
              "arg. ruta inadmisible" = is.character(ruta))

    ## TODO verificar la geometría es multipolígono
    nc <- names(x)
    if (is.character(npun)) {
        stopifnot("arg. cpun no es atributo" = is.element(npun, nc))
        npun <- x[[npun]]
    } else {
        stopifnot("arg. cpun difiere de num. de polig." =
                      length(npun) == nrow(x),
                  "arg. cpun todo cero" = any(npun > 0))
    }

    if (filled_char(orden)) {
        if (all(is.element(orden, nc))) {
            xo <- x[, orden, drop = TRUE]
            if (is.null(dim(xo))) {
                orden <- order(xo)
            } else {
                orden <- do.call("order", as.list(unname(xo)))
            }
            x <- x[orden,]
        } else {
            message("\n... sin ordenar: columna no existe !!!")
        }
    } else {
        if (filled_int(orden)) {#verificar sec. enteros correcta!!
            x <- x[orden,]
        }
    }
    
    mp <- sf::st_sample(x, npun)
    id <- data.frame(idpt = n_ids(pref, sum(npun), mxch),
                     stringsAsFactors = FALSE)

    if (filled(cols)) {
        if (is.numeric(cols)) {
            cols <- nc[cols]
        }
        cc <- intersect(cols, nc) %>%
            setdiff("geometry")
        
        if (is_vacuo(cc)) {
            message("\n... atributos en cols no existen !!!")
        } else {
            if (length(cc) < length(cols)) {
                message("\n... algunos atributos no existen !!!")
            }
            nn <- sf::st_intersects(mp, x) %>% simplify2array
            dd <- x[nn, cc, drop = TRUE] #atributos
            if (length(cc) == 1) {
                dd <- data.frame(x = dd) %>% set_names(cc)
            }
            stopifnot("error copia atributos" = nrow(id) == nrow(dd))
            id <- cbind(id, dd)
        }
    }
    mp <- cbind(id, mp)

    if (filled(ruta)) {
        if (ok_fname(ruta)) {
            shp_save(mp, ruta)
        } else {
            message("\n ruta no válida !!!")
        }
    }

    invisible(mp)
}

#' Réplicas-muestra
#' @description Produce submuestras o réplicas dentro de los grupos de
#'     datos (estratos o repeticiones) de una variable
#' @param x data.frame
#' @param estrato character o numeric: nombre (character) o número de
#'     la columna con los códigos o "id" del estrato
#' @param replicas numeric: número de réplicas
#' @param orden character o integer: nombre (character) de las
#'     columnas que se van a utilizar para ordenar el data.frame de
#'     forma creciente, o el orden (integer) previamente determinado
#'     con la función "order". Por omisión, los datos no se ordenan.
#' @param balance logical: TRUE si tamaño (número de elementos) de las
#'     submuestras debe ser igual dentro de un mismo estrato
#' @param msist logical: TRUE si las submuestras construidas de manera
#'     sistemática
#' @return integer
#' @seealso subgrupos_azar
#' @examples
#' aa <- data.frame(x = 1:12, y = rep(1:2, each = 6))
#' bb <- aa[sample(12),]
#' bb["rp"] <- sub_muestra(bb, 2, 3, "x")
#' @export
sub_muestra <- function(x, estrato = "estrato", replicas = 0L,
                        orden = integer(), balance = TRUE,
                        msist = TRUE) {

    stopifnot(exprs = {
        inherits(x, "data.frame")
        is.integer(orden) || is.character(orden)
        filled_log(balance)
        filled_num(replicas)
        length(replicas) == 1
        replicas > 1
        filled_char(estrato) || filled_num(estrato)
        length(estrato) == 1
    })

    nr <- nrow(x)
    if (balance) {
        stopifnot("num.reg. no es mult. rep." = nr == replicas *
                      (nr %/% replicas))
    }

    ## chk. estrato
    nx <- names(x)
    if (is.character(estrato)) {
        ii <- is.element(nx, estrato)
        estrato <- ifelse(any(ii), which(ii), 0)
    }
    stopifnot("arg. estr. inadmisible" = estrato >= 1 &&
                  estrato <= length(x))
    estrato <- x[[estrato]]

    ## chk. orden
    if (filled_char(orden)) {#no importa ord. x estrato?
        if (all(is.element(orden, nx))) {
                orden <- do.call("order", as.list(unname(x[orden])))
        } else {
            message("\n... faltan cols. orden; continúa sin ordenar")
        }
    }

    if (ordenado <- filled_int(orden)) {
        estrato <- estrato[orden]
    }

    rp <- subgrupos_azar(estrato, replicas, msist)

    if (ordenado) {#deshace orden
        rp[orden] <- rp
    }

    rp
}

