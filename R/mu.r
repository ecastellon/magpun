# -*- coding: utf-8 -*-

## funciones muestreo, factores expansión y relacionadas

#' Puntos polígonos
#' @description Selección aleatoria de puntos en polígonos
#' @param x obj. clase "sf": cobertura de los polígonos.
#' @param npun character o numeric: atributo de la cobertura
#'     (character) o vector de enteros (numeric) con el número de
#'     puntos asignados a cada polígono; por omisión, se asume que es
#'     un atributo con nombre "puntos"
#' @param cols columnas ('atributos') en la cobertura de polígonos que
#'     se trasladan a la de los puntos seleccionados. Si no se
#'     especifica (opción por defecto) no se trasladan atributos
#' @param ruta character: nombre del archivo donde guardar el
#'     resultado. Si no se especifica (es la opción por defecto) no se
#'     almacena el resultado.
#' @return objeto de clase "sf" o NULL
#' @export
muestra_puntos <- function(x, npun = "puntos", cols = character(),
                           ruta = character()) {
    stopifnot("arg. x inadmisible" = inherits(x, "sf"),
              "arg. cpun inadmisible" = filled_char(npun) ||
                  filled_num(cpun),
              "arg. cols inadmisible" = is.character(cols),
              "arg. rut inadmisible" = is.character(ruta))

    ## TODO verificar la geometría es multipolígono
    nc <- names(x)
    if (is.character(npun)) {
        stopifnot("arg. cpun no es atributo" = is.element(npun, nc))
        npun <- x[[npun]]
    } else {
        stopifnot("arg. cpun difiere de num. de polig." =
                      length(npun) == nrow(x)
                  "arg. cpun todo cero" = any(npun > 0))
    }

    pt <- st_sample(x, npun)

    if (!is_vacuo(cols)) {
        cols <- intersect(cols, nc) #no incluya la geom de x
        if (is_vacuo(cols)) {
            message("\n... atributos en cols no existen !!!")
        } else {
            nn <- st_intersects(pt, x) %>%
                simplify2array
            tm <- pt
            pt <- x[nn, cols, drop = TRUE] #quita la geometría
            st_geometry(pt) <- tm
        }
    }

    if (nzchar(ruta)) {
        shp_save(pt)
    }

    invisible(pt)
}

#' Replicated points
#' @description crear indicador de réplica (submuestra)
#' @param x data.frame con los datos de la cobertura de puntos
#' @param nrep número de réplicas
#' @param colrep columna con identificación del estrato
#' @param orden columnas que se van a utilizar para ordenar la
#'     asignación de las réplicas
#' @return vector con los números de réplicas asignadas
#' @export
replica <- function(x, nrep = 5,
                    colrep = "estrato", orden = NULL){

    assert_that(is.data.frame(x),
                msg = "no es data frame")
    assert_that(nrow(x) == nrep * (nrow(x) %/% nrep),
                msg = "muestra no es múltiplo de réplicas")

    if(is.null(orden)){#ordena por todas las columnas
        orden <- length(x)
    }
    id <- seq_len(nrow(x))
    nn <- order_df(x[orden])
    ##!!! chk colrep no tenga nombre id
    xx <- cbind(x[nn, colrep, drop = FALSE], id = id[nn])

    zz <- Reduce(rbind,
                 lapply(split(xx, xx[,colrep]),
                        function(x){
                            st <- nrow(x) %/% nrep
                            ni <- sapply(sample(seq_len(nrep), nrep), seq,
                                         by = nrep, length.out = st)
                            dim(ni) <- c(nrow(x), 1)
                            cbind(x[ni,], replica = rep(seq_len(nrep),
                                                        each = st))
                        }))
    mm <- match(id, zz$id)
    invisible(zz$replica[mm])
}
