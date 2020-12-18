## -*- coding: utf-8 -*-

## TODO: agregar otros datos pertinentes de los departamentos y
## municipios, tales como nombre de la cabecera departamental
## (municipal) y sus coordenadas, superficie, y otra de interés.

#' Departamento-municipio
#' @description Conectar con la base de datos de
#'     departamentos-municipios
#' @details Los nombres y códigos de los departamentos (dpt,
#'     departamento) y municipios (mun, municipio) están almacenados
#'     en una base de datos SQLite. Los códigos son enteros que
#'     corresponden a la nomenclatura oficial; los códigos de
#'     municipio formados por la concatenación del código del
#'     departamento el código del municipio.
#' @param x character: ruta de la base de datos; si se omite, se lee
#'     de la variable de ambiente DBDEPMUN
#' @return objeto SQLiteConnection o NULL
#' @examples con_dm("c:/path-dir/depmun.sqlite")
#' @keywords internal
#' @author eddy castellón
con_dm <- function(x = character()) {
    stopifnot("arg. x inadmisible" = is.character(x))

    if (is_vacuo(x)) {
        x <- Sys.getenv("DBDEPMUN")
    }
    
    cn <- try(DBI::dbConnect(RSQLite::SQLite(), x),
              silent = TRUE)
    if (inherits(cn, "try-error")) {
        warning("\n... base de datos ", x, "no existe !!!",
                call. = FALSE)
        cn <- NULL
    }
    
    invisible(cn)
}

#' Base datos
#' @description Consulta la base de datos de municipios
#' @param qry character: expresión de consulta SQL
#' @param dbf character: ruta de acceso a la base de datos; por
#'     omisión, la toma de la variable-ambiente DBDEPMUN
#' @return data.frame o NULL
#' @export
qry_dm <- function(qry = character(), dbf = character()) {
    stopifnot("arg. qry inadmisible" = filled_char(qry),
              "admite una consulta" = length(qry) == 1)
    
    db <- con_dm(dbf)
    if (is.null(db)) {
        x <- NULL
    } else {
        ## qry es SQL legítima?
        x <- DBI::dbGetQuery(db, qry)
        DBI::dbDisconnect(db)
    }
    invisible(x)
}

#' Municipios
#' @description devuelve data.frame con los códigos y nombres de los
#'     municipios y los departamentos a los que pertenecen
#' @param dbf character: ruta de la base de datos; si se omite, se
#'     obtiene de la variable-ambiente DBDEPMUN
#' @return data.frame o NULL
#'
#' @export
#' @author eddy castellón
municipios <- function(dbf = character()) {
    stopifnot("arg. dbf inadmisible" = is.character(dbf))
    cc <- paste("select a.mun, a.municipio, b.departamento",
                "from municipio a, departamento b",
                "where a.dpt = b.dpt",
                "order by a.mun")

    x <- qry_dm(dbf, cc)

    invisible(x)
}

#' Municipios
#' @description Municipios de un departamento
#' @param x código (integer) o nombre (character) del departamento
#' @param dbf ruta de acceso a la base de datos de municipios; por
#'     omisión, se toma de la variable-ambiente DBDEPMUN
#' @return data.frame o NULL
#' @export
municipios_dpt <- function(x, dbf = character()) {
    stopifnot("arg. x inadmisible" = filled_int(x) || filled_char(x),
              "arg. dbf inadmisible" = is.character(dbf))

    qr <- paste("select a.mun, a.municipio, b.dpt, b.departamento",
                "from municipio a, departamento b"
                "where a.mun = b.dpt")
    dm <- qry_dm(dbf, qr)

    rs <- NULL
    if (!is.null(dm)) {
        if (is.character(x)) {# estandariza para comparar
            dp <- dm$departamento %>% tolower() %>% sin_tilde()
            x %<>% ajustar_sp() %>% tolower() %>% sin_tilde()
            ii <- is.element(dp, x)
        } else {
            ii <- is.element(dm$dpt, x)
        }
        
        if (any(ii)) {
            rs <- dm[ii, c("mun", "municipio")]
        } else {
        }
    }

    rs
}

#' Departamentos
#' @description devuelve data.frame con los códigos y nombres de los
#'     departamentos
#' @param file character: ruta de la base de datos; si se omite, se
#'     obtiene de la variable-ambiente DBDEPMUN
#' @return data.frame o NULL
#'
#' @export
#' @author eddy castellón
departamentos <- function(file = character()) {
    cc <- paste("select dpt, departamento",
                "from departamento",
                "order by departamento")

    x <- qry_dm(file, cc)

    invisible(x)
}

#' Validar-Dpto-Muni
#' @description El código o nombre del departamento o municipio está
#'     en la base de datos?
#' @param x código (integer) o nombre (character)
#' @param dfm data.frame con el código (integer) y el nombre
#'     (character) del departamento o municipio
#' @param ccod character: nombre de la columna del data.frame con los
#'     códigos del departamento o municipio; por omisión "mun"
#' @param cnom character: nombre de la columna del data.frame con los
#'     nombres de los departamentos o municipios; por omisión
#'     "municipio"
#' @return logical
#' @export
es_dm <- function(x, dfm, ccod = "mun", cnom = "municipio") {
    stopifnot("arg. x inválido" = filled_int(x) || filled_char(x),
              "arg. dfm inválido" = is.data.frame(dfm),
              "arg. dfm inválido" = is.integer(dfm[[ccod]]),
              "arg. dfm inválido" = is.character(dfm[[cnom]]))

    if (is.integer(x)) {
        mm <- dfm[[ccod]]
    } else {
        mm <- tolower(dfm[[cnom]]) %>% sin_tilde()
        x %<>% ajustar_sp() %>%
            tolower() %>%
            sin_tilde()
    }
    is.element(x, mm)
}

#' Municipio-válido
#' @description Comprueba si el código o nombre del municipio está en
#'     la base de datos de municipios
#' @param x integer o character: código (integer) o nombre del
#'     municipio (character)
#' @param dbf character: ruta de la base de datos de los municipios;
#' por omisión, tomado de la variable-ambiente DBDEPMUN
#' @return logical
#' @seealso municipios
#' @export
es_municipio <- function(x, dbf = character()) {
    stopifnot("arg. x inadmisible" = filled_int(x) || filled_char(x),
              "arg. dbf inadmisible" = is.character(dbf))

    es_dm(x, municipios(dbf))
}

#' Departamento-válido
#' @description Comprueba si el código o nombre del departamento está
#'     en la base de datos de departamentos
#' @param x integer o character: código (integer) o nombre del
#'     departamento (character)
#' @param dbf character: ruta de la base de datos de los
#'     departamentos; por omisión, se toma de la variable-ambiente
#'     DBDEPMUN
#' @seealso departamentos
#' @export
#'
es_departamento <- function(x, dbf = character()) {
    stopifnot("arg. x inadmisible" = filled_int(x) || filled_char(x),
              "arg. dbf inadmisible" = is.character(dbf))

    es_dm(x, departamentos(dbf))
}

#' Municipio-código
#' @description Genera el código "extendido" de un municipio,
#'     concatenando los códigos de departamento y municipio
#' @details Se espera que los parámtros dp y mu tengan el mismo número
#'     de elementos; pero si difieren, el que tenga menos es
#'     "replicado" tantas veces como sea necesario.
#' @param dp integer: código del departamento
#' @param mu integer: código del municipio
#' @param dbf character: ruta de la base de datos de municipios
#' @return integer
#' @seealso municipios
#' @examples
#' cod_dm(5, 5) #-> 505
#' cod_dm(5, c(5, 20)) #-> c(505, 520)
#' @export
#' @author eddy castellón
cod_dm <- function(dp = integer(), mu = integer(),
                   dbf = character()) {
    stopifnot(exprs = {
        filled_int(dp)
        filled_int(mu)
        vapply(dp, num_entre, TRUE, 0, 100, inclusive = FALSE)
        vapply(mu, num_entre, TRUE, 0, 100, inclusive = FALSE)})

    nd <- length(dp)
    nm <- length(mu)
    if (nd > nm) {
        mu <- repeat(mu, length.out = nd)
    } else {
        if (nd < nm) {
            dp <- repeat(dp, length.out = nm)
        }
    }
    mm <- Map(concatenar_int, dp, mu)

    ii <- !es_municipio(mm, dbf)

    if (any(ii)) {
        warning("\n...", sum(ii), " códigos inválidos")
        mm[ii] <- NA_integer_
    }
    
    mm
}

## -- quitar; se puede obtener de df_muni
#' Códigos oficiales
#' @description la lista de códigos oficiales
#' @param file nombre del archivo; si no es indicado lo toma de la
#'     variable de ambiente DEPMUN definida en .Renviron
#' @param dfmun nombre del data.frame con los datos de los municipios;
#'     supone que la primera columna lleva los códigos
#' @seealso df_muni
#' @export
#' @author eddy castellón
cod_municipios <- function(file = character(), dfmun = "dmuni"){
    x <- df_muni(file, dfmun)
    as.integer(x[,1])
}


#' Nombres
#' @description nombre del departamento o municipio
#' @param cod códigos de los departamentos o municipios
#' @param x data.frame con los datos de municipios o departamentos
#' @author eddy castellón
nombre_cod <- function(cod = integer(), x){

    ii <- ok_int_chr(cod)
    assert_that(ii || ok_num(cod),
                msg = "códigos no válidos")
    if (ii){
        cod <- as.integer(cod)
    }

    dm <- vector("character", length(cod))
    mm <- match(cod, x[,1])
    ii <- is.na(mm)
    dm[!ii] <- x[mm[!ii], 2]
    if (any(ii)){
        dm[ii] <- NA_character_
        warning("!! NA códigos no válidos\n")
    }
    dm
}

#' Municipios
#' @description nombres oficiales de municipios
#' @param cod códigos de municipios (entero)
#' @param file nombre del archivo; si no es indicado lo toma de la
#'     variable de ambiente DEPMUN definida en .Renviron
#' @param dfmun nombre del data.frame con los datos de los municipios;
#'     supone que la segunda columna lleva los nombres
#' @return lista de nombres de municipios indicados en \code{cod}, o la
#'     de todos si no se indican los códigos
#' @seealso df_muni
#' @export
#' @author eddy castellón
municipios <- function(cod, file = character(), dfmun = "dmuni"){
    
    x <- df_muni(file, dfmun)
    if (missing(cod)){
        dm <- as.character(x[,2])
    } else {
        dm <- nombre_cod(cod, x)
    }
    dm
}

#' municipios - departamento
#' @description lista de nombres de municipios en un departamento
#' @param dpto código (entero) del departamento
#' @param nombre TRUE devuelve nombres (por omisión), FALSE devuelve
#'     códigos
#' @param file nombre del archivo con los códigos y nombres de
#'     municipios; si no es indicado lo toma de la variable de
#'     ambiente DEPMUN definida en .Renviron
#' @param dfmun nombre del data.frame con los datos de los municipios;
#'     por omisión 'dmuni'
#' @return lista de nombres o códigos de los municipios de un
#'     departamento
#' @seealso municipios, df_muni, file_depmun
#' @export
#' @importFrom assertthat assert_that
#' @author eddy castellón
municipios_dp <- function(dpto = numeric(), nombre = TRUE,
                          file = character(), dfmun = "dmuni"){
    assert_that(ok_num(dpto) && length(dpto) == 1,
                msg = "sólo un código de departamento\n")
    
    x <- df_muni(file, dfmun)
    
    cc <- as.integer(x[,1])
    ii <- (cc %/% 100L) == dpto
    if (any(ii)){
        if (nombre){
            nm <- x[ii, 2]
        } else {
            nm <- cc[ii]
        }
    } else {
        message("código de departamento no válido\n")
        nm <- NA_character_
    }
    nm
}

#' Codigo municipio
#' @description código compuesto de municipio
#' @param mu código de municipio (entero)
#' @param dp código departamento (entero)
#' @return vector de enteros con el código compuesto de municipio
#' @author eddy castellón
cod_muni_ <- function(mu, dp){
    
    nm <- length(mu)
    nd <- length(dp)
    if (nm != nd){
        warning("!! vectores de códigos con desigual número\n")
    }

    x <- as.integer(dp * 100L + mu)
    ii <- !ok_muni(x)
    if (any(ii)){
        x[ii] <- NA_integer_
        warning("NA código no válido\n")
    }
    invisible(x)
}

#' Codigo municipio
#' @description genera el código compuesto de municipio a partir del
#'     código de departamento y código de municipio dentro de
#'     departamento
#' @param mu código de municipio
#' @param dp código departamento
#' @return código tipo entero, NA si no es municipio válido
#' @examples cod_muni(5, 5) -> 505
#' @export
#' @importFrom assertthat assert_that
#' @author eddy castellón
cod_muni <- function(mu = integer(), dp = integer()){
    ii <- ok_int_chr(mu)
    jj <- ok_int_chr(dp)
    assert_that(ii || ok_num(mu), jj || ok_num(dp),
                msg = "!!! códigos no válidos\n")
    
    if (ii){
        mu <- as.integer(mu)
    }
    if (jj){
        dp <- as.integer(dp)
    }
    cod_muni_(mu, dp)
}

#' Código de municipio
#' @description función no vectorizada que devuelve código de
#'     municipio a partir de su nombre
#' @param nom nombre del municipio
#' @param dfmun data.frame con códigos-nombres de municipio oficiales;
#'     la primera columna lleva el código, la segunda el nombre
#' @return código de municipio si es válido; si no, NA
#' @author eddy castellón
cod_muni_nom <- function(nom = character(), dfmun){
    if (grepl("[[:alpha:]]+", nom)){#al menos una letra
        nom <- str_sin_tilde(tolower(str_bien_formada(nom)))
        mm <- pmatch(nom, tolower(as.character(dfmun[,2])),
                     duplicates.ok = FALSE)
        cod <- ifelse(is.na(mm), NA_integer_, dfmun[mm, 1])
    } else {#solo digitos
        cod <- NA_integer_
    }
    cod
}

#' Codigo municipio
#' @description devuelve código de municipio a partir de su nombre.
#' @param nom nombre del municipio
#' @param file nombre del archivo con los códigos y nombres de
#'     municipios; si no es indicado lo toma de la variable de
#'     ambiente DEPMUN definida en .Renviron
#' @param dfmun nombre del data.frame con los datos de los municipios;
#'     por omisión 'dmuni'
#' @return código de municipio si es válido; si no, NA
#' @seealso df_muni()
#' @export
#' @importFrom assertthat assert_that
#' @author eddy castellón
cod_muni_nombre <- function(nom = character(), file = character(),
                            dfmun = "dmuni"){
    assert_that(ok_chr(nom),
                msg = "código tipo caracter")
    x <- df_muni(file, dfmun)

    cod <- vapply(nom, cod_muni_nom, 0, x, USE.NAMES = FALSE)
    if (anyNA(cod)){
        warning("hay nombres no válidos\n")
    }
    cod
}

#' código departamento - municipio
#' @description devuelve el código de departamento que corresponde a
#'     nombre de municipio
#' @param nom nombre del municipio
#' @param file nombre del archivo con los códigos y nombres de
#'     municipios; si no es indicado lo toma de la variable de
#'     ambiente DEPMUN definida en .Renviron
#' @param dfmun nombre del data.frame con los datos de los municipios;
#'     por omisión 'dmuni'
#' @export
#' @author eddy castellón
cod_dpto_muni <- function(nom = character(), file = character(),
                          dfmun = "dmuni"){
    cm <- cod_muni_nombre(nom, file, dfmun)
    ii <- !is.na(cm)
    cm[ii] <- cm[ii] %/% 100L
    cm
}

#' nombre departamento - municipio
#' @description devuelve nombre del departamento al que pertenece el
#'     municipio
#' @param nom nombre del municipio
#' @param file nombre del archivo con los códigos y nombres de
#'     municipios; si no es indicado lo toma de la variable de
#'     ambiente DEPMUN definida en .Renviron
#' @param dfmun nombre del data.frame con los datos de los municipios;
#'     por omisión 'dmuni'
#' @param dfdpt nombre del data.frame con los datos de los
#'     departamentos; por omisión 'ddpto'
#' @author eddy castellón
nom_dpto_muni <- function(nom, file = character(),
                          dfmun = "dmuni", dfdpt = "ddpto"){
    x <- cod_dpto_muni(nom, file, dfmun)
    ii <- !is.na(x)
    nd <- vector("character", length(x))
    nd[ii] <- departamentos(x[ii], file, dfdpt)
    if (any(!ii)){
        nd[!ii] <- NA_character_
    }
    nd
}

#' Municipio válido
#' @description valida código de municipio
#' @param cod código extendido (departamento-municipio), caracter o
#'     numero
#' @param file nombre del archivo con data.frame del municipio; si no
#'     es indicado, se lee de la variable de ambiente DEPMUN definida
#'     en .Renviron
#' @param dfmun nombre del data.frame con los datos de los municipios;
#'     por omisión 'dmuni'
#' @return vector lógico con igual número de elementos que cod
#' @export
#' @importFrom assertthat assert_that
#' @author eddy castellón
ok_muni <- function(cod = integer(), file = character(),
                    dfmun = "dmuni"){
    ii <- ok_int_chr(cod)
    assert_that(ii || ok_num(cod), msg = "código tipo entero")
    if (ii){
        cod <- as.integer(cod)
    }
    cod %in% cod_municipios(file, dfmun)
}

## --- departamentos ---

#' Departamentos
#' @description códigos y nombres de los departamentos
#' @param file character: nombre del archivo con data.frame
#'     departamentos; si se omite, intenta obtener el nombre de la
#'     variable de ambiente DEPMUN definida en .Renviron
#' @param dfdpt character: nombre del data.frame con los datos de los
#'     departamentos; por omisión 'ddpto'
#' @return data.frame; primera columna el código oficial, y la segunda
#'     el nombre
#' @export
#' @author eddy castellón
df_dpto <- function(file = character(), dfdpt = "ddpto"){
    
    assert_that(length(file) <= 1,
                msg = "sólo nombre del archivo\n")
    fdm <- file_depmun(file)
    invisible(get_dff_c(dfdpt, fdm))
}

#' Codigos departamentos
#' @description lista de códigos de departamento
#' @param file nombre del archivo con data.frame departamentos; si no
#'     es indicado se lee de la variable de ambiente DEPMUN definida
#'     en .Renviron
#' @param dfdpt nombre del data.frame con los datos de los
#'     departamentos; por omisión 'ddpto'
#' @export
#' @author eddy castellón
cod_departamentos <- function(file, dfdpt = "ddpto"){
    x <- df_dpto(file, dfdpt)
    as.integer(x[,1])
}

#' Departamentos
#' @description lista de departamentos correspondientes a los códigos
#'     o lista completa si estos no se indican
#' @param file nombre del archivo con data.frame departamentos; si no
#'     es indicado se lee de la variable de ambiente DEPMUN definida
#'     en .Renviron
#' @param dfdpt nombre del data.frame con los datos de los
#'     departamentos; por omisión 'ddpto'
#' @return lista de nombres de departamentos indicados en \code{cod},
#'     o la de todos si no se indican los códigos
#' @seealso df_dpto
#' @export
#' @author eddy castellón
departamentos <- function(cod, file = character(),
                         dfdpt = "ddpto"){
    x <- df_dpto(file, dfdpt)

    if (missing(cod)){
        dm <- as.character(x[,2])
    } else {
        dm <- nombre_cod(cod, x)
    }
    dm
}

#' código departamento
#' @description devuelve código del departamento que corresponde al
#'     nombre del departamento
#' @param nom nombre del departamento
#' @param file nombre del archivo con los data.frame
#' @param dfdpt nombre del data.frame con datos de departamentos; por
#'     omisión 'ddpto'
#' @author eddy castellón
cod_dpto_nom <- function(nom, dfdpt = "ddpto"){

    if (grepl("[[:alpha:]]+", nom)){#al menos una letra
        nom <- str_sin_tilde(tolower(str_bien_formada(nom)))
        
        mm <- pmatch(nom, tolower(dfdpt[,2]),
                     duplicates.ok = FALSE)
        if (is.na(mm)){
            cod <- NA_integer_
        } else {
            cod <- dfdpt[mm, 1]
        }
    } else {#solo digitos
        cod <- NA_integer_
    }
    cod
}

#' Código departamento
#' @description devuelve código de departamento a partir del nombre
#'     del departamento
#' @param nom nombre del municipio o nombre del departamento
#' @param file nombre del archivo con data.frame del municipio; si no
#'     es indicado, se lee de la variable de ambiente DEPMUN definida
#'     en .Renviron
#' @param dfdpt nombre del data.frame con los datos de los
#'     departamentos; por omisión 'ddpto'
#' @return código de departamento si es código válido; si no, NA
#' @export
#' @importFrom assertthat assert_that
#' @author eddy castellón
cod_dpto_nombre <- function(nom = character(), file = character(),
                            dfdpt = "ddpto"){

    assert_that(ok_chr(nom),
                msg = "código tipo caracter o entero")

    dd <- df_dpto(file, dfdpt)
    
    cod <- vapply(nom, cod_dpto_nom, 0, dd, USE.NAMES = FALSE)
    if (anyNA(cod)){
        warning("hay nombres no válidos\n")
    }
    cod
}

#' Departamento válido
#' @description valida código de departamento
#' @param cod código de departamento
#' @param file donde están los códigos oficiales. Si se omite,
#'     lo lee desde .Renviron
#' @export
#' @importFrom assertthat assert_that
#' @author eddy castellón
ok_dpto <- function(cod = integer(), file = character(),
                    dfdpt = "ddpto"){
    ii <- ok_int_chr(cod)
    assert_that(ii || ok_num(cod), msg = "código tipo entero")

    if (ii){
        cod <- as.integer(cod)
    }
    cod %in% cod_departamentos(file, dfdpt)
}
