# -*- coding:utf-8 -*-

## private functions

#' length
#' @description vector has length equal zero
#' @param x vector
#' @return logical
#' @keywords internal
is_vacuo <- function(x) {
    length(x) == 0
}

#' length
#' @description vector has length equal zero
#' @param x vector
#' @return logical
#' @keywords internal
is_empty <- function(x) {
    length(x) == 0
}

#' length
#' @description vector has length greater than zero?
#' @param x vector
#' @return logical
#' @author eddy castellón
filled <- function(x) {
    length(x) > 0
}

#' character type
#' @description vector is of character type and has elements?
#' @param x vector
#' @return logical
filled_char <- function(x) {
    is.character(x) && length(x)
}

#' numeric mode
#' @description vector is of numeric mode and has elements?
#' @param x vector
#' @return logical
filled_num <- function(x) {
    is.numeric(x) && length(x)
}

#' integer type
#' @description vector is of integer type and has elements?
#' @param x vector
#' @return logical
filled_int <- function(x) {
    is.integer(x) && length(x)
}

#' Número-entre
#' @description Comprueba si un número está entre los límites de un
#'     intervalo
#' @param x numeric
#' @param x1 numeric: límite inferior
#' @param x2 numeric: límite superior
#' @param inclusive logical: con igualdad a uno de los límites?; FALSE
#'     por omisión
#' @return logical
#' @export
num_entre <- function(x, x1 = numeric(), x2 = numeric(),
                      inclusive = FALSE) {
    stopifnot("arg. x inválido" = filled_num(x),
              "arg. x1 inválido" = filled_num(x1),
              "arg. x2 inválido" = filled_num(x2),
              "args. x, x1 incomp" = length(x) == length(x1),
              "args. x, x2 incomp" = length(x) == length(x2))
    
    tf <- x > x1 & x < x2
    if (inclusive) {
        tf  <- tf | x == x1 | x == x2
    }

    tf
}

#' Concatenar-int
#' @description Concatenar enteros
#' @param x integer: entero al inicio
#' @param y integer: entero al final
#' @param desplaza integer: número de posiciones decimales que es
#'     desplazado el primer número
#' @return integer
#' @export
#' @examples
#' concatenar_int(2, 3) -> 203
#' concatenar_int(2, 3, 3) -> 2003
concatenar_int <- function(x, y, desplazar = 2L) {
    stopifnot(exprs = {
        filled_num(x)
        filled_num(y)
        filled_num(desplazar) && desplazar > 0})

    desplazar <- as.integer(desplazar)
    as.integer(x) * (10 ^ desplazar) + as.integer(y)
}

#' Id's
#' @description Construye "n" palabras a partir de un prefijo seguido
#'     de un número suficiente de dígitos que garantizan que cada
#'     palabra es única
#' @param x character o numeric: prefijo
#' @param n integer: número de palabras
#' @param mxn integer: número máximo de caracteres de la palabra; por
#'     omisión, 5
#' @return character
#' @export
n_ids <- function(x, n = integer(), mxn = 5L) {
    stopifnot("arg. x inadmisible" = filled_num(x) || filled_char(x),
              "arg. n inadmisible" = filled_num(n) && n > 0,
              "arg. mxn inadmisible" = filled_num(mxn))
    
    ss <- as.character(x)
    n1 <- nchar(ss)
    n2 <- nchar(as.character(n))

    nn <- n1 + n2
    if (mxn < nn) {
        message("... número de caracteres ajustado a ", nn)
    } else {
        if (mxn > n1 + n2) {
            n2 <- mxn - n1
        }
    }
    
    fmt <- paste0("%s", "%0", n2, "i")
    sprintf(fmt, ss, seq_len(n))
}

## -- archivos --

#' path maybe
#' @description test string begins with one o more alphanumeric
#'     characters followed by file path separators, or it begins with
#'     "./" or ".\\" followed by one or more alpanumeric characters;
#'     in both cases ending in alphanumeric character.
#' @param x character
#' @return TRUE if x has at least one slash followed by and ended by
#'     an alphanumeric character
#' @examples
#' \dontrun{
#' is_path(".aa/bb")
#' is_path("aa/bb")
#' is_path("aa/bb.")}
is_path <- function(x) {
    filled_char(x) && grepl("^((\\w+[/\\])|(\\.?[/\\]\\w+)).+\\w$", x)
}

#' file name
#' @description check file's name is valid using the function
#' \code{file.create} of base R. If any directory in the path chain
#' doesn't exists, the file's name is invalid.
#' @param x character; the file's name
#' @return logical
#' @author eddy castellón
ok_fname <- function(x = character()) {
    ok <- file.exists(x)
    if (!ok) {
        ok <- file.create(x)
        if (ok) {
            unlink(x)
        }
    }
    return(ok)
}

#' Archivos
#' @description Lista de los archivos cuyo nombre empieza con ciertos
#' caracteres y termina con cierta extensión
#' @param pre character: prefijo de los nombres; por omisión "*"
#' @param ext character: extensión; por omisión, "*"
#' @param rut character: ruta donde buscar; si omitido, la ruta actual
#' @return character o NULL
#' @examples
#' list_ar("ab", rut = "c:/xx") #-> c("abc.r", "abu.doc")
list_ar <- function(pre = character(), ext = character(),
                    rut = getwd()) {
    stopifnot(exprs = {
        is.character(pre)
        is.character(ext)
        is.character(rut)
    })

    if (is_vacuo(pre)) pre <- ""
    if (is_vacuo(ext)){
        ext <- "*"
    } else {
        ext <- sub("^\\.", "", ext)
    }
    
    ss <- paste0("^", pre, ".*\\.", ext, "$")

    list.files(rut, ss)
}

## Character -> Character
## tools::file_path_sans_ext("ABCD.csv")
#' File extension
#' @description remove extension from file's name
#' @param file file's name
sin_ext <- function(file){
    sub("\\..*$", "", file)
}

#' File extension
#' @description remove extension from file's name
#' @param file file's name
rm_ext <- function(file){
    sub("\\..*$", "", file)
}

#' check call
#' @description check arguments in a call
#' @param x arguments
#' @return logical
chk_vector_call <- function(x) TRUE

#' dot argument
#' @description arguments in ... returned as a character or integer
#'     vector
#' @param ... arguments
#' @return \code{NULL}, character or integer vector
#' @author eddy castellón
#' @examples
#' \dontrun{
#' dots_arg(a, b)
#' dots_arg("a", "b")
#' dots_arg(c("a", "b"))
#' dots_arg(1:3)}
dots_arg <- function(...) {
    xp <- eval(substitute(alist(...)))
    nn <- length(xp)

    if (nn > 1L) {
        vapply(xp, as.character, "a")
    } else {
        if (nn == 1L) {
            if (inherits(xp[[1]], "call")) {
                ## !!!
                ## check is c(..) or seq(., .)
                ## log(.) or similar error
                if (chk_vector_call(xp[[1]])) {
                    ex <- eval(xp[[1]])
                    if (is.numeric(ex)) {
                        return(NULL)
                    } else {
                        return(ex)
                    }
                } else {
                    NULL
                }
            } else {
                as.character(xp[[1]])
            }
        } else {
            message("\n!!! with out arguments")
            NULL
        }
    }
}

#' save
#' @description save without errors?
#' @param ... arguments passed to function save
#' @return logical; FALSE if save with errors
#' @author eddy castellón
save_ok <- function(...) {
    !inherits(try(save(...)), "try-error")
}

#' load file
#' @description load a file catching errors
#' @param x character; file's name
#' @param env object environment where objects are loaded;
#' \code{parent.frame} by default
#' @return character; objects' names or NULL
#' @examples
#' \dontrun{
#' try_load("xx.rda")
#' try_load("xx.rda", env = new.env())}
#' @author eddy castellón
try_load <- function(x, env = parent.frame()) {
  if (missing(x) || !filled_char(x)){
      message("... file's name is missing !!!")
      return(NULL)
  }
  
  if (!is.environment(env)) {
    message("... argument is NOT an environment !!!")
    return(NULL)
  }

  tryCatch(load(x, envir = env),
           error = function(e){
               if (file.exists(x)) {
                   message("... read ERROR !!!")
               } else {
                   message("... file doesn't exists !!!")
               }
               return(NULL)},
           warning = function(e){
               if (file.exists(x)) {
                   message("... read ERROR!!!")
               } else {
                   message("... file doesn't exists !!!")
               }
               return(NULL)}
           )
}

## --- string ---

#' Remover espacios
#' @description Remueve todos lo espacios, final de línea o tabulador,
#'     de una frase
#' @param x frase
#' @export
sin_sp <- function(x = character()) {
    stopifnot("arg. x inadmisible" = filled_char(x))
    gsub("[[:space:]]+", "", x)
}

#' Poda espacios
#' @description Elimina los espacios antes y después de una frase
#' @param x frase
#' @export
podar_str <- function(x = character()) {
    stopifnot("arg. x inadmisible" = filled_char(x))
    regmatches(x, regexpr("\\b.*\\b", x, perl = TRUE))
}

## sustituye dos o más caracteres tipo espacio (\n, \t, \s)
## por un espacio y poda extremos
## Character -> Character
## str_bien_formada("ja  ja ja ") -> "ja ja ja"

#' Quitar espacios
#' @description Remplaza dos o más espacios (final de línea o
#'     tabulador) consecutivos por un espacio y poda los extremos
#' @param x frase
#' @examples
#' ajustar_sp("ja   ja ja  ") #-> "ja ja ja"
#' @export
regular_sp <- function(x = character()) {
    stopifnot("arg. x inadmisible" = filled_char(x))
    podar_str(gsub("[[:space:]]+", " ", x))
}

## no requiere stringr

#' Sin tilde
#' @description Sustituye letras con acento por las equivalentes sin
#'     acento
#' @param x palabra con o sin acentos
#' @export
sin_tilde <- function(x = character()) {
    stopifnot("arg. x inadmisible" = filled_char(x))
    vv <- c("á"="a", "é"="e", "í"="i", "ó"="o", "ú"="u",
            "Á"="A", "É"="E", "Í"="I", "Ó"="O", "Ú"="U")
    while (any((mm <- regexpr("[ÁáÉéÍíÓóÚú]", x)) > -1L)){
        regmatches(x, mm) <- vv[regmatches(x, mm)]
    }
    x
}

#' Sin diéresis
#' @description Sustituye letras con diéresis por las equivalentes sin
#'     diéresis
#' @param x palabra con o sin diéresis
#' @export
sin_dieresis <- function(x = character()) {
    stopifnot("arg. x inadmisible" = filled_char(x))
    vv <- c("ü"="u", "Ü"="U")
    while (any((mm <- regexpr("[Üü]", x)) > -1L)){
        regmatches(x, mm) <- vv[regmatches(x, mm)]
    }
    x
}

#' String-estándar
#' @description Devuelve la secuencia de letras minúsculas, sin
#'     acentos o diéresis, de una frase
#' @param x character
#' @return character
#' @keywords internal
#' @examples
#' to_sec_ascii("  éEmn üs") #-> "eemnus"
solo_letras_ascii <- function(x = character()) {
    stopifnot("arg. x inadmisible" = filled_char(x))
    tolower(x) %>% sin_sp() %>%
        sin_tilde() %>% sin_dieresis()
}
