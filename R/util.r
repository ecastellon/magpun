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
#' @description El número está entre los límites?
#' @param x numeric
#' @param x1 numeric: límite inferior
#' @param x2 numeric: límite superior
#' @param inclusive logical: incluyendo igualdad con uno de los
#'     límites?; TRUE por omisión
#' @return logical
#' @export
num_entre <- function(x, x1, x2, inclusive = TRUE) {
    stopifnot("args. inválido" = filled_num(x) &&
                  filled_num(x1) && filled_num(x2) &&
                  length(x) == length(x1) && length(x) == length(x2))
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
    stopifnot("args. inválidos" = filled_int(x) && filled_int(y) &&
              filled_int(desplazar) && desplazar > 0)
    ##desplazar > 0; x e y enteros
    x * 10 ^ desplazar + y
}

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
#' @export
ajustar_sp <- function(x = character()) {
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

#' String-letras
#' @description Devuelve la secuencia de letras (en minúsculas) de una
#'     frase
#' @param x character
#' @return character
#' @keywords internal
sec_letras <- function(x = character()) {
    stopifnot("arg. x inadmisible" = filled_char(x))
    x %<>% gsub("[[:space:]]", "", .) %>%
        tolower() %>% sin_tilde() %>% sin_dieresis()
}
