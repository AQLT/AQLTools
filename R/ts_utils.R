#' Évolution et glissement annuel
#'
#' Fonctions qui permettent, pour une série temporelle, de calculer l'évolution par rapport à la
#' dernière période (\emph{i.e.} : évolution mensuelle pour des séries mensuelles, évolution trimestrielle
#'  pour des séries trimestrielles, etc. ; avec \code{ev}) ou le glissement annuel
#' (avec \code{ga()}) .
#'
#' @param x un objet de type \code{\link[stats]{ts}} ou \code{\link[xts]{xts}}.
#' @return Un objet de même type que celui en entrée.
#' @name ts_utils
#' @rdname ts_utils
#' @encoding UTF-8
#' @examples
#' x  <-  ts(1:10, frequency = 4, start = c(1959, 2))
#' ev(x)
#' ga(x)
#' @rdname ts_utils
#' @export
ev <- function(x){
    UseMethod("ev", x)
}
#' @export
ev.ts <- function(x){
    (x/stats::lag(x,-1)-1)*100
}
#' @export
ev.xts <- function(x){
    (x/stats::lag(x,1)-1)*100
}
#' @rdname ts_utils
#' @export
ga <- function(x){
    UseMethod("ga", x)
}
#' @export
ga.ts <- function(x){
    (x/stats::lag(x,-frequency(x))-1)*100
}
#' @export
ga.xts <- function(x){
    (x/stats::lag(x,frequency(x))-1)*100
}
