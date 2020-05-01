#' Évolution et glissement annuel
#'
#' Fonctions qui permettent, pour une série temporelle, de calculer l'évolution par rapport à la
#' dernière période (\emph{i.e.} : évolution mensuelle pour des séries mensuelles, évolution trimestrielle
#'  pour des séries trimestrielles, etc. ; avec \code{ev}) ou le glissement annuel
#' (avec \code{ga()}) .
#'
#' @param x un objet de type \code{\link[stats]{ts}} ou \code{\link[xts]{xts}}.
#' @return Un objet de même type que celui en entrée.
#' @encoding UTF-8
#' @examples
#' x  <-  ts(1:10, frequency = 4, start = c(1959, 2))
#' ev(x)
#' ga(x)
#' @name ts_utils
#' @rdname ts_utils
#' @export
ev <- function(x){
    UseMethod("ev", x)
}
#' @export
ev.ts <- function(x){
    result <- (x/stats::lag(x, k = -1) - 1) * 100
    return(result)
}
#' @export
ev.mts <- function(x){
    result <- (x/stats::lag(x, k = -1) - 1) * 100
    colnames(result) <- colnames(x)
    return(result)
}
#' @export
ev.xts <- function(x){
    result <- (x / stats::lag(x, k = 1) - 1) * 100
    return(result)
}
#' @rdname ts_utils
#' @export
ga <- function(x){
    UseMethod("ga", x)
}
#' @export
ga.ts <- function(x){
    result <- (x/stats::lag(x, k = -stats::frequency(x)) -1) * 100
    return(result)
}
#' @export
ga.mts <- function(x){
    result <- (x/stats::lag(x, k = -stats::frequency(x)) -1) * 100
    colnames(result) <- colnames(x)
    return(result)
}
#' @export
ga.xts <- function(x){
    result <- (x/stats::lag(x, k = stats::frequency(x)) -1) * 100
    return(result)
}
#' Volatilité
#'
#' Fonction qui permet de calculer la volatité d'une série temporelle (i.e. : l'écart-type de sa différence). Les
#' valeurs manquantes ne sont pas prises en compte.
#'
#' @param x un objet de type \code{\link[stats]{ts}} ou \code{\link[xts]{xts}}.
#' @param lag nombre de retards à utiliser pour le calcul de la différence (par défaut \code{lag = 1} : on calcule une
#' différence mensuelle pour les séries mensuelles).
#' @return Un objet de même type que celui en entrée.
#' @encoding UTF-8
#' @examples
#' x  <-  ts(1:10, frequency = 4, start = c(1959, 2))
#' volatilite(x)
#' @name volatilite
#' @export
volatilite <- function(x, lag = 1){
    UseMethod("volatilite", x)
}
#' @export
volatilite.ts <- function(x, lag = 1){
    result <- sd(base::diff(x, lag), na.rm = TRUE)
    return(result)
}
#' @export
volatilite.mts <- function(x, lag = 1){
    result <- base::diff(x, lag)
    result <- apply(result,2,sd, na.rm = TRUE)
    colnames(result) <- colnames(x)
    return(result)
}
#' @export
volatilite.xts <- function(x, lag = 1){
    result <- base::diff(x, lag)
    result <- apply(result,2,sd, na.rm = TRUE)
    colnames(result) <- colnames(x)
    return(result)
}

#' Convertisseur tableau en ts
#'
#' Fonctions qui permettent de convertir des tableaux en \code{\link[stats]{ts}}.
#'
#' @param x table contenant au moins deux colonnes dont une avec des dates.
#' @param col_date numéro ou nom de la colonne contenant les dates (par défaut c'est la première colonne).
#' @param sep_date caractère utilisé pour séparé le jour, le mois et l'année. Détection automatique par défaut.
#' @param frequence fréquence de l'objet \code{\link[stats]{ts}}. Par défaut la fonction fait une recherche automatique
#' (il peut y avoir des problèmes si l'on n'a pas d'année entière connue).
#'
#' @details Les fonctions \code{ymd_ts()} et \code{dmy_ts()} permettent de convertir des tableaux (par exemple
#' des \code{\link[base]{data.frame}}) en \code{\link[stats]{ts}}. Pour cela il faut dans le tableau d'entrée
#' une colonne contenant les dates qui est au format YYYY-MM-JJ (fonction \code{ymd_ts()}) ou au format JJ-MM-YYYY
#' (fonction \code{dmy_ts()}). Le séparateur utilisé entre le jour, le mois et l'année peut être modifié par le
#' paramètre \code{sep_date}.
#' @return Un \code{\link[stats]{ts}}.
#' @encoding UTF-8
#' @examples
#' x <- data.frame(dates=paste(rep(2000:2002, each=4),
#'                             rep(c("01", "04", "07", "10"),2) ,"01", sep = "-"),
#'                 donnees = 1:12, stringsAsFactors = FALSE)
#' ymd_ts(x)
#' @name data_to_ts
#' @rdname data_to_ts
#' @export
ymd_ts <- function(x, col_date = 1, sep_date, frequence = NULL){
    if (is.character(col_date)){
        col_date <- which(names(x)==col_date)
    }
    if(missing(sep_date)){
        sep_date <- "[^0-9.]"
    }
    dates <- x[,col_date]
    dates <- gsub(sep_date, "", dates)

    if ( is.null(frequence) ){#On cherche la fréquence
        #On regarde le nombre maximal d'occurence de chaque année
        frequence <- max(table(substr(dates,1,4)))
    }

    if(nchar(dates[1])>4){
        prem_date <- as.numeric(c(substr(dates[1],1,4),substr(dates[1],5,6)))
        prem_date <- prem_date[1] + (prem_date[2]-1)/frequence
    }else{
        # On n'identifie que l'année
        prem_date <- as.numeric(substr(dates[1],1,4))
    }


    data_hors_dates <- x[,-col_date]
    return(ts(data_hors_dates, start = prem_date, frequency = frequence))
}
#' @rdname data_to_ts
#' @export
dmy_ts <- function(x, col_date = 1,sep_date, frequence = NULL){
    if (is.character(col_date)){
        col_date <- which(names(x)==col_date)
    }
    if(missing(sep_date)){
        sep_date <- "[^0-9.]"
    }
    dates <- x[,col_date]
    dates <- gsub(sep_date, "", dates)

    if ( is.null(frequence) ){#On cherche la fréquence
        #On regarde le nombre maximal d'occurence de chaque année
        frequence <- max(table(substr(dates,5,8)))
    }

    if(nchar(dates[1])>4){
        prem_date <- as.numeric(c(substr(dates[1],5,8),substr(dates[1],3,4)))
        prem_date <- prem_date[1] + (prem_date[2]-1)/frequence
    }else{
        # On n'identifie que l'année
        prem_date <- as.numeric(substr(dates[1],1,4))
    }
    data_hors_dates <- x[,-col_date]
    return(ts(data_hors_dates, start = prem_date, frequency = frequence))
}


