#' Graphiques interactifs d'une série temporelle avec highcharter
#'
#' Fonctions qui permettent de faire des graphiques interactifs de séries temporelles à partir d'un objet de type \code{\link[stats]{ts}}.
#'
#' @param data un objet de type \code{\link[stats]{ts}} contenant une ou plusieurs séries à grapher.
#' @param titre titre du graphique (par défaut pas de titre).
#' @param sous_titre sous-titre du graphique (par défaut pas de sous-titre).
#' @param legende légende du graphique. Par défaut le nom des variables dans l'objet \code{data} en entrée. S'il n'y a qu'une série
#' à tracer alors aucune légende n'est affichée.
#' @param affiche_legende booléen indiquant si l'on affiche la légende.
#' @param x_lab  titre axe des abscisses (par défaut aucun titre).
#' @param y_lab  titre axe des ordonnées (par défaut "Date").
#' @param outDec séparateur décimal utilisé pour dans la légende des axes (par défaut la virgule).
#' @param useHTML Booléen indiquant si l'on veut utiliser du code HTML dans les titres.
#' @param type vecteur contenant le type à utiliser pour les séries (par défaut \code{"lines"}).
#' @param color vecteur contenant les couleurs à utiliser (par défaut on garde les couleurs de highchart).
#' @param digits nombre de chiffres après la virgule.
#' @return Un graphique \code{\link[highcharter]{highcharter}}.
#' @encoding UTF-8
#' @examples
#' data <- lectureBDM("001585942","001585980")
#' legende <- c("Carnets de commandes globaux","Carnets de commandes étrangers")
#' titre <- "Soldes d'opinion sur les carnets de commandes dans l'industrie manufacturière"
#' hc_lines(data, titre = titre, legende = legende)
#' hc_stocks(data, titre = titre, legende = legende)
#' @name hc_ts
#' @rdname hc_ts
#' @export
hc_lines <- function(data, titre = NULL, sous_titre = NULL,
                     legende = NULL,
                     affiche_legende = TRUE,
                     x_lab = NULL, y_lab = NULL,
                     outDec = ",", useHTML = FALSE,
                     digits){

    if (!is.ts(data))
        stop("Il faut que la table en entrée soit de type ts !")
    if(!require(highcharter))
        stop("Il faut installer highcharter")
    time <- time(data)
    freq <- frequency(data)
    dataGraph <- data.frame(cbind(zoo::as.yearmon(time), data))
    if (is.null(legende)){
        if(is.mts(data)){
            legende <- colnames(data)
        }else{
            legende <- ""
        }
    }
    colnames(dataGraph) <- c("date", legende)

    dataGraph <- reshape2::melt(dataGraph, id="date")  # convert to long format

    hcoptslang_nouv <- hcoptslang <- getOption("highcharter.lang")
    hcoptslang_nouv$decimalPoint <- outDec
    options(highcharter.lang = hcoptslang_nouv)
    on.exit(options(highcharter.lang = hcoptslang_nouv))

    hc <- hchart(dataGraph,
                 "line",
                 highcharter::hcaes(x = date, y = value, group = variable)) %>%
        hc_xAxis(title = list(text = x_lab, useHTML = useHTML)) %>%
        hc_yAxis(title = list(text = y_lab, useHTML = useHTML)) %>%
        hc_legend(enabled = affiche_legende)

    if(!is.null(titre)){
        hc <- hc %>%
            hc_title(text = titre,
                     useHTML = useHTML)
    }
    if(!is.null(sous_titre)){
        hc <- hc %>%
            hc_subtitle(text = sous_titre,
                        useHTML = useHTML)
    }
    if(!missing(digits)){
        hc <- hc %>%
            hc_tooltip(pointFormat = sprintf('{series.name}: <b>{point.y:.%if}</b><br/>',
                                             digits))
    }
   hc
}
#' @name hc_ts
#' @rdname hc_ts
#' @export
hc_stocks <- function(data, titre = NULL, sous_titre = NULL,
                     legende = NULL,
                     affiche_legende = TRUE,
                     x_lab = NULL, y_lab = NULL,
                     outDec = ",", useHTML = FALSE,
                     type = NULL, color = NULL,
                     digits){

    if (!is.ts(data))
        stop("Il faut que la table en entrée soit de type ts !")
    if(!require(highcharter))
        stop("Il faut installer highcharter")
    list_ts <- as.list(data)
    if (is.null(legende)){
        if(is.mts(data)){
            legende <- colnames(data)
        }else{
            legende <- ""
        }
    }else{
        legende <- legende[seq_len(length(list_ts))]
    }
    names(list_ts) <- legende

    if(is.null(type)){
        type_list <- lapply(list_ts, function(x) "line")
    }else{
        type_list <- rep(type, length(list_ts))[1:length(list_ts)]
        type_list <- as.list(type_list)
    }
    if(is.null(color)){
        color_list <- lapply(list_ts, function(x) NULL)
    }else{
        color_list <- rep(color, length(list_ts))[1:length(list_ts)]
        color_list <- as.list(color_list)
    }

    hcoptslang_nouv <- hcoptslang <- getOption("highcharter.lang")
    hcoptslang_nouv$decimalPoint <- outDec
    options(highcharter.lang = hcoptslang_nouv)
    on.exit(options(highcharter.lang = hcoptslang_nouv))


    hc <- highchart(type = "stock",
                    hc_opts = list(lang = list(decimalPoint = ",")))  %>%
        hc_xAxis(title = list(text = x_lab, useHTML = useHTML)) %>%
        hc_yAxis(title = list(text = y_lab, useHTML = useHTML))
    for(i in seq_along(list_ts)){
       hc <-  hc %>%
            hc_add_series(data = list_ts[[i]],
                          name = names(list_ts)[[i]],
                          id = paste0("id",i),
                          type = type_list[[i]],
                          color = color_list[[i]])
    }
    if(!is.null(titre)){
        hc <- hc %>%
            hc_title(text = titre,
                     useHTML = useHTML)
    }
    if(!is.null(sous_titre)){
        hc <- hc %>%
            hc_subtitle(text = sous_titre,
                        useHTML = useHTML)
    }

    if(!missing(digits)){
        hc <- hc %>%
            hc_tooltip(pointFormat = sprintf('{series.name}: <b>{point.y:.%if}</b><br/>',
                                             digits))
    }

    hc %>%
        hc_legend(enabled = affiche_legende)
}

