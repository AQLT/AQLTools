#' Graphique d'une série temporelle
#'
#' Fonction qui permet de faire des graphiques de séries temporelles à partir d'un objet de type \code{\link[stats]{ts}}.
#'
#' @param data un objet de type \code{\link[stats]{ts}} contenant une ou plusieurs séries à grapher.
#' @param titre titre du graphique (par défaut pas de titre).
#' @param sous_titre sous-titre du graphique (par défaut pas de sous-titre).
#' @param legende légende du graphique. Par défaut le nom des variables dans l'objet \code{data} en entrée. S'il n'y a qu'une série
#' à tracer alors aucune légende n'est affichée.
#' @param afficheVolatilite booléen indiquant si l'on souhaite afficher la volatilité des séries graphées (c'est-à-dire l'écart-type
#' de leur variation). Par défaut \code{afficheVolatilite = FALSE} : on n'affiche rien.
#' @param cex nombre indiquant le montant par lequel le texte associé à la volatilité doit être mis à l'échelle par rapport à la
#' valeur par défaut. Par défaut \code{cex = 0.6} : le texte est donc 60 \% plus petit que la valeur par défaut.
#' Par exemple, pour des séries trimestrielles cela permet d'avoir un graphique par trimestre traçant chacun les valeurs associées à son trimestre.
#' @param diviserParPeriode  diviser le graphique en fonction de la période : il y aura en sortie autant de graphiques que de périodes dans la série de départ.
#' @param x_lab  titre axe des abscisses (par défaut aucun titre).
#' @param x_lab_month boolean indiquant si on veut que les x_labels soit formatés avec le moois
#' @param y_lab  titre axe des ordonnées (par défaut "Date").
#' @param outDec séparateur décimal utilisé pour dans la légende des axes (par défaut la virgule).
#' @param n_xlabel nombre de labels pour l'axe des abscisses (par défaut une année sur deux).
#' @param n_ylabel nombre de labels pour l'axe des ordonnées (par défaut 12).
#' @return Un graphique \code{\link[ggplot2]{ggplot}}.
#' @encoding UTF-8
#' @examples
#' data <- lectureBDM("001585942","001585980")
#' legende <- c("Carnets de commandes globaux","Carnets de commandes étrangers")
#' titre <- "Soldes d'opinion sur les carnets de commandes dans l'industrie manufacturière"
#' graph_ts(data, titre = titre, legende = legende, afficheVolatilite = TRUE)
#' graph_ts(data, titre = titre, legende = legende, diviserParPeriode = TRUE)
#' @export
graph_ts <- function(data, titre = NULL, sous_titre = NULL, legende = NULL, afficheVolatilite = FALSE,
                     cex = 0.6, diviserParPeriode = FALSE, x_lab = NULL, x_lab_month = FALSE, y_lab = NULL,
                     outDec = ",",
                     n_xlabel = length(time(data)) %/% 24, n_ylabel = 12){

    if (!is.ts(data))
        stop("Il faut que la table en entrée soit de type ts !")
    if(!require(ggplot2))
        stop("Il faut installer ggplot2")

    time <- time(data)
    freq <- frequency(data)
    dataGraph <- data.frame(cbind(time, data))
    if (is.null(legende)){
        if(is.mts(data)){
            legende <- colnames(data)
        }else{
            legende <- ""
        }
    }
    colnames(dataGraph) <- c("date", legende)

    dataGraph <- reshape2::melt(dataGraph, id="date")  # convert to long format

    if (freq==1){
        periode <- "Y"
        periode <- factor(periode)
    }
    if (freq==2){
        periode <- ifelse(time(data)%%1==0, "S1", "S2")
        periode <- factor(periode,levels = c("S1","S2"), ordered = T)
    }
    if (freq==4){
        periode <- capitalize(quarters(zoo::as.yearqtr(dataGraph$date)))
        periode <- factor(periode,levels=capitalize(quarters(zoo::as.yearqtr((0:3)/4))),ordered = T)
    }
    if (freq==12){
        periode <- capitalize(months(zoo::as.yearmon(dataGraph$date)))
        periode <- factor(periode,levels=capitalize(months(zoo::as.yearmon((0:11)/12))),ordered = T)
    }

    dataGraph <- data.frame(dataGraph,periode=periode)
    p <- ggplot(data = dataGraph, aes(x = date, y = value, group = variable, colour = variable))+
        # coord_cartesian(xlim = c(min(time) + 0.5, max(time) - 0.5)) +
        geom_line(size=0.70)
    #Paramètres graphiques (titre, labels etc.)
    p <- p +
        labs(title = titre, subtitle = sous_titre,
             x = x_lab, y = y_lab) +
        scale_x_continuous(breaks = scales::pretty_breaks(n = n_xlabel),
                           labels = function(x) creation_x_label(x, x_lab_month = x_lab_month, outDec = outDec)) +
        scale_y_continuous(breaks = scales::pretty_breaks(n = n_ylabel),
                           labels = function(x) format(x, decimal.mark = outDec))+
        theme_aqltools()

    if(!is.mts(data))
        p <- p + theme(legend.position="none")

    if(afficheVolatilite){
        if(is.mts(data)){#On a au moins deux variables
            volatilite <- round(apply(diff(data, 1), 2, sd, na.rm=TRUE), 1)
        }else{
            volatilite <- round(sd(diff(data, 1), na.rm=TRUE), 1)
        }

        volatilite <- format(volatilite, digits = 1, nsmall = 1,
                             decimal.mark = outDec)
        texte <- paste(paste("Volatilité",legende,"=",volatilite),collapse = "\n")
        texte <- grid::grobTree(grid::textGrob(texte, x = 0.98, y = 0.94,
                                               hjust = 1,
                                               gp = grid::gpar(col = "black", cex = cex)))
        p <- p + annotation_custom(texte)
    }
    if(diviserParPeriode){
        p <- p + facet_wrap(~periode)
    }
    p
}
#
# graph_ts_simple <- function(data, titre = NULL, sous_titre = NULL, legende = NULL,
#                      cex = 0.6, x_lab = NULL, x_lab_month = FALSE, y_lab = "Date",
#                      outDec = ",",
#                      n_xlabel = length(time(data)) %/% 24, n_ylabel = 12){
#     if(!require(ggplot2))
#         stop("Il faut installer ggplot2")
#     if (!is.ts(data))
#         stop("Il faut que la table en entrée soit de type ts !")
#
#
#     time <- time(data)
#     freq <- frequency(data)
#     dataGraph <- data.frame(cbind(time, data))
#     if (is.null(legende)){
#         if(is.mts(data)){
#             legende <- colnames(data)
#         }else{
#             legende <- ""
#         }
#     }
#     colnames(dataGraph) <- c("date", legende)
#
#     dataGraph <- reshape2::melt(dataGraph, id="date")  # convert to long format
#
#     if (freq==1){
#         periode <- "Y"
#         periode <- factor(periode)
#     }
#     if (freq==2){
#         periode <- ifelse(time(data)%%1==0, "S1", "S2")
#         periode <- factor(periode,levels = c("S1","S2"), ordered = T)
#     }
#     if (freq==4){
#         periode <- capitalize(quarters(zoo::as.yearqtr(dataGraph$date)))
#         periode <- factor(periode,levels=capitalize(quarters(zoo::as.yearqtr((0:3)/4))),ordered = T)
#     }
#     if (freq==12){
#         periode <- capitalize(months(zoo::as.yearmon(dataGraph$date)))
#         periode <- factor(periode,levels=capitalize(months(zoo::as.yearmon((0:11)/12))),ordered = T)
#     }
#
#     dataGraph <- data.frame(dataGraph,periode=periode)
#     p <- ggplot(data = dataGraph, aes(x = date, y = value, group = variable, colour = variable))+
#         coord_cartesian(xlim = c(min(time) + 0.5, max(time) - 0.5)) +
#         geom_line(size=0.70)
#     #Paramètres graphiques (titre, labels etc.)
#     p <- p +
#         labs(title = titre, subtitle = sous_titre,
#              x = x_lab, y = y_lab) +
#         scale_x_continuous(breaks = scales::pretty_breaks(n = n_xlabel),
#                            labels = function(x) creation_x_label(x, x_lab_month = x_lab_month, outDec = outDec)) +
#         scale_y_continuous(breaks = scales::pretty_breaks(n = n_ylabel),
#                            labels = function(x) format(x, decimal.mark = outDec))+
#         theme_aqltools()
#
#     if(!is.mts(data))
#         p <- p + theme(legend.position="none")
#     p
# }
#On le reprend le code de la fonction du package Hmisc
capitalize <- function(string){
    capped <- grep("^[A-Z]", string, invert = TRUE)
    substr(string[capped], 1, 1) <- toupper(substr(string[capped],1, 1))
    return(string)
}

creation_x_label <- function(x, x_lab_month = FALSE, outDec = ","){
    if(x_lab_month){
        zoo::as.yearmon(x)
    }else{
        format(x, decimal.mark = outDec)
    }
}

theme_aqltools <- function (base_size = 11, base_family = "") {
    theme_grey(base_size = base_size, base_family = base_family) %+replace%
        theme(panel.background = element_rect(fill = "white", colour = NA),
              panel.border = element_rect(fill = NA, colour = "grey20"),
              panel.grid.major = element_line(colour = "grey92"),
              panel.grid.minor = element_line(colour = "grey92", size = 0.25),
              strip.background = element_rect(fill = "grey85", colour = "grey20"),
              complete = TRUE,
              plot.title = element_text(hjust = 0.5),
              legend.background = element_rect(fill = alpha('gray99', 0.4),
                                               colour = "gray80", linetype = "solid"),
              legend.justification = c(0,0),
              legend.position = c(0,0),
              legend.key = element_blank(),
              legend.title = element_blank()
              )
}
