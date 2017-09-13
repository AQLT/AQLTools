#' Graphique d'une série temporelle
#'
#' Fonction qui permet de faire des graphiques de séries temporelles à partir d'un objet de type \code{\link[stats]{ts}}.
#'
#' @param data un objet de type \code{\link[stats]{ts}} contenant une ou plusieurs séries à grapher.
#' @param titre titre du graphique (par défaut pas de titre).
#' @param legende légende du graphique. Par défaut le nom des variables dans l'objet \code{data} en entrée. S'il n'y a qu'une série
#' à tracer alors aucune légende n'est affichée.
#' @param afficheVolatilite booléen indiquant si l'on souhaite afficher la volatilité des séries graphées (c'est-à-dire l'écart-type
#' de leur variation). Par défaut \code{afficheVolatilite = FALSE} : on n'affiche rien.
#' @param cex nombre indiquant le montant par lequel le texte associé à la volatilité doit être mis à l'échelle par rapport à la
#' valeur par défaut. Par défaut \code{cex = 0.6} : le texte est donc 60 \% plus petit que la valeur par défaut.
#' Par exemple, pour des séries trimestrielles cela permet d'avoir un graphique par trimestre traçant chacun les valeurs associées à son trimestre.
#' @return Un graphique \code{\link[ggplot2]{ggplot}}.
#' @encoding UTF-8
#' @examples
#' data <- lectureBDM("001585942","001585980")
#' legende <- c("Carnets de commandes globaux","Carnets de commandes étrangers")
#' titre <- "Soldes d'opinion sur les carnets de commandes dans l'industrie manufacturière"
#' graph_ts(data, titre = titre, legende = legende,afficheVolatilite = TRUE)
#' graph_ts(data, titre = titre, legende = legende, diviserParPeriode = TRUE)
#' @export
graph_ts<-function(data, titre = "", legende = NULL, afficheVolatilite = FALSE,
                   cex = 0.6, diviserParPeriode = FALSE){
    if (!is.ts(data))
        stop("Il faut que la table en entrée soit de type ts !")

    if (!require("ggplot2", quietly = TRUE)){
        install.packages("ggplot2",dependencies = TRUE)
        require("ggplot2", quietly = TRUE)
    }

    if (!requireNamespace("reshape2", quietly = TRUE))
        install.packages("reshape2")

    gg_color_hue <- function(n) {
        hues = seq(15, 375, length = n + 1)
        hcl(h = hues, l = 65, c = 100)[1:n]
    }
    if (is.null(legende)&&is.mts(data)){
        legende<-colnames(data)
    }

    time<-time(data)
    freq<-frequency(data)
    dataGraph<-data.frame(cbind(time,data))
    colnames(dataGraph)<-c("date",paste0("val",seq(ncol(dataGraph)-1)))
    valCouleurs<-gg_color_hue(ncol(dataGraph)-1)
    names(valCouleurs)<-colnames(dataGraph)[-1]

    dataGraph <- reshape2::melt(dataGraph, id="date")  # convert to long format
    if (freq==2){
        periode<-ifelse(time(data)%%1==0,"S1","S2")
        periode<-factor(periode,levels=c("S1","S2"),ordered = T)
    }
    if (freq==4){
        periode<-capitalize(quarters(zoo::as.yearqtr(dataGraph$date)))
        periode<-factor(periode,levels=capitalize(quarters(zoo::as.yearqtr((0:3)/4))),ordered = T)
    }
    if (freq==12){
        periode<-capitalize(months(zoo::as.yearmon(dataGraph$date)))
        periode<-factor(periode,levels=capitalize(months(zoo::as.yearmon((0:11)/12))),ordered = T)
    }

    dataGraph<-data.frame(dataGraph,periode=periode)
    p<-ggplot(data=dataGraph,aes(x=date, y=value,group=variable,colour=variable))+coord_cartesian(xlim=c(min(time)+0.5,max(time)-0.5)) +
        geom_line(size=0.70)+theme_bw()
    #Paramètres graphiques (titre, labels etc.)
    p<-p+labs(title=titre,x="",y="")+
        scale_x_continuous(breaks=scales::pretty_breaks(n=round(length(unique(round(time)))/2)),labels=function(x) substr(x,3,4)) +
        scale_y_continuous(breaks=scales::pretty_breaks(n=12))+
        theme(plot.title=element_text(hjust = 0.5)
        )
    if(is.mts(data)){#Il y a au moins 2 séries et on fait une légende

        p<-p+theme(legend.background = element_rect(fill=alpha('gray99', 0.4),colour="gray80",linetype = "solid"),legend.justification=c(0,0), legend.position=c(0,0),legend.key = element_blank())+
            scale_colour_manual(name=NULL,breaks = names(valCouleurs)
                                ,values = valCouleurs
                                ,labels=legende)
    }else{
        p<-p + theme(legend.position="none")+
            scale_colour_manual(name=NULL,breaks = c("val1")
                                ,values = c("val1"="black"))
    }
    if(afficheVolatilite){
        if(is.mts(data)){#On a au moins deux variables
            volatilite<-round(apply(diff(data,1),2,sd,na.rm=TRUE),1)
        }else{
            volatilite<-round(sd(diff(data,1),na.rm=TRUE),1)
        }

        volatilite<-gsub(".",",",format(volatilite,digits=1,nsmall=1),fixed=T)
        texte <- paste(paste("Volatilité",legende,"=",volatilite),collapse = "\n")
        texte <- grid::grobTree(grid::textGrob(texte, x=0.98,  y=0.94,hjust = 1,
                                 gp=grid::gpar(col="black", cex=cex)))
        p<-p+annotation_custom(texte)
    }
    if(diviserParPeriode){
        p<-p+facet_wrap(~periode)
    }
    p
}

#On le reprend le code de la fonction du package Hmisc
capitalize <- function(string){
    capped <- grep("^[A-Z]", string, invert = TRUE)
    substr(string[capped], 1, 1) <- toupper(substr(string[capped],1, 1))
    return(string)
}

