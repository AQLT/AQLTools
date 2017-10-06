#' Téléchargement d'une série de la BDM Insee
#'
#' Fonction qui permet d'importer des séries de la Banque de données macro-économiques (BDM) de l'Insee à partir de son idbank.
#' L'idbank d'une série peut être trouvé sur le site de la bdm (\url{https://www.bdm.insee.fr/bdm2/index}).
#' permet également l'utilisation de régressions quantiles.
#'
#' @param idbank un vecteur d'un ou plusieurs idbank à télécharger.
#' @param ... d'autres vecteurs d'un ou plusieurs idbank à télécharger.
#' @details La fonction permet de télécharger des séries temporelles présentes la Banque de données macro-économiques (BDM) de l'Insee
#' à partir de son idbank. Elle utilise pour cela le service web SDMX de l'Insee (voir \url{http://www.bdm.insee.fr/bdm2/statique?page=sdmx}
#' pour plus d'informations). Elle utilise le package \pkg{rsdmx} pour la lecture des données et \pkg{reshape2}
#' pour la mise en forme. Les idbank en paramètre doivent correspondre à des séries qui ont la même périodicité.
#' Si un des idbank en entrée n'est pas trouvé alors la fonction renvoit également un message d'avertissement.
#'
#'
#' @return Retourne un objet de type \code{ts} ou \code{mts}, en fonction du nombre d'idbank en entrée,
#' de la même fréquence que les séries en entrée.
#'
#' @export
#' @encoding UTF-8
#' @examples
#'
#' # Pour télécharger l'IPI manufacturier CVS : 001654241
#' lectureBDM("001654241")
#' # Pour télécharger les IPI dans l'industrie manufacturière et dans l'agroalimentaire :
#' lectureBDM(c("001654241","001654289"))
#' # ou bien :
#' lectureBDM("001654241","001654289")
#'
lectureBDM<-function(idbank, ...)
{
    #On récupère les idbank et on supprime les éventuels espaces
    idbank<-gsub(" ","",c(idbank,unlist(list(...))))

    #Les url pour télécharger le(s) série(s)
    UrlData <- paste0("https://bdm.insee.fr/series/sdmx/data/SERIES_BDM/",paste(idbank,collapse = "+"))

    tryCatch({
        dataBDM <- as.data.frame(rsdmx::readSDMX(UrlData,isURL = T))
    },error=function(e){
        stop(paste0("Il y a une erreur dans le téléchargement des données. Vérifier le lien\n",UrlData),
             call. = FALSE)
    })

    FREQ <- levels(dataBDM$FREQ)

    if (length(FREQ)!=1)
        stop("Les séries ne sont pas de la même périodicité !")

    freq<-switch(FREQ
                 ,M=12
                 ,B=6
                 ,T=4
                 ,S=2
                 ,A=1)
    #On détermine le format de la colonne qui contient les dates en fonction de la fréquence
    sepDate<-switch(FREQ
                    ,M="-"
                    ,B="-B"
                    ,T="-Q"
                    ,S="-S"
                    ,A=" ")
    dataBDM <- reshape2::dcast(dataBDM,"TIME_PERIOD ~ IDBANK",value.var = "OBS_VALUE")
    dataBDM <- dataBDM[order(dataBDM$TIME_PERIOD),]

    #On récupère la première date
    dateDeb <- dataBDM$TIME_PERIOD[1]
    dateDeb <- regmatches(dateDeb,gregexpr(sepDate,dateDeb),invert=T)[[1]]
    dateDeb <- as.numeric(dateDeb)

    #On supprime la colonne des dates et on convertit les séries en numérique
    dataBDM$TIME_PERIOD <- NULL
    dataBDM <- apply(dataBDM,2,as.numeric)

    if(ncol(dataBDM) != length(idbank))
        warning(paste("Le ou les idbank suivant n'existent pas :",
                      paste(grep(paste(colnames(dataBDM),collapse="|"),idbank,value=T,invert = T),
                            collapse=", ")))
    if(ncol(dataBDM) > 1){
        # On a au moins 2 colonnes : on replace les colonnes dans le même ordre que les séries en entrée
        idbank <- idbank[idbank %in% colnames(dataBDM)] #On ne garde que les idbank présents dans la base
        dataBDM <- dataBDM[,idbank]
    }
    dataBDM <- ts(dataBDM,start=dateDeb,freq=freq)
    return(dataBDM)
}
