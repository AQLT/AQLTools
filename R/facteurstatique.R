#' Analyse factorielle statique
#'
#' Fonction qui permet de calculer un indicateur synthétique par une analyse factorielle statique.
#'
#' @param data un objet de type \code{\link[ts]{mts}} contenant les données.
#' @param date_deb date de début d'estimation.
#' @param date_fin date de fin d'estimation.
#' @param normalise booléen indiquant s'il faut renormaliser les données à une moyenne 100 et écart-type 10.
#' @param retard_contrib nombre de retards à prendre en compte pour le calcul des contribs.
#' @return Un objet de même type que celui en entrée.
#' @encoding UTF-8
#' @examples
#' series_cvs <- c(`Capacité d'épargne actuelle` = "000857195", `Capacité d'épargne future` = "000857198",
#'                 `Évolution future du chômage` = "000857190", `Niveau de vie futur en France` = "000857189",
#'                 `Niveau de vie passé en France` = "000857188",`Opportunité de faire des achats importants`= "000857193",
#'                 `Situation financière future` = "000857197", `Situation financière passée` = "000857196")
#' data <- lectureBDM(series_cvs)
#' facteurStatique(data)
#' @export
facteurStatique=function(data, date_deb, date_fin, normalise = FALSE, retard_contrib = -1){
    if(!is.mts(data))
        stop("Il faut des données de type mts !")

    # Suppression des lignes (=dates) avec des observations NA
    if(missing(date_deb))
        date_deb <- start(data)
    if(missing(date_fin))
        date_fin <- end(data)
    tabletravail = window(data,start = date_deb,
                          end = date_fin)
    manques = (apply(is.na(tabletravail),1,sum) > 0)
    tabletravail = tabletravail[manques == F,]

    # Calcul des loadings
    Fact = factanal(tabletravail,1,scores = "regression")
    loadings = loadings(Fact)

    # Calcul du facteur a partir des loadings
    passage = solve(cor(tabletravail),loadings)
    datanorm = scale(data)
    facteur = datanorm %*% passage
    # Le vecteur passage correspond en fait aux coefficients
    # Justification du calcul : voir aide de la fonction factanal

    # Mise en forme du facteur
    if(normalise){
        sd.facteur = sd(facteur, na.rm = T)
        facteur = scale(facteur) * 10 + 100
    } else {
        facteur = facteur * 10 + 100
    }
    facteur = ts(facteur,start = start(data),freq = 12)

    # Calcul des contribtions de chaque question a la variation du climat des affaires
    variationssoldes = datanorm[(1-retard_contrib) : dim(datanorm)[1],] -
        lag(datanorm,retard_contrib)[1:(dim(datanorm)[1] + retard_contrib),]
    if(normalise) {
        contributions = (10/sd.facteur)*variationssoldes %*% diag(passage[,1])
    } else {
        contributions = 10 * variationssoldes %*% diag(passage[,1])
    }
    contributions = ts(contributions,start = start(data) + c(0, -retard_contrib),freq=12)
    colnames(contributions) = colnames(data)

    coefs = passage

    # Liste des variables de sortie
    list(facteur = facteur,coefs = coefs,contributions=contributions,loadings = loadings,Fact= Fact)
}
