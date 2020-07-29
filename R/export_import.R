#' Importer des données depuis le presse-papier
#'
#' Fonction qui permet d'importer sous R des données depuis le presse-papiers.
#' Ces données peuvent par exemple venir d'un tableur (Excel, Calc, etc.) :
#' copier les données du tableur et ensuite lancer sous R la fonction \code{ctr_v()}
#'
#' @param header booléen indiquant si la première ligne doit être
#' interprétée comme les noms des colonnes (\code{TRUE} par défaut). Paramètre
#' utilisé uniquement si les données du presse-papier contiennent plusieurs colonnes.
#' @param row.names un vecteur contenant les noms des lignes. Cela peut être un vecteur contenant les noms des lignes,
#' un entier donnant le numéro de la colonne contenant les noms des lignes ou une chaîne de caractères donnant
#' le nom de la colonne qui contient les noms des lignes. Lorsque \code{row.names = NULL} alors les lignes sont
#' numérotées.
#' @param stringsAsFactors booléen indiquant si l'on souhaite que les chaînes de
#' charactères soient transformées en facteur dans la table de sortie (\code{FALSE} par défaut).
#' Paramètre utilisé uniquement si les données du presse-papier contiennent plusieurs colonnes.
#' @param dec le caractère utilisé comme séparateur décimal dans les données du presse-papiers.
#' Par défaut la virgule est utilisée : utiliser le point pour si les données proviennent d'un
#' tableur non francophone. Paramètre utilisé uniquement si les données du presse-papier
#' contiennent plusieurs colonnes.
#' @param ... autres paramètres de \code{\link{read.table}} (uniquement si les données
#' du presse-papier contiennent plusieurs colonnes).
#' @return Un vecteur de caractères s'il n'y a qu'une colonne, sinon un \code{\link{data.frame}}.
#' @seealso \code{\link{ctrl_c}} pour exporter vers le presse-papier.
#' @encoding UTF-8
#' @export
#' @examples
#' # Après avec copié des données depuis un tableur
#' ctrl_v()
ctrl_v <- function(header = TRUE, row.names, stringsAsFactors = FALSE,
                   dec = ",", ...){
  os <- Sys.info()[['sysname']]
  if (os == "Windows") {
    tableau <- utils::readClipboard()
    file <- "clipboard"
  } else if (os == "Darwin") {
    # Pour mac
    file <- base::pipe("pbpaste")
    tableau <- base::readLines(file)
    base::close(file)
    file <- base::pipe("pbpaste")
  }
  if (length(grep("\t",tableau)) > 0) {
    # Il y a plusieurs colonnes
    if (header) {
      en_tete <- utils::read.delim(text = tableau [1], header = FALSE,
                            stringsAsFactors = FALSE)
      if(!missing(row.names) && !is.null(row.names)){
          if (is.character(row.names)) {
              if (length(row.names) == 1L) {
                  rowvar <- (1L:ncol(en_tete))[match(en_tete, row.names, 0L) ==
                                          1L]
                  en_tete <- en_tete[-rowvar]
              }
          }
          else if (is.numeric(row.names) && length(row.names) == 1L) {
              rowvar <- row.names
              en_tete <- en_tete[-rowvar]
          }
      }
    }


    if(!missing(row.names)){
        tableau <- utils::read.delim(file = file, header = header,
                                     stringsAsFactors = stringsAsFactors, dec = dec,
                                     row.names = row.names, ...)
    }else{
        tableau <- utils::read.delim(file = file, header = header,
                                     stringsAsFactors = stringsAsFactors, dec = dec)
    }

    if (header) {
      colnames(tableau) <- en_tete[1,]
    }
  }
  return(tableau)
}

#' Exporter des données vers le presse-papier
#'
#' Fonction qui permet d'exporter des données (un vecteur ou un \code{\link{data.frame}} par exemple)
#' vers le presse-papier (pour ensuite les coller dans un tableur par exemple).
#'
#' Il est possible qu'il y ait une ligne en trop si la fonction est utilisée sous Mac Os.
#'
#' @param data les données à exporter.
#' @param col.names Booléen indiquant si l'on souhaite exporter les noms des colonnes (par défaut
#' \code{col.names = TRUE})
#' @param row.names Booléen indiquant si l'on souhaite exporter les noms des lignes (par défaut
#' \code{row.names = FALSE})
#' @param dec le caractère utilisé comme séparateur décimal dans le tableur où l'on souhaite
#' exporter les données. Par défaut la virgule est utilisée : utiliser le point pour si l'on utilise
#' tableur non francophone.
#' @param memory entier indiquant la taille maximale de l'objet à copier dans le presse-papier. Par défaut
#' la limite est de 32 Kb (=2^5 Kb).
#' @param ... autres paramètres de \code{\link{read.table}} (uniquement si les données
#' du presse-papie contiennent plusieurs colonnes).
#' @encoding UTF-8
#' @export
#' @seealso \code{\link{ctrl_v}} pour importer depuis le presse-papier.
#' @examples
#' ctrl_c(c("line 1", "line 2 \n and line 3"))
ctrl_c <- function(data, col.names = TRUE, row.names = FALSE, memory = 2^5, dec = ",", ...) {
  os <- Sys.info()[['sysname']]

  if (!is.na(col.names) && col.names && row.names){
      col.names <- NA
  }

  if (os == "Windows") {
      utils::write.table(x = data, file = paste0("clipboard-",memory), sep = "\t",
                  col.names = col.names, row.names = row.names, dec = dec, ...)
  } else if (os == "Darwin") {
      # mac os
      clip_w_mac <- base::pipe("pbcopy", "w")
      utils::write.table(x = data, file = clip_w_mac, sep = "\t",
                  col.names = col.names, row.names = row.names, dec = dec, ...)
      base::close(clip_w_mac)
  }
  return(invisible(NULL))
}


