Package qui rassemble des fonctions diverses et variées écrites durant ces dernières années, parfois utiles, parfois non.

Installation
------------

Pour l'installer rien de plus simple ! Après avoir installé le package devtools (`install.packages("devtools")`) il suffit de lancer le code suivant :

    devtools::install_github("AQLT/AQLTools")
    library(AQLTools)

Configuration du proxy (parfois nécessaire pour installation)
---------------

En fonction de l'ordinateur utilisé (mais surtout du réseau internet utilisé) il peut être nécessaire de configurer le proxy. Pour cela trois solutions :

1. Modifier le *.Renviron* de RStudio. Pour cela il suffit de lancer sous RStudio la commande `file.edit("~/.Renviron")` et de rajouter les deux lignes suivantes dans le fichier ainsi ouvert :
 
        http_proxy = adresseduproxy
        https_proxy = adresseduproxy
 
    Sauvegarder le fichier et le fermer. Cette solution a l'avantage qu'une fois le fichier *.Renviron* modifié il n'y aura plus besoin de configurer le proxy pour les prochains téléchargements.

2. Avec le package [httr](https://CRAN.R-project.org/package=httr). Avant de lancer le téléchargement du package lancer la commande `httr::set_config(use_proxy(url="adresseduproxy", port=8080))`. Il faudra relancer cette commande à chaque fois que la session de R est fermée.

3. Avec le package [RCurl](https://CRAN.R-project.org/package=RCurl). Avant de lancer le téléchargement du package, lancer la commande suivante :

        options(RCurlOptions = list(
            proxy         = "adresseproxy",
            proxyport     = 8080
        ))
    
    Il faudra relancer cette commande à chaque fois que la session de R est fermée.

