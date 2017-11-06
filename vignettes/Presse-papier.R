## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, size = "small")

## ----echo=FALSE----------------------------------------------------------
require(AQLTools,quietly = T)
require(DT,quietly = T)
options(OutDec= ",",
        DT.options = list(dom = 'Bt',
                          buttons = list(list(
                                             extend = 'excel',
                                             text = 'T�l�charger tableau'
                                         )),
                          language=list(decimal=",")
        )
)
data<-matrix(c("Indicateur synth�tique","100","106","108","109","108","Production pass�e","5","9","17","14","10","Stocks","13","7","6","5","6","Carnets de commandes globaux","-18","-9","-6","-5","-3","Carnets de commandes �trangers","-14","-5","-1","-2","-2","Perspectives personnelles de production","5","11","12","16","13","Perspectives g�n�rales de production","-9","3","1","7","17"),
          nrow=7,byrow = T)
data<-data.frame(data,stringsAsFactors = F)
data[,-1]<-apply(data[,-1],2,as.numeric)
colnames(data)<-c("Industrie manufacturi�re","Moy*","Mars-17","Avr-17","Mai-17","Juin-17")
knitr::kable(data, caption = "",align=c("l",rep("c",5)))

## ----eval=FALSE----------------------------------------------------------
#  x <- ctrl_v()
#  x

## ----echo=F--------------------------------------------------------------
# colnames(data)<-gsub("\\W","\\.",colnames(data))
data

## ----eval=F--------------------------------------------------------------
#  ctrl_c(x)
