

library(shiny)
library(ggplot2)
library (reshape)

shinyServer(function(input, output) {

selectedmin = reactive({ selectedmin = input$limiti[1]})
selectedmax = reactive({ selectedmax = input$limiti[2]})
selectedinizio =reactive({ selectedinizio = input$Anni[1]})
selectedfine =reactive({ selectedfine = input$Anni[2]})

'importo il dataset'
dati70 = read.csv("Dataset R0BM70 2005-2014.csv", sep = ";")
attach(dati70)

'sostituisco gli spazi vuoti per distinguerli dal cluster "altro"'
dati70$Testo[dati70$Testo==""] = "Vuoto"
TestoChar = as.character(dati70$Testo)
TestoChar[is.na(TestoChar)==TRUE] = "Vuoto"
' determino il vettore dei cluster categoriali'
ClusterVector = c("COLAZION","SPESE VARIE","RINFRESCO","RASSEGNA STAMPA","Vuoto", "BIGLIETTI", "CAFFE","CAFFE","INCISIONI","ABBONAMENTI","NECRO","VARI","PRESEPE","NATALE","BIGLIETTI","XXXXXXX","OMAGGI","POD","RAPPRESENTANZA","NATALIZI","CRA","PANETTONI","CATERING","ACER","CADORNINO","INCIS","APPLE","CORNICE","rappresentanza","auguri","Strenna","colazione","natalizi","Circolo Ricreativo Aziendale","natalizia","funebre","funerale","riunione","Natalizie","panini","bar","RIUNION","STRENNE","strenn","Stren","Natal","Necrologio","natal","Messa","messa","BlackBerry","Cesti","Orologi","Natalizio","Confezioni","Orologi","MAGGI NATALE","CERAMICHE ARTISTICHE","barometri","trenini storici")
' determino il vettore dei nomi dei cluster'
NameVector = c( "Colazioni di Lavoro","Spese Varie","Rinfreschi e Ristorazione","Rassegna Stampa","Vuoto", "Biglietti da Visita", "Rinfreschi e Ristorazione","Rinfreschi e Ristorazione", "Opere d Arte","Biglietti e Abbonamenti Eventi", "Necrologi e Spese Funerarie","Spese Varie", "Spese S. Natale","Spese S. Natale","Biglietti e Abbonamenti Eventi", "Opere d Arte","Omaggistica","Elettronica (Ipod e TV)","ALTRO","Spese S. Natale", "Acquisti da CRA","Spese S. Natale","Rinfreschi e Ristorazione","Elettronica (Ipod e TV)","Rinfreschi e Ristorazione","Opere d Arte","Elettronica (Ipod e TV)","Omaggistica","ALTRO", "Spese S. Natale","Spese S. Natale","Colazioni di Lavoro","Spese S.Natale","Acquisti da CRA","Spese S. Natale","Necrologi e Spese Funerarie","Necrologi e Spese Funerarie","Spese per Riunioni","Spese S. Natale","Rinfreschi e Ristorazione","Rinfreschi e Ristorazione","Spese per Riunioni","Spese S. Natale","Spese S. Natale","Spese S. Natale","Spese S. Natale","Necrologi e Spese Funerarie","Spese S. Natale","Spese S. Natale","Spese S. Natale","Elettronica (Ipod e TV)","Spese S. Natale","Omaggistica","Spese S. Natale","Omaggistica","Omaggistica","Spese S. Natale", "Spese S. Natale","ALTRO","Acquisti da CRA")

functclustering = function(object,searchvect, namevect){0
  num = length(object)
  result = rep(NA,num)
  for(i in 1:length(searchvect)){
    indexes = grep(searchvect[i],object)
    result[indexes] = namevect[i]
    result[is.na(result) == TRUE] = "ALTRO"
  }
  return(result)
}

stringa = functclustering(TestoChar,ClusterVector,NameVector)
'assegno ad ogni registrazione il cluster'
Databasetab70= data.frame(Societa, Soggetto,"Passivo" = Cluster,"Natura"= stringa,"Importo" = Importo.in.divisa.interna,Anno,"Qualifica"=Qualifica, "Assegnazione"=Assegnazione)
Databasetab70tab = subset(Databasetab70,Databasetab70$Societa == "F000" & Databasetab70$Qualifica == "Amministratori")
'creo una variabile di comodo con l unione di natura e nome'
attach(Databasetab70)
identificativo= paste(Natura,Soggetto,sep="|")
Dataframe70 = data.frame(identificativo,Passivo,Importo)
'computo i totali per cluster e per persona'
DataSelection = reactive({
  Datavis70 = data.frame(cast(Dataframe70, identificativo ~ Passivo,sum,value ='Importo'))
  attach(Datavis70)
  ' computo i totali'  
  totale = FI+non.FI  
  ' definisco il rapporto FI su non FI'
  rapporto = round((FI/totale),2) 
  ' sostituisco gli infinito'
  rapporto[is.infinite(rapporto)==TRUE] <- 0  
  'separo lID'
  nomi = t(data.frame((strsplit(as.character(Datavis70$identificativo),"[|]"))))
  Datavis70 = data.frame(cast(Dataframe70, identificativo ~ Passivo,sum,value ='Importo'))  
  attach(Datavis70)
  ' computo i totali'  
  totale = FI+non.FI  
  ' definisco il rapporto FI su non FI'
  rapporto = round((FI/totale),2)  
  ' sostituisco gli infinito'
  rapporto[is.infinite(rapporto)==TRUE] <- 0  
  'separo lID'
  nomi = t(data.frame((strsplit(as.character(Datavis70$identificativo),"[|]"))))
  'rinomino le colonne'
  colnames(nomi) = c("Natura","Soggetto")
  datadef70= data.frame("Natura"= nomi[,1],"Soggetto"= nomi[,2],rapporto,totale)
  datadef70 = datadef70[order(datadef70$Natura),]
  datadef70 = subset(datadef70,datadef70$Soggetto !="non assegnato" )
  
  'modifico la variabile rapporto ai fini della rappresentazione: sostituisco gli 0 con 0,1 e moltiplico tutto per 4'
  datadef70$rapporto[datadef70$rapporto == 0] = 0.1
  datadef70$rapporto = datadef70$rapporto*4
  
  'detach(dati70,Databasetab70,Datavis70)'
  attach(datadef70)
  ' filtro il dataset in base al minimo e massimo selezionati'
  a <- subset(datadef70,datadef70$totale >= selectedmin() & datadef70$totale <= selectedmax() )  
  return(a)
})

'etichette = c(paste(datadef70$Soggetto,format(datadef70$totale, big.mark=".", scientific=FALSE),sep=": € "))'
output$gcompl70 <- renderPlot({  
  ' etichette = c(paste(Soggetto,format(totale, big.mark=".", scientific=FALSE),sep=": E "))'
  g=   ggplot(DataSelection(), aes(x=Natura,y=totale,size=rapporto,colour=factor(Soggetto)))
  g+   
    geom_point(shape=21)+  
    geom_vline(xintercept = 1, colour = gray(0.5),linetype = 2 )+
    geom_vline(xintercept = 2, colour = gray(0.5),linetype = 2 )+
    geom_vline(xintercept = 3, colour = gray(0.5),linetype = 2 )+
    geom_vline(xintercept = 4, colour = gray(0.5),linetype = 2 )+
    geom_vline(xintercept = 5, colour = gray(0.5),linetype = 2 )+
    geom_vline(xintercept = 6, colour = gray(0.5),linetype = 2 )+
    geom_vline(xintercept = 7, colour = gray(0.5),linetype = 2 )+
    geom_vline(xintercept = 8, colour = gray(0.5),linetype = 2 )+
    geom_vline(xintercept = 9, colour = gray(0.5),linetype = 2 )+
    geom_text(size=4,aes(label= (paste(Soggetto,format(totale, big.mark=".", scientific=FALSE),sep=": E "))))+
    scale_size_area(max_size=30,guide="none")+
     
    labs(x = "natura delle spese", y = "totale spese ( unita' di euro)", title = paste("Classificazione e attribuzione Spese di Rappresentanza ( R0BM70) compresi fra ", toString(selectedmin()),"e ", toString(selectedmax()), " euro", ", dal ", toString(selectedinizio()), " al ", toString(selectedfine())), sub = "conto R0BM90, anno 2013, società F000 e F001")+
    theme_bw(   )  
})
})

