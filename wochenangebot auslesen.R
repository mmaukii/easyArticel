require(dplyr)
require (pdftools)
require(openxlsx)

pdfFile<-"Wochenangebot_KW06.pdf"

text<- pdf_data(pdfFile)
text1<-as.data.frame(text[1])
for (i in 2:length(text)){
  dftemp<-as.data.frame(text[i])
  dftemp$y<-dftemp$y+(i*10000)#zum richtigen ordnen
  if(!i%%2)dftemp$x<-dftemp$x+22
  text1<-rbind(text1,dftemp)
}
text1<-text1[order(text1$y),]

df <- data.frame(Bestellnummer=integer(),
                 Name=character(),
                 Produzent=character(),
                 Herkunft=character(),
                 Nettopreis=double(),
                 Gebindegroeße=double(),
                 Einheit=character(),
                 Kategorie=character(),
  stringsAsFactors=FALSE)
iZeile<-0
iSpalte<-2
Kategorie<-NA

for (i in 1:nrow(text1)){
 #print (text1$text[i] )
  #Kategorie auslesen
  if (grepl("[0-9.]{3}",text1$text[i])&& text1$height[i]== 26 && 
      grepl("[a-z]",text1$text[i+1]) && text1$height[i+1]== 26){
    #print (text1$x[i+1])
    Kategorie<-text1$text[i+1]
  }
  
  #Artikelnummer übernehmen
  if(grepl("[0-9]{6}",text1$text[i])){
    #print (text1$text[i] )
    #Artikelnummer in die erste spalte
    iZeile<-iZeile+1
    df[iZeile,1]<- as.integer(text1$text[i])
    if (!is.na(Kategorie))df$Kategorie[iZeile]<-Kategorie
    #print(df)
  }

  
  #Artikelbezeichnung übernehmen
   if (text1$x[i]>72 && text1$x[i]<205 && text1$height[i]== 15){#Bezeichnungen nach Artikelnummer schreiben
     #print (text1$x[i])
     df$Name[iZeile]<-paste(df$Name[iZeile],text1$text[i],sep=" ")
     df$Name[iZeile]<-gsub("NA ","",df$Name[iZeile])
     df$Name[iZeile]<-gsub("- ","",df$Name[iZeile])
   }
  #Hersteller übernehmen
  if (text1$x[i]>199 && text1$x[i]<230 && text1$height[i]== 6){#Bezeichnungen nach Artikelnummer schreiben
    #print (text1$x[i])
    df$Produzent[iZeile]<-paste(df$Produzent[iZeile],text1$text[i],sep=" ")
    df$Produzent[iZeile]<-gsub("NA ","",df$Produzent[iZeile])
  }
  #Herkunft übernehmen
  if (text1$x[i]>448 && text1$x[i]<490 && text1$height[i]== 6){#Bezeichnungen nach Artikelnummer schreiben
    #print (text1$x[i])
    df$Herkunft[iZeile]<-paste(df$Herkunft[iZeile],text1$text[i],sep=" ")
    df$Herkunft[iZeile]<-gsub("NA ","",df$Herkunft[iZeile])
  }
  #Nettopreis übernehmen
  if (text1$x[i]>350 && text1$x[i]<380&& text1$height[i]== 15){#Bezeichnungen nach Artikelnummer schreiben
    #print (text1$x[i])
    df$Nettopreis[iZeile]<-paste(df$Nettopreis[iZeile],text1$text[i],sep=" ")
    df$Nettopreis[iZeile]<-gsub("NA ","",df$Nettopreis[iZeile])
    df$Nettopreis[iZeile]<-gsub(" ","",df$Nettopreis[iZeile])
    df$Nettopreis[iZeile]<-gsub(",",".",df$Nettopreis[iZeile])
  }
  #Gebindegroeße übernehmen
  if (text1$x[i]>315 && text1$x[i]<325){#Bezeichnungen nach Artikelnummer schreiben
    #print (text1$x[i])
    df$Gebindegroeße[iZeile]<-paste(df$Gebindegroeße[iZeile],text1$text[i],sep=" ")
    #print(text1$text[i])
    df$Gebindegroeße[iZeile]<-gsub("NA ","",df$Gebindegroeße[iZeile])
    df$Gebindegroeße[iZeile]<-gsub(" ","",df$Gebindegroeße[iZeile])
    df$Gebindegroeße[iZeile]<-gsub(",",".",df$Gebindegroeße[iZeile])
  }
  #Einheit xx noch abklären ob so richtig
  if (text1$x[i]>390 && text1$x[i]<400){#Bezeichnungen nach Artikelnummer schreiben
    #print (text1$x[i])
    df$Einheit[iZeile]<-paste(df$Einheit[iZeile],text1$text[i],sep=" ")
    df$Einheit[iZeile]<-gsub("NA ","",df$Einheit[iZeile])
    df$Einheit[iZeile]<-gsub("C/","1 ",df$Einheit[iZeile])
    df$Einheit[iZeile]<-gsub(" A","",df$Einheit[iZeile])
  }
}
#konvert to numm
df$Gebindegroeße<-as.double(df$Gebindegroeße)
df$Nettopreis<-as.double(df$Nettopreis)

#nur österreichische Herkunft
df<-df[df$Herkunft %in% c("Österreich","Waldviertel"),]

#entfernen zeilen mit NA an einer stelle
df<-df[complete.cases(df), ]

#Artikelnummer ans Ende Statt der Menge und Einheit
df$Name<-paste0(gsub("[0-9].*$", "", df$Name),df$Bestellnummer)
df$Einheit<-gsub("Ta.","Tasse",df$Einheit)

dfalt<-df

#Umstellen von 1 kg auf 500 g pro für bestimmte Produkte
df1kgto500g<-c("Chinakohl","Zuckerhut","Porree","Kohl","Sellerie","Karotten","Kohlrüben",
                "Süsskarto", "Karotten", "Pastinaken","Rote Rüben","Schwarzwurzel",
               "Topinambur","Topinampur", "Peterwurzeln")
for (i in 1:(nrow(df)-1)){
  #wenn nachfolgender Artikel kleiner Artikel, dann nicht, sonst in dfkurz
  if (sum(sapply(df1kgto500g,grepl, df$Name[i])) &&
  grepl("1 kg",df$Einheit[i])){
    print(df$Name[i])
    df$Einheit[i]<-"500 g"
    df$Nettopreis[i]<-df$Nettopreis[i]/2
    df$Gebindegroeße[i]<-df$Gebindegroeße[i]*2
  }
}

#Umstellen von 1 kg auf 250 g pro für bestimmte Produkte
df1kgto500g<-c("Kren")
for (i in 1:(nrow(df)-1)){
  #wenn nachfolgender Artikel kleiner Artikel, dann nicht, sonst in dfkurz
  if (sum(sapply(df1kgto500g,grepl, df$Name[i])) &&
      grepl("1 kg",df$Einheit[i])){
    print(df$Name[i])
    df$Einheit[i]<-"250 g"
    df$Nettopreis[i]<-df$Nettopreis[i]/4
    df$Gebindegroeße[i]<-df$Gebindegroeße[i]*4
  }
}


df$Nettopreis<-ceiling(df$Nettopreis/0.05)*0.05


df$Status<-NA
df$Notiz<-NA
df$MwSt<-10
df$Pfand<-NA
df$geschu1<-NA
df$geschu2<-NA

df<-df[,c("Status","Bestellnummer","Name","Notiz","Produzent",
          "Herkunft","Einheit","Nettopreis","MwSt","Pfand","Gebindegroeße",
          "geschu1","geschu2","Kategorie")]



dfAlle<-df
dfKurz<-df[0,]
#nur Artikel mit dem kleinsten Wert
for (i in 1:(nrow(df)-1)){
  #wenn nachfolgender Artikel kleiner Artikel, dann nicht, sonst in dfkurz
  if (gsub("([A-Za-z]+).*", "\\1", df[i,"Name"]) == gsub("([A-Za-z]+).*", "\\1", df[i+1,"Name"]) &&#erstes Wort vergleichen
      df$Bestellnummer[i]==df$Bestellnummer[i+1]-1 &&
      df$Produzent[i+1]==df$Produzent[i] ){ 
    #nichts
  }else{
    dfKurz[nrow(dfKurz)+1,]<-df[i,]
  }
}  
  

con<-file(gsub(".pdf","kurz.csv",pdfFile),encoding="UTF-8")
write.csv2(dfKurz,con, row.names = FALSE, na="")

con<-file(gsub(".pdf","alle.csv",pdfFile),encoding="UTF-8")
write.csv2(dfAlle,con, row.names = FALSE, na="")
