#load ("RESULT/crimes_dsa_ukr.RData")
#load ("RESULT/crimes_ukr.RData")
#в цій частині коду виправляються помилки звітів щодо частин, наприклад ящо існує статття 
#вказана в звіті як така, що має частин і одночасно вказані її частини
#такий рядо з частиною 0 складається з рядком з частиною 1
#перелік встановлених випадків у векторі ArtInds з якого року це починається startY

#це прості помилки
dsa<-subset(dsa,!((dsa$Article=="223")&(dsa$Part==0)))
dsa<-subset(dsa,!((dsa$Article=="232-1")&(dsa$Part==0)))
dsa[(dsa$Article=="363"&dsa$Part=="1"&dsa$Year==2013),3]<-0


ArtInds<-c("297","304","325","419","354","435","436-1","254","360")
startY<- c(2013, 2013, 2013, 2013, 2014, 2015, 2015,    2020,2020)
for (ArtInd in 1:length(ArtInds))
{

  for (YearD in startY[ArtInd]:2023)#!!! IMPORTATNT END YEAR 2022
{

  r297_0<-subset (dsa,(Article==ArtInds[ArtInd]&Part=="0"&Year==YearD))
r297_1<-subset (dsa,(Article==ArtInds[ArtInd]&Part=="1"&Year==YearD))


r_297_combo<-r297_0[1,6:108]+r297_1[1,6:108]
dsa[(dsa$Article==ArtInds[ArtInd]&dsa$Part=="1"&dsa$Year==YearD),6:108]<-r_297_combo
dsa<-subset(dsa,!(Article==ArtInds[ArtInd]&Part=="0"&Year==YearD))

  }
  
}
dsa_clean<-dsa
save (dsa_clean,file = "dsa_clean.RData")
save (dsa, file = "RESULT/crimes_dsa_ukr.RData")

# наступний тест відшукує помилки коли є частина 0 і одночасно більше 0
nullart<-subset (dsa,Part==-1)
for (yr in 2013:2023)
{
nullYr<-subset (dsa, Year==yr)  
nullpart<-nullYr$Article[nullYr$Part==0]
nullart1<-nullYr[(nullYr$Article %in% nullpart)&(nullYr$Part>0),]
nullart<-rbind(nullart1,nullart)
}
