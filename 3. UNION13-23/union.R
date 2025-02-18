library (tidyverse)
library (xlsx)

load("Origin/crimes_dsa_ukr.RData")
load ("Origin/crimes_ukr.RData")
load ("Origin/f2_crimes_CHPT.RData")
f2_add<-f2_crimes
f2_add$Chapter<-as.numeric(as.roman(f2_add$Chapter))
f2_add<-select(f2_add,Year,Chapter,Art,7:ncol(f2_add))
f2_add<-f2_add %>% rename ("Article" = "Art")


common<-left_join(crimes,f2_add, by=c("Year","Chapter","Article"))


f2_CHPT<-f2_CHPT %>% rename ("ChapterR"="Chapter")



f2_CHPT<-select(f2_CHPT,1,3,7:ncol(f2_CHPT))
commonCHPT<-left_join(crimesCHPT,f2_CHPT, by=c("Year","ChapterR"))

chapter_label <- c("I","II","III","IV","V","VI","VII","VIII","IX","X",
                   "XI","XII","XIII","XIV","XV","XVI","XVII","XVIII","XIX","XX","КК 1960")

crimesCHPR<-commonCHPT
for (yr in 2013:2023)
  {for (ind in 4:373)
  {
    basevar<-sum(commonCHPT[commonCHPT$Year == yr,ind],na.rm = TRUE)
    crimesCHPR[crimesCHPR$Year == yr,ind]<-round(commonCHPT[commonCHPT$Year == yr,ind]/basevar*100,2)}
}

crimesCHPT<-commonCHPT
pcommon<-common
common<-select(common,c(1:132,134:376,"ChapterR"))
crimesPR<-common
crimes<-common
for (yr in 2013:2023)
  for (chpt in chapter_label)
    for (ind in 6:375)
    {
      basevar<-sum(crimes[crimes$Year == yr&crimes$ChapterR==chpt, ind],na.rm = TRUE)
      crimesPR[crimesPR$Year == yr&crimesPR$ChapterR==chpt, ind]<-round(crimes[crimes$Year==yr&crimes$ChapterR==chpt, ind]/basevar*100,2)}

save(crimes,crimesCHPR,crimesCHPT,crimesPR,file="Result/crimes_ukr.RData")
load ("field_rule.RData")
crimes_l<-crimes
library (labelled)
var_labels <- setNames(as.list(n_field$nfield), names(crimes_l))
crimes_l <- crimes_l %>%
  set_variable_labels(.labels = var_labels, .strict = FALSE)
save (crimes_l,file = "Result/crimes_l.RData")


