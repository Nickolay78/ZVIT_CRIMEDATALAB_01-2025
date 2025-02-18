#створення фрейму перевірки після очищення dsa від помилок та отримання dsa_clean
load ("dsa_clean.RData")

check.frame<-subset(dsa_clean,Year==2250)
for (i in 2013:2023)#!!! IMPORTANT END YEAR 2022
{
  Year<-subset(dsa_clean,Year==i)
  check.frame[-2012+i,4]<-i
  for (j in 6:108)
  {check.frame[-2012+i,j]<-sum (Year[,j],na.rm = TRUE)}
}
