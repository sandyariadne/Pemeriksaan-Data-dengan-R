#Sumber Data:  https://www.japanhoppers.com/id/chubu/furukawa/weather/

#input data
MasSandy=read.delim("clipboard")
MasSandy

#Data yang telah didapatkan oleh praktikkan bernilai negatif maka data tersebut diubah ke dalam positif dengan cara di inverskan
#Invers
n=MasSandy[,2]
data_positif=function(n,c)
{
  x=length(n)
  r=NULL
  for(i in 1:x)
    if(n[i]==0)
    {
      r[i]=1/(n[i]^2+c)
      cat(r[i],"\n")
    }
  else
  {
    r[i]=1/(n[i]^2)
    cat(r[i],"\n")
  }
  r
}
dat=data_positif(n,c)
dat

#Transformasi data invers square
#TRANSFORMASI Invers square
jarque.bera=function(n){
   {
       x=length(n)
       xbar=sum(n)/x
 	  m1=sum((n-xbar)^2)/x
 	  m2=sum((n-xbar)^3)/x
 	  m3=sum((n-xbar)^4)/x
 	  b1=(m2/m1^(3/2))
 	  b2=(m2/m1^2)
 	  statistic.jb=x*(b1^2)/6+x*(b2-3)^2/24
 	  pvalue=1-pchisq(statistic.jb,df=2)
   cat("==================UJI KENORMALAN JARQUE-BERRA===================\n")
   cat("i.   Uji Hipotesis\n")
   cat("     Ho = Data berdistribusi normla\n")
   cat("     H1 = Data tidak berdistribusi normal\n")
   cat("ii.  Tingkat Signifikansi (alpha=0.05)\n")
   cat("iii. Statistik Uji=",statistic.jb,"\n")
   cat("     p-value =",pvalue,"\n")
   cat("iv.  Daerah Kritis:\n")
   cat("     Ho ditolak jika pvalue<alpha\n")
   cat("v.   Kesimpulan\n")
 }
 qqnorm(n)
 qqline(n)
 if (pvalue<0.05)
 {
     cat("     karena nilai pvalue < 0.05, maka Ho ditolak \n")
 cat("     sehingga data tidak berdistribusi normal\n")
 cat("     hal ini terlihat pada qq plot yang tidak berupa garis lurus\n")
 cat("#Lakukan Transformasi\n")
 cat("================================================================\n")
 }
 else
 {
 cat("     karena nilai pvalue > 0.05, maka Ho gagal ditolak \n")
 cat("     sehingga data berdistribusi normal\n")
 cat("     hal ini terlihat pada qq plot yang berupa garis lurus\n")
 cat("================================================================\n")
 }
 }
jarque.bera(MasSandy$Change)
windows()

#Setelah itu dilakukan transformasi box-cox
#Melakukantransformasi Box-Cox
library(car)
powerTransform(dat)

#Uji Normalitas dan membuat plot
#Membuat plot
#Uji Normalitas dg qqnorm, qqline, qqplot
library(tseries) 
jarque.bera.test(dat^-0.03573524)
jarque.bera.test(((dat^-0.03573524)-1)/-0.03573524)
plot(dat^-0.03573524)
windows()
qqnorm(dat^-0.03573524)
qqline(dat^-0.03573524)
