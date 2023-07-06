
setwd("C:/Users/ABRAHA/Desktop/IPO PROJECT")
IPO=read.csv("IPO.csv")
head(IPO)

o1=lm(Listing_Close~Sectors+QIB+Total+HNI+RII+Issue_Size+Issue+Listing_Gains
      ,data=IPO)


summary(o1)


po1=o1$fitted.values
head(po1)



library(Metrics)

rmse(IPO$Listing_Close,po1)

mae(IPO$Listing_Close,po1)

mape(IPO$Listing_Close,po1)



o2=lm(Listing_Gains~Sectors+Total++HNI+RII+QIB+Issue+Issue_Size+Listing_Open
      ,data=IPO)
summary(o2)



o3=glm(difference~Sectors+QIB+Total+RII+HNI+Issue+Issue_Size+Listing_Open+Listing_Gains,
       data=IPO,family=binomial(link="logit"))

summary(o3)




head(o3$fitted.values)
head(data.frame(o3$fitted.values))


fitted=c(1,1,0,1,0,1,0,0,1,1,1,1,1,0,0,0,0,0,1,0,0,1,0,0,0,0,0,0,1,1,0,0,0,0,0,
         0,1,0,1,0,0,0,1,0,1,1,0,1,1,1,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,0,0,1,
         1,1,1,0,1,0,1,0,0,1,1,0,0,1,1,0,0,0,1,1,0,0,0,0,0,0,0,0,1,0,1,0,0,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,1,0,0,0,0,0,1,1,1,0,1,1,0,0,0,
         0,1,0,0,0,0,0,0,1,0,1,0,0,0,0,1,1,0,0,0,0,0,0,0,1,0,1,1,1,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,1,0,0,1,
         0,0,0,1,0,0,1,1,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,
         0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,1,0,1,0,0,1,0,0)



matrix(c(44,28,69,121),nrow=2,ncol=2,byrow=TRUE)


onull=glm(difference~1,data=IPO,family=binomial(link="logit"))
pseudoR2=1-logLik(o3)/logLik(onull)
pseudoR2



boxplot(IPO$Listing_Open~IPO$Sectors,data=IPO,xlim=c(0,16),ylab="Listing Close"
        ,xlab="Sectors",col=7,border=1)


matcor=IPO[,c(4,5,6,7,8,9)]
corrplot(cor(matcor),method="shade",type="lower",
         tl.col=1,tl.srt=45,tl.cex=0.8,number.cex = 0.7,number.font=2)


library(marplot)

s2=glm(difference~Listing_Gains,data=IPO,family=binomial(link="logit"))
plot(IPO$Listing_Gains,s2$fitted.values,xlab="Listing Gains",ylab="difference")

s3=glm(difference~QIB,data=IPO,family=binomial(link="logit"))
plot(IPO$QIB,s3$fitted.values,xlab="QIB",ylab="difference")

s4=glm(difference~HNI,data=IPO,family=binomial(link="logit"))
plot(IPO$HNI,s4$fitted.values,xlab="HNI",ylab="difference")

s5=glm(difference~RII,data=IPO,family=binomial(link="logit"))
plot(IPO$RII,s5$fitted.values,xlab="RII",ylab="difference")






