#library(tsm)

library(vars)
library(mFilter)
library(tseries)
library(robustHD)
library(tidyverse)
library(quantmod)
library(urca)
library(zoo)
library(dplyr)
library(lmtest)


data<-read.csv("C:/Users/32442/OneDrive/바탕 화면/논문/진짜수정.csv",header=1)
data
#data<-data[,-c(0,1)]
#colnames(data)
colnames(data)<-c("변환","기관투자자",
                  "개인","외국인","기타법인","환율","kospi")




#x11()
#plot(x=data$국채,y=data$특수채)


kospi<-ts(data$kospi,
       start=c(2010,2),
       end=c(2020,12),
       frequency=12)

환율<-ts(data$환율,
       start=c(2010,2),
       end=c(2020,12),
       frequency=12)

기관투자자<-ts(data$기관투자자,
       start=c(2010,2),
       end=c(2020,12),
       frequency=12)

개인<-ts(data$개인,
       start=c(2010,2),
       end=c(2020,12),
       frequency=12)

외국인<-ts(data$외국인,
       start=c(2010,2),
       end=c(2020,12),
       frequency=12)



기타법인<-ts(data$기타법인,
       start=c(2010,2),
       end=c(2020,12),
       frequency=12)


#외생변수  선행연구 참고 
x11()

#hist(data[,"국채"]) #이상치 존재
#hist(data[,"특수채"]) #가우시안 분포에 따르지 않음,이상치 존재
#hist(data[,"회사채"])
#hist(data[,"기관투자자"])
#hist(data[,"개인"])
#hist(data[,"외국인투자등록번호"])# 가우시안 분포를 따르지않음
                                 #이상치 존재 
#hist(data[,"기타법인"]) #가우시안 분포를따르지않음
                        #이상치 존재
#hist(data[,"kospi"]) #가우시안 분포를 따르지않음
                     #이상치 존재


x11()
plot(cbind(국채,특수채,회사채,기관투자자,개인,외국인투자등록번호,
             기타법인,kospi))

colors<-c("green","red","pink","blue","yellow",
          "lightsalmon","black","gray","cyan","purple","maroon","pink")
x11()
matplot(matrix(data$기관투자자,nrow=12,ncol=12),
        type="l",col=colors,lty=1,lwd=2.5,xaxt="n")
legend("topleft",legend=2010:2020,lty=1,col=colors)
axis(1,at=1:12,labels = c("jan","feb",
                          "mar","apr","may","jun",
                          "jul","aug","sep","oct",
                          "nov","dec"))










x11()
mons<-c("jan","feb",
        "mar","apr","may","jun",
        "jul","aug","sep","oct",
        "nov","dec")

matplot(t(matrix(data$kospi, nrow = 11, ncol =12 )),
        type='l',col=colors,lty=1,lwd=2.5)
legend("bottomleft",legend=mons,col=colors,lty=1)

#20년은 개인 투자자들의 주식매입량이 다른년도 에 비해 확실한 증가추세를 보여줌
#10년 과 20년은 전체적인 월로 봤을떄 성장하는 kospi 지수를 보여줌
class(months)


x11()
name<-단기금리
par(mfrow=c(1,2))
acf(name,lag.max=24)
pacf(name,lag.max=24)

print(단기금리)
#기관투자자>> 지난월에 영향
#개인 >> 지속적인 투자 유치로 인해 pacf 값과 acf 값이 정확히 보이지않음
#외국인 투자등록번호 >> 애매
#기타법인>> 애매
#kospi>> 비정상시계열



#국채<-rollapply(zoo(국채),3,function(w) mean(w),align = "right",
#              partial = TRUE)

#특수채<-rollapply(zoo(특수채),3,function(w) mean(w),align = "right",
#               partial = TRUE)
#회사채<-rollapply(zoo(회사채),3,function(w) mean(w),align = "right",
#               partial = TRUE)
#기관투자자<-rollapply(zoo(기관투자자),4,function(w) mean(w),align = "center",
#                 partial = TRUE)
#개인<-rollapply(zoo(개인),4,function(w) mean(w),align = "center",
#              partial = TRUE)
#외국인투자등록번호<-rollapply(zoo(외국인투자등록번호),4,function(w) mean(w),align = "center",
#                     partial = TRUE)
#기타법인<-rollapply(zoo(기타법인),4,function(w) mean(w),align = "center",
#                partial = TRUE)
#kospi<-rollapply(zoo(kospi),3,function(w) mean(w),align = "right",
#                 partial = TRUE)




summary(ur.df(개인,type="none"))
PP.test(개인)

summary(ur.df(기관투자자,type="none"))

summary(ur.df(기타법인,type="none"))


summary(ur.df(외국인,type="none"))
summary(ur.df(환율,type="none"))
summary(ur.df(kospi,type="trend"))


PP.test(개인)
PP.test(기관투자자)
PP.test(기타법인)
PP.test(기타외국인)
PP.test(외국인)
PP.test(단기금리)
PP.test(환율)
PP.test(kospi)





df<-cbind(외국인,개인,기관투자자,기타법인,kospi,단기금리,환율)

#linear<-lm(기타법인~외국인+개인+기관투자자+kospi+단기금리+환율)
#summary(linear)
#summary(ur.df(linear$residuals))

#linear1<-lm(외국인~기타법인+개인+기관투자자+kospi+단기금리+환율)
#summary(linear1)
#summary(ur.df(linear$residuals))

linear2<-lm(개인~외국인+기타법인+기관투자자)
summary(linear2)
summary(ur.df(linear$residuals))

Trend<-seq_along(개인)
Trend
linear3<-lm(개인~외국인+기타법인+기관투자자+0)
summary(linear3)
summary(ur.df(linear3$residuals))

#linear4<-lm(개인~Trend+기타법인+기관투자자+외국인)
#summary(linear4)
#summary(ur.df(linear4$residuals))
#linear3<-lm(기관투자자~외국인+개인+기타법인+kospi+단기금리+환율)
#summary(linear3)
#summary(ur.df(linear$residuals))




















x11()
#hist(국채) #이상치 존재
#hist(특수채) #가우시안 분포에 따르지 않음,이상치 존재
#hist(회사채)
hist(기관투자자)
hist(개인)
hist(외국인투자등록번호)# 가우시안 분포를 따르지않음
#이상치 존재 
hist(기타법인) #가우시안 분포를따르지않음
#이상치 존재
hist(kospi) #가우시안 분포를 따르지않음
#이상치 존재





class(exn)
exn<-cbind(외국인,개인,기관투자자,기타법인)
colnames(exn)<-c("외국인","개인",'기관투자자','법인')
#train_exn<-exn[seq(1,100),]
#test_exn<-exn[seq(101,),]


exo<-cbind(kospi,환율)
colnames(exo)<-c("kospi","환율")

#train_exo<-exo[seq(1,100),]
#test_exo<-exo[seq(101,),]






info<-VARselect(exn,lag.max = 6,type="none",exog=exo)
info
#exog 는 외생변수
#none trend const both

#info$selection["기관투자자",12]
#lag 는 2로 결정 or 1 or 3

varmodel<-VAR(exn,p=1,type="none",exogen=exo)

summary(varmodel)

residuals(varmodel)[,1]
cov(residuals(varmodel)[,1],residuals(varmodel)[,1])

Phi(varmodel)


dwtest(varmodel)
dwtest(varmodel$varresult$개인)
dwtest(varmodel$varresult$외국인)
dwtest(varmodel$varresult$법인)
# 더비 왓슨 가설 검증으로 각각의 목표 변수 에대한 회귀식에서
#잔차에 대한 자기 상관성이 없다 즉 각 시계열에 따라 독립적이다

byserial<-serial.test(varmodel,lags.pt = 12,type="PT.asymptotic")
colnames(byserial$resid)<-c("기관투자자","개인","외국인","법인")


byserial 
#귀무가설 자기상관이 존재하지 않음
#대립가설 자기상관이존재함
# 5# 유의 수준으로 봤을때자기상관이 있다고 볼수 있습니다

#모델을 좀더 복잡하게 만들까

x11()
plot(byserial)







byarch<-arch.test(varmodel,lags.multi =10,multivariate.only = TRUE)
byarch


# 귀무가설 : 이분산성이 없다 arch효과 없다
# 대립가설 이분산성이 있다   arch효과 있다
#p-value 가 5% 유의수준으로 이분산성이 없다고 증명
# 즉 구간별 분상성 과 비례적 분산성이 없다는 이야기

bynorm<-normality.test(varmodel, multivariate.only = FALSE)
bynorm


#잔차의 분포는 정규성이 없다고 판단
#귀무가설 정규성

#but 왜도 기준으로 봤을떄 정규성을 가지고 있다고 판단할수 있음


bycusum<-stability(varmodel,type="OLS-CUSUM")

X11()
plot(bycusum)

#bycusum3<-stability(varmodel3,type="OLS-CUSUM")

#X11()
#plot(bycusum3)

#lag 가 1 가 더 적절


model_summray<-summary(varmodel)

model_summray$covres
model_summray$corres

nam<-"기관투자자"

irff1<-irf(varmodel,impulse =nam,response = "기관투자자",
           n.ahead = 20,boot=TRUE,ortho=TRUE)
irff2<-irf(varmodel,impulse =nam,response = "외국인",
          n.ahead = 20,boot=TRUE,ortho=TRUE)
irff3<-irf(varmodel,impulse =nam,response = "개인",
           n.ahead = 20,boot=TRUE,ortho=TRUE)
irff4<-irf(varmodel,impulse=nam,response = "법인",n.ahead = 20)


#irff1$irf
irf1_df <- data.frame(value = irff1$irf,upper=irff1$Upper,down=irff1$Lower)
irf2_df <- data.frame(value = irff2$irf,upper=irff2$Upper,down=irff2$Lower)
irf3_df <- data.frame(value = irff3$irf,upper=irff3$Upper,down=irff3$Lower)
irf4_df <- data.frame(value = irff4$irf,upper=irff4$Upper,down=irff4$Lower)




ir<-cbind(irf1_df,irf2_df,irf3_df,irf4_df)

write.csv(ir,"C:/Users/32442/OneDrive/바탕 화면/논문/기관.csv")
#x11()
#par(mfcol=c(2,2))
#plot(irff,xlim=c(0,21))
#plot(irff1,ylab="기관투자자",xlim=c(0,21),main="response= 기관투자자",
#     cex.main=2.5)
#plot(irff2,ylab="개인",xlim=c(0,21),main="response= 개인",
#     cex.main=2.5)
#plot(irff3,ylab="외국인",xlim=c(0,21),main="response= 외국인",
#     cex.main=2.5)
#plot(irff4,ylab="법인",xlim=c(0,21),main="response= 법인",
#     cex.main=2.5)





bvvardec<-fevd(varmodel,n.ahead = 12)
bvvardec$외국인

외국<-bvvardec$외국인
개<-bvvardec$개인
개
기관<-bvvardec$기관투자자
법<-bvvardec$법인

법

#summary(varmodel)
#varmodel$restrictions
#외국

write.csv(외국,"C:/Users/32442/OneDrive/바탕 화면/논문/외국.csv")
write.csv(개,"C:/Users/32442/OneDrive/바탕 화면/논문/개.csv")
write.csv(기관,"C:/Users/32442/OneDrive/바탕 화면/논문/기관.csv")
write.csv(법,"C:/Users/32442/OneDrive/바탕 화면/논문/법.csv")
