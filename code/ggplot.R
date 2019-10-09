rm(list=ls())
library(ggplot2)
Dir = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(paste0(strsplit(Dir,split = "/code"), "/dataset"))
bodyfat=read.csv('BodyFat.csv')

##boxplot
dat <- data.frame(position = factor(rep(c("BODYFAT","DENSITY","AGE","WEIGHT","HEIGHT","ADIPOSITY","NECK","CHEST","ABDOMEN","HIP","THIGH","KNEE","ANKLE","BICEPS","FOREARM","WRIST"), each=252)), dataquantity = c(bodyfat$BODYFAT,bodyfat$DENSITY,bodyfat$AGE,bodyfat$WEIGHT,bodyfat$HEIGHT,bodyfat$ADIPOSITY,bodyfat$NECK,bodyfat$CHEST,bodyfat$ABDOMEN,bodyfat$HIP,bodyfat$THIGH,bodyfat$KNEE,bodyfat$ANKLE,bodyfat$BICEPS,bodyfat$FOREARM,bodyfat$WRIST))
ggplot(dat, aes(x = position, y = dataquantity)) +
  geom_boxplot(outlier.colour="red", outlier.shape=16,
               outlier.size=1, notch=FALSE)+
  guides(fill=FALSE) +
  coord_flip()+ggtitle("Boxplot of Bodyfat dataset")+theme(plot.title = element_text(hjust = 0.5))


reverse.density=1/bodyfat$DENSITY


#残差图
siri=495/bodyfat$DENSITY - 450
res=bodyfat$BODYFAT-siri
num=c(1:length(res))
index=which(abs(res)>1)
frame=data.frame(num,res)
library(ggplot2)
p1 = ggplot(frame, aes(x = num, y = res)) + geom_point( alpha=0.9,size=1)+
  geom_hline(aes(yintercept=0),col="blue",linetype="dashed")+
  annotate("text",x=frame$num,y=frame$res,label=ifelse(frame$res>3|frame$res< -3, frame$num,""),vjust=1,color="red")+ 
  labs(title="Residuals", x="ID", y="Res")+theme(plot.title = element_text(hjust = 0.5))+ylim(-10,20) 
p1


#siri1
revdes=1/bodyfat$DENSITY 
frame1=data.frame(num=num,fat = bodyfat$BODYFAT,revdes,res)
p2 = ggplot(frame1, aes(x = revdes, y = fat)) + geom_point( alpha=0.9,size=1)+
  labs(title="Siri's Equation", x="1/Density", y="Bodyfat")+theme(plot.title = element_text(hjust = 0.5))+
  annotate("text",x = frame1$revdes, y = frame1$fat,label=ifelse(frame1$res>3|frame1$res< -3|frame1$revdes>1, frame1$num,""),vjust=1,color="red")
p2

##change values
a=495/bodyfat$DENSITY[48]- 450
bodyfat$BODYFAT[48]=a
bodyfat$DENSITY[96]=495/(bodyfat$BODYFAT[96]+450) 
fat=495/bodyfat$DENSITY[76]- 450
bodyfat$BODYFAT[76]=fat

#siri2
revdes=1/bodyfat$DENSITY 
frame1=data.frame(fat=bodyfat$BODYFAT[-c(182)],resd=revdes[-c(182)],n=num[-c(182)])
p2 = ggplot(frame1, aes(x = resd, y =fat)) + geom_point( alpha=0.9,size=1)+
  geom_point(data = frame1, aes(x = max(resd), y=max(fat)), colour = 'red', size = 3) +
  labs(title="Siri's Equation", x="1/Density", y="Bodyfat")+theme(plot.title = element_text(hjust = 0.5))+
  annotate("text",x =frame1$resd, y = frame1$fat,label=ifelse(frame1$resd>1, frame1$n,""),vjust=2,color="red")
p2


#siri3
revdes=1/bodyfat$DENSITY 
frame1=data.frame(fat=bodyfat$BODYFAT[-c(182,216)],resd=revdes[-c(182,216)])
p3 = ggplot(frame1, aes(x = resd, y =fat)) + geom_point( alpha=0.9,size=1)+
  labs(title="Siri's Equation", x="1/Density", y="Bodyfat")+theme(plot.title = element_text(hjust = 0.5))
p3

#bmi
bmi=bodyfat$WEIGHT/(bodyfat$HEIGHT)^2
estimate=0.07035+702.07386*bmi
res=bodyfat$ADIPOSITY-estimate
num=c(1:252)
num1=num[-c(182,216)]
frame1=data.frame(adip=bodyfat$ADIPOSITY[c(-182,-216)],b=bmi[c(-182,-216)],r=res[c(-182,-216)],n=num1)
p3 = ggplot(frame1, aes(x = b, y =adip)) + geom_point( alpha=0.9,size=1)+
  labs(title="BMI Equation", x="WEIGHT/(HEIGHT)^2", y="ADIPOSITY")+theme(plot.title = element_text(hjust = 0.5))+
  annotate("text",x=frame1$b,y=frame1$adip,label=ifelse(frame1$r>2 | frame1$r < -2, frame1$n,""),vjust=2,hjust = 1,color="red",size=3,angle = 45)
p3

#change values
model.bmi=lm(bodyfat$ADIPOSITY[-c(42,163,221)]~bmi[-c(42,163,221)])

bmi.42=(bodyfat$ADIPOSITY[42]-0.07035)/702.07386
height=sqrt(bodyfat$WEIGHT[42]/bmi.42)
#hence the height of the 42 guy is 69.4345 much reliable
bodyfat$HEIGHT[42]=height

bmi.221=(bodyfat$ADIPOSITY[221]-0.07035)/702.07386
weight=bmi.221*(bodyfat$HEIGHT[221])^2
bodyfat$WEIGHT[221]=weight


bmi.163=bodyfat$WEIGHT[163]/(bodyfat$HEIGHT[163])^2
adi=0.1942+698.6151*bmi.163
bodyfat$ADIPOSITY[163]=adi


##ggplot
bmi=bodyfat$WEIGHT/(bodyfat$HEIGHT)^2
frame1=data.frame(adip=bodyfat$ADIPOSITY[c(-182,-216)],b=bmi[c(-182,-216)],r=res[c(-182,-216)],n=num1)
p3 = ggplot(frame1, aes(x = b, y =adip)) + geom_point( alpha=0.9,size=1)+
  labs(title="BMI Equation", x="WEIGHT/(HEIGHT)^2", y="ADIPOSITY")+theme(plot.title = element_text(hjust = 0.5))+
  annotate("text",x=frame1$b,y=frame1$adip,label=ifelse(frame1$r>2 | frame1$r < -2, frame1$n,""),vjust=2,hjust = 1,color="red",size=3,angle = 45)
p3
