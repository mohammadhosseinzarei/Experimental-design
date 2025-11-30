library(daewr )
?daewr
str(bread)
mod01<-lm(height~time, data=bread)
summary(mod01)
mod02<-aov(height~time, data=bread)
summary(mod02)
contrasts(bread$time)<-contr.poly(3)
contrasts(bread$time)
mod5<-aov(height~time, data=bread)
summary.lm(mod5)
my_data=data.frame(loaf=1:12,
                   time=c(35, 35, 35, 35, 40, 40,40, 40,45, 45, 45, 45),
                   height=c(4.5, 5.00, 5.50, 6.75, 6.50, 6.50, 10.50, 9.50, 9.75, 8.75, 6.50, 8.25)
                   )
str(my_data)
my_data$time<-factor(my_data$time)
mean(bread$height)

set.seed(7638)
f <- factor( rep( c(35, 40, 45 ), each = 4))
fac <- sample( f, 12 )
eu <- 1:12
plan <- data.frame( loaf=eu, time=sort(fac) )
write.csv( plan, file = "Plan.csv", row.names = FALSE)
read.csv("plan.csv")
install_github("ehassler/MontgomeryDAE")
pkgbuild::has_build_tools(debug = TRUE)

library(MontgomeryDAE)
data(package="MontgomeryDAE")

Table13.1   
str(Table13.1)
my_data<-Table13.1   
my_data$ob

library(montgomeryDAE)





# نمونه وارد کردن دیتا

my_data=data.frame(loaf=1:12,
                   time=c(35, 35, 35, 35, 40, 40,40, 40,45, 45, 45, 45),
                   height=c(4.5, 5.00, 5.50, 6.75, 6.50, 6.50, 10.50, 9.50, 9.75, 8.75, 6.50, 8.25)
)
str(my_data)
my_data$time<-factor(my_data$time)
str(my_data)
##############آزمون کردن با استفاده از دستور ال ام و تفاوت معنی دار

mode00<-lm(height~time, data=my_data)
summary(mode00)$r.squared # دستتور ضریب تعیین مدل


boxplot(height~time, data=my_data)

dev.off() 
par(mfrow=c(2,2),mar=c(4,4,2,1))
plot(mode00)

mod000=aov(height~time, data=bread)
summary(mod000)
mod.tuky<-TukeyHSD(mod000)
wri



library(daewr)
? daewr
str(sugarbeet)
mod4 <- aov( yield ~ treat, data = sugarbeet )
con <- matrix(c(1, -1/3, -1/3, -1/3, 0, 1, -1, 0,
                  + 0, 0, 1, -1 ), 4, 3 )
L <- t(con)
rownames(L) <- c("-fertilizer effect", "-plowed vs. broadcast"
                   , "-January vs. April")
library(gmodels)
fit.contrast(mod4, "treat", L)la

contrasts(bread$time) <- contr.poly(3)
contrasts(bread$time)

set.seed(3685)
f<-factor(rep(c("35","40", "45"), each=4))
s<-sample(f, 12)
h=c(4.5, 5.00, 5.50, 6.75, 6.50, 6.50, 10.50, 9.50, 9.75, 8.75, 6.50, 8.25)
my_data1<-data.frame(loaf=1:12, time=sort(s), height=h)
my_data1
contrasts(my_data1$time)<-contr.poly(3)
contrasts(my_data1$time)
mod<-aov(height~time, data=my_data1)
summary.lm(mod)

########################################################
# enter data col

t<-factor(rep(c("15", "70", "125"), each=12))
m_ty<-factor(rep(c("1", "2", "3"), each=2, times=6))
bat<-c(130, 74, 150, 159, 138, 168,
       155, 180, 188, 126, 110, 160, 
       34, 80, 136, 106, 174, 150,
       40, 75, 122, 115, 120, 139, 
       20, 82, 25, 58, 96, 82,
       70, 58, 70, 45, 104, 60)
my_data<-data.frame(
  typeperature=t, material_type=m_ty, Battrey=bat
)

mode=aov(Battrey~material_type*typeperature, data=my_data)
summary(mode)

str(my_data)
################## install packge 
library(remotes)
remotes::install_github("ehassler/MontgomeryDAE")
###### use library table 5.1
library(MontgomeryDAE)
data(package="MontgomeryDAE")
Table5.1                   
my_data1<-Table5.1                   
str(my_data1)
my_data1$MaterialType<-factor(my_data1$MaterialType)
my_data1$Temperature<-as.factor(my_data1$Temperature)
str(my_data1)
mode1<-aov(BatteryLife~MaterialType*Temperature, data=my_data1)
summary(mode1)
mode1$fitted.values
mode1$residuals
mode2<-lm(BatteryLife~MaterialType*Temperature, data=my_data1)
summary(mode2$effects)
summary(mode2).r-squared
model.tables(mode1, type="means", se= T)
model.tables(mode1, type="effects", se=T)
?interaction.plot
?pdf
pdf("ineraction plot", height=7, width=9)
interaction.plot(
  x.factor=my_data1$Temperature,
  trace.factor = my_data1$MaterialType,
  response = my_data1$BatteryLife,
  type="b",
  pch=19,
  xlab="Temperature",
  ylab="BatteryLife", 
  main="interaction temperature:materialtype",
  legend = TRUE,
  trace.label = deparse1(substitute(type.material))
)
dev.off()

TukeyHSD(mode1, "Temperature")  #test tukey for Temperature
TukeyHSD(mode1, "MaterialType")  #test tukey for MaterialType

library(emmeans)
?emmeans
emmeans(mode1, pairwise~MaterialType|Temperature)
emmeans(mode1, pairwise~MaterialType|Temperature, at=list(Temperature="70"))
par(mfrow=c(2,,2))
plot(mode1)
plot(mode1$residuals~as.character(my_data1$Temperature))
plot(mode1$residuals~as.character(my_data1$MaterialType))
mode3<-model.tables(mode1, type="means", se=T)
############# The Assumption of No Interaction in a  Two-Factor Model
library(MontgomeryDAE)
data(package="MontgomeryDAE")
my_data2<-Table5.1
str(my_data2)
my_data2$Temperature<-as.factor(my_data2$Temperature)
str(my_data2)
mod4<-aov(my_data2$BatteryLife~my_data2$MaterialType + my_data2$Temperature)
summary(mod4)
plot(mod4$fitted.values, mode1$fitted.values-mod4$fitted.values, pch=19)
### enter method row and col
set.seed(1245)


f1 <- factor(rep(c("1", "2", "3"), each = 12))       
f2 <- factor(rep(rep(c("15", "70", "75"), each = 4), 3)) 

data <- c(
  130, 155, 34, 40, 20, 70, 
  74, 180, 80, 75, 82, 58,
  150, 188, 136, 122, 25, 70,
  159, 126, 106, 115, 58, 45,
  138, 110, 174, 120, 96, 104,
  168, 160, 150, 139, 82, 60
)

data1 <- data.frame(matrial_type=f1, temperature=f2, data=data)
model <- aov(data ~ matrial_type * temperature, data = data1)
summary(model)

library(MontgomeryDAE)
data(paka)

#### enter data metod row

batt<-c(130, 155, 34, 40, 20, 70, 
        74, 180, 80, 75, 82, 58, 
        150, 188, 136, 122, 25, 70, 
        159, 126, 106, 115, 58, 45, 
        138, 110, 174, 120, 96, 104,
        168, 160, 150, 139, 82, 60
)

tt<-factor(rep(c("15", "70", "125"), each=2, times=6))
m_tyy<-factor(rep(c("1", "2", "3"), each=12))
my_dataa<-data.frame(
  typeperaturee=tt, material_typee=m_tyy, Battreyy=batt
)

modee=aov(Battreyy~material_typee*typeperaturee, data=my_dataa)
summary(modee)

####### sloution bock
Surface_Finish <-c(
  74, 79, 82, 99, 
  64, 68, 88, 104, 
  60, 73, 92, 96, 
  92, 98, 99, 104,
  86, 104, 108, 110,
  88, 88, 95, 99,
  99, 104, 108, 114,
  98, 99, 110, 111,
  102, 95, 99, 107
)
Depth_of_cut<-factor(rep(c(0.15, 0.18, 0.20, 0.25), times=9))
feed<-factor(rep(c(0.20, 0.25, 0.30), each=12))

length(Depth_of_cut)==length(Surface_Finish)

my_datasurface<-data.frame(
  Surface_Finish=Surface_Finish, Depth_of_cut=Depth_of_cut, feed=feed
)
str(my_datasurface)
mod12<-aov(Surface_Finish~Depth_of_cut*feed, data=my_datasurface)
table(feed, Depth_of_cut )
summary(mod12)
?interaction.plot
interaction.plot(
  x.factor = my_datasurface$feed,
  trace.factor = my_datasurface$Depth_of_cut,
  response= my_datasurface$Surface_Finish,
  type="b",
  pch=20,
  xlab="feed",
  ylab="Dapth of cut",
  trace.label=deparse1(substitute(feed)),
  main="interction feed:Dapth of cut"
)
interaction.plot(
  x.factor = my_datasurface$Depth_of_cut,
  trace.factor = my_datasurface$feed,
  response= my_datasurface$Surface_Finish,
  type="b",
  pch=20,
  xlab="feed",
  ylab="Dapth of cut",
  trace.label=deparse1(substitute(Depth_of_cut)),
  main="interction feed:Dapth of cut"
)
################################################################
#solution problem 5.6
############################################################### 
brightness=c(
  280, 290, 285, 230, 235, 240,
  300, 310, 295, 260, 240, 235, 
  290, 285, 290, 220, 225, 230
)
type_Glass<-factor(rep(c(1,2), each=3, times=3))
phosphor_type=factor(rep(c(1,2,3), each=6))
my_databrightness<-data.frame(
  type_Glass=type_Glass,
  phosphor_type=phosphor_type,
  brightness=brightness
)
mode12<-aov(brightness~type_Glass*phosphor_type, data=my_databrightness)
summary(mode12)
mode13<-lm(brightness~type_Glass*phosphor_type, data=my_databrightness)
summary(mode13)
summary(mode13)$r.squared
model.tables(model12, type="means", se=T)
model.tables(model12, type="effects", set=T)
?interaction.plot
pdf("inercation_phosphor.pdf", width = 12, height=9)
interaction.plot(
  x.factor=my_databrightness$type_Glass,
  trace.factor=my_databrightness$phosphor_type,
  response=my_databrightness$brightness,
  trace.label = deparse1(substitute(trace_phosphor_type)),
  type="b",
  pch=19,
  xlab="type_Glass",
  ylab="brightness",
  main="intrcation type_Glass::phosphor_type"
)
dev.off()
##### example5.3
A<-factor(rep(c(10, 12, 14), each=2, times=4))
B<-factor(rep(c(25, 30),each=12))
C<- factor(rep(c(200, 250), each = 6, times =2 ))
y<-c(-3, -1, 0, 1, 5, 4, 
        -1, 0, 2, 1, 7, 6,
        -1, 0, 2, 3, 7, 9, 
        1, 1, 6, 5, 10, 11)
eamaple5.3<-data.frame(A , B, C , y)

mode0<-aov(y~A*B*C, data=eamaple5.3)
summary(mode0)
##############################################################
library(MontgomeryDAE)
Eamaple5_13<-Table5.13
Eamaple5_13$PctCarbonation<-factor(Eamaple5_13$PctCarbonation)
Eamaple5_13$OperatingPressure<-as.factor(Eamaple5_13$OperatingPressure)
Eamaple5_13$LineSpeed<-as.factor(Eamaple5_13$LineSpeed)
str(Eamaple5_13)
mod<-aov(FillHeightDeviation~PctCarbonation*OperatingPressure*LineSpeed, data = Eamaple5_13)
summary(mod)

interaction.plot(
  x.factor=Eamaple5_13$PctCarbonation, 
  trace.factor=Eamaple5_13$OperatingPressure,
  response= Eamaple5_13$FillHeightDeviation
)
#interaction Pressure (B)
interaction.plot(
  x.factor=Eamaple5_13$PctCarbonation, 
  trace.factor=rep(1, times=length(Eamaple5_13$PctCarbonation)),
  response= Eamaple5_13$FillHeightDeviation
)
##interaction OperatingPressure (A)
interaction.plot(
  x.factor=Eamaple5_13$OperatingPressure, 
  trace.factor=rep(1, times=length(Eamaple5_13$OperatingPressure)),
  response= Eamaple5_13$FillHeightDeviation
)

interaction.plot(
  x.factor=Eamaple5_13$LineSpeed, 
  trace.factor=rep(1, times=length(Eamaple5_13$LineSpeed)),
  response= Eamaple5_13$FillHeightDeviation
)


