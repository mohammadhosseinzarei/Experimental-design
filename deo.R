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
type_glass=factor(rep(c(1,2), each=3, times=3))
type_pho=factor(rep(c(1,2,3), each=6))
brightness<-c(
  280,290, 285, 230, 235, 240, 
  300, 310, 295, 260, 240, 235, 
  290, 285, 290, 220, 225, 230
)
my_data1<-data.frame(
  type_glass=type_glass,
  type_phosphor=type_pho,
  brightness=brightness
)
mod1<-aov(brightness~type_glass*type_pho, data = my_data1)
summary(mod1)