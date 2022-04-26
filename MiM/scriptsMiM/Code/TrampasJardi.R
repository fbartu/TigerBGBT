rm(list=ls())
getwd()

library(ggplot2)

T <- Trampas

DATE1<-sort(T$`End date`)[1]
T$NUM_DAYS  <- difftime(T$`End date`,DATE1,units="days")

ggplot(T, aes(x=T$`End date`, y= T$`Number per species  [EGGS: Total no. E x No. specimens hatched-analysed (subsample) / total no. hatched-analysed individuals]` )) + geom_point() + geom_smooth(method="lm")+ 
  xlab("Date(months)") + ylab("Number of Mosquitoes") #GGPLOT USING ALL TRAPS TOGETHER.

#NOW WE FILTER BY TRAPS
D2 <- T[which(T$`trap ID [E (ovitraps) or A (BGS traps)  + location name + numbering]`== "A_SP_BL_2"),]
D3 <- T[which(T$`trap ID [E (ovitraps) or A (BGS traps)  + location name + numbering]`== "A_SP_BL_3"),]
D4 <-T[which(T$`trap ID [E (ovitraps) or A (BGS traps)  + location name + numbering]`== "A_SP_BL_4"),]
D5 <-T[which(T$`trap ID [E (ovitraps) or A (BGS traps)  + location name + numbering]`== "A_SP_BL_5"),]
D6 <- T[which(T$`trap ID [E (ovitraps) or A (BGS traps)  + location name + numbering]`== "A_SP_BL_6"),]

#if we want to plot only one trap, for example, trap4
ggplot(D4, aes(x= `End date`, y=`Number per species  [EGGS: Total no. E x No. specimens hatched-analysed (subsample) / total no. hatched-analysed individuals]`)) + geom_point() + geom_smooth(method="lm")
 + xlab("Date") + ylab("Number of Mosquitoes")

#get column names
colnames(T)

# Rename column where name is "Number per species..." and "Trap ID..."
names(T)[27] <- "NumberMosquitoes"
names(T)[7] <- "Trapname"

#Ggplot and lm with all traps
ggplot(T, aes(x=`End date`, y=NumberMosquitoes, colour=Trapname,fill=Trapname,shape=Trapname)) + geom_point(size=2) + geom_smooth(method="lm")+ 
  xlab("Date(months)") + ylab("Number of Mosquitoes")
# We need to remove rows 40, 41, 56 and 57 from T because "A_SP_BL4A/4B" and "A_SP_BL5A/5B" are extra data which is causing confusion
T2 <- T[-c(40, 41, 56, 57), ]
#Now we repeat the ggplot with the data cleared
ggplot(T2, aes(x=`End date`, y=NumberMosquitoes, colour=Trapname,fill=Trapname,shape=Trapname)) + geom_point(size=2) + geom_smooth(method="lm")+ 
  xlab("Date(months)") + ylab("Number of Mosquitoes")

#we're going to change the long column titles in the separated data of the traps
names(D2)[27] <- "NumberMosquitoes"
names(D2)[7] <- "Trapname"
names(D3)[27] <- "NumberMosquitoes"
names(D3)[7] <- "Trapname"
names(D4)[27] <- "NumberMosquitoes"
names(D4)[7] <- "Trapname"
names(D5)[27] <- "NumberMosquitoes"
names(D5)[7] <- "Trapname"
names(D6)[27] <- "NumberMosquitoes"
names(D6)[7] <- "Trapname"

#now, we plot each trap
ggplot(D2, aes(x= `End date`, y=`NumberMosquitoes`)) + geom_point() + geom_smooth(method="lm") + xlab("Date") + ylab("Number of Mosquitoes")
ggplot(D3, aes(x= `End date`, y=`NumberMosquitoes`)) + geom_point() + geom_smooth(method="lm") + xlab("Date") + ylab("Number of Mosquitoes")
ggplot(D4, aes(x= `End date`, y=`NumberMosquitoes`)) + geom_point() + geom_smooth(method="lm") + xlab("Date") + ylab("Number of Mosquitoes")
ggplot(D5, aes(x= `End date`, y=`NumberMosquitoes`)) + geom_point() + geom_smooth(method="lm") + xlab("Date") + ylab("Number of Mosquitoes")
ggplot(D6, aes(x= `End date`, y=`NumberMosquitoes`)) + geom_point() + geom_smooth(method="lm") + xlab("Date") + ylab("Number of Mosquitoes")

#We analyse all traps
M_all<-lm(NumberMosquitoes~NUM_DAYS,data=T2)
summary(M_all)

#... and each trap
M2<-lm(NumberMosquitoes~NUM_DAYS,data=D2)
M3<-lm(NumberMosquitoes~NUM_DAYS,data=D3)
M4<-lm(NumberMosquitoes~NUM_DAYS,data=D4)
M5<-lm(NumberMosquitoes~NUM_DAYS,data=D5)
M6<-lm(NumberMosquitoes~NUM_DAYS,data=D6)
#Let's see if intercept is correct
summary(M6)
summary (M5)
summary(M4)
summary(M3)
summary(M2)
