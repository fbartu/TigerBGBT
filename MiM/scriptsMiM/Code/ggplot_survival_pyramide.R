#setwd("/home/laurablanco/Doctorado/RStudio")
setwd("C:/Users/fbartu/Desktop/")
library(readxl)
library(tidyverse)
library(ggplot2)
library(dplyr)
set.seed(1234)
survival <- read_excel("Data_Survival.xlsx", 
                       col_types = c("numeric", "numeric", "skip", 
                                     "skip", "skip", "skip", "skip", "skip", 
                                     "skip", "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric"))
survival$ORIGIN <- as.factor(survival$ORIGIN)
survv <- survival %>% group_by(ORIGIN) %>% summarize(grp.mean = mean(total_lived))
survv
#TOTAL LIVED
a <- ggplot(survival, aes(x = num_days))

#TOTAL LIVED BY ORIGIN
a + geom_histogram(aes(color = ORIGIN), fill = "white", bins = 50) + theme_minimal()
a + geom_histogram(aes(color = ORIGIN, fill = ORIGIN), alpha =0.4, position = "identity", bins = 50) + scale_fill_manual(values = c("#00AFBB", "#E7B800")) + scale_color_manual(values = c("#00AFBB", "#E7B800"))

#DENSITY
a + geom_density()
#HISTOGRAM AND DENSITY BY ORIGIN
a + geom_histogram(aes(y = ..density.., color = ORIGIN, fill = ORIGIN),  alpha = 0.4, position = "identity") + geom_density(aes(color = ORIGIN), size =1)
#PYRAMIDE
p <- ggplot(survival, aes(x = MUESTRA, y = num_days, fill = ORIGIN)) + geom_col(data = subset(survival, ORIGIN == "0") %>% transform(num_days= - num_days), width = 0.5) + geom_col(data = subset(survival, ORIGIN == "1"), width = 0.5) +  coord_flip() + ylab("age structure") + theme_bw() +
  labs(title =        "Age structure of Aedes albopictus. 
Comparison between number of days lived by lab-grown vs. captured mosquitoes",
       x = "Number of mosquitoes in tubes",
       y = "Eclosionador                                                                              Traps",
       caption = "Source: MosquitoAlert")+
  scale_y_continuous(breaks = seq(-40, 40, 5), labels = as.character(c(seq(40,0, -5), seq(5, 40, 5))), limits = c(-40, 40)) 
p + scale_fill_discrete(limits=c("0", "1"), labels=c("Eclosionador", "Trampa")) 

#survv <- survival %>% group_by(ORIGIN=0) 

survv<-survival[which(survival$ORIGIN==0),]
survv<-survv[which(survv$COHORT_2==1),]
plot(survv$num_days,survv$num_pre)
m1<-lm(survv$num_days~survv$num_pre)
anova(m1)
survv


survival %>% group_by(COHORT_1) %>% summarize(grp.mean = mean(num_days))
survival %>% group_by(COHORT_1) %>% summarize(grp.sd = sd(num_days))

