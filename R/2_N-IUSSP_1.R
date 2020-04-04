############# Written by JMA
############# 23/03/2020
rm(list=ls(all=TRUE))
library(data.table)
library(ggplot2)
library(plotly)
# Loading data
load("R/Data/HMD_Data.RData")
#load("R/Data/Italy_HMD.RData")

Italy.mx <- HMDL[PopName == 'ITA',c(1:3,10,12,13)]
Italy.mx <- Italy.mx[Year >= 1920]
Italy.mx$Sex1 <- Italy.mx$Sex
#load useful functions
source("R/Functions_N-IUSSSP.R")

############## Calculate lifespan equality measures
Results           <- Italy.mx[,list(sd=my.sd.frommx(mx = mx,sex = Sex[1]), 
                                eo = ex[1],
                                Period = cut(Year,breaks = c(1900,1920,1959,Inf),labels = c("1900-1921","1921-1959","1960 onwards"),include.lowest = T)),
                          by = list(PopName,Sex,Sex1,Year)]

mean(Results[Year==2017]$sd)
mean(Results[Year==1920]$sd)

##### Fig 2
Italy.dx <- HMDL[Year %in% c(1920,2017) & Sex == 'f' & PopName == 'ITA']
Italy.dx$dx <- Italy.dx$dx/1000
Italy.dx$lx <- Italy.dx$lx/100000


Fig1 <-ggplot(Italy.dx, aes(x = Age,y = dx)) +
  ggtitle('Distribution of deaths for Italian females') +
  geom_line(aes(group = as.factor(Year),colour= as.factor(Year)), size= 2) +
  geom_segment(aes(x =Italy.dx[Year ==  1920]$ex[1] , y = 0, xend = Italy.dx[Year ==  1920]$ex[1], yend = Italy.dx[Year ==  1920]$dx[47]),
              col ='#517AC9',lty = 3, size = 1 )+
  geom_segment(aes(x =Italy.dx[Year ==  2017]$ex[1] , y = 0, xend = Italy.dx[Year ==  2017]$ex[1], yend = Italy.dx[Year ==  2017]$dx[85]),
               col ='#C05D5D',lty = 3, size = 1 )+
  labs(x = "Age", y = "Proportion of deaths (%)")+
  scale_colour_manual('Year', values = c('#517AC9','#C05D5D'), labels = c('1920', '2017')) + 
  theme_light()+
  theme(text = element_text(size=16),legend.position = c(.85, .85))
Fig1

#ggplotly(Fig1)

pdf(file="R/Figures/Fig1.pdf",width=5.5,height=5.5,pointsize=4,useDingbats = T)
Fig1
dev.off()

Results[Sex =='f']

##### Fig 2
Fig2 <-ggplot(Results, aes(x = eo,y = sd, Year=Year)) +
  ggtitle('Italy 1920-2017') +
  geom_point(aes(group = PopName,colour=Sex), size= 2) +
  theme_light()+
  labs(x = "Life expectancy", y = "Lifespan inequality (standard deviation)")+
  scale_colour_manual('Sex', values = c('#7E9960','#C0852D'), labels = c('Females', 'Males')) + 
  theme(text = element_text(size=16),legend.position = c(.85, .85))
Fig2

#ggplotly(Fig1)

pdf(file="R/Figures/Fig2.pdf",width=5,height=5,pointsize=4,useDingbats = T)
Fig2
dev.off()

## Potential gains in life expectancy and lifespan inequality by reducing 1% deaths at each age
library(DemoDecomp)
library(patchwork)

Italy.decomp <- Italy.mx[Year %in% c(1920,2017) & Sex == 'f']
Italy.decomp[,Potential.sd:= horiuchi(func = my.sd.frommx,pars1 = mx,pars2 = mx*.95,sex = Sex1[1],N = 100), by = list(Year)]
Italy.decomp[,Potential.eo:= horiuchi(func = LifeExpectancy,pars1 = mx,pars2 = mx*.95,sex = Sex1[1],N = 100), by = list(Year)]
Italy.decomp[,sum(Potential.eo), by = Year]

Fig3.A <-ggplot(Italy.decomp, aes(x = Age,y = Potential.eo)) +
  ggtitle('Change in life expectancy by reducing 5%') +
  geom_line(aes(group = as.factor(Year),colour= as.factor(Year)), size= 2) +
  labs(x = "Age", y = "Change (years)")+
  coord_cartesian(ylim=c(0, .03))+
  geom_hline(yintercept = 0,col= 'black')+
  scale_y_continuous(
    labels = scales::number_format(accuracy = 0.01))+
  scale_colour_manual('Year', values = c('#517AC9','#C05D5D'), labels = c('1920', '2017')) + 
  theme_light()+
  theme(text = element_text(size=16),legend.position = c(.85, .85))
Fig3.A

Fig3.B <-ggplot(Italy.decomp, aes(x = Age,y = Potential.sd)) +
  ggtitle('Change in lifespan inequality by reducing 5%') +
  geom_line(aes(group = as.factor(Year),colour= as.factor(Year)), lty = 1, size= 2,show.legend = F) +
  labs(x = "Age", y = "Change (years)")+
  coord_cartesian(ylim=c(-0.07,.01))+
  geom_hline(yintercept = 0,col= 'black')+
  scale_y_continuous(
    labels = scales::number_format(accuracy = 0.01))+
  scale_colour_manual('Year', values = c('#B4C2EB','#EDB4B5'), labels = c('1920', '2017')) + 
  theme_light()+
  theme(text = element_text(size=16),legend.position = c(.85, .85))
Fig3.B

Fig3.A / Fig3.B

pdf(file="R/Figures/Fig3.pdf",width=5.5,height=11,pointsize=4,useDingbats = T)
Fig3.A / Fig3.B
dev.off()




