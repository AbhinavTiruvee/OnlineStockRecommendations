#loads data
stocks = read.csv(file = file.choose())
attach(stocks)

#gives mean adjusted growth
mean(AdjGrowth)

#gives median adjusted growth
median(AdjGrowth)

#makes ANOVA model
stocks.aov = aov(AdjGrowth~Reccomender)
summary(stocks.aov)

#checks ANOVA conditions
plot(stocks.aov)

#makes the table
mean= tapply(AdjGrowth,Reccomender,mean)
means
means =sort(means, decreasing = TRUE)
median = tapply(AdjGrowth,Reccomender,median)
tab = cbind(mean,median)
tab
colnames(tab)[0] = "Reccomender"
tab
colnames(tab)[1] = "Mean"
tab
colnames(tab)[2] = "Median"
tab

# Libraries
library(ggplot2)
library(dplyr)
library(tidyverse)
library(hrbrthemes)
library(viridis)

# Density Plot
stocks %>%
  filter( AdjGrowth<1000 ) %>%
  ggplot( aes(x=AdjGrowth)) +
  xlim(-250,1000)+
  geom_density(fill="#FF9933", color="#e9ecef", alpha=0.8)

#boxplot that looks nice
stockBox =stocks %>%
  ggplot( aes(x=Reccomender, y=AdjGrowth, fill=Reccomender)) +
  geom_boxplot() +
  ylim(-100,500)+
  xlab("Reccomender")+
  ylab("Adjusted Growth (%)")+
  scale_fill_viridis(discrete = TRUE, alpha=.4, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=30),
    axis.text.x= element_text(size=13)
  ) +
  
  ggtitle("Adjusted Growth of Reccomenders") +
  xlab("Reccomender")
stockBox

#dotplot
stockPlot = ggplot(stocks, aes(x=Reccomender, y=AdjGrowth,fill = Reccomender)) + 
  ylim(-100,500)+
  geom_dotplot(binaxis = 'y', dotsize =1,stackdir = 'center',stackratio=0)pstockPlot
stockPlot

#histogram
stockHist =ggplot(stocks, 
       aes(x=AdjGrowth)) + 
  xlim(-100,1000)+
  xlab("Adjusted Growth(%)")+
  ylab("Number of Reccomendations")+
  
  geom_histogram(binwidth=20,color = " dark blue",fill = "Blue")

stockHist+ theme(text = element_text(size = 20))   
#t-test
t.test(AdjGrowth, y=NULL,
       mu=0,var.equal=FALSE)
