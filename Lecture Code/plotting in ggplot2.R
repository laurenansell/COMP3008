library(ggplot2)
library(lubridate)
library(dplyr)

chocoloate_data<-read.csv("./Lecture Code/Chocolate Sales.csv")
pokemon_data<-read.csv("./Lecture Code/pokemon_stats_2025.csv")

chocoloate_data$Date<-as.Date(chocoloate_data$Date)

ggplot(chocoloate_data,aes(x=Date,y=Boxes.Shipped)
       )+geom_line(linewidth = 1,linetype = 2,col="hotpink")+
  theme(axis.title = element_text(size=1,colour = "yellow"),
        axis.text.x = element_text(size=20,colour = "brown"))+
  scale_y_continuous(breaks = c(0,50,75,123,200,240,273,321,400,401,451,
                                550,555,632,666,712,777))

pokemon_data %>% group_by(type_1) %>% count() %>% 
  ggplot()+geom_bar(aes(x=type_1,y=n,colour = type_1,fill = type_1),
                    stat="identity")+
  labs(x="Rainbow")+
  theme(axis.title.x = element_text(size=2,colour="gray"),
        legend.position = "top")


ggplot(pokemon_data,aes(x=height))+geom_boxplot(col="white",fill="brown") 


