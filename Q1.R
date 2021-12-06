library(ggplot2)
library(dplyr)
library(scales)
library(grid)
library(gridExtra)


df <- data.frame(Center=c("BDT", "DBA", "FMP","LTA","MVT", "OCE", "ODP", "ONK", "PLB","RDM", "SBC", "SDT", "SEP", "WBA", "WLT"),
                 Success=c(0.098, 0.133, 0.133, 0.098, 0.09, 0.2, 0.149, 0.091, 0.118, 0.103, 0.093, 0.159, 0.17, 0.084, 0.08 ),
                 Average=c(0.121, 0.121, 0.121, 0.121, 0.121, 0.121, 0.121, 0.121, 0.121, 0.121, 0.121, 0.121, 0.121, 0.121, 0.121),
                 Vaccines=c(2519, 4142, 4075, 1840, 1478, 4495, 2244, 1546, 2589, 1796, 2036, 2221, 3630, 1091, 1954),
                 Opportunities=c(25703, 31249, 30548, 18857, 16474, 22497, 15063, 17064, 21933, 17479, 21937, 13983, 21395, 13042, 24562))

                 

head(df)

avg <- 0.121

#a
p<-ggplot(data=df, aes(x=reorder(Center, -Success), y=Success)) +
  ggtitle("Succesful Opportunities by Center (FLU) ") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Center") +
  geom_bar(stat="identity", color="purple", fill="purple")+
  scale_y_continuous(labels = scales::percent) +
  geom_hline(aes(yintercept=mean(Success)),
             color="black", size=1)+
  geom_text(x=10 ,y= avg+0.005, label = "Average = 12,1%", color = "black")

p

#b
p<-ggplot(data=df, aes(x=reorder(Center, -Success), y=Success)) +
  ggtitle("Succesful Opportunities by Center (FLU) ") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Center") +
  geom_bar(stat="identity", color="purple", fill="purple")+
  scale_y_continuous(labels = scales::percent) +
  geom_hline(aes(yintercept=mean(Success)),
             color="black",linetype="dashed", size=1)+
  geom_text(x=10 ,y= avg+0.005, label = "Average = 12,1%", color = "black")

p

p<-ggplot(data=df, aes(y=Center, x=Success)) +
  ggtitle("Succesful Opportunities by Center (FLU) ") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_bar(stat="identity", color="purple", fill="purple")+
  scale_x_continuous(labels = scales::percent) +
  geom_vline(aes(xintercept=mean(Success)),
             color="black", linetype="dashed", size=1)+
  geom_text(y=15, x= avg+0.021, label = "Average = 12.1%", color = "black")

p

p<-ggplot(data=df, aes(y=reorder(Center, Success), x=Success)) +
  ggtitle("Succesful Opportunities by Center (FLU) ") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Center") +
  geom_bar(stat="identity", color="purple", fill="purple")+
  scale_x_continuous(labels = scales::percent) +
  geom_vline(aes(xintercept=mean(Success)),
             color="black", linetype="dashed", size=1)+
  geom_text(y=4, x= avg+0.021, label = "Average = 12.1%", color = "black")

p


#c

#10%
df_sub <- df %>%
  mutate(Target = Success >= 0.1)

ggplot(df_sub, aes(x = Center, y = Success, fill = Target)) +
  ggtitle("Succesful Opportunities by Center (FLU) ") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_bar(stat = "identity")+
  geom_text(x=8, y= avg+0.08, label = "Target = 10%", color = "black")

ggplot(df_sub, aes(x = reorder(Center, -Success), y = Success, fill = Target)) +
  ggtitle("Succesful Opportunities by Center (FLU) ") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Center") +
  geom_bar(stat = "identity")+
  geom_text(x=8, y= avg+0.08, label = "Target = 10%", color = "black")

ggplot(df_sub, aes(x = reorder(Center, -Success), y = Success, fill = Target)) +
  ggtitle("Succesful Opportunities by Center (FLU) ") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Center") +
  geom_bar(stat = "identity")+
  geom_text(x=10, y=0.11, label = "Target = 10%", color = "black")+
  geom_hline(aes(yintercept=0.1),
           color="black", linetype="dashed", size=1)

#25%
df_sub <- df %>%
  mutate(Target = Success >= 0.25)

ggplot(df_sub, aes(x = Center, y = Success, fill = Target)) +
  ggtitle("Succesful Opportunities by Center (FLU) ") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_bar(stat = "identity")+
  geom_text(x=8, y= avg+0.08, label = "Target = 25%", color = "black")

ggplot(df_sub, aes(x = reorder(Center, -Success), y = Success, fill = Target)) +
  ggtitle("Succesful Opportunities by Center (FLU) ") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Center") +
  geom_bar(stat = "identity") +
  geom_text(x=8, y= avg+0.08, label = "Target = 25%", color = "black")

ggplot(df_sub, aes(x = reorder(Center, -Success), y = Success, fill = Target)) +
  ggtitle("Succesful Opportunities by Center (FLU) ") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Center") +
  geom_bar(stat = "identity") +
  geom_text(x=8, y= 0.26, label = "Target = 25%", color = "black") +
  geom_hline(aes(yintercept=0.25),
             color="black", linetype="dashed", size=1)

#e


p<-ggplot(data=df, aes(x= Center, y=  1-(Vaccines/Opportunities)  )) +
  ggtitle("Succesful Opportunities by Center (FLU) ") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Center") +
  ylab("Not Vaccinated") +
  geom_bar(stat="identity", color="purple", fill="purple")+
  scale_y_continuous(labels = scales::percent) +
  geom_hline(aes(yintercept=mean(1-Success)),
             color="black", linetype="dashed", size=1)+
  geom_text(x=10 ,y= 0.92, label = "Average = 88%", color = "black")

p

p<-ggplot(data=df, aes(x= Center, y=1-Success)) +
  ggtitle("Succesful Opportunities by Center (FLU) ") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Center") +
  ylab("Not Vaccinated") +
  geom_bar(stat="identity", color="purple", fill="purple")+
  scale_y_continuous(labels = scales::percent) +
  geom_hline(aes(yintercept=mean(1-Success)),
             color="black", linetype="dashed", size=1)+
  geom_text(x=10 ,y= 0.92, label = "Average = 88%", color = "black")
  
p

#f


p<-ggplot(data=df, aes(x=Center, y=Success, label = scales::percent(Success))) +
  ggtitle("Succesful Opportunities by Center (FLU) ") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_bar(stat="identity", color="skyblue", fill="skyblue")+
  scale_y_continuous(labels = scales::percent) +
  geom_hline(aes(yintercept=mean(Success)),
             color="black", linetype="dashed", size=1)+
  geom_text(x=10, y= 0.125, label = "Average = 12.1%", color = "black")+
  geom_text(aes(label = scales::percent(Success,accuracy=0.1)),vjust = 1.5, colour = "white")

p

p<-ggplot(data=df, aes(x=reorder(Center, -Success), y=Success, label = scales::percent(Success))) +
  ggtitle("Succesful Opportunities by Center (FLU) ") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Center") +
  geom_bar(stat="identity", color="skyblue", fill="skyblue")+
  scale_y_continuous(labels = scales::percent) +
  geom_hline(aes(yintercept=mean(Success)),
             color="black", linetype="dashed", size=1)+
  geom_text(x=10, y= 0.125, label = "Average = 12.1%", color = "black")+
  geom_text(aes(label = scales::percent(Success,accuracy=0.1)),vjust = 1.5, colour = "white")

p





