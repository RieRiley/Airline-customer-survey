
library(ggplot2)
install.packages("dplyr") 
library(dplyr)
library(tidyr)

df_airline<- read.csv("Z:/Rie/Study/JustIT-Data Technician Bootcamp/w11 - R/test.csv")
df_head<-head(df_airline)
df_summary<-summary(df_airline)
df_summary

colSums(is.na(df_airline)) #to get the sum of missing values for each columns

which(is.na(df_airline$Arrival.Delay.in.Minutes))

#df %>% drop_na(Arrival.Delay.in.Minutes)
na.omit(df_dataset2, cols='revenue')

# Basic box plot to reveal outliers
plot1 <- ggplot(df_airline, aes(x=Age, y=Class)) + 
  geom_boxplot()
plot1

#Customer loyality and type analysis
#bar chart colour filled with legends
#ggplot(aes(x= Class, y= Count, fill= Gender)) +
geom_bar(stat= "identity", position= position_dodge()) #bar plot

ggplot(df_airline, aes(x = Class, y = Age, fill = Class ))+
  geom_bar(stat = "identity") +
  scale_fill_hue(labels = c("Business", "Eco", "Eco Plus")) 


ggplot(df_airline, aes(x = Class,  fill = Age)) + 
  geom_bar(position="stack", stat="count") +
  scale_fill_hue(labels = c("Business", "Eco", "Eco Plus")) 


#customer distribution by age:

ggplot(df, aes(x= Age, fill= Class))+
  geom_histogram(binwidth= 5, position= "dodge") #histogram
#position= position_dodge() separates the bars side-by-side based on the Class variable

ggplot(ggplot(df_airline, aes(x=id, y=Age)) + geom_point() + geom_smooth() + labs(title="Distribution of Ages of Passengers",
                                                                                  x = "ID", y = "Age of Passengers"))
       
ggplot(df_airline, aes(x=id, y=Age)) + geom_point() + labs(title="Distribution of Ages of Passengers",
                                                                  x = "ID", y = "Age of Passengers") + theme_classic() + facet_grid(Class ~ .)

#satisfaction by gender - histogram
ggplot(df_airline, aes(x=Gender, fill = satisfaction))+
  geom_histogram(stat="count")

#Proportion of loyal and disloyal customers
ggplot(df_airline, aes(x=Customer.Type)) +
  geom_bar(aes(fill= Customer.Type))

#customer loyality and satisfaction
ggplot(df_airline, aes(x= Customer.Type, fill= satisfaction))+
  geom_histogram(binwidth= 5, position= "dodge", stat="count")

#Satisfaction comparison by jessica
df_airline %>%
  group_by(Customer.Type, satisfaction) %>%
  summarise(Count= n()) %>%
  ggplot(aes(x= satisfaction, y= Count, fill= Customer.Type)) +
  geom_bar(stat= "identity", position= position_dodge()) +
  labs(title= "Customer Satisfaction by Customer Type", x= "Satisfaction", y= "Count")

#Travel details analysyis
# customer loyality with travel type
ggplot(df_airline, aes(x= Customer.Type, fill= Type.of.Travel))+
  geom_histogram(binwidth= 5, position= "identity", stat="count")

# customer class with travel type

ggplot(df_airline, aes(x = Class, y = Type.of.Travel, fill = Type.of.Travel ))+
  geom_bar(stat = "identity") +
  scale_fill_hue(labels = c("Business", "Personal")) 

# flight distance correlation type of travel and class

ggplot(df_airline, aes(x=Flight.Distance, y=Type.of.Travel,  color='red'))+
  geom_point()+
  labs(
    title='Airline customer satisfaction by type',
    
    x='Flight Distance',
    y='Type.of.Travel'
  )+
  geom_smooth(method='lm', color='purple')

#Service quality and satisfaction analysis
  #Airline services aspects ratings
Inflight.wifi.service - 2.7
Seat.comfort - 3.44
Leg.room.service
food.and.drink
Cleanliness
Inflight.entertainment

mean(df_airline$Inflight.wifi.service)
mean(df_airline$Seat.comfort)
mean(df_airline$Leg.room.service)
mean(df_airline$Food.and.drink)
mean(df_airline$Cleanliness)
mean(df_airline$Inflight.entertainment)

#service ratings and customer satisfaction


#Flight experience analysis
