
library(ggplot2)
library(lubridate)
library(scales)

ny = read.csv('new_york_city.csv')
wash = read.csv('washington.csv')
chi = read.csv('chicago.csv')

head(ny)

head(wash)

head(chi)

# Add Gender and Birth.Year to wash dataset

wash$Gender <- NA
wash$Birth.Year <- NA

head(wash)

#Add a 'City' column

ny$City <- 'New York City'
wash$City <- 'Washington'
chi$City <- 'Chicago'

# Use rbind to add all rows of data into the city data frame.

all_data <- rbind(ny, chi, wash)

sample <- all_data[sample(nrow(all_data), size=10),]
sample

#Change the Start.Time to correct datatypes. 

all_data$Weekday <- weekdays(as.Date(all_data$Start.Time))
table(all_data$Weekday)

all_data$Day.Of.Month <- day(as.Date(all_data$Start.Time))
table(all_data$Day.Of.Month)

options(repr.plot.width=20, repr.plot.height=8)

ggplot (aes(x=Day.Of.Month), data=all_data) +
    geom_histogram (binwidth = 1, alpha=0.8) +
    scale_x_continuous(breaks= 1:31)+
    theme(text = element_text(size = 25))+ 
    labs(title = "Distribution of Rides per Day",
         x = 'Day of the Month',
         y = 'Count')

ggplot(aes(x=Weekday, fill=City), data=subset(all_data, !(is.na(Weekday)))) +
    geom_bar(position='dodge') +
    theme(text = element_text(size = 25))+ 
    labs(title = "Most Popular Weekdays per City",
         x = 'Day of the week',
         y = 'Count')

cust_count <- table(all_data$City)
cust_count

options(repr.plot.width=15, repr.plot.height=8)

barplot(cust_count, main="Customers Per City", xlab='City', ylab='Count', 
        names=c('Chicago', 'New York City', 'Washington'),
       col = c('cornsilk', 'royalblue', 'forestgreen'), 
       border=TRUE)



options(repr.plot.width=20, repr.plot.height=10)

ggplot(data=subset(all_data, !(is.na(User.Type)|User.Type=="")), aes(x= User.Type, fill='City')) +
    geom_bar(alpha= 0.9) +
    facet_wrap(~City)+
    theme_bw()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "none") +
    theme(text = element_text(size = 25))+ 
    labs(title = "User Type per City",
         x = 'User Type',
         y = 'Count')


summary(all_data$Trip.Duration)

#Trip duration is currently in seconds, to explore further I want to convert it to minutes. 

all_data$Ride.Mins <- all_data$Trip.Duration/60


options(repr.plot.width=20, repr.plot.height=15)

ggplot(all_data, aes(x=City, y=Ride.Mins, fill = City))+
    geom_boxplot(show.legend=FALSE,
                 outlier.shape= NA)+
    theme(text = element_text(size = 25))+
    scale_y_continuous(limits = c(0, 40))+ 
    labs(title = "Boxplot of Ride Minutes per City",
         x = 'City',
         y = 'Ride Duration in Minutes')


options(repr.plot.width=20, repr.plot.height=10)

ggplot(aes(x=Ride.Mins), data=all_data)+
    geom_histogram(binwidth=4)+
    scale_x_continuous(limits=c(0, 75), breaks=seq(0, 75, 5))+
    theme(text = element_text(size = 25))+ 
    labs(title = "Distribution of Trip Duration in Minutes",
         x = 'Ride in Minutes',
         y = 'Count')

ggplot(aes(x=Ride.Mins), data=subset(all_data, User.Type!=""))+
    geom_histogram(binwidth=4, fill='blue', alpha=0.5)+
    scale_x_continuous(limits=c(0, 75), breaks=seq(0, 75, 5))+
    facet_wrap(~User.Type)+
    theme(text = element_text(size = 23))+
    theme_bw()+
    theme(legend.position = "none")+
    theme(text = element_text(size = 25))+ 
    theme(strip.background = element_rect(fill='lavender'))+
    labs(title = "Distribution of Trip Duration in Minutes by Customer Type",
         x = 'Ride in Minutes',
         y = 'Count')


options(repr.plot.width=20, repr.plot.height=25)

ggplot(aes(x=Ride.Mins), data=subset(all_data, User.Type!=""))+
    geom_histogram(binwidth=4, fill='orange', alpha=0.7)+
    scale_x_continuous(limits=c(0, 75), breaks=seq(0, 75, 5))+
    facet_grid(vars(City), vars(User.Type))+
    theme(text = element_text(size = 23))+
    theme_bw()+
    theme(legend.position = "none")+
    theme(text = element_text(size = 25))+ 
    labs(title = "Distribution of Trip Duration in each City by Customer Type",
         x = 'Ride in Minutes',
         y = 'Count')


system('python -m nbconvert Explore_bikeshare_data.ipynb')
