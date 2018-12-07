
library(chron)
library(ggplot2)
library(sqldf)
library(doBy)
library(plyr)
require(rgdal) 
library(sp)  
library(psych)
library(dplyr)
library(MASS)
crime.data<-as.data.frame(read.csv("crime.data.csv" ,na.strings=' '))

str(crime.data)





df <-as.data.frame(sqldf("select *
                          from 'crime.data'
                          where Year>2011"))
                         #and (Beat =211 or Beat= 212)"))

crime.data <-df
crime.data <- subset(crime.data, !duplicated(crime.data$Case.Number))

head(crime.data)
crime.data$time<-as.chron(as.character(crime.data$Date), "%m/%d/%Y %H:%M")

crime.data$time<-as.POSIXlt(crime.data$time)
crime.data$time<-format(as.POSIXlt(crime.data$time, tz = "ISD"), "%H:%M:%S")
crime.data$time <- chron(times=crime.data$time)
time.tag <- chron(times=c("00:00:00", "06:00:00", "12:00:00", "18:00:00",
                            "23:59:00"))
crime.data$time.tag<-cut(crime.data$time, breaks=time.tag,labels=c("Night","Morning","Afternoon","Evening"),include.lowezst=TRUE)
count<-table(crime.data$time.tag)
plot(count,col='darkred', main='Crimes in chicago by Time of the day', lwd=15,
       type='h',ylim = c(0, max(count)*1.1), ylab = 'Total Number of crimes', xlab = 'Time of the day')
table(crime.data$Description)


crime.data$Weekday<-as.chron(as.character(crime.data$Date), "%m/%d/%Y %H:%M")
crime.data$Weekday<-as.POSIXlt(crime.data$Weekday)


#crime.data$date1<-format(as.POSIXlt(crime.data$date1, tz = "ISD"), "%A")
crime.data$Weekday<-format(as.POSIXlt(crime.data$Weekday, tz = "ISD"), "%A")

df <-as.data.frame(sqldf("select  Weekday,count(*) as count
                          from 'crime.data'
                          group by Weekday"))
                        


               
df

df$Weekday <- factor(df$Weekday,levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday","Friday", "Saturday"))
ggplot(df,aes(x = Weekday,y = count)) + 
  geom_bar(aes(fill = count),stat = "identity",position = "dodge") 
  
##################################
crime.data$Month<-as.chron(as.character(crime.data$Date), "%m/%d/%Y %H:%M")
crime.data$Month<-as.POSIXlt(crime.data$Month)
crime.data$Month<-format(as.POSIXlt(crime.data$Month, tz = "ISD"), "%b")

df <-as.data.frame(sqldf("select Month, count(*) as count
                          from 'crime.data'
                          group by Month"))




df

df$Month <- factor(df$Month,levels = c("Jan", "Feb", "Mar", "Apr", "May","Jun", "Jul","Aug", "Sep","Oct", "Nov", "Dec"))
ggplot(df,aes(x = Month,y = count)) + 
  geom_bar(aes(fill = count),stat = "identity",position = "dodge") 



crime.data$crime <- as.character(crime.data$Description)
crime.data$crime <- ifelse(crime.data$crime %in% c("CRIME SEXUAL ASSAULT", "PROSTITUTION", "SEX OFFENSE"), 'SEX', crime.data$crime)


crime.data$crime <- ifelse(crime.data$crime %in% c("MOTOR VEHICLE THEFT"),
                           "MVT", crime.data$crime)
crime.data$crime <- ifelse(crime.data$crime %in% c("GAMBLING", "INTERFERE WITH PUBLIC OFFICER", "INTERFERENCE WITH PUBLIC OFFICER", "INTIMIDATION",
                                                    "LIQUOR LAW VIOLATION", "OBSCENITY", "NON-CRIMINAL", "PUBLIC PEACE VIOLATION",
                                                    "PUBLIC INDECENCY", "STALKING", "NON-CRIMINAL (SUBJECT SPECIFIED)"),
                           "NONVIO", crime.data$crime)

crime.data$crime <- ifelse(crime.data$crime == "CRIMINAL DAMAGE", "DAMAGE",
                           crime.data$crime)
crime.data$crime <- ifelse(crime.data$crime == "CRIMINAL TRESPASS",
                           "TRESPASS", crime.data$crime)
crime.data$crime <- ifelse(crime.data$crime %in% c("NARCOTICS", "OTHER
                                                    NARCOTIC VIOLATION", "OTHER NARCOTIC VIOLATION"), "DRUG", crime.data$crime)
crime.data$crime <- ifelse(crime.data$crime == "DECEPTIVE PRACTICE",
                           "FRAUD", crime.data$crime)
crime.data$crime <- ifelse(crime.data$crime %in% c("OTHER OFFENSE", "OTHER
                                                    OFFENSE"), "OTHER", crime.data$crime)
crime.data$crime <- ifelse(crime.data$crime %in% c("KIDNAPPING", "WEAPONS
                                                    VIOLATION", "OFFENSE INVOLVING CHILDREN"), "VIO", crime.data$crime)
table(crime.data$crime)


qplot(crime.data$crime, xlab = "Crime", main ="Crimes in Chicago") +
scale_y_continuous("Number of crimes")
temp <- aggregate (crime.data$crime, by=list(crime.data$crime, crime.data$time.tag), FUN=length)

names(temp) <- c("crime", "time.tag", "count")
ggplot(temp, aes(x= crime, y= factor(time.tag))) +
geom_tile(aes(fill= count)) + scale_x_discrete("Crime", expand = c(0,0)) +
scale_y_discrete("Time of day", expand = c(0,-2)) +
scale_fill_gradient("Number of crimes", low = "white", high = "steelblue") +
theme_bw() + ggtitle("Crimes by time of day") +
theme(panel.grid.major = element_line(colour =NA), panel.grid.minor =element_line
      (colour = NA))


crime.data$Date <- as.character(crime.data$Date)
temp <- ddply(crime.data, .(crime, Weekday), summarise, count =length(Date))



ggplot(temp, aes(x=crime, y=Weekday, fill=count))+
geom_tile(aes(fill=count))+
scale_x_discrete("Crime", expand =c(0,0))+
scale_y_discrete("Day of week", expand =c(0,-2))+
scale_fill_gradient("Number of crimes", low ="white", high =
                     "steelblue")+
theme_bw()+ ggtitle("Crimes by day of week")+
theme(panel.grid.major =element_line(colour =NA), panel.grid.minor =
      element_line(colour =NA))

temp <- summaryBy(Case.Number ~ crime+ Month, data=crime.data, FUN=length) 
names(temp)[3] <- 'count'
                    ggplot(temp, aes(x=crime, y=Month, fill=count))+
                    geom_tile(aes(fill=count))+
                    scale_x_discrete("Crime", expand =c(0,0))+
                    scale_y_discrete("Month", expand =c(0,-2))+
                    scale_fill_gradient("Number of crimes", low ="white", high ="steelblue")+
                    theme_bw()+ ggtitle("Crimes by Month")+ theme(panel.grid.major =element_line
                                                                    (colour =NA), panel.grid.minor =element_line(colour =NA))

                    
                    
                


crime.agg <- ddply(crime.data, .(crime, Arrest, Beat, Date,X.Coordinate,Y.Coordinate,
                                 Y.Coordinate, time.tag, Weekday, Month), summarise, count = length(Date),
                   .progress= 'text')  

today <- crime.agg[1, 'Date']
crime.agg$X.COORDINATE <- as.numeric(crime.agg$X.Coordinate)
crime.agg$Y.COORDINATE <- as.numeric(crime.agg$Y.Coordinate)


length(unique(crime.agg$Beat))
length(unique(crime.agg$Date))
length(unique(crime.agg$Date))
beats <- sort(unique(crime.agg$Beat))
dates <- sort(as.character(unique(crime.agg$Date)))
temp <- expand.grid(beats, dates)
names(temp) <- c("Beat", "Date")
temp <- orderBy(~ Beat, data=temp)
model.data <- aggregate(crime.agg[, c('count', 'Arrest')], by =
                          list(crime.agg$Beat, as.character(crime.agg$Date)), FUN= sum)


 names(model.data) <- c("Beat", "Date", "count", "ARREST")
 model.data <- merge(temp, model.data, by= c('Beat', 'Date'), all.x= TRUE)
 head(model.data)
 model.data$count[is.na(model.data$count)] <- 0
 model.data$Beat[is.na(model.data$Beat)] <- 0
 model.data$ARREST[is.na(model.data$ARREST)] <- 0
 
 
 
 model.data$day<-as.chron(as.character(model.data$Date), "%m/%d/%Y %H:%M")
 model.data$day<-as.POSIXlt(model.data$day)
 model.data$day<-format(as.POSIXlt(model.data$day, tz = "ISD"), "%b")
 
 model.data$month<-as.chron(as.character(model.data$Date), "%m/%d/%Y %H:%M")
 model.data$month<-as.POSIXlt(model.data$month)
 model.data$month<-format(as.POSIXlt(model.data$month, tz = "ISD"), "%b")
 
 

 pastDays <- function(x) {
   c(0, rep(1, x))
 }
 
 pastDays(3)
 
 filter_.default <- function(.data, ..., .dots) {
   fargs <- formals(stats::filter)
   dnames <- names(.dots)
   pos.args <- lapply(.dots[nchar(dnames) == 0], identity)
   named.args <- lapply(.dots[dnames %in% names(fargs)], identity)
   call.args <- lapply(c(pos.args, named.args), lazyeval::lazy_eval)
   do.call(stats::filter, c(list(.data), call.args))
 }
 
 model.data1<- as.data.frame(model.data)

model.data$Beat<-as.numeric(model.data$Beat)
model.data$count<-as.numeric(model.data$count)

model.data$past.crime.1 <- ave(model.data$count, model.data$Beat,
                                  FUN= function(x) dplyr::filter(x, pastDays(1), sides=1))

model.data$past.crime.7 <- ave(model.data$count, model.data$Beat,
                                  FUN= function(x) dplyr::filter(x, pastDays(7), sides=1))

model.data$past.crime.30 <- ave(model.data$count, model.data$Beat,
                                FUN= function(x) filter(x, pastDays(30), sides=1))

meanNA <- function(x){
  mean(x, na.rm= TRUE)
}



model.data$past.crime.1 <- ifelse(is.na(model.data$past.crime.1),
                                    meanNA(model.data$past.crime.1), model.data$past.crime.1)


model.data$past.crime.7 <- ifelse(is.na(model.data$past.crime.7),
                                    meanNA(model.data$past.crime.7), model.data$past.crime.7)
model.data$past.crime.30 <- ifelse(is.na(model.data$past.crime.30),
                                     meanNA(model.data$past.crime.30), model.data$past.crime.30)


model.data$past.arrest.30 <- ave(model.data$ARREST, model.data$Beat,
                                 FUN= function(x) filter(x, pastDays(30), sides= 1))
model.data$past.arrest.30 <- ifelse(is.na(model.data$past.arrest.30),
                                      meanNA(model.data$past.arrest.30), model.data$past.arrest.30)
cor(model.data$past.crime.30, model.data$past.arrest.30)

model.data$policing <- ifelse(model.data$past.crime.30 == 0, 0,
                              model.data$past.arrest.30/model.data$past.crime.30)

model.data$crime.trend <- ifelse(model.data$past.crime.30 == 0, 0,
                                 model.data$past.crime.7/model.data$past.crime.30)
 model.data$season <- as.factor(ifelse(model.data$month %in% c("Mar", "Apr",
                                                                 "May"), "spring",
                                        ifelse(model.data$month %in% c("Jun", "Jul",
                                                                        "Aug"), "summer",
                                               ifelse(model.data$month %in% c("Sep", "Oct",
                                                                               "Nov"), "fall", "winter"))))

 
 
 model.cor <- cor(model.data[, c('count', 'past.crime.1', 'past.crime.7',
                                    'past.crime.30','policing', 'crime.trend')])
  cor.plot(model.cor) 
  

model.data <- orderBy(~ Date, data= model.data)
rows <- c(1:floor(nrow(model.data)*0.9)) 
test.data <- model.data[-rows, ]
model.data <- model.data[rows, ]  

test <- rbind(model.data[1, ] , model.data)

test <- test[-1,]
mean(model.data$count)

 var(model.data$count)
 
 foo <- function() {
   m0 <- glm.nb(y~group)
   if (m0$th.warn == "iteration limit reached") {
     m0 <- glm(y~group, family=poisson)
   }
   m0
 }

crime.model <- glm.nb(count ~ past.crime.1 + past.crime.7 + past.crime.30 +
                        + policing + crime.trend + factor(day) + season, data= model.data)
summary(crime.model)
crime.model.pred <- predict(crime.model, test.data, type= "response")
sqrt(mean((test.data$count - crime.model.pred)^2))

crime.model <- glm.nb(count ~ past.crime.1 + past.crime.7 + past.crime.30 +
                      + crime.trend + policing + factor(day) + season  + I(past.crime.30^2),
                      data= model.data)
summary(crime.model)
crime.model.pred <- predict(crime.model, test.data, type¼ “response”)
sqrt(mean((test.data$count - crime.model.pred)^2))



