library(data.table)
library(ggplot2)
library(scales)
library(gridExtra)
library(choroplethr)
library(choroplethrMaps)
library(forecast)
library(xts)

MyPath<-setwd("C:\\Users\\Caio Laptop\\OneDrive - The University of Kansas\\Documents\\PhD\\11. Courses\\19. EECS 731 - Introduction to Data Science\\Final Project")
colum <- c("enddate","grade","state","population","rawpoll_clinton","rawpoll_trump","samplesize","adjpoll_clinton","adjpoll_trump","type")

poll <- fread("C:\\Users\\Caio Laptop\\OneDrive - The University of Kansas\\Documents\\PhD\\11. Courses\\19. EECS 731 - Introduction to Data Science\\Final Project\\presidential_polls.csv",
              stringsAsFactors = T)
poll$enddate <- as.Date(poll$enddate, "%m/%d/%Y") # Format date
poll$month <- as.Date(cut(poll$enddate,breaks = "month"))
poll <- poll[order(enddate)] # Order by Date

poll_by_grade <- poll %>% group_by(grade)

ggplot(data = poll, aes(month)) + 
  geom_smooth(aes(y = adjpoll_clinton, colour = "Clinton",fill="Adj")) + 
  geom_smooth(aes(y = adjpoll_trump, colour = "Trump",fill="Adj")) + 
  # geom_smooth(aes(y = rawpoll_clinton, colour = "Clinton",fill="Raw")) + 
  # geom_smooth(aes(y = rawpoll_trump, colour = "Trump",fill="Raw")) + 
  scale_x_date(labels = date_format("%Y-%m"),
               date_breaks = "1 month")  + 
  labs(x = "Month", y = "Averaged Poll Results (%)", 
       title = "Presidential Election Poll Results From 12/2015 - 11/2016")

ggplot(data = poll, aes(month)) + 
  aes(y = adjpoll_clinton, colour = "Clinton",fill="Adj") +
  aes(y = adjpoll_trump, colour = "Trump",fill="Adj") +
  scale_x_date(labels = date_format("%Y-%m"),
               date_breaks = "1 month")  + 
  labs(x = "Month", y = "Averaged Poll Results (%)", 
       title = "Presidential Election Poll Results From 12/2015 - 11/2016")

#Poll Results by Grade

##()
ggplot(poll,aes(month,group=month,alpha=.3))+
  geom_boxplot(aes(y = adjpoll_clinton, colour = "Clinton"))+
  geom_boxplot(aes(y = adjpoll_trump, colour = "Trump")) + 
  scale_x_date(labels = date_format("%Y-%m"),
               date_breaks = "1 month")+
  facet_wrap(~grade)+labs(x = "Month", y = "Averaged Poll Results (%)", 
                          title = "Presidential Election Poll Results From 12/2015 - 11/2016")


#================================================================
#Time Series
xts_poll<-xts(poll, poll$enddate)
Clinton_Polls<-xts(poll$rawpoll_clinton, poll$enddate)
Trump_Polls<-xts(poll$rawpoll_trump, poll$enddate)
Clinton_Trump_Polls<-merge(Clinton_Polls,Trump_Polls)

autoplot(xts_poll[,c("rawpoll_clinton","rawpoll_trump")]) +
  ggtitle("Clinton Polls") +
  xlab("Time") +
  ylab("Percentage")

autoplot(Clinton_Trump_Polls) + colours(distinct = TRUE) +
  ggtitle("Clinton/Trump Polls") +
  xlab("Time") +
  ylab("Percentage")


fit_clinton <- auto.arima(as.vector(Clinton_Trump_Polls[,"Clinton_Polls"]))
fit_clinton %>% forecast(h=10) %>% autoplot(include=80)
checkresiduals(fit_clinton)

fit_trump <- auto.arima(as.vector(Clinton_Trump_Polls[,"Trump_Polls"]))
fit_trump %>% forecast(h=10) %>% autoplot(include=80)
checkresiduals(fit_trump)
autoplot(fit_trump)

fit <- auto.arima(as.vector(Clinton_Trump_Polls))
fit %>% forecast(h=10) %>% autoplot(include=80)

# arima(as.vector(Clinton_Trump_Polls[,"Clinton_Polls"]), order=c(1,0,0))
#================================================================

#================================================================
#Markov Models
us_presid_results_1828_2016 <- fread("C:\\Users\\Caio Laptop\\OneDrive - The University of Kansas\\Documents\\PhD\\11. Courses\\19. EECS 731 - Introduction to Data Science\\Final Project\\US Presidential Results & PVIs by State 1828-2016_edited.csv",
                                      stringsAsFactors = T)
us_presid_results_1828_2016<-us_presid_results_1828_2016[,-(4:6)]
# us_presid_results_1828_2016<-us_presid_results_1828_2016[!(us_presid_results_1828_2016$State=="Nationwide")]

us_presid_results_1828_2016_by_state <- us_presid_results_1828_2016 %>% group_by(State)
us_presid_results_1828_2016_by_state2 <- split(us_presid_results_1828_2016, us_presid_results_1828_2016$State)

myStates<-names(us_presid_results_1828_2016_by_state2)
myYears<-unique(us_presid_results_1828_2016$Year)
min(myYears)

for (i in 1:length(us_presid_results_1828_2016_by_state2)){
  my_state<-as.data.frame(us_presid_results_1828_2016_by_state2[i] , stringsAsFactors = T)[,-3]
  colnames(my_state)<-c("Winner","Year")
  assign(names(us_presid_results_1828_2016_by_state2[i]), my_state)
}
rm(i, my_state)

library(markovchain)

Errors_matrix<-matrix(NA, nrow = (length(myYears)-2), ncol= 2)
Predictions_2020<-NULL
errors_states<-NULL

for (j in 3:(length(myYears)-1)){
  
  if(j==(length(myYears)-1)){
    pdf(paste(MyPath,"/Markov",min(myYears)+4*(j+1),".pdf", sep=""), width=15, height=40)
    par(mfrow=c(13,4))
    par(mar=c(5,5,5,5))
  }  
  
  error=0
  for(i in 1:length(us_presid_results_1828_2016_by_state2)){
    mysequence<-subset(get(myStates[i]), Year<=min(myYears)+4*(j-1))[,1]
    cons_matrix<-createSequenceMatrix(mysequence, possibleStates = c("dem", "rep"))
    myFit<-markovchainFit(data=mysequence,confidencelevel = .9, possibleStates = c("dem", "rep"))
    Realized<-as.character(subset(get(myStates[i]), Year==min(myYears)+4*j)[1],stringsAsFactors =F)
    if(Realized==1){Realized=c("dem")}
    if(Realized==2){Realized=c("rep")}
    myPrediction<-predict(myFit$estimate, newdata=Realized, n.ahead =1)
    print(paste("prediction for state: ", myStates[i], " | Year = ", min(myYears)+4*(j+1), " is ", myPrediction, sep="")) 
    
    if(j==(length(myYears)-1)){
      Predictions<-c(myStates[i], myPrediction)
      Predictions_2020<-rbind(Predictions_2020, Predictions)
    }else{
      Realized_t1<-subset(get(myStates[i]), Year==(min(myYears)+4*(j+1)))[1]
      if(Realized_t1==1){Realized_t1=c("dem")}
      if(Realized_t1==2){Realized_t1=c("rep")}
      
      if(myPrediction==Realized_t1){
        error=error
      }else{ 
        error=error+1 
        errors_states<-rbind(errors_states, myStates[i])
        }
    }
    
    if(j==(length(myYears)-1)){  
      test<-as.matrix(myFit$estimate@transitionMatrix)
      dtmcA <- new("markovchain",transitionMatrix=test, states=c("dem","rep"), name="MarkovChain A")
      plot(dtmcA, cex=1, label.cex=5, label.font= 2, size=4, size2=4, arrow.size=1, #cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
           main=paste("Markov chain plot - Transition Matrix - ", myStates[i], sep=""),
           edge.arrow.size=1)
    }  
  }
  
  if (j<(length(myYears)-1)){
  Errors_matrix[j,1] <- (min(myYears)+4*(j+1))
  Errors_matrix[j,2] <- error
  }else{dev.off()}
}

#Cleaning NAs and defining my data.frame for the number of errors
Errors_matrix<-Errors_matrix[-(1:2),]
colnames(Errors_matrix) <- c("Year", "Number_Errors")
Errors_matrix<-as.data.frame(Errors_matrix)

# Plot the number of Errors per Election from 1916 to 2016
ggplot(data=Errors_matrix, aes(Year)) +
  geom_smooth(aes(y=Number_Errors, color = "red") ,show.legend = FALSE)

#Defining my data.frame of number of errors per state 
errors_states<-as.data.frame(errors_states)  
colnames(errors_states) <- c("State")

# Plot Number of Errors per State (1 - Flipped w/ legend)
ggplot(errors_states) +
  stat_count(mapping = aes(x = State, fill = State)) +
  coord_flip() +
  theme(legend.position = "top")

# Plot Number of Errors per State (2 - Flipped without legend)
ggplot(errors_states) +
  stat_count(mapping = aes(x = State, fill = State)) +
  coord_flip()

# Plot Number of Errors per State (3 - Flipped with legend)
ggplot(errors_states) +
  stat_count(mapping = aes(x = State, fill = State)) +
  theme(legend.position = "top")

# Plot Number of Errors per State (4 - Flipped without legend)
ggplot(errors_states) +
  stat_count(mapping = aes(x = State, fill = State))



Predictions_2020 <- apply(Predictions_2020,2,tolower)
Predictions_2020 <-as.data.frame(Predictions_2020, stringsAsFactors = TRUE)
colnames(Predictions_2020) <-c("State", "Pred_winner")
Predictions_2020_by_party <- split(Predictions_2020, Predictions_2020$Pred_winner)

library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)

all_states <- map_data("state") 
states_demographic  <-as.vector(Predictions_2020_by_party$dem[,1])
states_republican  <- as.vector(Predictions_2020_by_party$rep[,1])

ggplot(states, aes(x=long, y=lat, group = group)) +
  geom_polygon(fill="grey", colour = "white") +
  geom_polygon(fill="blue", data = filter(states, region %in% states_demographic), color = "white") +
  geom_polygon(fill="red", data = filter(states, region %in% states_republican), color = "white") +
  labs(title = "2020 Presidential Election using Markov Chain")

#================================================================
###---(1)---
## Sketches

# states <- map_data("state")
# dim(states)
# head(states)
# ggplot(data = states) + 
#   geom_polygon(aes(x = long, y = lat, fill = region, group = group), color = "white") + 
#   coord_fixed(1.3) +
#   guides(fill=FALSE)  # do this to leave off the color legend

###---(2)---
## Sketches
## https://www.r-bloggers.com/presidential-election-predictions-2016-an-asa-competition/
# library(XML)
# 
# url = "http://projects.fivethirtyeight.com/2016-election-forecast/national-polls/"
# doc <- htmlParse(url, useInternalNodes = TRUE)
# 
# sc = xpathSApply(doc, "//script[contains(., 'race.model')]", 
#                  function(x) c(xmlValue(x), xmlAttrs(x)[["href"]]))
# 
# jsobj = gsub(".*race.stateData = (.*);race.pathPrefix.*", "\\1???, sc)
# 
# data = fromJSON(jsobj)
# allpolls <- data$polls
# 
# #unlisting the whole thing
# indx <- sapply(allpolls, length)
# pollsdf <- as.data.frame(do.call(rbind, lapply(allpolls, `length<-`, max(indx))))