library(dplyr) 
library(plotly)       # plotly is dependent on dplyr
library(tidyverse)
library(tidyr)
library(plyr)
library(ggplot2)
library(stringr)
###using themes
library(ggthemes)
brew <- read.csv(file.choose(),header = TRUE)
brew

# 1 breweries per state

brewState <- brew %>% group_by(State)%>% summarize(count = count(State))


#2. Merge beer data with the breweries data. Print the first 6 observations and the last six observations to check the merged file.  (RMD only, this does not need to be included in the presentation or the deck.)

#Import Beers data
beers <- read.csv(file.choose(),header = TRUE)
beers

#Observe that ID on beers - Brewery_id and and breweries - Brew_ID  - we rename by Brew_ID

mergeData = inner_join(brew,beers, by = c("Brew_ID" = "Brewery_id"))
names(mergeData)

mergeData <- dplyr::rename(mergeData, brewName = Name.x)
mergeData <- dplyr::rename(mergeData, beerName = Name.y)

#3. Address the missing values in each column.

#mergeData %>% summarise_all(fun(sum(is.na(.))))
colSums(is.na(mergeData))

 #4.Compute the median alcohol content and international bitterness unit for each state. Plot a bar chart to compare.


#we will create 2 df for ABV not NA and IBU not NA - we dont want 1 df for both because we loose a lot of data - if we considerABV,... only 62 deleted and IBU

mergeData.cleanABV <- mergeData %>% filter(!is.na(ABV))
colSums(is.na(mergeData.cleanABV))
summary(mergeData.cleanABV)
#MedianABV <- mergeData.cleanABV %>% group_by(State) %>% summarise(medABV = median(ABV), State = count(State))
MedianABV <- ddply(mergeData.cleanABV, .(State), summarise, medABV=median(ABV))
MedianABV %>% ggplot(aes(fill=State, y=medABV, x=State)) + geom_bar(stat="identity")

mergeData.cleanIBU <- mergeData %>%  filter(!is.na(IBU))
#MedianIBU <- mergeData.cleanIBU %>% group_by(State) %>% summarise(medIBU = median(c(IBU)))
MedianIBU <- ddply(mergeData.cleanIBU, .(State), summarise, medIBU=median(IBU))
MedianIBU %>% ggplot(aes(fill=State, y=medIBU, x=State)) + geom_bar(stat="identity")

# 5.Which state has the maximum alcoholic (ABV) beer? Which state has the most bitter (IBU) beer?

mergeData.cleanABV %>% filter(ABV == max(ABV)) %>% select(State,ABV)
mergeData.cleanIBU %>% filter(IBU == max(IBU)) %>% select(State,IBU)


# 6.Comment on the summary statistics and distribution of the ABV variable. ???
summary(mergeData.cleanABV)





######################## Using maps for illustration #######################
library(maps)
map_data('usa')
map()
usa = map_data('usa')

us_states <- map_data("state")

map <- read.csv(file.choose(),header = TRUE,sep=";")  #This is the usa coordinates csv file

names(map)

#map <- map %>% select(State, Latitude, Longitude) %>% group_by(State) %>% summarise(long = median(Longitude))
map <- ddply(map, .(State), summarise, Long=median(Longitude), Lat = median(Latitude))

coords <- map

# Add coordinate to merge data by join
mergeData.clean = mergeData.cleanABV  %>% filter(!is.na(IBU))
mergeData.clean$State <- str_trim(mergeData.clean$State)
mergeData.clean$State <- as.character(mergeData.clean$State)
coords$State <- as.character(coords$State)
newData <- inner_join(mergeData.clean,coords, by = c("State" = "State"))


# Question 1
brewState$count$x <- as.character(str_trim(brewState$count$x))

g = c(brewState$count$x) 
e = c(brewState$count$freq)
Q = data.frame(State = g, Count = e)

Q$State = as.character(Q$State)
Q1 = inner_join(newData,Q, by = c("State" = "State"))
Q1 = distinct(Q1,State,.keep_all = TRUE)
Q1range = Q1 %>% filter(Count == max(Count) | Count == min(Count)) #select only max an min count
Q1rangew = Q1 %>% filter(Count != max(Count) & Count != min(Count)) # filter out max and min
theme_set(theme_dark())
ggplot() + geom_polygon(data = us_states, aes(long,lat,group=group), fill = 'blue', color = 'black') + geom_text(data = Q1,aes(Long,Lat, label = State),hjust = 0, nudge_x = 0, color = 'black', size = 2) + geom_text(data = Q1rangew,aes(Long,Lat, label = Count),hjust = 0, nudge_x = 0.2, nudge_y = -0.8, color = 'white', size = 2.5)  + geom_text(data = Q1range,aes(Long,Lat, label = Count),hjust = 0, nudge_x = 0.2, nudge_y = -0.8, color = 'red', size = 2.5)

#### Map without Alaska and Hawaii
Q1w = Q1 %>% filter(State != 'AK' & State != 'HI')
Q1wrange = Q1w %>% filter(Count == max(Count) | Count == min(Count)) # filter in only max and min
Q1wrangew = Q1w %>% filter(Count != max(Count) & Count != min(Count)) # filter out max and min
ggplot() + geom_polygon(data = us_states, aes(long,lat,group=group), fill = 'blue', color = 'black') + geom_text(data = Q1w,aes(Long,Lat, label = State),hjust = 0, nudge_x = 0, color = 'black', size = 2) + geom_text(data = Q1wrangew,aes(Long,Lat, label = Count),hjust = 0, nudge_x = 0.2, nudge_y = -0.3, color = 'white', size = 2.3) + geom_text(data = Q1wrange,aes(Long,Lat, label = Count),hjust = 0, nudge_x = 0.2, nudge_y = -0.3, color = 'red', size = 2.5)

## Question 4 ######
###### Using maps for the median data ########
MedianABV$State <- str_trim(as.character(MedianABV$State))
MedianIBU$State <- str_trim(as.character(MedianIBU$State))
Q4 = inner_join(newData,MedianABV, by = c("State" = "State"))
Q4 = inner_join(Q4,MedianIBU, by = c("State" = "State"))
Q4 = distinct(Q4,State,.keep_all = TRUE)
Q4ABVrange = Q4 %>% filter(medABV == max(medABV) | medABV == min(medABV)) # filter in max and min ABV
Q4IBUrange = Q4 %>% filter(medIBU == max(medIBU) | medIBU == min(medIBU)) # filter in max and min IBU

Q4ABVrangeWO = Q4 %>% filter(medABV != max(medABV) & medABV != min(medABV)) ## filter out min max ABV
Q4IBUrangeWO = Q4 %>% filter(medIBU != max(medIBU) & medIBU != min(medIBU)) # filter out min max IBU

ggplot() + geom_polygon(data = us_states, aes(long,lat,group=group), fill = 'blue', color = 'black') + geom_text(data = Q4,aes(Long,Lat, label = State),hjust = 0, nudge_x = 0, color = 'black', size = 2) + geom_text(data = Q4ABVrangeWO,aes(Long,Lat, label = medABV),hjust = 0, nudge_x = 0, nudge_y = -0.5, color = 'white', size = 2.5) + geom_text(data = Q4ABVrange,aes(Long,Lat, label = medABV),hjust = 0, nudge_x = 0, nudge_y = -0.5, color = 'red', size = 2.5)

ggplot() + geom_polygon(data = us_states, aes(long,lat,group=group), fill = 'blue', color = 'black') + geom_text(data = Q4,aes(Long,Lat, label = State),hjust = 0, nudge_x = 0, color = 'black', size = 2) + geom_text(data = Q4IBUrangeWO,aes(Long,Lat, label = medIBU),hjust = 0, nudge_x = 0.1, nudge_y = -0.5, color = 'white', size = 2.5) + geom_text(data = Q4IBUrange,aes(Long,Lat, label = medIBU),hjust = 0, nudge_x = 0, nudge_y = -0.5, color = 'red', size = 2.5)

# Now lets take the outlier states to make the gragh look better.
Q4w = Q4 %>% filter(State != 'AK' & State != 'HI')
Q4wABVrange = Q4w %>% filter(medABV == max(medABV) | medABV == min(medABV)) #filter in min max
Q4wIBUrange = Q4w %>% filter(medIBU == max(medIBU) | medIBU == min(medIBU)) #Filter in min max IBU

Q4wABVrangeWO = Q4w %>% filter(medABV != max(medABV) & medABV != min(medABV)) #filter out min max
Q4wIBUrangeWO = Q4w %>% filter(medIBU != max(medIBU) & medIBU != min(medIBU)) #Filter out min max IBU

ggplot() + geom_polygon(data = us_states, aes(long,lat,group=group), fill = 'blue', color = 'black') + geom_text(data = Q4w,aes(Long,Lat, label = State),hjust = 0, nudge_x = 0, color = 'black', size = 2) + geom_text(data = Q4wABVrangeWO,aes(Long,Lat, label = medABV),hjust = 0, nudge_x = 0, nudge_y = -0.5, color = 'white', size = 2.3) + geom_text(data = Q4wABVrange,aes(Long,Lat, label = medABV),hjust = 0, nudge_x = 0, nudge_y = -0.5, color = 'red', size = 2.5)

ggplot() + geom_polygon(data = us_states, aes(long,lat,group=group), fill = 'blue', color = 'black') + geom_text(data = Q4w,aes(Long,Lat, label = State),hjust = 0, nudge_x = 0, color = 'black', size = 2) + geom_text(data = Q4wIBUrangeWO,aes(Long,Lat, label = medIBU),hjust = 0, nudge_x = 0.1, nudge_y = -0.5, color = 'white', size = 2.3) + geom_text(data = Q4wIBUrange,aes(Long,Lat, label = medIBU),hjust = 0, nudge_x = 0, nudge_y = -0.5, color = 'red', size = 2.5)

###### Question 5 ##########
Q5 <- Q4
Q5ABVrange = Q5 %>% filter(ABV == max(ABV) | ABV == min(ABV))
Q5IBUrange = Q5 %>% filter(IBU == max(IBU) | IBU == min(IBU))

# select the states that has the max and min. use distinct to avoid repetition. we should only have 2 rows of min and max data.
firstABV = distinct(Q5ABVrange,ABV,.keep_all = TRUE)$State[1] #first state
secondABV = distinct(Q5ABVrange,ABV,.keep_all = TRUE)$State[2] #second state
firstIBU = distinct(Q5IBUrange,IBU,.keep_all = TRUE)$State[1]
secondIBU = distinct(Q5IBUrange,IBU,.keep_all = TRUE)$State[2]

# exclude the states with min and max values
Q5ABVrangeWO = Q5 %>% filter(ABV != max(ABV) & ABV != min(ABV) & State != firstABV & State != secondABV) 
Q5IBUrangeWO = Q5 %>% filter(IBU != max(IBU) & IBU != min(IBU) & State != firstIBU & State != secondIBU)



ggplot() + geom_polygon(data = us_states, aes(long,lat,group=group), fill = 'blue', color = 'black') + geom_text(data = Q5,aes(Long,Lat, label = State),hjust = 0, nudge_x = 0, color = 'black', size = 2) + geom_text(data = Q5ABVrangeWO,aes(Long,Lat, label = ABV),hjust = 0, nudge_x = 0, nudge_y = -0.5, color = 'white', size = 2) + geom_text(data = Q5ABVrange,aes(Long,Lat, label = ABV),hjust = 0, nudge_x = 0, nudge_y = -0.5, color = 'red', size = 2)

ggplot() + geom_polygon(data = us_states, aes(long,lat,group=group), fill = 'blue', color = 'black') + geom_text(data = Q5,aes(Long,Lat, label = State),hjust = 0, nudge_x = 0, color = 'black', size = 2) + geom_text(data = Q5IBUrangeWO,aes(Long,Lat, label = IBU),hjust = 0, nudge_x = 0.1, nudge_y = -0.5, color = 'white', size = 2) + geom_text(data = Q5IBUrange,aes(Long,Lat, label = IBU),hjust = 0, nudge_x = 0, nudge_y = -0.5, color = 'red', size = 2)


### Question 5 without AK and HI
Q5 <- Q4w
Q5ABVrange = Q5 %>% filter(ABV == max(ABV) | ABV == min(ABV))
Q5IBUrange = Q5 %>% filter(IBU == max(IBU) | IBU == min(IBU))

# select the states that has the max and min. use distinct to avoid repetition. we should only have 2 rows of min and max data.
firstABV = distinct(Q5ABVrange,ABV,.keep_all = TRUE)$State[1] #first state
secondABV = distinct(Q5ABVrange,ABV,.keep_all = TRUE)$State[2] #second state
firstIBU = distinct(Q5IBUrange,IBU,.keep_all = TRUE)$State[1]
secondIBU = distinct(Q5IBUrange,IBU,.keep_all = TRUE)$State[2]

# exclude the states with min and max values
Q5ABVrangeWO = Q5 %>% filter(ABV != max(ABV) & ABV != min(ABV) & State != firstABV & State != secondABV) 
Q5IBUrangeWO = Q5 %>% filter(IBU != max(IBU) & IBU != min(IBU) & State != firstIBU & State != secondIBU)


ggplot() + geom_polygon(data = us_states, aes(long,lat,group=group), fill = 'blue', color = 'black') + geom_text(data = Q5,aes(Long,Lat, label = State),hjust = 0, nudge_x = 0, color = 'black', size = 2)  + geom_text(data = Q5ABVrange,aes(Long,Lat, label = ABV),hjust = 0, nudge_x = 0, nudge_y = -0.5, color = 'red', size = 3)
ggplot() + geom_polygon(data = us_states, aes(long,lat,group=group), fill = 'blue', color = 'black') + geom_text(data = Q5,aes(Long,Lat, label = State),hjust = 0, nudge_x = 0, color = 'black', size = 2) + geom_text(data = Q5IBUrange,aes(Long,Lat, label = IBU),hjust = 0, nudge_x = 0, nudge_y = -0.5, color = 'red', size = 3)


