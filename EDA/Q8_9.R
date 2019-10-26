brewState <- brew %>% group_by(State)%>% summarize(count = count(State))
brew %>% filter(State == 'SD')

#2. Merge beer data with the breweries data. Print the first 6 observations and the last six observations to check the merged file.  (RMD only, this does not need to be included in the presentation or the deck.)

#Import Beers data
beers <- read.csv(file.choose(),header = TRUE)
beers



#Observe that ID on beers - Brewery_id and and breweries - Brew_ID  - we rename by Brew_ID

mergeData = inner_join(brew,beers, by = c("Brew_ID" = "Brewery_id"))
names(mergeData)

mergeData <- dplyr::rename(mergeData, brewName = Name.x)
mergeData <- dplyr::rename(mergeData, beerName = Name.y)

mergeData.clean <- mergeData %>% filter(!is.na(ABV) &!is.na(IBU))
view(mergeData.clean)

typeAles <- cbind(mergeData.clean, type='Ales', stringsAsFactors=F) %>% filter(grepl('\\bale\\b', Style, ignore.case=T) | grepl('\\bale\\b', beerName, ignore.case=T))

typeIPA <- cbind(mergeData.clean, type='IPA', stringsAsFactors=F) %>% filter(grepl('\\bIPA\\b', Style, ignore.case=T) | grepl('\\bIPA\\b', beerName, ignore.case=T))

IPA.Ales <- union(typeAles, typeIPA)
IPA.Ales$type <- as.factor(IPA.Ales$type)



#KNN model
set.seed(100)
splitPerc = .7
iterations = 500
numks = 100
masterAcc = matrix(nrow = iterations, ncol = numks)


for(j in 1:iterations) {
  accs = data.frame(accuracy = numeric(numks), k = numeric(numks))
  trainIndices = sample(1:dim(IPA.Ales)[1],round(splitPerc * dim(IPA.Ales)[1]))
  train = IPA.Ales[trainIndices,]
  test = IPA.Ales[-trainIndices,]
  for(i in 1:numks) {
    classifications = knn(train[,c('IBU','ABV')],test[,c('IBU','ABV')],as.factor(train$type), prob = TRUE, k = i)
    table(as.factor(test$type),classifications)
    CM = confusionMatrix(table(as.factor(test$type),classifications))
    masterAcc[j,i] = CM$overall[1]
  }
}



MeanAcc = colMeans(masterAcc)

plot(seq(1,numks,1),MeanAcc, type = "l")
k <- which.max(MeanAcc)


trainIndices = sample(1:dim(IPA.Ales)[1],round(splitPerc * dim(IPA.Ales)[1]))
train = IPA.Ales[trainIndices,]
test = IPA.Ales[-trainIndices,]

classifications = knn(train[,c('IBU','ABV')],test[,c('IBU','ABV')],as.factor(train$type), prob = TRUE, k = k)
table(as.factor(test$type),classifications)
cfm = confusionMatrix(table(as.factor(test$type),classifications))

Sensitivity = cfm$byClass['Sensitivity']
Specificity = cfm$byClass['Specificity']
Accuracy = cfm$overall['Accuracy']

Accuracy
Sensitivity
Specificity



#This for getting which of the 2 classes performed better bases on the model and the use of 100 different iterations for Spliting the data

iterations = 100

stat = data.frame()

splitPerc = .7 #Training / Test split Percentage

for(j in 1:iterations)
{
  
  trainIndices = sample(1:dim(IPA.Ales)[1],round(splitPerc * dim(IPA.Ales)[1]))
  train = IPA.Ales[trainIndices,]
  test = IPA.Ales[-trainIndices,]
  
  model = knn(train[,c('IBU','ABV')],test[,c('IBU','ABV')],as.factor(train$type), prob = TRUE, k = k)
  table(as.factor(test$type),classifications)
  CM = confusionMatrix(table(as.factor(test$type),classifications))
  
  acc = CM$overall[1]
  spec = CM$byClass[1]
  sens= CM$byClass[2]
  
  stat <- bind_rows(stat,data.frame(c(acc),c(spec),c(sens)))
}

stat <- tibble::rowid_to_column(stat, "Seed")

MeanAcc = mean(stat$c.acc.)
MeanSpec = mean(stat$c.spec.)
MeanSens = mean(stat$c.sens.)

MeanAcc 
MeanSpec
MeanSens

#(9)
# Now lets train using KNN but with IBU and ABV used as a seperate variable to classify
mergeData.cleanIBU <- mergeData %>% filter(!is.na(IBU))
typeAles <- cbind(mergeData.cleanIBU, type='Ales', stringsAsFactors=F) %>% filter(grepl('\\bale\\b', Style, ignore.case=T) | grepl('\\bale\\b', beerName, ignore.case=T))

typeIPA <- cbind(mergeData.cleanIBU, type='IPA', stringsAsFactors=F) %>% filter(grepl('\\bIPA\\b', Style, ignore.case=T) | grepl('\\bIPA\\b', beerName, ignore.case=T))

IPA.Ales <- union(typeAles, typeIPA)

iterations = 100

stat = data.frame()

splitPerc = .7 #Training / Test split Percentage

for(j in 1:iterations)
{
  
  trainIndices = sample(1:dim(IPA.Ales)[1],round(splitPerc * dim(IPA.Ales)[1]))
  train = IPA.Ales[trainIndices,]
  test = IPA.Ales[-trainIndices,]
  
  classifications = knn(train[,c('IBU','Ounces')],test[,c('IBU', 'Ounces')],as.factor(train$type), prob = TRUE, k = k)
  table(as.factor(test$type),classifications)
  CM = confusionMatrix(table(as.factor(test$type),classifications))
  
  acc = CM$overall[1]
  spec = CM$byClass[1]
  sens= CM$byClass[2]
  
  stat <- bind_rows(stat,data.frame(c(acc),c(spec),c(sens)))
}

stat <- tibble::rowid_to_column(stat, "Seed")

MeanAcc = mean(stat$c.acc.)
MeanSpec = mean(stat$c.spec.)
MeanSens = mean(stat$c.sens.)

MeanAcc 
MeanSpec
MeanSens

# Do the same for ABV
mergeData.cleanABV <- mergeData %>% filter(!is.na(ABV))
typeAles <- cbind(mergeData.cleanABV, type='Ales', stringsAsFactors=F) %>% filter(grepl('\\bale\\b', Style, ignore.case=T) | grepl('\\bale\\b', beerName, ignore.case=T))

typeIPA <- cbind(mergeData.cleanABV, type='IPA', stringsAsFactors=F) %>% filter(grepl('\\bIPA\\b', Style, ignore.case=T) | grepl('\\bIPA\\b', beerName, ignore.case=T))

IPA.Ales <- union(typeAles, typeIPA)

stat = data.frame()

for(j in 1:iterations)
{
  
  trainIndices = sample(1:dim(IPA.Ales)[1],round(splitPerc * dim(IPA.Ales)[1]))
  train = IPA.Ales[trainIndices,]
  test = IPA.Ales[-trainIndices,]
  
  classifications = knn(train[,c('ABV','Ounces')],test[,c('ABV', 'Ounces')],as.factor(train$type), prob = TRUE, k = k)
  table(as.factor(test$type),classifications)
  CM = confusionMatrix(table(as.factor(test$type),classifications))
  
  acc = CM$overall[1]
  spec = CM$byClass[1]
  sens= CM$byClass[2]
  
  stat <- bind_rows(stat,data.frame(c(acc),c(spec),c(sens)))
}

stat <- tibble::rowid_to_column(stat, "Seed")

MeanAcc = mean(stat$c.acc.)
MeanSpec = mean(stat$c.spec.)
MeanSens = mean(stat$c.sens.)

MeanAcc 
MeanSpec
MeanSens
