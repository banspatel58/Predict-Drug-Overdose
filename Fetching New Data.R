#read the data
train = read.csv("C:/Users/daxpa/Downloads/train.csv")
test =  read.csv("C:/Users/daxpa/Downloads/test.csv")


library("gdata")

#define a mode function
getmode <- function(v) {
  
  uniqv <- unique(v) 
  uniqv[which.max(tabulate(match(v, uniqv)))]
  
}

#replace the "Homeless" and "Unknown" values with NA
train$Home_Zip = unknownToNA(train$Home_Zip, unknown = "Homeless")
train$Home_Zip = unknownToNA(train$Home_Zip, unknown = "Unknown")

#replace the NA's with mode of zip code's
train$Home_Zip[which(is.na(train$Home_Zip))] = getmode(train$Home_Zip)

#get the cencus API key and install it
library(acs)
library(rgdal)    # for readOGR and others
library(sp)       # for spatial objects
library(leaflet) 
api.key.install(key = "c60f7533002dadf54180de7ca7103f3ddb838fcb")

#find tables
mytable_forEducation = acs.lookup(2015, span = 5, dataset = "acs", keyword="education",  case.sensitive = F)
mytable_forAge = acs.lookup(2015, span = 5, dataset = "acs", keyword = "Age", case.sensitive = F)
mytable_forInsurance = acs.lookup(2015, span = 5, dataset = "acs", keyword = "Health Insurance", case.sensitive = F)
mytable_forRace = acs.lookup(2015, span = 5, dataset = "acs", keyword = "Race", case.sensitive = F)
mytable_forOccupation = acs.lookup(2015, span = 5, dataset = "acs", keyword = "Occupation", case.sensitive = F)
mytable_forEmployment = acs.lookup(2015, span = 5, dataset = "acs", keyword = "Employment", case.sensitive = F)
mytable_forMairrage = acs.lookup(2015, span = 5, dataset = "acs", keyword = "Marriage", case.sensitive = F)
mytable_forIncome = acs.lookup(2015, span = 5, dataset = "acs", keyword = "income", case.sensitive = F)
mytable_forLanguage = acs.lookup(2015, span = 5, dataset = "acs", keyword = "Language", case.sensitive = F)
mytable_forFamily = acs.lookup(2015, span = 5, dataset = "acs", keyword = "Family", case.sensitive = F)

#find the names and numbers of each table
Education = mytable_forEducation[1]
attributes(Education)$results$table.name 
attributes(Education)$results$table.number 
attributes(Education)$results$variable.name

Age = mytable_forAge[3]
attributes(Age)$results$table.name 
attributes(Age)$results$table.number 
attributes(Age)$results$variable.name

Insurance = mytable_forInsurance[130]
attributes(Insurance)$results$table.name 
attributes(Insurance)$results$table.number 
attributes(Insurance)$results$variable.name

Race = mytable_forRace[8]
attributes(Race)$results$table.name 
attributes(Race)$results$table.number 
attributes(Race)$results$variable.name

Occupation = mytable_forOccupation[107]
attributes(Occupation)$results$table.name 
attributes(Occupation)$results$table.number 
attributes(Occupation)$results$variable.name

Employment = mytable_Employment[3]
attributes(Employment)$results$table.name 
attributes(Employment)$results$table.number 
attributes(Employment)$results$variable.name

Marriage = mytable_forMairrage[1]
attributes(Marriage)$results$table.name 
attributes(Marriage)$results$table.number 
attributes(Marriage)$results$variable.name

Income = mytable_Income[1]
attributes(Income)$results$table.name 
attributes(Income)$results$table.number 
attributes(Income)$results$variable.name

language = mytable_forLanguage[19]
attributes(language)$results$table.name 
attributes(language)$results$table.number 
attributes(language)$results$variable.name

Family_data = mytable_forFamily[8]
attributes(Family_data)$results$table.name 
attributes(Family_data)$results$table.number 
attributes(Family_data)$results$variable.name

#list of zip codes in indianapolis
Indianapolis_ZipCodes = c("46220", "46201", "46227", "46202", "46226", "46236", "46219"
                          , "46250", "46231", "46205", "46217", "46224", "46203", "46218"
                          , "46225","46214", "46222", "46237", "46260", "46229", "46107"
                          , "46221", "46235", "46254", "46204", "46240", "46268", "46173"
                          , "46016", "46239", "46234", "46077", "46183", "46113",  "46278"
                          , "46259", "46208", "46241")

library("gdata")

#define a mode function
getmode <- function(v) {
  
  uniqv <- unique(v) 
  uniqv[which.max(tabulate(match(v, uniqv)))]
  
}

#replace the "Homeless" and "Unknown" values with NA
train$Home_Zip = unknownToNA(train$Home_Zip, unknown = "Homeless")
train$Home_Zip = unknownToNA(train$Home_Zip, unknown = "Unknown")

#replace the NA's with mode of zip code's
train$Home_Zip[which(is.na(train$Home_Zip))] = getmode(train$Home_Zip)

#count number of overdose
train$count = 1
Overdose = aggregate( count ~ Home_Zip, data = train, FUN = sum)

#create new data frame with home xip and overdose count as two column
data = data.frame("Zip_Code" = Overdose$Home_Zip, "Overdose_Count" = Overdose$count)

#check to see all zip codes required are there and eliminate rest
data = data[is.element(data$Zip_Code, Indianapolis_ZipCodes),]

#convert zip codes in to list
ZipCode_asCharacter = as.character(Overdose$Home_Zip)

#list the header
header = c( attributes(Education)$results$variable.name
            , attributes(Age)$results$variable.name
            , attributes(Insurance)$results$variable.name
            , attributes(Race)$results$variable.name
            , attributes(Occupation)$results$variable.name
            , attributes(Employment)$results$variable.name
            , attributes(Marriage)$results$variable.name
            , attributes(Income)$results$variable.name
            , attributes(language)$results$variable.name
            , attributes(Family_data)$results$variable.name)

combined_Data = as.data.frame(matrix( ncol = 10, nrow = 0))

names(combined_Data) = header

#use for loop to query the data
for (i in 1:length(ZipCode_asCharacter)) {
  
  ZipCode = ZipCode_asCharacter[i]
  Sub_urb = geo.make(zip.code = ZipCode_asCharacter)
  
  C1 = acs.fetch(2015, span = 5, geography = Sub_urb, variable = Education )
  C1_value = attributes(C1)$estimate[1]
  
  C2 = acs.fetch(2015, span = 5, geography = Sub_urb, variable = Age )
  C2_value = attributes(C2)$estimate[1]
  
  C3 = acs.fetch(2015, span = 5, geography = Sub_urb, variable = Insurance )
  C3_value = attributes(C3)$estimate[1]
  
  C4 = acs.fetch(2015, span = 5, geography = Sub_urb, variable = Race )
  C4_value = attributes(C4)$estimate[1]
  
  C5 = acs.fetch(2015, span = 5, geography = Sub_urb, variable = Occupation )
  C5_value = attributes(C5)$estimate[1]
  
  #C6 = acs.fetch(2015, span = 5, geography = Sub_urb, variable = Employment )
  #C6_value = attributes(C6)$estimate[1]
  
  C7 = acs.fetch(2015, span = 5, geography = Sub_urb, variable = Marriage )
  C7_value = attributes(C7)$estimate[1]
  
  C8 = acs.fetch(2015, span = 5, geography = Sub_urb, variable = Income )
  C8_value = attributes(C8)$estimate[1]
  
  C9 = acs.fetch(2015, span = 5, geography = Sub_urb, variable = language )
  C9_value = attributes(C9)$estimate[1]
  
  C10 = acs.fetch(2015, span = 5, geography = Sub_urb, variable = Family_data )
  C10_value = attributes(C10)$estimate[1]
  
  Final = c(C1_value, C2_value, C3_value, C4_value, C5_value,C7_value
            , C8_value, C9_value, C10_value)
  
  data = rbind(data, Final)
  names(data) = header
  
}

#apply glm
model_glm = glm( Home_Zip ~ ., data = data, family = "binomial")

#summary of glm
print(summary.glm(model_glm))

#plot histogram of counts
hist(Overdose$count)

