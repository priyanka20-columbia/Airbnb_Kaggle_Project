rm(list=ls()) 
RNGversion(vstr="3.6.1")

#reading data#
getwd()
setwd('/Users/jitkalra8/Desktop/Columbia/semester1/APANFramework/Assignments/KaggleProject/Submission1')
data = read.csv('analysisData.csv')
scoringData = read.csv('scoringData.csv') 

#observing data#
str(data)
dim(data)
names(data)

#Identifying missing values #
data= read.csv('analysisData.csv',na.strings = c("NA","N/A",""))
scoringData = read.csv('scoringData.csv',na.strings = c("NA","N/A",""))

#checking negative values in price#
sum(data$price>0)

# Checking missing value in target variable price#
sum(is.na(data$price))

#Finding  missing value #
temprarydata = sapply(data, function(data) sum(is.na(data)) )
missingdata = sort(temprarydata, decreasing=T)
missingdata[missingdata>0]

tempraryscoring = sapply(scoringData, function(scoringData) sum(is.na(scoringData)) )
misssingscoring = sort(tempraryscoring, decreasing=T)
misssingscoring[misssingscoring>0]

#Remove variables with larger missing values in data##
Drop = names(data) %in% c("jurisdiction_names","license","host_acceptance_rate")
data = data[!Drop]

Drop = names(scoringData) %in% c("jurisdiction_names","license","host_acceptance_rate")
scoringData = scoringData[!Drop]

#Checking  missing values again##
temprarydata = sapply(data, function(data) sum(is.na(data)) )
missingdata = sort(temprarydata, decreasing=T)
missingdata[missingdata>0]

tempraryscoring = sapply(scoringData, function(scoringData) sum(is.na(scoringData)) )
misssingcoring = sort(tempraryscoring, decreasing=T)
misssingcoring[misssingcoring>0]

#Dealing with numeric missing values#
#1 beds
#Replacing missing values with mean value
class(data$beds)
data[is.na(data$beds),"beds"] = mean(data$beds,na.rm=T)
scoringData[is.na(scoringData$beds),"beds"] = mean(scoringData$beds,na.rm=T)
sum(is.na(data$beds))
sum(is.na(scoringData$beds))

#2 reviews_per_month
class(data$reviews_per_month)
#Replacing missing values with mean value
data[is.na(data$reviews_per_month),"reviews_per_month"] = mean(data$reviews_per_month,na.rm=T)
scoringData[is.na(scoringData$reviews_per_month),"reviews_per_month"] = mean(scoringData$reviews_per_month,na.rm=T)
sum(is.na(data$reviews_per_month))
sum(is.na(scoringData$reviews_per_month))


#3 host_listings_count
class(data$host_listings_count)
#Replacing missing values with mean value
data[is.na(data$host_listings_count),"host_listings_count"] = mean(data$host_listings_count,na.rm=T)
scoringData[is.na(scoringData$host_listings_count),"host_listings_count"] = mean(scoringData$host_listings_count,na.rm=T)
sum(is.na(data$host_listings_count))
sum(is.na(scoringData$host_listings_count))

#4 host_total_listings_count
class(data$host_total_listings_count)
#Replacing missing values with mean value
data[is.na(data$host_total_listings_count),"host_total_listings_count"] = mean(data$host_total_listings_count,na.rm=T)
scoringData[is.na(scoringData$host_total_listings_count),"host_total_listings_count"] = mean(scoringData$host_total_listings_count,na.rm=T)
sum(is.na(data$host_total_listings_count))
sum(is.na(scoringData$host_total_listings_count))

#5 guests_included
class(data$guests_included)
sum(is.na(data$guests_included))
sum(is.na(scoringData$guests_included))

#6 extra_people
class(data$extra_people)
sum(is.na(data$extra_people))
sum(is.na(scoringData$extra_people))

#7 minimum_nights_avg_ntm
class(data$minimum_nights_avg_ntm)
sum(is.na(data$minimum_nights_avg_ntm))
sum(is.na(scoringData$minimum_nights_avg_ntm))

#8 maximum_nights_avg_ntm
class(data$maximum_nights_avg_ntm)
sum(is.na(data$maximum_nights_avg_ntm))
sum(is.na(scoringData$maximum_nights_avg_ntm))

#9 minimum_nights
class(data$minimum_nights)
sum(is.na(data$minimum_nights))
sum(is.na(scoringData$minimum_nights))

#10 maximum_nights
class(data$maximum_nights)
sum(is.na(data$maximum_nights))
sum(is.na(scoringData$maximum_nights))

#11 availability_30
class(data$availability_30)
sum(is.na(data$availability_30))
sum(is.na(scoringData$availability_30))

#12 availability_60
class(data$availability_60)
sum(is.na(data$availability_60))
sum(is.na(scoringData$availability_60))

#13 availability_90
class(data$availability_90)
sum(is.na(data$availability_90))
sum(is.na(scoringData$availability_90))

#14 number_of_reviews
class(data$number_of_reviews)
sum(is.na(data$number_of_reviews))
sum(is.na(scoringData$number_of_reviews))
sum(is.na(data$number_of_reviews_ltm))
sum(is.na(scoringData$number_of_reviews_ltm))

#15 square_feet
class(data$square_feet)
#Replacing rows with mean value
data[is.na(data$square_feet),"square_feet"] = mean(data$square_feet,na.rm=T)
scoringData[is.na(scoringData$square_feet),"square_feet"] = mean(scoringData$square_feet,na.rm=T)
sum(is.na(data$square_feet))
sum(is.na(scoringData$square_feet))

#16 security_deposit
class(data$security_deposit)
#Replacing rows with mean value
str(data)
class(data$security_deposit)
data$security_deposit = as.numeric(data$security_deposit)
data[is.na(data$security_deposit),"security_deposit"] = mean(data$security_deposit,na.rm=T)
str(scoringData)
class(scoringData$security_deposit)
scoringData$security_deposit = as.numeric(scoringData$security_deposit)
scoringData[is.na(scoringData$security_deposit),"security_deposit"] = mean(scoringData$security_deposit,na.rm=T)
sum(is.na(data$security_deposit))
sum(is.na(scoringData$security_deposit))


#17 weekly_price
#Replacing rows with mean value
class(data$weekly_price)
data$weekly_price = as.numeric(data$weekly_price)
data[is.na(data$weekly_price),"weekly_price"] = mean(data$weekly_price,na.rm=T)
class(scoringData$weekly_price)
scoringData$weekly_price = as.numeric(scoringData$weekly_price)
scoringData[is.na(scoringData$weekly_price),"weekly_price"] = mean(scoringData$weekly_price,na.rm=T)
sum(is.na(data$weekly_price))
sum(is.na(scoringData$weekly_price))

#18 cleaning_fee
#Replacing rows with mean value
class(data$cleaning_fee)
data$cleaning_fee = as.numeric(data$cleaning_fee)
data[is.na(data$cleaning_fee),"cleaning_fee"] = mean(data$cleaning_fee,na.rm=T)
class(scoringData$cleaning_fee)
scoringData$cleaning_fee = as.numeric(scoringData$cleaning_fee)
scoringData[is.na(scoringData$cleaning_fee),"cleaning_fee"] = mean(scoringData$cleaning_fee,na.rm=T)
sum(is.na(data$cleaning_fee))
sum(is.na(scoringData$cleaning_fee))

#19 review_scores_rating
class(data$review_scores_rating)
sum(is.na(data$review_scores_rating))
sum(is.na(scoringData$review_scores_rating))

#21review_scores_value
class(data$review_scores_value)
data$review_scores_value = as.numeric(data$review_scores_value)
data[is.na(data$review_scores_value),"review_scores_value"] = mean(data$review_scores_value,na.rm=T)
class(scoringData$review_scores_value)
scoringData$review_scores_value = as.numeric(scoringData$review_scores_value)
scoringData[is.na(scoringData$review_scores_value),"review_scores_value"] = mean(scoringData$review_scores_value,na.rm=T)
sum(is.na(data$review_scores_value))
sum(is.na(scoringData$review_scores_value))

#22 review_scores_location
#Replacing rows with mean value
class(data$review_scores_location)
data[is.na(data$review_scores_location),"review_scores_location"] = mean(data$review_scores_location,na.rm=T)
scoringData[is.na(scoringData$review_scores_location),"review_scores_location"] = mean(scoringData$review_scores_location,na.rm=T)
sum(is.na(data$review_scores_location))
sum(is.na(scoringData$review_scores_location))

#23 calculated_host_listings_count
class(data$calculated_host_listings_count)
sum(is.na(data$calculated_host_listings_count))
sum(is.na(scoringData$calculated_host_listings_count))

#24review_scores_checkin
class(data$review_scores_checkin)
data[is.na(data$review_scores_checkin),"review_scores_checkin"] = mean(data$review_scores_checkin,na.rm=T)
scoringData[is.na(scoringData$review_scores_checkin),"review_scores_checkin"] = mean(scoringData$review_scores_checkin,na.rm=T)
sum(is.na(data$review_scores_checkin))
sum(is.na(scoringData$review_scores_checkin))

#25bedrooms
class(data$bedrooms)
data[is.na(data$bedrooms),"bedrooms"] = mean(data$bedrooms,na.rm=T)
scoringData[is.na(scoringData$bedrooms),"bedrooms"] = mean(scoringData$bedrooms,na.rm=T)
sum(is.na(data$bedrooms))
sum(is.na(scoringData$bedrooms))

#26 bathrooms##
class(data$bathrooms)
data[is.na(data$bathrooms),"bathrooms"] = mean(data$bathrooms,na.rm=T)
scoringData[is.na(scoringData$bathrooms),"bathrooms"] = mean(scoringData$bathrooms,na.rm=T)
sum(is.na(data$bathrooms))
sum(is.na(scoringData$bathrooms))

#27 review_scores_communication
class(data$review_scores_communication)
data[is.na(data$review_scores_communication),"review_scores_communication"] = mean(data$review_scores_communication,na.rm=T)
scoringData[is.na(scoringData$review_scores_communication),"review_scores_communication"] = mean(scoringData$review_scores_communication,na.rm=T)
sum(is.na(data$review_scores_communication))
sum(is.na(scoringData$review_scores_communication))

##28 review_scores_accuracy
class(data$review_scores_accuracy)
data[is.na(data$review_scores_accuracy),"review_scores_accuracy"] = mean(data$review_scores_accuracy,na.rm=T)
scoringData[is.na(scoringData$review_scores_accuracy),"review_scores_accuracy"] = mean(scoringData$review_scores_accuracy,na.rm=T)
sum(is.na(data$review_scores_accuracy))
sum(is.na(scoringData$review_scores_accuracy))

#29 review_scores_cleanliness
class(data$review_scores_cleanliness)
data[is.na(data$review_scores_cleanliness),"review_scores_cleanliness"] = mean(data$review_scores_cleanliness,na.rm=T)
scoringData[is.na(scoringData$review_scores_cleanliness),"review_scores_cleanliness"] = mean(scoringData$review_scores_cleanliness,na.rm=T)
sum(is.na(data$review_scores_cleanliness))
sum(is.na(scoringData$review_scores_cleanliness))

#30 zipcode
#add a level to replace NA
class(data$zipcode)
data[is.na(data$zipcode),"zipcode"] = 0
sum(is.na(data$zipcode))
scoringData[is.na(scoringData$zipcode),"zipcode"] = 0
sum(is.na(scoringData$zipcode))

##31 latitude
class(data$latitude)
data$latitude = (data$latitude-40)*1000
scoringData$latitude = (scoringData$latitude-40)*1000

##32 longitude
class(data$longitude)
data$longitude = (data$longitude+73)*1000
scoringData$longitude = (scoringData$longitude+73)*1000


#Dealing with factor variables
#1 host_response_rate
#add a level to replace NA
class(data$host_response_rate)
data$host_response_rate = addNA(data$host_response_rate)
scoringData$host_response_rate=addNA(scoringData$host_response_rate)
sum(is.na(data$host_response_rate))
sum(is.na(scoringData$host_response_rate))

#2 host_location
#add a level to replace NA
class(data$host_location)
data$host_location = addNA(data$host_location)
scoringData$host_location=addNA(scoringData$host_location)
sum(is.na(data$host_location))
sum(is.na(scoringData$host_location))

#3 host_response_time
#add a level to replace NA
class(data$host_response_time)
data$host_response_time = addNA(data$host_response_time)
scoringData$host_response_time=addNA(scoringData$host_response_time)
sum(is.na(data$host_response_time))
sum(is.na(scoringData$host_response_time))


#5 neighbourhood_cleansed
#add a level to replace NA
class(data$neighbourhood_cleansed)
data$neighbourhood_cleansed = addNA(data$neighbourhood_cleansed)
scoringData$neighbourhood_cleansed=addNA(scoringData$neighbourhood_cleansed)
sum(is.na(data$neighbourhood_cleansed))
sum(is.na(scoringData$neighbourhood_cleansed))

#6 street
#add a level to replace NA
class(data$street)
data$street = addNA(data$street)
scoringData$street=addNA(scoringData$street)
sum(is.na(data$street))
sum(is.na(scoringData$street))

#7 neighbourhood
#add a level to replace NA
class(data$neighbourhood)
data$neighbourhood = addNA(data$neighbourhood)
scoringData$neighbourhood=addNA(scoringData$neighbourhood)
sum(is.na(data$neighbourhood))
sum(is.na(scoringData$neighbourhood))

#8 city
#add a level to replace NA
class(data$city)
data$city = addNA(data$city)
scoringData$city=addNA(scoringData$city)
sum(is.na(data$city))
sum(is.na(scoringData$city))

#9 smart_location
#add a level to replace NA
class(data$smart_location)
data$smart_location = addNA(data$smart_location)
scoringData$smart_location=addNA(scoringData$smart_location)
sum(is.na(data$smart_location))
sum(is.na(scoringData$smart_location))

#10 host_neighbourhood
#add a level to replace NA
class(data$host_neighbourhood)
data$host_neighbourhood = addNA(data$host_neighbourhood)
scoringData$host_neighbourhood=addNA(scoringData$host_neighbourhood)
sum(is.na(data$host_neighbourhood))
sum(is.na(scoringData$host_neighbourhood))

#11 host_has_profile_pic#
class(data$host_has_profile_pic)
data$host_has_profile_pic <- as.numeric(data$host_has_profile_pic)
data[is.na(data$host_has_profile_pic),"host_has_profile_pic"] <- TRUE
class(scoringData$host_has_profile_pic)
scoringData$host_has_profile_pic <- as.numeric(scoringData$host_has_profile_pic)
scoringData[is.na(scoringData$host_has_profile_pic),"host_has_profile_pic"] <- TRUE
sum(is.na(data$host_has_profile_pic))
sum(is.na(scoringData$host_has_profile_pic))

#Dealing  with text variables
#1 description
#convert to numeric first
class(data$description)
data$description = nchar(as.character(data$description))
scoringData$description = nchar(as.character(scoringData$description))
#replacing missing value with mean
data[is.na(data$description),"description"] = mean(data$description,na.rm=T)
scoringData[is.na(scoringData$description),"description"] = mean(scoringData$description,na.rm=T)
summary(data$description)
sum(is.na(data$description))
sum(is.na(scoringData$description))

#2 amenities
#convert to numeric
class(data$amenities)
data$amenities = nchar(as.character(data$amenities))
class(scoringData$amenities)
scoringData$amenities = nchar(as.character(scoringData$amenities))
sum(is.na(data$amenities))
sum(is.na(scoringData$amenities))

#3 host_verifications
#convert to numeric
class(data$host_verifications)
data$host_verifications = nchar(as.character(data$host_verifications))
class(scoringData$host_verifications)
scoringData$host_verifications = nchar(as.character(scoringData$host_verifications))
sum(is.na(data$host_verifications))
sum(is.na(scoringData$host_verifications))

#4 summary
#convert to numeric
class(data$summary)
data$summary = nchar(as.character(data$summary))
class(scoringData$summary)
scoringData$summary = nchar(as.character(scoringData$summary))
#replacing missing value with mean
data[is.na(data$summary),"summary"] = mean(data$summary,na.rm=T)
scoringData[is.na(scoringData$summary),"summary"] = mean(scoringData$summary,na.rm=T)
summary(data$summary)
sum(is.na(data$summary))
sum(is.na(scoringData$summary))

#5 space
#convert to numeric
class(data$space)
data$space = nchar(as.character(data$space))
class(scoringData$space)
scoringData$space = nchar(as.character(scoringData$space))
#replacing missing value with mean
data[is.na(data$space),"space"] = mean(data$space,na.rm=T)
scoringData[is.na(scoringData$space),"space"] = mean(scoringData$space,na.rm=T)
summary(data$space)
sum(is.na(data$space))
sum(is.na(scoringData$space))

#6 neighborhood_overview
#convert to numeric
class(data$neighborhood_overview)
data$neighborhood_overview = nchar(as.character(data$neighborhood_overview))
class(scoringData$neighborhood_overview)
scoringData$neighborhood_overview = nchar(as.character(scoringData$neighborhood_overview))
#replacing missing value with mean
data[is.na(data$neighborhood_overview),"neighborhood_overview"] = mean(data$neighborhood_overview,na.rm=T)
scoringData[is.na(scoringData$neighborhood_overview),"neighborhood_overview"] = mean(scoringData$neighborhood_overview,na.rm=T)
summary(data$neighborhood_overview)
sum(is.na(data$neighborhood_overview))
sum(is.na(scoringData$neighborhood_overview))

#7 notes
#convert to numeric
class(data$notes)
data$notes = nchar(as.character(data$notes))
class(scoringData$notes)
scoringData$notes = nchar(as.character(scoringData$notes))
#replacing missing value with mean
data[is.na(data$notes),"notes"] = mean(data$notes,na.rm=T)
scoringData[is.na(scoringData$notes),"notes"] = mean(scoringData$notes,na.rm=T)
summary(data$notes)
sum(is.na(data$notes))
sum(is.na(scoringData$notes))

#8 transit
#convert to numeric
class(data$transit)
data$transit = nchar(as.character(data$transit))
class(scoringData$transit)
scoringData$transit = nchar(as.character(scoringData$transit))
#replacing missing value with mean
data[is.na(data$transit),"transit"] = mean(data$transit,na.rm=T)
scoringData[is.na(scoringData$transit),"transit"] = mean(scoringData$transit,na.rm=T)
summary(data$transit)
sum(is.na(data$transit))
sum(is.na(scoringData$transit))

#9 access
#convert to numeric
class(data$access) 
data$access = nchar(as.character(data$access))
class(scoringData$access)
scoringData$access = nchar(as.character(scoringData$access))
#replacing missing value with mean
data[is.na(data$access),"access"] = mean(data$access,na.rm=T)
scoringData[is.na(scoringData$access),"access"] = mean(scoringData$access,na.rm=T)
summary(data$access)
sum(is.na(data$access))
sum(is.na(scoringData$access))

#10 interaction
#convert to numeric
class(data$interaction)
data$interaction = nchar(as.character(data$interaction))
class(scoringData$interaction)
scoringData$interaction = nchar(as.character(scoringData$interaction))
#replacing missing value with mean
data[is.na(data$interaction),"interaction"] = mean(data$interaction,na.rm=T)
scoringData[is.na(scoringData$interaction),"interaction"] = mean(scoringData$interaction,na.rm=T)
summary(data$interaction)
sum(is.na(data$interaction))
sum(is.na(scoringData$interaction))

#11 house_rules
#convert to numeric
class(data$house_rules)
data$house_rules = nchar(as.character(data$house_rules))
class(scoringData$house_rules)
scoringData$house_rules = nchar(as.character(scoringData$house_rules))
#replacing missing value with mean
data[is.na(data$house_rules),"house_rules"] = mean(data$house_rules,na.rm=T)
scoringData[is.na(scoringData$house_rules),"house_rules"] = mean(scoringData$house_rules,na.rm=T)
summary(data$house_rules)
sum(is.na(data$house_rules))
sum(is.na(scoringData$house_rules))

#12 host_about
#convert to numeric
class(data$host_about)
data$host_about = nchar(as.character(data$host_about))
class(scoringData$host_about)
scoringData$host_about = nchar(as.character(scoringData$host_about))
#replacing missing value with mean
data[is.na(data$host_about),"host_about"] = mean(data$host_about,na.rm=T)
scoringData[is.na(scoringData$host_about),"host_about"] = mean(scoringData$host_about,na.rm=T)
summary(data$host_about)
sum(is.na(data$host_about))
sum(is.na(scoringData$host_about))

#12 monthly_price###
#convert to numeric
class(data$monthly_price)
data$monthly_price = nchar(as.character(data$monthly_price))
class(scoringData$monthly_price)
scoringData$monthly_price = nchar(as.character(scoringData$monthly_price))
#replacing missiong value with mean
data[is.na(data$monthly_price),"monthly_price"] = mean(data$monthly_price,na.rm=T)
scoringData[is.na(scoringData$monthly_price),"monthly_price"] = mean(scoringData$monthly_price,na.rm=T)
summary(data$monthly_price)
sum(is.na(data$monthly_price))
sum(is.na(scoringData$monthly_price))


#Dealing with date variable
#1 host_since
# convert to days until today first
data$host_since = as.integer(as.Date("2020-04-13")-as.Date(data$host_since))
scoringData$host_since = as.integer(as.Date("2020-04-13")-as.Date(scoringData$host_since))
#Replacing missing values with mean value
data[is.na(data$host_since),"host_since"] = mean(data$host_since,na.rm=T)
scoringData[is.na(scoringData$host_since),"host_since"] = mean(scoringData$host_since,na.rm=T)
class(data$host_since)
class(scoringData$host_since)
sum(is.na(data$host_since))
sum(is.na(scoringData$host_since))

#2 last_review
# convert to days until today first
data$last_review = as.integer(as.Date("2020-04-13")-as.Date(data$last_review))
scoringData$last_review = as.integer(as.Date("2020-04-13")-as.Date(scoringData$last_review))
data[is.na(data$last_review),"last_review"] = mean(data$last_review,na.rm=T)
scoringData[is.na(scoringData$last_review),"last_review"] = mean(scoringData$last_review,na.rm=T)
sum(is.na(data$last_review))
sum(is.na(scoringData$last_review))


#3 first_review
# convert to days until today first
data$first_review = as.integer(as.Date("2020-04-13")-as.Date(data$first_review))
scoringData$first_review = as.integer(as.Date("2020-04-13")-as.Date(scoringData$first_review))
#Delete missing rows in data. Replace rows with mean value
data[is.na(data$first_review),"first_review"] = mean(data$first_review,na.rm=T)
scoringData[is.na(scoringData$first_review),"first_review"] = mean(scoringData$first_review,na.rm=T)
sum(is.na(data$first_review))
sum(is.na(scoringData$first_review))


#Dealing  with logical variables
#1 host_is_superhost
#Let missing rows = false
class(data$host_is_superhost)
data[is.na(data$host_is_superhost),"host_is_superhost"] = "false"
scoringData[is.na(scoringData$host_is_superhost),"host_is_superhost"] = "false"
sum(is.na(data$host_is_superhost))
sum(is.na(scoringData$host_is_superhost))

#2 host_identity_verified
#Let missing rows = false
data[is.na(data$host_identity_verified),"host_identity_verified"] = "false"
scoringData[is.na(scoringData$host_identity_verified),"host_identity_verified"] = "false"
sum(is.na(data$host_identity_verified))
sum(is.na(scoringData$host_identity_verified))

#3 has_availability
sum(is.na(data$has_availability))
sum(is.na(scoringData$has_availability))


#Checking missing value again
temprarydata = sapply(data, function(data) sum(is.na(data)) )
missingdata = sort(temprarydata, decreasing=T)
missingdata[missingdata>0]

tempraryscoring = sapply(scoringData, function(scoringData) sum(is.na(scoringData)) )
missingscoring = sort(tempraryscoring, decreasing=T)
missingscoring[missingscoring>0]
########################

#Checking  categorial factors' level that I prefer to use further
#1 room_type
levels(scoringData$room_type)==levels(data$room_type)

#2 neighbourhood_group_cleansed
levels(scoringData$neighbourhood_group_cleansed)==levels(data$neighbourhood_group_cleansed)

#3 property_type
levels(scoringData$property_type)
levels(data$property_type)
## "Lighthouse","Farm stay","Treehouse" in scoringData does not show in data
table(scoringData$property_type)
#Convert these levels to "Other"
scoringData$property_type = as.character(scoringData$property_type)
scoringData = within(scoringData,{property_type[property_type=="Lighthouse"] = "Other"})
scoringData = within(scoringData,{property_type[property_type=="Treehouse"] = "Other"})
scoringData = within(scoringData,{property_type[property_type=="Farm stay"] = "Other"})
scoringData$property_type = as.factor(scoringData$property_type)
table(scoringData$property_type)
levels(scoringData$property_type )=levels(data$property_type)

#4 bed_type
levels(data$bed_type)==levels(scoringData$bed_type)
sum(is.na(data$bed_type))
sum(is.na(scoringData$bed_type))

#5 cancellation_policy
levels(data$cancellation_policy)
levels(scoringData$cancellation_policy)
sum(is.na(data$cancellation_policy))
sum(is.na(scoringData$cancellation_policy))

#Splitting data
library(caTools)
set.seed(617)
split=sample.split(data$price,SplitRatio = 0.7)
train=data[split,]
test=data[!split,]
nrow(train)+nrow(test)==nrow(data)

## final best model used###
install.packages("randomForest")
library(randomForest)
set.seed(617)
forestFinal = randomForest(price~zipcode+neighbourhood_group_cleansed+property_type
                          +bathrooms+bedrooms+accommodates
                          +guests_included +room_type+amenities+number_of_reviews 
                          +calculated_host_listings_count_private_rooms
                          +availability_365+availability_90
                          +cleaning_fee+host_since+calculated_host_listings_count+ host_listings_count
                          ,data=train,ntree = 100,mtry = 3)


# Make predictions
predTest = predict(forestFinal, newdata = test)

# SSE
SSE = sum((predTest - test$price)^2);SSE

# RMSE
RMSE = sqrt(mean((predTest - test$price)^2));RMSE

# Baseline
baseline = mean(train$price);baseline


# SSE of baseline model on testing set
SSEbaseline = sum((baseline - test$price)^2);SSEbaseline


# Applying model to generate predictions
predscoringdata=predict(forestFinal,newdata=scoringData)

# Construct submission from predictions
pn19thSubmission = data.frame(id = scoringData$id, price = predscoringdata)
write.csv(pn19thSubmission, 'pn19thSubmission.csv',row.names = F)

######## Checking relative influence of major variables using boosting model####

library(gbm)
set.seed(617)
boost = gbm(price~zipcode+neighbourhood_group_cleansed+property_type
            +bathrooms+bedrooms+accommodates
            +guests_included +room_type+amenities+number_of_reviews 
            +calculated_host_listings_count_private_rooms
            +availability_365+availability_90
            +cleaning_fee+host_since+calculated_host_listings_count+ host_listings_count,
            data=train,distribution="gaussian",
            n.trees = 5000,interaction.depth = 20,shrinkage = 0.001)
summary(boost)


