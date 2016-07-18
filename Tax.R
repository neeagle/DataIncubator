file_name <- "Historic_Secured_Property_Tax_Rolls.csv"
tax <- read.csv(file=file_name)
library(dplyr)

#Q1 use "summary" to get the number of most common class 
#Ans: 0.4707253227
sum(is.na(tax$Property.Class.Code))
#result is 0, so no need to remove na
sprintf("%.10f", summary(tax$Property.Class.Code)[1]/dim(tax)[1])

#Q2 Note: this answer is obtained by getting the lastest year first then removing zero; if switched, the result is different
#Ans: 209240.0000
taxQ2 <- tax %>% select(Closed.Roll.Fiscal.Year, Block.and.Lot.Number, Closed.Roll.Assessed.Improvement.Value)
taxQ2 <- taxQ2[complete.cases(taxQ2),]
taxQ2 <- taxQ2 %>% group_by(Block.and.Lot.Number) %>% filter(Closed.Roll.Fiscal.Year==max(Closed.Roll.Fiscal.Year)) %>% filter(Closed.Roll.Assessed.Improvement.Value> 0)
sprintf("%.4f", median(taxQ2$Closed.Roll.Assessed.Improvement.Value))

#Q3 Not sure since the highest priced neighborhood name is blank, meaning others
#Ans: 15416056.73
#Without the "other" is : 5085780.383
taxQ3 <- tax %>% select(Closed.Roll.Fiscal.Year, Block.and.Lot.Number, Neighborhood.Code, Closed.Roll.Assessed.Improvement.Value)
taxQ3 <- taxQ3[complete.cases(taxQ3), ]
taxQ3 <- taxQ3 %>% group_by(Block.and.Lot.Number) %>% filter(Closed.Roll.Fiscal.Year==max(Closed.Roll.Fiscal.Year))
taxQ3 <- taxQ3 %>% filter(Closed.Roll.Assessed.Improvement.Value>0) %>% group_by(Neighborhood.Code) %>% summarise(mean.value=mean(Closed.Roll.Assessed.Improvement.Value))
taxQ3 <- arrange(taxQ3, desc(mean.value))
taxQ3[2,2]-taxQ3[88,2]

#Q4 log(P) = log(P0) + rt
taxQ4 <- tax %>% select(Closed.Roll.Fiscal.Year, Closed.Roll.Assessed.Land.Value)
taxQ4 <- taxQ4[complete.cases(taxQ4),]
taxQ4 <- taxQ4 %>% filter(Closed.Roll.Assessed.Land.Value>0)
taxQ4_lm <- lm(log(Closed.Roll.Assessed.Land.Value)~Closed.Roll.Fiscal.Year, data=taxQ4)
sprintf("%.11f", exp(taxQ4_lm$coefficients[2]) - 1)

#Q5
taxQ5 <- tax %>% select(Closed.Roll.Fiscal.Year, Block.and.Lot.Number, Neighborhood.Code, Location)
taxQ5 <- taxQ5[complete.cases(taxQ5),]
taxQ5 <- taxQ5 %>% group_by(Block.and.Lot.Number) %>% filter(Closed.Roll.Fiscal.Year==max(Closed.Roll.Fiscal.Year))
taxQ5 <- taxQ5[taxQ5$Location!="",]
taxQ5$LocSplit <- strsplit(as.character(taxQ5$Location), ", ")
options(digits=15)
taxQ5$Latitude <- unlist(taxQ5$LocSplit)[c(TRUE, FALSE)]
taxQ5$Latitude <- as.numeric(unlist(strsplit(taxQ5$Latitude, "\\("))[c(FALSE, TRUE)])
taxQ5$Longitude <- unlist(taxQ5$LocSplit)[c(FALSE, TRUE)]
taxQ5$Longitude <- as.numeric(unlist(strsplit(taxQ5$Longitude, "\\)")))
#http://www.csgnetwork.com/degreelenllavcalc.html
#I put 37.7860 (as most of the data) into the above site
#I got 110.99km(Long) and 88.09km(Lat) for each degree dimension
taxQ5 <- taxQ5 %>% group_by(Neighborhood.Code) %>% summarise(sdlong=sd(Longitude)*110.99, sdlat = sd(Latitude)*88.09)
taxQ5$area <- abs(pi*taxQ5$sdlong*taxQ5$sdlat)
taxQ5 %>% arrange(desc(area))
#from the result, the largest one is other, so it should be excluded
#I use the next 10J 3.033344217

#Q6
#Ans: 0.4868138565
taxQ6 <- tax %>% select(Closed.Roll.Fiscal.Year, Block.and.Lot.Number, Year.Property.Built, Number.of.Units)
taxQ6 <- taxQ6[complete.cases(taxQ6),]
taxQ6 <- taxQ6 %>% group_by(Block.and.Lot.Number) %>% filter(Closed.Roll.Fiscal.Year==min(Closed.Roll.Fiscal.Year))
taxQ6 <- taxQ6 %>% filter(Number.of.Units != 0)
taxQ6 <- taxQ6[(taxQ6$Year.Property.Built>1800 & taxQ6$Year.Property.Built<=2015),]
mean(taxQ6[taxQ6$Year.Property.Built>=1950,]$Number.of.Units)-mean(taxQ6[taxQ6$Year.Property.Built<1950,]$Number.of.Units)

#Q7
#Ans: 3.807560137
taxQ7 <- tax %>% select(Closed.Roll.Fiscal.Year, Block.and.Lot.Number, Number.of.Bedrooms, Number.of.Units, Zipcode.of.Parcel)
taxQ7 <- taxQ7[complete.cases(taxQ7),]
taxQ7 <- taxQ7 %>% group_by(Block.and.Lot.Number) %>% filter(Closed.Roll.Fiscal.Year==max(Closed.Roll.Fiscal.Year))
taxQ7 <-taxQ7[(taxQ7$Number.of.Bedrooms!=0) & (taxQ7$Number.of.Units!=0), ]
taxQ7 <- taxQ7 %>% group_by(Zipcode.of.Parcel) %>% summarise(mean.bed=mean(Number.of.Bedrooms), mean.unit=mean(Number.of.Units))
taxQ7$ratio <- taxQ7$mean.bed/taxQ7$mean.unit
arrange(taxQ7, desc(ratio))

#Q8
#Ans: 12.33984572
taxQ8 <- tax %>% select(Closed.Roll.Fiscal.Year, Block.and.Lot.Number, Property.Area.in.Square.Feet, Lot.Area, Zipcode.of.Parcel)
taxQ8 <- taxQ8[complete.cases(taxQ8),]
taxQ8 <- taxQ8 %>% group_by(Block.and.Lot.Number) %>% filter(Closed.Roll.Fiscal.Year==max(Closed.Roll.Fiscal.Year))
taxQ8 <- taxQ8 %>% group_by(Zipcode.of.Parcel) %>% summarise(total.property=sum(Property.Area.in.Square.Feet), total.lot=sum(Lot.Area))
taxQ8$ratio <- taxQ8$total.property/taxQ8$total.lot
arrange(taxQ8, desc(ratio))
