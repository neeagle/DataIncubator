library(zipcode)
library(ggmap)
library(dplyr)
data(zipcode)

zip_price_rent <- read.csv(file="Zip_PriceToRentRatio_AllHomes.csv")
zip_pr_201605 <- merge(zip_price_rent, zipcode, by.x="RegionName", by.y="zip")
zip_pr_201605 <- select(zip_pr_201605, RegionName, City, State, latitude, longitude, X2016.05)
zip_pr_201605$rent_to_price <- 1/zip_pr_201605$X2016.05
map<-get_map(location='united states', zoom=4, maptype = "toner", source='google',color='color')
ggmap(map) + geom_point(aes(x=longitude, y=latitude, show_guide = TRUE, colour=log(rent_to_price)), data=zip_pr_201605, na.rm=TRUE) +  scale_color_gradient(low="yellow", high="purple") + ggtitle("Return of Residential Rentals")+ theme(plot.title = element_text(size=22))

irs2013 <- read.csv(file="13zpallnoagi.csv")
zip_pr_201605 <- merge(zip_pr_201605, irs2013, by.x="RegionName", by.y="ZIPCODE")
zip_pr_201605 <- zip_pr_201605 %>% select(RegionName, rent_to_price, A00100)
ggplot(zip_pr_201605, aes(RegionName, rent_to_price)) + geom_point(aes(colour = log(A00100)))+scale_colour_gradient(low="yellow", high="purple") + xlab("Zip Code") + ggtitle("Return and Income vs Zip") + labs(colour="log(income)") + theme(plot.title = element_text(size=22))
