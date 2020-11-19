

### How to Preprocess Crime in India File 




```r

##### Loading d2 = Cases Under Crime Against Women #####
d2 <- read_csv("https://raw.githubusercontent.com/BenitaDiop/9891FinalProject/main/datasets/Crime_In_India/42_Cases_under_crime_against_women.csv", col_names = T)

######  Loading d3 =Arrest Against Crime Against Women ######
d3 <- read_csv("https://raw.githubusercontent.com/BenitaDiop/9891FinalProject/main/datasets/Crime_In_India/43_Arrests_under_crime_against_women.csv", col_names = T)

dt <- merge(d2,d3, by= c("Area_Name","Year","Group_Name", "Sub_Group_Name"))

dt2 <- fastDummies::dummy_cols(dt, select_columns = "Group_Name")
names(dt2)
names(dt2)[35] <-"Cruelty_by_Husband_and_Relatives"
names(dt2)[36] <-"DowryDeaths"
names(dt2)[37] <-"Immoral_Traffic_Acts"
names(dt2)[38] <-"ImportationOfGirls"
names(dt2)[39] <-"Indecent_Representation_of_Women"
names(dt2)[40] <-"Kidnapping&Abduction"
names(dt2)[41] <-"Molestation"
names(dt2)[43] <-"Rape"
names(dt2)[43] <-"Sati_Prevention"
names(dt2)[44] <-"SexualHarrasment"
names(dt2)[45] <-"TotalCrimeAgainstWomen"
dt2$ID <- paste(dt2$Area_Name, dt2$Year, sep = "_")
rownames(dt2) <-dt2$ID
d <- subset(dt2, select = -c(Area_Name, Year, ID))
data <- data.frame(cbind(dt2$ID, d))
names(data)[1] <- "ID"
names(data)[19] <- "TotalCases"

write.csv(data, file = 'CrimeInIndia.csv')
```
