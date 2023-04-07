###############################CAFE OCEAN DATA ANALYSIS#########################

#1. Load the data as a dataframe for ease of use and correct the column names if required

#Initially data set in .xlsx format hence converting it into .csv file

getwd()

setwd('C:/Personal/Salim Malik/Data Science Classes/Data Science with R/Project')

library('readxl')

Cafe_Ocean <- read_xlsx("Cafe_Ocean.xlsx")
View(Cafe_Ocean)
export(Cafe_Ocean, format = 'csv')

data_frame <- read.csv('Cafe_Ocean.csv')
data_frame
class(data_frame)
length(data_frame)
dim(data_frame)
str(data_frame)
View(data_frame)
head(data_frame)
tail(data_frame)
names(data_frame)

colnames(data_frame)[2] <- 'Bill_Number'
colnames(data_frame)[3] <- 'Item_Desc'

View(data_frame)

#After converting the file, observed Date and Time column format got changed.

data_frame$Date 
data_frame$Time

data_frame$Date <- sub('T',' ',data_frame$Date)
data_frame$Date <- sub('Z','',data_frame$Date)

data_frame$Time <- sub('T',' ',data_frame$Time)
data_frame$Time <- sub('Z','',data_frame$Time) 


data_frame$Date <- as.POSIXct(data_frame$Date, format = "%Y-%m-%d %H:%M:%S", 
                              tz=Sys.timezone())
data_frame$Time <- as.POSIXct(data_frame$Time, format = "%Y-%m-%d %H:%M:%S",
                              tz=Sys.timezone())
                              
data_frame$Time <- strftime(data_frame$Time, format = "%H:%M:%S")

str(data_frame)

View(data_frame)  # Now the data is in proper format.

colSums(is.na(data_frame))

#2. Include insights about the most famous item overall and in each category

#Most Famous item can be defined in two perspectives:-
#1)First Perspective: Most selling item in the Cafe
#2)Second Perspective: number of times customers purchased particular item(ie wrt to bill number).

#1)Below analysis First Perspective (Cafe Perspective):

length(unique(data_frame$Category))  #We have 8 Unique Categories
length(unique(data_frame$Item_Desc))  # We have 579 Unique items in Item_Desc

#finding most famous item overall
#here most famous items means that most selling item

#I found that some items has purchased more times i.e Quantity

data_frame2 <- aggregate(x = data_frame$Bill_Number, 
                         by = list(data_frame$Item_Desc, data_frame$Category,
                                   data_frame$Quantity), length)
View(data_frame2)
colnames(data_frame2)[1] <- 'Item_Desc'
colnames(data_frame2)[2] <- 'Category'
colnames(data_frame2)[3] <- 'Quantity'
colnames(data_frame2)[4] <- 'nTimes_Purchased'
data_frame2$Total_Purchased <- c(data_frame2$Quantity*data_frame2$nTimes_Purchased)

data_frame3 <- aggregate(x = data_frame2$Total_Purchased,
                         by = list(data_frame2$Item_Desc), FUN = sum)
View(data_frame3)

colnames(data_frame3)[1] <- 'Item_Desc'
colnames(data_frame3)[2] <- 'Total_Purchased'

library(dplyr)

data_frame3 <- arrange(data_frame3, -Total_Purchased)

View(data_frame3)

head(data_frame3) 
#here we can see item "NIRVANA HOOKAH SINGLE" has purchased 8686 times
#hence 'NIRVANA HOOKAH SINGLE' is most famous item overall

data_frame4 <- aggregate(x=data_frame2$Total_Purchased, 
          by = list(data_frame2$Item_Desc, data_frame2$Category), FUN = sum)

View(data_frame4)

colnames(data_frame4)[1] <- 'Item_Desc'
colnames(data_frame4)[2] <- 'Category'
colnames(data_frame4)[3] <- 'Total_Purchased'

unique(data_frame4$Category)
data_frame4 <- arrange(data_frame4, -Total_Purchased)

data_frame5 <- subset(data_frame4,Category %in% c("BEVERAGE"))
head(data_frame5) 

#I observed CAPPUCCINO is purchased 7144 quantity in BEVERAGE Category
#Hence CAPPUCCINO is the most famous item in BEVERAGE Category

data_frame6 <- subset(data_frame4, Category %in% c("FOOD"))
head(data_frame6) 

#I observed OCEAN SPECIAL SHAKE is purchased 5914 quantity in FOOD Category
#Hence OCEAN SPECIAL SHAKE is the most famous item in FOOD Category

data_frame7 <- subset(data_frame4, Category %in% c("LIQUOR"))
head(data_frame7)

#I observed CARLSBERG is purchased 3380 quantity in LIQUOR Category
#Hence CARLSBERG is the most famous item in LIQUOR Category

data_frame8 <- subset(data_frame4, Category %in% c('LIQUOR & TPBACCO'))
head(data_frame8)

#I observed BEER HOOKAH is purchased 49 quantity in LIQUOR & TPBACCO Category
#Hence BEER HOOKAH is the most famous item in LIQUOR & TPBACCO Category

data_frame9 <- subset(data_frame4, Category %in% c('MERCHANDISE'))
head(data_frame9)

#I observed OCEAN SPECIAL T-SHIRTS is purchased 37 quantity in MERCHANDISE Category
#Hence OCEAN SPECIAL T-SHIRTS is the most famous item in MERCHANDISE Category

data_frame10 <- subset(data_frame4, Category %in% c('MISC'))
head(data_frame10)

#I observed ADD ON S is purchased 448 quantity in MISC Category
#Hence ADD ON S is the most famous item in MISC Category

data_frame11 <- subset(data_frame4, Category %in% c('TOBACCO'))
head(data_frame11)

#I observed NIRVANA HOOKAH SINGLE is purchased 8686 quantity in TOBACCO Category
#Hence NIRVANA HOOKAH SINGLE is the most famous item in TOBACCO Category

data_frame12 <- subset(data_frame4, Category %in% c('WINES'))
head(data_frame12)

#I observed VLN CAB SAUV (GLS) is purchased 216 quantity in WINES Category
#Hence VLN CAB SAUV (GLS) is the most famous item in WINES Category

#2)Below analysis Second Perspective (Customer Perspective):

data_frame_fam <- aggregate(x = data_frame$Bill_Number,
                            by = list(data_frame$Item_Desc,data_frame$Category),
                            length)

View(data_frame_fam)

data_frame_fam <- arrange(data_frame_fam, -x)

colnames(data_frame_fam)[1] <- 'Item_Desc'
colnames(data_frame_fam)[2] <- 'Category'
colnames(data_frame_fam)[3] <- 'nCust_Purchased'

head(data_frame_fam)
#here we can see item "NIRVANA HOOKAH SINGLE" has appeared 8553 times
#hence 'NIRVANA HOOKAH SINGLE' is most famous item overall

data_frame_fam1 <- subset(data_frame_fam, Category %in% c("BEVERAGE"))
head(data_frame_fam1) 

#here we can see item CAPPUCCINO is purchased 5495 Customers in BEVERAGE Category
#Hence CAPPUCCINO is the most famous item in BEVERAGE Category

data_frame_fam2 <- subset(data_frame_fam, Category %in% c("FOOD"))
head(data_frame_fam2) 

#here we can see item OCEAN SPECIAL SHAKE is purchased 4895 Customers in FOOD Category
#Hence OCEAN SPECIAL SHAKE is the most famous item in FOOD Category

data_frame_fam3 <- subset(data_frame_fam, Category %in% c("LIQUOR"))
head(data_frame_fam3)

#here we can see item CARLSBERG is purchased 1716 Customers in LIQUOR Category
#Hence CARLSBERG is the most famous item in LIQUOR Category

data_frame_fam4 <- subset(data_frame_fam, Category %in% c('LIQUOR & TPBACCO'))
head(data_frame_fam4)

#here we can see item BEER HOOKAH is purchased 40 Customers in LIQUOR & TPBACCO Category
#Hence BEER HOOKAH is the most famous item in LIQUOR & TPBACCO Category

data_frame_fam5 <- subset(data_frame_fam, Category %in% c('MERCHANDISE'))
head(data_frame_fam5)

#here we can see item OCEAN SPECIAL T-SHIRTS is purchased 34 Customers in MERCHANDISE Category
#Hence OCEAN SPECIAL T-SHIRTS is the most famous item in MERCHANDISE Category

data_frame_fam6 <- subset(data_frame_fam, Category %in% c('MISC'))
head(data_frame_fam6)

#here we can see item ADD ON S is purchased 379 Customers in MISC Category
#Hence ADD ON S is the most famous item in MISC Category

data_frame_fam7 <- subset(data_frame_fam, Category %in% c('TOBACCO'))
head(data_frame_fam7)

#here we can see item NIRVANA HOOKAH SINGLE is purchased 8553 Customers in TOBACCO Category
#Hence NIRVANA HOOKAH SINGLE is the most famous item in TOBACCO Category

data_frame_fam8 <- subset(data_frame_fam, Category %in% c('WINES'))
head(data_frame_fam8)

#here we can see item VLN CAB SAUV (GLS) is purchased 146 Customers in WINES Category
#Hence VLN CAB SAUV (GLS) is the most famous item in WINES Category

#for the observation no changes is found from two perspectives.

#3. Find the costliest item overall and in each category

#Most costliest item can be defined in two perspectives:-
#1)Cafe Perspective: Most costliest item in the Cafe i.e rate
#2)Customer Perspective: Most costliest item which includes, Rate+Discount+Taxes.
#Performing Both perspectives in the below.

#1)Below analysis from Cafe Perspective ie from rate, finding the costliest item

data_frame_cost <- aggregate(x = data_frame$Category, 
                             by = list(data_frame$Item_Desc, data_frame$Category, 
                                       data_frame$Rate), length)

View(data_frame_cost)

colnames(data_frame_cost)[1] <- 'Item_Desc'
colnames(data_frame_cost)[2] <- 'Category'
colnames(data_frame_cost)[3] <- 'Rate'
colnames(data_frame_cost)[4] <- 'ntimes'

data_frame_cost <- arrange(data_frame_cost, -Rate)

head(data_frame_cost)

#Here we can see item GOSSIPS CHARD AUS (BTL) has rate 2100.
#Hence GOSSIPS CHARD AUS (BTL) is most costliest item overall

data_frame_cost1 <- subset(data_frame_cost, Category %in% c('BEVERAGE'))
head(data_frame_cost1)

#Here we can see item 5 RED BULL has rate 450 in BEVERAGE Category.
#Hence 5 RED BULL is most costliest item in BEVERAGE Category.

data_frame_cost2 <- subset(data_frame_cost, Category %in% c('FOOD'))
head(data_frame_cost2)

#Here we can see item J.PCHENET SPARKLING ROSE (BTL) has rate 1700 in FOOD Category.
#Hence J.PCHENET SPARKLING ROSE (BTL) is most costliest item in FOOD Category.

data_frame_cost3 <- subset(data_frame_cost, Category %in% c('LIQUOR'))
head(data_frame_cost3)

#Here we can see item STELLA 1LTR 2+1 has rate 1300 in LIQUOR Category.
#Hence STELLA 1LTR 2+1 is most costliest item in LIQUOR Category.


data_frame_cost4 <- subset(data_frame_cost, Category %in% c('MERCHANDISE'))
head(data_frame_cost4)

#Here we can see item FLAVOR 1000 GMS has rate 1470 in MERCHANDISE Category.
#Hence FLAVOR 1000 GMS is most costliest item in MERCHANDISE Category.

data_frame_cost5 <- subset(data_frame_cost, Category %in% c('TOBACCO'))
head(data_frame_cost5)

#Here we can see item LATE HARVEST SULA CHENIN (BTL) and  VALENTINE SPECIAL SHEESHA has rate 500 in TOBACCO Category.
#Hence LATE HARVEST SULA CHENIN (BTL) and  VALENTINE SPECIAL SHEESHA is most costliest item in TOBACCO Category.

data_frame_cost6 <- subset(data_frame_cost, Category %in% c('MISC'))
head(data_frame_cost6)

#Here we can see item HOEGAARDEN LTR MUGS (2+1) has rate 1300 in MISC Category.
#Hence HOEGAARDEN LTR MUGS (2+1) is most costliest item in MISC Category.

data_frame_cost7 <- subset(data_frame_cost, Category %in% c('WINES'))
head(data_frame_cost7)

#Here we can see item GOSSIPS CHARD AUS (BTL) has rate 2100 in WINES Category.
#Hence GOSSIPS CHARD AUS (BTL) is most costliest item in WINES Category.

data_frame_cost8 <- subset(data_frame_cost, Category %in% c('LIQUOR & TPBACCO'))
head(data_frame_cost8)

#Here we can see item 4 DOM BEER + 1SPL SHEESHA has rate 750 in LIQUOR & TPBACCO Category.
#Hence 4 DOM BEER + 1SPL SHEESHA is most costliest item in LIQUOR & TPBACCO Category.

#2)Below analysis from customer Perspective:

#Here Total includes Rate+Discount+Taxes
#From the data, I see that Total includes Rate+Taxes. Assuming Discount already considered in rate.

data_frame_cost_cus <- aggregate(x = data_frame$Bill_Number,
                                 by =  list(data_frame$Item_Desc, data_frame$Category,
                                            data_frame$Quantity, data_frame$Total), length)

View(data_frame_cost_cus)

colnames(data_frame_cost_cus)[1] <- 'Item_Desc'
colnames(data_frame_cost_cus)[2] <- 'Category'
colnames(data_frame_cost_cus)[3] <- 'Quantity'
colnames(data_frame_cost_cus)[4] <- 'Total'

data_frame_cost_cus$Total_Per_Item <- round(data_frame_cost_cus$Total/data_frame_cost_cus$Quantity, 2)

data_frame_cost_cus1 <- aggregate(x = data_frame_cost_cus$Total_Per_Item,
                                  by = list(data_frame_cost_cus$Item_Desc,
                                            data_frame_cost_cus$Category,
                                            data_frame_cost_cus$Total_Per_Item), length)
View(data_frame_cost_cus1)

colnames(data_frame_cost_cus1)[1] <- 'Item_Desc'
colnames(data_frame_cost_cus1)[2] <- 'Category'
colnames(data_frame_cost_cus1)[3] <- 'Total'

data_frame_cost_cus1 <- arrange(data_frame_cost_cus1, -Total)

head(data_frame_cost_cus1)
#Here we can see item GOSSIPS CHARD AUS (BTL) has Total 2756.25.
#Hence GOSSIPS CHARD AUS (BTL) is most costliest item overall

data_frame_cost_cus2 <- subset(data_frame_cost_cus1, Category %in% c('BEVERAGE'))
head(data_frame_cost_cus2)

#Here we can see item N R G HOOKAH has Total 594.00 in BEVERAGE Category.
#Hence N R G HOOKAH is most costliest item in BEVERAGE Category.

data_frame_cost_cus3 <- subset(data_frame_cost_cus1, Category %in% c('FOOD'))
head(data_frame_cost_cus3)

#Here we can see item J.PCHENET SPARKLING ROSE (BTL) has Total 2142.00 in FOOD Category.
#Hence J.PCHENET SPARKLING ROSE (BTL) is most costliest item in FOOD Category.

data_frame_cost_cus4 <- subset(data_frame_cost_cus1, Category %in% c('LIQUOR'))
head(data_frame_cost_cus4)

#Here we can see item STELLA 1LTR 2+1 has Total 1706.25 in LIQUOR Category.
#Hence STELLA 1LTR 2+1 is most costliest item in LIQUOR Category.


data_frame_cost_cus5 <- subset(data_frame_cost_cus1, Category %in% c('MERCHANDISE'))
head(data_frame_cost_cus5)

#Here we can see item FLAVOR 1000 GMS has Total 1653.75 in MERCHANDISE Category.
#Hence FLAVOR 1000 GMS is most costliest item in MERCHANDISE Category.

data_frame_cost_cus6 <- subset(data_frame_cost_cus1, Category %in% c('TOBACCO'))
head(data_frame_cost_cus6)

#Here we can see item VALENTINE SPECIAL SHEESHA has Total 660 in TOBACCO Category.
#Hence VALENTINE SPECIAL SHEESHA is most costliest item in TOBACCO Category.

data_frame_cost_cus7 <- subset(data_frame_cost_cus1, Category %in% c('MISC'))
head(data_frame_cost_cus7)

#Here we can see item HOEGAARDEN LTR MUGS (2+1) has Total 1706.25 in MISC Category.
#Hence HOEGAARDEN LTR MUGS (2+1) is most costliest item in MISC Category.

data_frame_cost_cus8 <- subset(data_frame_cost_cus1, Category %in% c('WINES'))
head(data_frame_cost_cus8)

#Here we can see item GOSSIPS CHARD AUS (BTL) has Total 2756.25 in WINES Category.
#Hence GOSSIPS CHARD AUS (BTL) is most costliest item in WINES Category.

data_frame_cost_cus9 <- subset(data_frame_cost_cus1, Category %in% c('LIQUOR & TPBACCO'))
head(data_frame_cost_cus9)

#Here we can see item 4 DOM BEER + 1SPL SHEESHA has Total 984.38 in LIQUOR & TPBACCO Category.
#Hence 4 DOM BEER + 1SPL SHEESHA is most costliest item in LIQUOR & TPBACCO Category.

#At finally no major changes found in cafe perspective and customer perspective, only change of
#     item found in Beverage category.

#4. Create plots to explore category-wise share of revenue

data_frame_share <- aggregate(x = data_frame$Total
                              , by = list(data_frame$Item_Desc, data_frame$Category),
                              FUN = sum)
View(data_frame_share)

colnames(data_frame_share)[1] <- 'Item_Desc'
colnames(data_frame_share)[2] <- 'Category'
colnames(data_frame_share)[3] <- 'Amount'

data_frame_share1 <- subset(data_frame_share, Category %in% c('BEVERAGE'))
data_frame_share2 <- subset(data_frame_share, Category %in% c('FOOD'))
data_frame_share3 <- subset(data_frame_share, Category %in% c('LIQUOR'))
data_frame_share4 <- subset(data_frame_share, Category %in% c('MERCHANDISE'))
data_frame_share5 <- subset(data_frame_share, Category %in% c('TOBACCO'))
data_frame_share6 <- subset(data_frame_share, Category %in% c('MISC'))
data_frame_share7 <- subset(data_frame_share, Category %in% c('WINES'))
data_frame_share8 <- subset(data_frame_share, Category %in% c('LIQUOR & TPBACCO'))



data_frame_share_TB <- data.frame(Category = c((unique(data_frame_share1$Category)), 
                                               (unique(data_frame_share2$Category)), 
                                               (unique(data_frame_share3$Category)), 
                                               (unique(data_frame_share4$Category)), 
                                               (unique(data_frame_share5$Category)), 
                                               (unique(data_frame_share6$Category)), 
                                               (unique(data_frame_share7$Category)), 
                                               (unique(data_frame_share8$Category))),
                                  Amount = c(sum(data_frame_share1$Amount), 
                                             sum(data_frame_share2$Amount), 
                                             sum(data_frame_share3$Amount), 
                                             sum(data_frame_share4$Amount), 
                                             sum(data_frame_share5$Amount), 
                                             sum(data_frame_share6$Amount), 
                                             sum(data_frame_share7$Amount), 
                                             sum(data_frame_share8$Amount)))

View(data_frame_share_TB) #We can see Revenue Category wise

sum(data_frame_share_TB$Amount) #Total Revenue = 32805895

###Pie Chart

perc <- round(data_frame_share_TB$Amount/sum(data_frame_share_TB$Amount)*100, 1)

lab <- paste(data_frame_share_TB$Category, perc, '%')

pie(data_frame_share_TB$Amount, labels = lab,
    radius = 1, col = hcl.colors(n = 8),
    border = 'black', main = 'Total Revenue')

library(plotrix)
pie3D(data_frame_share_TB$Amount,
      explode = 0.1,
      main = "Total Revenue",
      labels = lab)

###Bar Chart

#Barchat with amount as y axis
barplot(data_frame_share_TB$Amount, xlab = 'Category',
        ylab = 'Amount',names.arg = data_frame_share_TB$Category,
        col = hcl.colors(8, palette = 'Earth'),
        main = 'Total Revenue')  

#Barchat with percentage as y axis
barplot(perc, xlab = 'Category',
        ylab = 'Percentage',names.arg = data_frame_share_TB$Category,
        col = hcl.colors(8, palette = 'Earth'),
        main = 'Total Revenue') 

###ggplot
library(ggplot2)

#ggplot with amount as y axis
ggplot(data_frame_share_TB, aes(x = Category, y = Amount)) + 
    geom_bar(stat = "identity", fill = hcl.colors(8, 'Earth'))+
    ggtitle('Bar Plot for Total Revenue in Amount')+
    theme(plot.title = element_text(hjust=0.5))+
    geom_label(aes(label = Amount))

#ggplot with percentage as y axis
ggplot(data_frame_share_TB, aes(x = Category, y = perc)) + 
    geom_bar(stat = "identity", fill = hcl.colors(8, 'Earth'))+ 
    labs(x = 'Category', y = 'Pecentage')+
    ggtitle('Bar Plot for Total Revenue in Percentage')+
    theme(plot.title = element_text(hjust=0.5)) +
    geom_label(aes(label = paste(perc,'','%')))

#5. Describe the day-wise sales trends

data_frame_sales <- aggregate(x = data_frame$Total, 
                              by = list(data_frame$Date),
                              FUN = sum)
View(data_frame_sales)

colnames(data_frame_sales)[1] <- 'Date'
colnames(data_frame_sales)[2] <- 'Revenue'

length(unique(data_frame_sales$Date))
#we see total number of days is 361, if the data is for year, assuming
#     remaining 5 days has holidays or Cafe not worked

head(arrange(data_frame_sales, -Revenue))

#here we see highest revenue of 350968.5 on 2020-10-01

head(arrange(data_frame_sales, Revenue))

#here we see lowest revenue of 47176.66 on 2020-02-04

#Kernal Density Plot

plot(density(data_frame_sales$Revenue),
     main = "Total Revenue") 
  polygon(density(data_frame_sales$Revenue), col = "green", border = "blue")

#From kernal denisty plot we observed that most-
    #of revenue for a cafe is in between 50000 and 150000

#Scatter Plot for day-wise sales trend:
  
sca <- as.Date(as.POSIXct(data_frame_sales$Date), tz = "")

#Scatter Plot with month as x axis
plot(data_frame_sales$Date,
     data_frame_sales$Revenue, pch = 19, col = 'blue',
     xlab = "Date", ylab = 'Revenue', main = "Day-wise Sales Revenue")

#Scatter Plot with Date as x axis
plot(sca,
     data_frame_sales$Revenue, pch = 19, col = 'blue',
     xlab = "Date", ylab = 'Revenue', main = "Day-wise Sales Revenue")
axis(1, sca,format(sca, "%Y-%m-%d"))  
#Since it has 361 days of data, plot looks bit ugly


#Line Chart

plot(data_frame_sales$Revenue, type = 'l',
     xlab = "Day",
     ylab = 'Revenue',
     main = 'Revenue vs Day',
     col = 'dark blue')

#Monthly Revenue
data_frame_sales_month <- aggregate(data_frame_sales$Revenue, 
                                    list(months(data_frame_sales$Date)), sum)
View(data_frame_sales_month)

class(data_frame_sales_month$Group.1)#Character

#converting data_frame_sales_month$Group.1 to factor to view months in sequence

data_frame_sales_month2 <- data_frame_sales_month %>% 
  mutate(Month = factor(Group.1, 
                        levels = month.name)) %>% arrange(Month)

View(data_frame_sales_month2)

colnames(data_frame_sales_month2)[2] <- "Revenue"

#ggplot for monthly revenue
ggplot(data_frame_sales_month2, aes(x = Month, y = Revenue)) +
  geom_bar(stat = "identity", fill= "#693ead")

#6. Describe customer traffic in monthly, daily, and hourly granularity

 
data_frame_customer <-aggregate(x = data_frame$Bill_Number, 
                               by = list(data_frame$Date, data_frame$Bill_Number, 
                                         substr(data_frame$Time,1,2)),  length)
View(data_frame_customer)

colnames(data_frame_customer)[1] <- 'Date'
colnames(data_frame_customer)[2] <- 'Bill_Number'
colnames(data_frame_customer)[3] <- 'Hours'


data_frame_customer2 <- aggregate(x = data_frame_customer$Bill_Number,
                                  by = list(data_frame_customer$Date), length)

colnames(data_frame_customer2)[1] <- 'Date'
colnames(data_frame_customer2)[2] <- 'nCustomers'

#here in data_frame_customer2 we can see daily customer traffic
View(data_frame_customer2)

#Below is analyzing for Daily customer Traffic:

#we see total number of days in data is 361. if the data is for year, i am 
#   assuming remaining 4 days as holidays/not working of cafe.

summary(data_frame_customer2)

head(arrange(data_frame_customer2, -nCustomers)) 
#here we can see on 2020-10-01 has highest customer traffic in whole year with 595 customers

head(arrange(data_frame_customer2, nCustomers)) 
#here we can see on 2020-02-04 has lowest customer traffic in whole year with 120 customers

#Kernal Density Plot for Customer Traffic

plot(density(data_frame_customer2$nCustomers),
     main = "Kernal Denisty Plot for Customers Data") 
  polygon(density(data_frame_customer2$nCustomers), col = "yellow", border = "blue")

#from Kernal density plot observed that most of the customers traffic is between 100 to 300 in cafe.

#Scatter Plot for Daily Customer Traffic

plot(data_frame_customer2$Date, data_frame_customer2$nCustomers, pch = 19, col = 'red',
     xlab = "Date", ylab = 'Customers', main = "Daily Customer Traffic")

#Below is analyzing for monthly customer Traffic:

data_frame_customer3 <- aggregate(data_frame_customer2$nCustomers, 
                                    list(months(data_frame_customer2$Date)), sum)
View(data_frame_customer3)

#converting data_frame_sales_month$Group.1 to factor to view months in sequence

data_frame_customer3 <- data_frame_customer3 %>% 
  mutate(Month = factor(Group.1, 
                        levels = month.name)) %>% arrange(Month)
str(data_frame_customer3)

head(arrange(data_frame_customer3, -x))
#Here we can see highest customer traffic in month of September with 6793

head(arrange(data_frame_customer3, x))
#Here we can see Lowest customer traffic in month of February with 4933

#Pie Chart for Montly customer Traffic

perc2 <- round(data_frame_customer3$x/sum(data_frame_customer3$x)*100, 1)

lab2 <- paste(data_frame_customer3$Month, perc2, '%')

pie(data_frame_customer3$x, labels = lab2,
    radius = 1, col = hcl.colors(n = 8),
    border = 'black', main = 'Monthly Customer Traffic')

library(plotrix)
pie3D(data_frame_customer3$x,
      explode = 0.1,
      main = "Monthly Customer Traffic",
      labels = lab2)

#bar Chart for Montly customer Traffic

barplot(data_frame_customer3$x, xlab = 'Month',
        ylab = 'Customers',names.arg = data_frame_customer3$Month,
        col = hcl.colors(10, palette = 'Earth'),
        main = 'Monthly Customer Traffic')

#Line Chart for Montly customer Traffic

plot(data_frame_customer3$x, type = 'b',
     xlab = "Month",
     ylab = 'Customers',
     main = 'Monthly Customer Traffic',
     col = 'blue')

#Below is analyzing for hourly customer Traffic:

data_frame_customer4 <- aggregate(x= data_frame_customer$Bill_Number,
                                  list(data_frame_customer$Hours), length)

View(data_frame_customer4)
# from data_frame_customer4 observed that in 24 hours of customers data, 
#   there is no customers in hour at 7 and 8 o'clock hence adding zero
#     customers at 7 and 8 o'clock

data_frame_customer4[nrow(data_frame_customer4) + 1,] <- c('07',0)
data_frame_customer4[nrow(data_frame_customer4) + 1,] <- c('08',0)

library(dplyr)
data_frame_customer4 <- arrange(data_frame_customer4, Group.1)

str(data_frame_customer4)# need to convert char to int

data_frame_customer4$Group.1 <- as.integer(as.character(data_frame_customer4$Group.1))
data_frame_customer4$x <- as.integer(as.character(data_frame_customer4$x))

str(data_frame_customer4)

#From data_frame_customer4, in x column we got total number of customers at that particular hour
#   in a whole year. Hence calculating average of customers per day in particular hour

customers_average <- round(data_frame_customer4$x/(361), 2) #361 is number of working days

data_frame_customer4 <- cbind(data_frame_customer4, customers_average)

View(data_frame_customer4)

summary(data_frame_customer4)

head(arrange(data_frame_customer4, x))
#here observed that no customers at 7 and 8 Oclock and there is only 1 customer at 6 oclock.
#   and there is 8 customers at 5 oclock in whole year which is average customer rate is 0.02/day

head(arrange(data_frame_customer4, -x))
#here observed that Customers is peaks at 7 PM with 6354 customers in year and average
#   customer rate at 17.60/day at 7 PM

#Barplot for Average Number of customers per hour in a year

barplot(data_frame_customer4$customers_average, xlab = 'Hour',
        ylab = 'No. of Customers',names.arg = data_frame_customer4$Group.1,
        col = hcl.colors(8, palette = 'Earth'),
        main = 'Average Number of customers per hour in a year',) 

#Scatter Plot for Average Number of customers per hour in a year

plot(data_frame_customer4$Group.1, data_frame_customer4$customers_average, pch = 20, 
     col = 'Black', xlab = "Hour", ylab = 'Customers', 
     main = "Average Number of customers per hour in a year")

#Line Chart for Average Number of customers per hour in a year

plot(data_frame_customer4$customers_average, type = 'b',
     xlab = "Hour",
     ylab = 'Customers',
     main = 'Average Number of customers per hour in a year',
     col = 'Black')

library(ggplot2)
#ggplot for Average Number of customers per hour in a year
ggplot(data_frame_customer4, aes(x = Group.1 , y = customers_average))+
      labs(x = "Hour", y = "Average Customers") +
      geom_bar(stat = "identity", fill= "#693ead")+
      ggtitle("Average Number of customers per hour in a year")+
      theme(plot.title = element_text(hjust = 0.5))+
      geom_label(aes(label = customers_average, y = customers_average), 
                 fill = 'white', colour = 'black')

#7. Perform menu analysis to come up with the combo the customers will like the most.

#Here to come up with the combo the customers will like the most using Association Rules method

library(arules)
library(arulesViz)

df_Ass <- read.transactions(file = 'Cafe_Ocean.csv', 
                            format = 'single',
                            header = TRUE,
                            sep = ',',
                            rm.duplicates= TRUE,
                            cols = c("Bill Number", "Item Desc"))
df_Ass

View(df_Ass)

str(df_Ass)

summary(df_Ass)

inspect(df_Ass)

# item frequency
itfv <- itemFrequency(df_Ass, type = 'absolute')
itfv[order(itfv, decreasing = T)]

# plotted
itemFrequencyPlot(df_Ass, type = 'absolute', topN = 20)


itemFrequencyPlot(df_Ass, type = 'relative', topN = 20)

set.seed = 220


# Training Apriori on the dataset

#Here finding the combos for 2,3,4 and 5 items.

#for combo of 2 items:
ass_rules2 <- apriori(df_Ass, parameter = list(supp = 0.0004,
                                             conf = 0.04,
                                             minlen = 2))
ass_rules2

inspect(ass_rules2[1:5])

r_sort2 <- sort(ass_rules2, by = 'supp', decreasing = T)

inspect(r_sort2[1:5])

#From the result: 
#Taking the first combo in the result since it has high confidence.
#Item POUTINE WITH FRIES has 0.6% times appeared in the cart (support).
#Items POUTINE WITH FRIES and NIRVANA HOOKAH SINGLE has 12% Confidence to brought together

#for combo of 3 items:
ass_rules3 <- apriori(df_Ass, parameter = list(supp = 0.0004,
                                               conf = 0.04,
                                               minlen = 3))
ass_rules3

inspect(ass_rules3[1:5])

r_sort3 <- sort(ass_rules3, by = 'supp', decreasing = T)

inspect(r_sort3[1:5])

#From the result: 
#we see three top most result has support of 0.091% times in the cart. Hence Taking combo which has high confidence.
#Items MINERAL WATER(1000ML) and OCEAN SPECIAL SHAKE both appeared in the cart with 0.09% times (support)
#Items MINERAL WATER(1000ML), OCEAN SPECIAL SHAKE and SAMBUC has 22% Confidence to brought together

#for combo of 4 items:

ass_rules4 <- apriori(df_Ass, parameter = list(supp = 0.00004,
                                               conf = 0.004,
                                               minlen = 4))
ass_rules4

inspect(ass_rules4[1:5])

r_sort4 <- sort(ass_rules4, by = 'supp', decreasing = T)

inspect(r_sort4[1:5])

#From the result:
#we see four top most result has support of 0.02% times in -
#   the cart.Hence Taking combo which has high confidence.
#Items B.M.T. PANINI, MAGGI NDL ARRABIATA and MINERAL WATER(1000ML) appeared in the cart with 0.02% times (support)
#Items B.M.T. PANINI, MAGGI NDL ARRABIATA, MINERAL WATER(1000ML) and SAMBUCA has 80% Confidence to brought together

#for combo of 5 items:

ass_rules5 <- apriori(df_Ass, parameter = list(supp = 0.00004,
                                               conf = 0.004,
                                               minlen = 5))
ass_rules5

inspect(ass_rules5[1:5])

r_sort5 <- sort(ass_rules5, by = 'supp', decreasing = T)

inspect(r_sort5[1:11])


#From the result:
#we see top ten results has support of 0.01% hence taking combo which as high confidence.
#   In result 3 and 8th record shown confidence of 77%. considereing 3rd record.
#Items B.M.T. PANINI, MAGGI NDL ARRABIATA, MINERAL WATER(1000ML) and OCEAN SPECIAL SHAKE
#   appeared in the cart with 0.01% times (support)
#Items B.M.T. PANINI, MAGGI NDL ARRABIATA, MINERAL WATER(1000ML), OCEAN SPECIAL SHAKE and
#   SAMBUCA has 100% Confidence to brought together

#Conclusion:

#If the cafe wants to go with combo of 2 Items then suggesting Below Items:
#1. POUTINE WITH FRIES and 
#2. NIRVANA HOOKAH SINGLE

#If the cafe wants to go with combo of 3 Items then suggesting Below Items:
#1. MINERAL WATER(1000ML), 
#2. OCEAN SPECIAL SHAKE and 
#3. SAMBUC

#If the cafe wants to go with combo of 4 Items then suggesting Below Items:
#1. B.M.T. PANINI, 
#2. MAGGI NDL ARRABIATA, 
#3. MINERAL WATER(1000ML) and 
#4. SAMBUCA

#If the cafe wants to go with combo of 5 Items then suggesting Below Items:
#1. B.M.T. PANINI,
#2. MAGGI NDL ARRABIATA,
#3. MINERAL WATER(1000ML),
#4. OCEAN SPECIAL SHAKE and
#5. SAMBUCA

###########################End of Code##########################################