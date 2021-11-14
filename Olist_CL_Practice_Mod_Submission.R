# load libraries

library(pacman)
p_load(readr,tidyverse,ggplot2,dplyr,lubridate,scales,stats,prophet,Metrics,mvrsquared,plm) #https://facebook.github.io/prophet/docs/quick_start.html#r-api

# load datasets

# customer dataset
customer <- read_csv("~/OneDrive/Documents/Courses/Semester 2/Practice Module/Olist/olist_customers_dataset.csv")
View(customer)

# orders dataset
orders <- read_csv("~/OneDrive/Documents/Courses/Semester 2/Practice Module/Olist/olist_orders_dataset.csv")
View(orders)

# payments dataset
payments <- read_csv("~/OneDrive/Documents/Courses/Semester 2/Practice Module/Olist/olist_order_payments_dataset.csv")
View(payments)

# order items dataset
order_items <- read_csv("~/OneDrive/Documents/Courses/Semester 2/Practice Module/Olist/olist_order_items_dataset.csv")

# seller dataset
sellers <- read_csv("~/OneDrive/Documents/Courses/Semester 2/Practice Module/Olist/olist_sellers_dataset.csv")
View(sellers)

# merge orders + payments datasets
mergedata = merge(x=orders,y=payments, by="order_id",all.y=TRUE)

# merge with customer dataset
data = merge(x=mergedata,y=customer, by="customer_id",all.y=TRUE) #note: by merging the datasets, duplicate records have been removed.

# merge with order items dataset
data2=merge(x=data,y=order_items,by="order_id",all.y=TRUE) #add on order items

# merge with seller dataset
data3=merge(x=data2,y=sellers,by="seller_id",all.y=TRUE) #add on seller data

data4=data3%>% drop_na() #2793 records with NA removed from clean dataset

# export merged dataset to csv for future use
write.csv(data4,"/Users/zephyr/OneDrive/Documents/Courses/Semester 2/Practice Module/Olist/Merged Datasets/MergedData_7Nov.csv", row.names = TRUE)

##################################  EXPLORATORY DATA ANALYSIS PT 1 ##################################  

#summarize data for charting
summary <- data %>% count (customer_state,customer_unique_id, sort=TRUE)

#plot chart #1 - no. of repeat purchases by unique customers state
ggplot(data=summary,mapping = aes(x=customer_state,y=n)) + geom_point(alpha=1/3) + ggtitle("Spread of Repeat Purchases by Unique Customers by State") + xlab("State") + ylab ("No. of Repeat Purchases")
# analysis: can see outlier points for repeat unique customers, where highest frequency is 33. next thing to check is how much they spent.
# analysis: SP customers have the highest repeat purchase across all states. there's one outlier in the MT state where the person has made close to 30 repeat purchases in the past 2 years.

#plot chart #2 - revenue by unique customers state
ggplot(data=revenue_by_mth,mapping = aes(x=order_purchase_mthyr,y=payment_value_sum)) + geom_point(alpha=1/3) + 
  ggtitle("Spread of Revenue per Unique Customers") + xlab("Year/Month") + ylab ("Revenue per Customer ($)") +
  scale_y_continuous(labels = scales::comma)

#retrieve mean & standard deviation for revenue data by state
customerrev= revenue_by_mth %>% group_by(customer_state) %>% summarise(customer_state_rev = sum(payment_value_sum), customer_state_mean=mean(payment_value_sum), customer_state_sd=sd(payment_value_sum)) 

#retrieve mean & standard deviation for revenue data for entire dataset
customerrev1= revenue_by_mth %>% group_by(order_purchase_mthyr) %>% summarise(revenue_sum = sum(payment_value_sum), revenue_mean=mean(payment_value_sum), revenue_sd=sd(payment_value_sum)) 

#summarize- no. of customers by state
customerstate= summary %>% group_by(customer_state) %>% summarise(customer_state_n = n(), customer_state_mean=mean(n), customer_state_sd=sd(n)) 
print(customerstate)
overallstat = summary%>% summarise(customer_state_mean=mean(n), customer_state_sd=sd(n)) #retrieve mean + standard deviation for whole dataset to compare against state level data.

#plot chart #3- No. of Unique Customers by State
ggplot(customerstate, aes(x = reorder(customer_state, desc(customer_state_n)), y = customer_state_n, 
                          fill = customer_state, position_dodge(width = 1),vjust = -0.5)) + 
  geom_bar(stat = "identity") + ggtitle("No. of Unique Customers by State") + xlab("State") + ylab ("No. of Customers") +
  geom_text(data=customerstate, aes(label = comma(round(customer_state_n, digits = -1))), size=3) +
  scale_y_continuous(labels = scales::comma)

#cross check unique customer by state
unicustomer = subset(customer, select=-c(customer_id,customer_zip_code_prefix,customer_city))
duplicated(unicustomer)
unicustomer = unicustomer[!duplicated(unicustomer),] #to check: 3305 duplicate rows removed.

#summarise merged dataset by revenue
revenue_state= data %>% group_by(customer_state) %>% summarise(payment_value_s=sum(payment_value, na.rm=TRUE)) 

#plot chart #4 - Revenue by State
ggplot(revenue_state, aes(x = reorder(customer_state, desc(payment_value_s)), y = payment_value_s, 
                          fill = customer_state, position_dodge(width = 1),vjust = -0.5)) + 
  geom_bar(stat = "identity") + ggtitle("Revenue by State") + xlab("State") + ylab ("Revenue") +
  geom_text(data=revenue_state, aes(label = comma(payment_value_s)), size=3) +
  scale_y_continuous(labels = scales::comma)

##################################  DATA PREPARATION ##################################  

#retrieve time dimensions from timestamp
data$order_purchase_date = lubridate::date(data$order_purchase_timestamp)
data$order_purchase_month = format(data$order_purchase_timestamp,format="%m")
data$order_purchase_year = format(data$order_purchase_timestamp,format="%Y")
data$order_purchase_qtr = quarters(as.Date(data$order_purchase_timestamp))
data$order_purchase_qtryr=paste(data$order_purchase_year,data$order_purchase_qtr)
data$order_purchase_mthyr=paste(data$order_purchase_year,data$order_purchase_month)

# from the dataset we can see that the time period of the dataset is from oct 2016 to aug 2018. 
# however, there is missing data from nov-dec 2016, which renders 2016 data unusable. 
# due to the lack of full year time data, we will have to do forecasting on a quarterly basis instead. 

# remove 2016 datapoints
data_clean=data[!(data$order_purchase_year==2016),] #346 records from 2016 are removed

# remove NA values in the dataset
data_clean=data_clean%>% drop_na() #3090 records with NA removed from clean dataset

# export clean merged dataset to csv for future use
write.csv(data_clean,"/Users/zephyr/OneDrive/Documents/Courses/Semester 2/Practice Module/Olist/Merged Datasets/CleanMergedData.csv", row.names = TRUE)

##################################  EXPLORATORY DATA ANALYSIS PT 2 ##################################  

####### NO. OF ORDERS BY QUARTER & MONTH #######

#summarise data of no. of orders by quarter & year 
orders_by_qtr <- data_clean %>% group_by(customer_state,order_purchase_qtryr) %>% summarise(order_id = n())

#ggplot for quarterly no. of orders by state by year. only showing data labels for top market

ggplot(data=orders_by_qtr, aes(x=order_purchase_qtryr, y=order_id, fill=customer_state))+      
  geom_bar(stat = "identity",color="black") + ggtitle("No. of Orders by State by Quarter/Year") + xlab("Year-Quarter") + ylab ("No. of Orders") +
  geom_text(data=subset(orders_by_qtr, customer_state== "SP"), 
            aes(label=order_id), size=4,position = position_stack(vjust = 0.5)) 

#summarise data of no. of orders by month & year 
orders_by_mth <- data_clean %>% group_by(customer_state,order_purchase_mthyr) %>% summarise(order_id = n())

#ggplot for monthly no. of orders by year 
ggplot(orders_by_mth, aes(x = order_purchase_mthyr, y = order_id, fill=customer_state,label=order_id)) + 
  geom_bar(stat = "identity",color="black") + ggtitle("No. of Orders by Month/Year") + xlab("Year-Month") + ylab ("No. of Orders") +
  geom_text(size = 3, position = position_stack(vjust = 0.5))

#ggplot for monthly no. of orders by state by year ## use this
ggplot(orders_by_mth, aes(x = order_purchase_mthyr, y = order_id, fill=customer_state,label=order_id)) + 
  geom_bar(stat = "identity",color="black") + ggtitle("No. of Orders by Month/Year") + xlab("Year-Month") + ylab ("No. of Orders") +
  geom_text(data=subset(orders_by_mth, customer_state== "SP"), 
            aes(label=order_id), size=3,position = position_stack(vjust = 0.5)) +
  scale_y_continuous(labels = scales::comma)

####### SALES REVENUE BY QUARTER & MONTH #######

#summarise revenue by quarter & year 
revenue_by_qtr <- data_clean %>% group_by(customer_state,order_purchase_qtryr) %>% summarise(payment_value_sum = sum(payment_value))
revenue_by_qtr1 <- data_clean %>% group_by(order_purchase_qtryr) %>% summarise(payment_value_sum = sum(payment_value))

#ggplot for quarterly revenue by year 
ggplot(revenue_by_qtr1, aes(x = order_purchase_qtryr, y = payment_value_sum)) + 
  geom_bar(stat = "identity") + ggtitle("Revenue by Quarter/Year") + xlab("Year-Quarter") + ylab ("Revenue ($)") + geom_text(aes(label = round(payment_value_sum, digits = 0)), vjust = -0.5)

#summarise revenue by month & year 
revenue_by_mth <- data_clean %>% group_by(customer_state,order_purchase_mthyr) %>% summarise(payment_value_sum = sum(payment_value))
revenue_by_mth1 <- data_clean %>% group_by(order_purchase_mthyr) %>% summarise(payment_value_sum = sum(payment_value))

#ggplot for monthly revenue by year 
ggplot(revenue_by_mth1, aes(x = order_purchase_mthyr, y = payment_value_sum)) + 
  geom_bar(stat = "identity") + ggtitle("Revenue by Month/Year") + xlab("Year-Month") + ylab ("Revenue ($)") + 
  geom_text(aes(label = round(payment_value_sum, digits = 0)), vjust = -0.5) +
  scale_y_continuous(labels = scales::comma)

#ggplot for monthly revenue by state by year ## use this
ggplot(revenue_by_mth, aes(x = order_purchase_mthyr, y = payment_value_sum, fill = customer_state, label = payment_value_sum)) + 
  geom_bar(stat = "identity",color="black") + ggtitle("Revenue by Month/Year by State") + xlab("Year-Month") + ylab ("Revenue ($)") + 
  geom_text(data=subset(revenue_by_mth, customer_state== "SP"),
  aes(label = comma(payment_value_sum)), size=3,position = position_stack(vjust = 0.5)) +
  scale_y_continuous(labels = scales::comma)

################################## Overall Forecast ##################################  

#check dataset again - remove duplicates
data = data_clean %>% distinct()

#check dataset again - remove NAs
data = data %>% drop_na()

names(data) #retrieve all column names in dataset
# [1] "customer_id"                   "order_id"                      "order_status"                 
# [4] "order_purchase_timestamp"      "order_approved_at"             "order_delivered_carrier_date" 
# [7] "order_delivered_customer_date" "order_estimated_delivery_date" "payment_sequential"           
# [10] "payment_type"                  "payment_installments"          "payment_value"                
# [13] "customer_unique_id"            "customer_zip_code_prefix"      "customer_city"                
# [16] "customer_state"                "order_purchase_date"           "order_purchase_month"         
# [19] "order_purchase_year"           "order_purchase_qtr"            "order_purchase_qtryr"         
# [22] "order_purchase_mthyr"  

#select only relevant columns from dataset for forecasting
datafc = subset(data,select=c(payment_type,payment_installments,payment_value,customer_state,order_purchase_date,order_purchase_month))

datafc_sp = subset(datafc,customer_state=="SP")

# export dataset to csv for time series forecasting
write.csv(datafc,"/Users/zephyr/OneDrive/Documents/Courses/Semester 2/Practice Module/Olist/Merged Datasets/TimeSeriesData.csv", row.names = TRUE)

write.csv(datafc_sp,"/Users/zephyr/OneDrive/Documents/Courses/Semester 2/Practice Module/Olist/Merged Datasets/TimeSeriesData_SaoPaolo.csv", row.names = TRUE)

autoplot(TimeSeriesData_Month$Revenue)

P_Data= data %>% group_by(order_purchase_date) %>% summarise(revenue = sum(payment_value)) #summarize data for all states
P_SP_Data= datafc_sp %>% group_by(order_purchase_date) %>% summarise(revenue = sum(payment_value)) #summarize data for Sao Paolo only

colnames(P_SP_Data) <- c('ds','y') # prophet model requires renaming of col names

write.csv(P_SP_Data,"/Users/zephyr/OneDrive/Documents/Courses/Semester 2/Practice Module/Olist/Merged Datasets/ProphetData_SaoPaolo.csv", row.names = TRUE)

P_Data = P_Data %>% rename (ds=order_purchase_date,y=revenue)
write.csv(P_Data,"/Users/zephyr/OneDrive/Documents/Courses/Semester 2/Practice Module/Olist/Merged Datasets/ProphetData.csv", row.names = TRUE)
write.csv(forecast,"/Users/zephyr/OneDrive/Documents/Courses/Semester 2/Practice Module/Olist/Merged Datasets/ProphetForecast.csv", row.names = TRUE)

##### Need to split train & test data

train = subset(P_Data, ds <= "2018-04-30")
test = subset(P_Data,ds>"2018-04-30")

m1=prophet(train,interval.width=0.95,yearly.seasonality = TRUE) #confidence interval of 95%, with applied yearly seasonality

##### Need to split train & test data

m=prophet(P_Data) #The underlying algorithm is a generalized additive model that is decomposable into three main components: trend, seasonality, and holidays. 

# Disabling yearly seasonality. Run prophet with yearly.seasonality=TRUE to override this.
# Disabling daily seasonality. Run prophet with daily.seasonality=TRUE to override this.

future = make_future_dataframe(m1,periods=245)
tail(future)
forecast = predict(m1, future)
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

# predict test data

forecast_test = predict(m1,test)
plot(m1,forecast_test, xlab = "Year", ylab = "Predicted Revenue vs. Actual Revenue") 

#plot residuals
forecast_test %>%
  mutate(resid = trend - yhat) %>%
  ggplot(aes(x = ds, y = resid)) +
  geom_hline(yintercept = 0, color = "red") +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth()

#plot actual revenue with forecasted revenue
plot(m1,forecast, xlab = "Year", ylab = "Predicted Revenue vs. Actual Revenue") 

prophet_plot_components(m2, forecast)

dyplot.prophet(m1, forecast, main="Revenue over Time (Generalized Additive Model)",xlab = "Year", ylab = "Revenue")

summary(m1)

#diagnostics

# cutoffs <- as.Date(c('2017-03-01', '2017-07-01', '2017-12-01','2018-03-01','2018-06-01'))
# df.cv2 <- cross_validation(m1, cutoffs = cutoffs, horizon = 1, units = 'days')

df.cv = cross_validation(m2, initial = 604, period = 121, horizon = 1, units = 'days')
head(df.cv)

# performance metrics
dfp <- performance_metrics(df.cv)
head(dfp)

write.csv(dfp2,"/Users/zephyr/OneDrive/Documents/Courses/Semester 2/Practice Module/Olist/Merged Datasets/dfp2.csv", row.names = TRUE)

write.csv(dfcv2,"/Users/zephyr/OneDrive/Documents/Courses/Semester 2/Practice Module/Prophet/dfcv2.csv", row.names = TRUE)

write.csv(forecast,"/Users/zephyr/OneDrive/Documents/Courses/Semester 2/Practice Module/Prophet/forecast.csv", row.names = TRUE)


plot_cross_validation_metric(dfcv2, metric = 'mape')

plot_cross_validation_metric(dfcv2, metric = 'rmse')



############# look at distribution of reviews data

reviewsdata= reviews %>% group_by(review_score) %>% summarise(review_score_n = count(review_score))

summary(reviews)

actual=forRMSE$y
predicted=forRMSE$yhat
rmse=rmse(actual,predicted)
rmse

mape=mape(actual,predicted)
mape

sse = sse(actual,predicted)
sse

r2=calc_rsquared(y=forRMSE$y,yhat=forRMSE$yhat)
r2

sst = 97205687974

r2_1=1-sse/sst

names(dfp2)
dfp2_agg <- dfp2 %>% summarise(mean(mse),mean(rmse),mean(mae),mean(mape),mean(mdape),mean(smape),mean(coverage))
dfp2_agg

################################## Forecast for #1 SP (Sao Paolo) ##################################  

m2=prophet(P_SP_Data)

future = make_future_dataframe(m2,periods=245)
tail(future)
forecast = predict(m2, future)
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

#plot actual revenue with forecasted revenue
plot(m2,forecast, xlab = "Year", ylab = "Predicted Revenue vs. Actual Revenue") 

prophet_plot_components(m2, forecast)

dyplot.prophet(m2, forecast, main="Sao Paulo Revenue over Time (Generalized Additive Model)",xlab = "Year", ylab = "Revenue")

summary(m2)

#remove outlier 

P_SP_Data1=P_SP_Data[-c(324), ]

m3=prophet(P_SP_Data1)
forecastm3 = predict(m3, future)
tail(forecastm3[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

plot(m3,forecastm3, xlab = "Year", ylab = "Predicted Revenue vs. Actual Revenue") 

prophet_plot_components(m3, forecastm3)

dyplot.prophet(m3, forecastm3, main="Sao Paulo Revenue over Time (Generalized Additive Model)",xlab = "Year", ylab = "Revenue")

summary(m3)

#include year seasonality 

m4=prophet(P_SP_Data1,yearly.seasonality = TRUE)
forecastm4 = predict(m4, future)
tail(forecastm4[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

plot(m4,forecastm4, xlab = "Year", ylab = "Predicted Revenue vs. Actual Revenue") 

prophet_plot_components(m4, forecastm4)

dyplot.prophet(m4, forecastm4, main="Sao Paulo Revenue over Time (Generalized Additive Model)",xlab = "Year", ylab = "Revenue")

summary(m4)