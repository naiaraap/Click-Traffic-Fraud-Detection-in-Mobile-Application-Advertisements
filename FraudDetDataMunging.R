# Project developed during the DSA BigData Analystics with R and Microsoft Azure course:

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#                                                                         #
# Click Traffic Fraud Detection in Mobile Application Advertisements      #
#                                                                         #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Project available on the kaggle platform
# DESCRIPTION:
# Fraud risk is everywhere, but for companies that advertise online,
# click fraud can happen at an overwhelming volume, resulting in misleading click data and wasted money.
# Ad channels can drive up costs by simply clicking on the ad at a large scale.
# China is the largest
# mobile market in the world and therefore suffers from huge volumes of fradulent traffic.
# TalkingData, China’s largest independent big data service platform, 
# covers over 70% of active mobile devices nationwide. 
# They've built an IP blacklist and device blacklist.
# In this project, I am supose to build an algorithm that predicts whether a user will download an app 
# after clicking a mobile app ad. 


# Setting work directory
setwd("/home/naiara/Documentos/DataScience/FCD/BigDataRAzure/Project_Fraud_Detection")

# Azure variable to develop in Microsoft Azure Machile Learning platform
Azure <- FALSE

# Importing train and test datasets
if (Azure) {
  train <- maml.mapInputPort(1)[10000000, ]
  test <- maml.mapInputPort(2)
}else {
  # Getting the first 10.000.000 rows of train dataset
  train <- read.csv(unz("train.csv.zip", "train.csv"), nrows = 10000000)
  # Getting test dataset
  test <- read.csv("test.csv")
}

# Checking if there are missing values.
any(is.na(train[,-7]))
any(is.na(test))

# Modifying variable types for analysis:
# Converting categorical variables to factor
cnames <- c('ip', 'app', 'device', 'os', 'channel')
for (var in cnames) {
  train[,var] <- as.factor(train[,var])
  test[,var] <- as.factor(test[,var])
}

# Converting target variable "is_attributted" to factor
train$is_attributed <- as.factor(train$is_attributed)

# Converting temporal variables to datetime format
train$click_time <- as.POSIXct(train$click_time, tz = Sys.timezone())
train$attributed_time <- dplyr::na_if(train$attributed_time, "")
train$attributed_time <- as.POSIXct(train$attributed_time, tz = Sys.timezone())
test$click_time <- as.POSIXct(test$click_time, tz = Sys.timezone())

# Converting "click_id" from test dataset to factor
test$click_id <- as.factor(test$click_id)

# Train dataset head
head(train)
# Teste dataset head
head(test)

# Train dataset summary
str(train)
summary(train)
summary(test)

# Plots using ggplot2
library(ggplot2)
library(dplyr)

# Get the number of unique values for categoriacal dependent variables
uniq <- sapply(train[,cnames], function(x) {return(length(unique(x)))})
uniq_data <- data.frame(value=uniq, name=names(uniq), row.names = NULL)


# Bar plot of the number of single values
uniq_plot <- ggplot(uniq_data, aes(x=name, y=value, fill=name)) +
  geom_bar(stat = "identity") +
  ggtitle("Number of unique values per feature (from 10.000.000 first rows)") +
  xlab("Feature") +
  ylab("log(unique count)") +
  scale_y_log10(limits = c(1,1e6)) +
  geom_text(aes(label = sprintf("%d", value), y=value),  vjust = -0.5) +
  theme(axis.text.x = element_text(hjust = 0.5, size=11,color="black")) +
  theme(axis.text.y = element_text(hjust = 0.5, size=10,color="black"))

uniq_plot

# Filtering and summarizing attribute variables from downloaded application logs
downloaded <- train[train$is_attributed == "1", c("attributed_time", "is_attributed")]
summary(downloaded)

# Plotting proportion of apps download and not download 
app_prop <- count(group_by(train[,c("app", "is_attributed")], is_attributed))
app_prop$n <- round(app_prop$n/sum(app_prop$n)*100, 2)
app_prop$is_attributed <- c("Not Downloaded (0)", "App Downloaded (1)")


appPropPlot <- ggplot(app_prop, aes(x=is_attributed, y=n)) +
  geom_bar(stat = "identity", fill="darkred") +
  ggtitle("App Downloaded x Not Downloades") +
  ylab("Percentage") +
  ylim(0,120) +
  geom_text(aes(label = sprintf("%.2f%%", n), y=n),  vjust = -0.5) +
  theme(axis.text.x = element_text(hjust = 0.5, size=11,color="black")) +
  theme(axis.text.y = element_text(hjust = 0.5, size=10,color="black"))

appPropPlot

# Count ips and their respective frequencies
temp <- as.data.frame(table(train$ip))
colnames(temp) <- c("ip", "count")
temp <- temp[order(temp$count, decreasing = TRUE),]
rownames(temp) <- 1:length(rownames(temp))
temp <- temp[1:10,]
temp


# Count downloaded ips and their respective frequencies
temp_downloaded <- as.data.frame(table(train[train$is_attributed == "1",]$ip))
colnames(temp_downloaded) <- c("downloaded_ip", "count_downloads")
temp_downloaded <- temp_downloaded[order(temp_downloaded$count, decreasing = TRUE),]
rownames(temp_downloaded) <- 1:length(rownames(temp_downloaded))
temp_downloaded <- temp_downloaded[1:10,]
temp_downloaded


# Checking coincide between 10 most clicked apss and the 10 most clicked downloaded apss
table(c(temp$ip,temp_downloaded$downloaded_ip))


# Get statistic data from downloaded ip dataset
# Minimum, maximum, average, median, quartiles.
summary(as.integer(train[train$is_attributed == "1", "ip"]))

# Count values
length(as.numeric(train[train$is_attributed == "1", "ip"]))

# Count unique values
length(unique(as.integer(train[train$is_attributed == "1", "ip"])))

# Converting "is_attributed" to numeric temporarily to plotting
train$is_attributed <- as.numeric(train$is_attributed)-1

# Conversion Rates over Counts of 300 Most Popular IPs
# Exibing table results
count_rate <- train %>% 
  group_by(ip) %>%
  summarise(click_count=n(), prop_downloaded = round(sum(is_attributed)/n(), 4)) %>%
  arrange(desc(click_count)) %>%
  slice(1:300) %>%
  data.frame()

count_rate

# Obtain the coefficient so that both y axes are on the same scale 
count_rate$num <- seq(1:nrow(count_rate))
MAX <- max(count_rate$click_count)
mx <- max(count_rate$prop_downloaded)
coef <- mx/MAX

# Disabling scientific notation in R
options(scipen = 999)

# Plotting Conversion Rates over Counts of 300 Most Popular IPs
ggplot(count_rate, aes(x=num)) +
  geom_line(aes(x=num, y=click_count), color="darkred") +
  geom_line(aes(x=num, y=prop_downloaded/coef), color="darkgreen") +
  ggtitle("Conversion Rates over Counts of 300 Most Popular IPs") +
  scale_y_continuous(
    # Features of the first axis
    name = "Number of clicks",
  
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coef, name="Proportion Downloaded")
  ) +
  theme(
    axis.title.y = element_text(color = "darkred", size=13),
    axis.title.y.right = element_text(color = "darkgreen", size=13),
    plot.title = element_text(hjust = 0.5)
  ) +
  xlab("")


# Conversion Rates over Counts of 100 Most Popular Apps
count_rate <- train %>% 
  group_by(app) %>%
  summarise(click_count=n(), prop_downloaded = round(sum(is_attributed)/n(), 4)) %>%
  arrange(desc(click_count)) %>%
  slice(1:100) %>%
  data.frame()

count_rate

# Obtain the coefficient so that both y axes are on the same scale 
count_rate$num <- seq(1:nrow(count_rate))
MAX <- max(count_rate$click_count)
mx <- max(count_rate$prop_downloaded)
coef <- mx/MAX

# Plotting Conversion Rates over Counts of 100 Most Popular Apps
ggplot(count_rate, aes(x=num)) +
  geom_line(aes(x=num, y=click_count), color="darkred") +
  geom_line(aes(x=num, y=prop_downloaded/coef), color="darkgreen") +
  ggtitle("Conversion Rates over Counts of 100 Most Popular Apps") +
  scale_y_continuous(
    # Features of the first axis
    name = "Number of clicks",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coef, name="Proportion Downloaded")
  ) +
  theme(
    axis.title.y = element_text(color = "darkred", size=13),
    axis.title.y.right = element_text(color = "darkgreen", size=13),
    plot.title = element_text(hjust = 0.5)
  ) +
  xlab("")



# Conversion Rates over Counts of Most Popular Operational Systems
count_rate <- train %>% 
  group_by(os) %>%
  summarise(click_count=n(), prop_downloaded = round(sum(is_attributed)/n(), 4)) %>%
  arrange(desc(click_count)) %>%
  slice(1:100) %>%
  data.frame()

count_rate

# Obtain the coefficient so that both y axes are on the same scale 
count_rate$num <- seq(1:nrow(count_rate))
MAX <- max(count_rate$click_count)
mx <- max(count_rate$prop_downloaded)
coef <- mx/MAX

# Plotting
ggplot(count_rate, aes(x=num)) +
  geom_line(aes(x=num, y=click_count), color="darkred") +
  geom_line(aes(x=num, y=prop_downloaded/coef), color="darkgreen") +
  ggtitle("Conversion Rates over Counts of 100 Most Popular Operating Systems") +
  scale_y_continuous(
    # Features of the first axis
    name = "Number of clicks",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coef, name="Proportion Downloaded")
  ) +
  theme(
    axis.title.y = element_text(color = "darkred", size=13),
    axis.title.y.right = element_text(color = "darkgreen", size=13),
    plot.title = element_text(hjust = 0.5)
  ) +
  xlab("")


# Conversion Rates and Counts of Most Popular by device
count_rate <- train %>% 
  group_by(device) %>%
  summarise(click_count=n(), prop_downloaded = round(sum(is_attributed)/n(), 4)) %>%
  arrange(desc(click_count)) %>%
  slice(1:100) %>%
  data.frame()

count_rate

# Obtain the coefficient so that both y axes are on the same scale 
count_rate$num <- seq(1:nrow(count_rate))
MAX <- max(count_rate$click_count)
mx <- max(count_rate$prop_downloaded)
coef <- mx/MAX

# Plotting
ggplot(count_rate, aes(x=num)) +
  geom_line(aes(x=num, y=click_count), color="darkred") +
  geom_line(aes(x=num, y=prop_downloaded/coef), color="darkgreen") +
  ggtitle("Conversion Rates over Counts of 100 Most Popular Devices") +
  scale_y_continuous(
    # Features of the first axis
    name = "Number of clicks",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coef, name="Proportion Downloaded")
  ) +
  theme(
    axis.title.y = element_text(color = "darkred", size=13),
    axis.title.y.right = element_text(color = "darkgreen", size=13),
    plot.title = element_text(hjust = 0.5)
  ) +
  xlab("")



# Conversion Rates over Counts of Most Popular Channels
count_rate <- train %>% 
  group_by(channel) %>%
  summarise(click_count=n(), prop_downloaded = round(sum(is_attributed)/n(), 4)) %>%
  arrange(desc(click_count)) %>%
  slice(1:100) %>%
  data.frame()

count_rate

# Obtain the coefficient so that both y axes are on the same scale 
count_rate$num <- seq(1:nrow(count_rate))
MAX <- max(count_rate$click_count)
mx <- max(count_rate$prop_downloaded)
coef <- mx/MAX

# Plotting
ggplot(count_rate, aes(x=num)) +
  geom_line(aes(x=num, y=click_count), color="darkred") +
  geom_line(aes(x=num, y=prop_downloaded/coef), color="darkgreen") +
  ggtitle("Conversion Rates over Counts of 100 Most Popular Channels") +
  scale_y_continuous(
    # Features of the first axis
    name = "Number of clicks",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coef, name="Proportion Downloaded")
  ) +
  theme(
    axis.title.y = element_text(color = "darkred", size=13),
    axis.title.y.right = element_text(color = "darkgreen", size=13),
    plot.title = element_text(hjust = 0.5)
  ) +
  xlab("")

# # # # # # # # # 
# Time patterns #
# # # # # # # # # 

# The analysis of temporal patterns will be made with the sample provided by the kaggle.
# The first lines of the dataset are organized by time and therefore are not random.
# Thus, they are inappropriate for detecting temporal patterns.

# Importing a random training dataset sample for time pattern analysis
sampleTrain <- read.csv("train_sample.csv")

head(sampleTrain)

#convert click_time and attributed_time to time series
sampleTrain$attributed_time <- dplyr::na_if(sampleTrain$attributed_time, "")
sampleTrain$attributed_time <- as.POSIXct(sampleTrain$attributed_time, tz = Sys.timezone())
sampleTrain$click_time <- as.POSIXct(sampleTrain$click_time, tz = Sys.timezone())

# Convert "is_attributed to numeric
sampleTrain$is_attributed <- as.numeric(sampleTrain$is_attributed)

#round the time to nearest hour
sampleTrain$click_round <- lubridate::round_date(sampleTrain$click_time, "hour")

# Checking for hourly patterns
count_rate <- sampleTrain[,c("click_round", "is_attributed")] %>% 
  group_by(click_round) %>%
  summarise(click_count=n(), conversion_rate = round(sum(is_attributed)/n(), 4)) %>%
  data.frame()

head(count_rate, 100)

# Plotting
require(gridExtra)

# Plotting hourly clicks
plot1 <- ggplot(count_rate, aes(x=click_round)) +
  geom_line(aes(x=click_round, y=click_count), color="darkred") +
  ggtitle("Hourly click frequency") +
  scale_y_continuous(name = "Number of clicks") +
  scale_x_datetime(labels = scales::date_format("%H:%M\n%d-%b\n%Y"), date_breaks = "12 hours") +
  theme(
    axis.title.y = element_text(color = "black", size=13),
    axis.text.x= element_text(color = "black"),
    axis.text.y= element_text(color = "black"),
    plot.title = element_text(hjust = 0.5)
  )

# Plotting hourly conversion rate
plot2 <- ggplot(count_rate, aes(x=click_round)) +
  geom_line(aes(x=click_round, y=conversion_rate), color="darkblue") +
  ggtitle("Hourly conversion rate") +
  scale_y_continuous(name = "Conversion rate") +
  scale_x_datetime(labels = scales::date_format("%H:%M\n%d-%b\n%Y"), date_breaks = "12 hours",
                   limits = ) +
  theme(
    axis.title.y = element_text(color = "black", size=13),
    axis.text.x= element_text(color = "black"),
    axis.text.y= element_text(color = "black"),
    plot.title = element_text(hjust = 0.5)
  )

grid.arrange(plot1, plot2, nrow = 2)

# Extract hour from "click_time" and add to sample train dataset
sampleTrain$click_hour <- as.factor(lubridate::hour(sampleTrain$click_time))

# Getting number of clicks by hour
count_rate <- sampleTrain[,c("click_hour", "is_attributed")] %>% 
  group_by(click_hour) %>%
  summarise(click_count=n(), conversion_rate = round(mean(is_attributed), 4)) %>%
  data.frame()

View(count_rate)

# Plotting number of clicks by hour
ggplot(count_rate, aes(x=click_hour, y=click_count)) +
  geom_bar(stat = "identity", fill="purple", width = 0.5) +
  geom_line( aes(x=as.numeric(click_hour), y=click_count)) +
  ggtitle("Hourly click frequency") +
  scale_y_continuous(name = "Count of clicks") +
  theme(
    axis.title.y = element_text(color = "black", size=13),
    axis.text.x= element_text(color = "black"),
    axis.text.y= element_text(color = "black"),
    plot.title = element_text(hjust = 0.5)
  )


# Plotting hourly conversion rate
ggplot(count_rate, aes(x=click_hour, y=conversion_rate)) +
  geom_bar(stat = "identity", fill="purple", width = 0.5) +
  geom_line( aes(x=as.numeric(click_hour), y=conversion_rate)) +
  ggtitle("Hourly conversion rate") +
  scale_y_continuous(name = "Conversion rate") +
  theme(
    axis.title.y = element_text(color = "black", size=13),
    axis.text.x= element_text(color = "black"),
    axis.text.y= element_text(color = "black"),
    plot.title = element_text(hjust = 0.5)
  )


# Plotting conversion rate and count of clicks to check if the variables correlate
# Obtain the coefficient so that both y axes are on the same scale 
MAX <- max(count_rate$click_count)
mx <- max(count_rate$conversion_rate)
coef <- mx/MAX

# Plotting
ggplot(count_rate, aes(x=num)) +
  geom_line(aes(x=as.numeric(click_hour), y=click_count, color="click time")) +
  geom_line(aes(x=as.numeric(click_hour), y=conversion_rate/coef, color="conversion rate")) +
  ggtitle("Conversion rates and click count by hour") +
  scale_y_continuous(
    # Features of the first axis
    name = "Count of clicks",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coef, name="Conversion Rate")
  ) + xlab("Hour") +
  theme(
    axis.title.y = element_text(color = "purple", size=13),
    axis.title.x = element_text(color = "black", size=13),
    axis.title.y.right = element_text(color = "darkred", size=13),
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_color_manual(values = c("purple", "darkred")) 
 
  
# Checking the time difference between clicking add and download it
sampleTrain$timeDiff <- hms::as_hms(sampleTrain$attributed_time - sampleTrain$click_time)

# Checking first rows and the time passaed between click and download
head(sampleTrain[sampleTrain$is_attributed == 1,], 15)

# Getting time passed summary
as.data.frame(lapply(summary(as.numeric(sampleTrain[sampleTrain$is_attributed == 1, 
                                                    "timeDiff"])), hms::as_hms))


# Checking the time difference between clicking add and download it
# on the first 10.000.000 rows of train datset
train$timeDiff <- hms::as_hms(train$attributed_time - train$click_time)

# Summary
as.data.frame(lapply(summary(as.numeric(train[train$is_attributed == 1, 
                                                    "timeDiff"])), hms::as_hms))

# The deference varias from 0 to almost 24 hours (one day)
# on first rows of train dataset


