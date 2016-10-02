library(forecast)
library(lubridate)

wl.features <- read.csv("../Data/Kaggle/Walmart/features.csv")
wl.stores <- read.csv("../Data/Kaggle/Walmart/stores.csv")
wl.train <- read.csv(file = "../Data/Kaggle/Walmart/train.csv")
wl.train$Store <- as.factor(wl.train$Store)
wl.train$Dept <- as.factor(wl.train$Dept)
wl.train$Date <- as.Date(wl.train$Date)
