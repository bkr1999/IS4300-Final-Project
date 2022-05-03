library(dslabs)
library(tidyverse)
library(ggplot2)

# Pulling the dataset in from github
urlfile <- "https://raw.githubusercontent.com/bkr1999/IS4300-Final-Project/main/Final%20Project%20Dataset.csv"
statistics <- read_csv(url(urlfile))
mdata <- as.data.frame(statistics)
head(mdata)
data <- mdata %>% select(lfp,k5,k618,age,wc,hc,lwg,inc)
names(data) <- c("LaborForceParticipant","KidsUnder5", "Kids_6to18", "Age","Wife_College","Husband_College","Expected_Wage_Rate","Income")
data$LaborForceParticipant <- as.factor(data$LaborForceParticipant)
head(data)

# First scatterplot of the data
plot <- data %>% ggplot()
plot + geom_point(aes(Income,Age,col=LaborForceParticipant)) + labs(title = "Income vs. Age Scatterplot")


# Bar plot visualization of the data
plot + geom_bar(aes(KidsUnder5,fill=LaborForceParticipant)) + labs(title = "Barplot of Kids Under 5 Years Old")

# Separating the data based on if the wife attended college and showing the mean expected wage rates
x <- data %>% filter(Wife_College == "no") %>% select(Expected_Wage_Rate)
y <- data %>% filter(Wife_College == "yes")%>% select(Expected_Wage_Rate)
mean(y$Expected_Wage_Rate)
mean(x$Expected_Wage_Rate)

# T-test for mean expected wage rates
t.test(x$Expected_Wage_Rate,y$Expected_Wage_Rate)

# Seperating the data depending on whether the husband went to college and showing mean income
x <- data %>% filter(Husband_College == "no") %>% select(Income)
y <- data %>% filter(Husband_College == "yes")%>% select(Income)
mean(y$Income)
mean(x$Income)

# T-test for the income variables
t.test(x$Income,y$Income)