
#Set working directory 
library(dplyr)
library(tidyr)
library(here)
library(tidyverse)
library(stringr)
library(ggplot2)

# Load the data on income 
df1_orig <- read.csv(here("data", "time_spent_in_lockdown_income.csv"), fileEncoding = "latin1")

# Remove unwanted columns (the things not needed for the graph)
df1 <- df1_orig %>%
  select(-X.3, -X.4, -X.5,
         -X.8, -X.9, -X.10,
         -X.11, -X.12, -X2014.2015.vs.March.April.2020) 

# Make column names meaningful 
names(df1)[names(df1) == "X"] <- "activity"
names(df1)[names(df1) == "X2014.2015"] <- "low_1415"
names(df1)[names(df1) == "X.1"] <- "med_1415"
names(df1)[names(df1) == "X.2"] <- "high_1415"
names(df1)[names(df1) == "March.April.2020"] <- "low_2020"
names(df1)[names(df1) == "X.6"] <- "med_2020"
names(df1)[names(df1) == "X.7"] <- "high_2020"

# Identify all cells that are empty as NA
df1[df1==""] <- NA

# Keep only rows with no NA
df1 <- na.omit(df1)

# Make the columns numeric 
df1$low_1415 <- as.numeric(as.character(df1$low_1415))
df1$med_1415 <- as.numeric(as.character(df1$med_1415))
df1$high_1415 <- as.numeric(as.character(df1$high_1415))
df1$low_2020 <- as.numeric(as.character(df1$low_2020))
df1$med_2020 <- as.numeric(as.character(df1$med_2020))
df1$high_2020 <- as.numeric(as.character(df1$high_2020))


# Calculate percentage change columns 
df2 <- df1 %>% 
  mutate(low = ((low_2020 - low_1415)/low_1415)*100, 
         med = ((med_2020 - med_1415)/med_1415)*100,
         high = ((high_2020 - high_1415)/high_1415)*100)
df2<- select(df2, low, med, high, activity)


# Put percentage change columns into a long data frame 
keycol <- "income"
valuecol <- "time_ch"
gathercols <- c("low", "med", "high")

df2 <- gather_(df2, keycol, valuecol, gathercols)


# Look at the graph with all the activities
# This plot produces a graph that I was not happy with

#ggplot(df2, aes(y = time_ch, x = activity, fill = income)) +
#geom_col(position = position_dodge()) +
#  theme(plot.title = element_text (size = 20, face = "bold"),
#        plot.subtitle = element_text(face = "italic"),
#        plot.caption = element_text(vjust = 30),
#        axis.text.x = element_text(angle = 45, hjust=1, size = 10),
#        axis.title.x = element_text(size = 15, vjust = 20),
#        axis.title.y = element_text(size = 15),
#        axis.text.y = element_text(size = 10),
#        legend.title = element_text(size=18),
#        legend.text = element_text(size=11)) +
#  scale_x_discrete(labels=c("Socialising",
#                            "Gardening/DIY",
#                            "Keep fit",
#                            "Other",
#                            "Personal care",
#                            "Sleep/ rest",
#                            "Study",
#                            "Travel/ transport",
#                            "Unpaid childcare",
#                            "Unpaid house work",
#                            "Working
#                            (from home)",
#                            "Working
#                            (not from home)")) +
#  labs(y = "Time (% change)",
#       x = "Activity",
#       fill = "Income")
# ggsave(here("graph", "bar_chart_bad.png"))


# Calculate mean by activity 
df2 <- df2 %>% 
  group_by(activity) %>% 
  mutate(mean_by_activity = mean(time_ch))


# Put columns to 0 dp 
df2 <- df2 %>% mutate_if(is.numeric, round, digits=0)

# Get rid of all values between -16 and 16. Note:**2 means square and 256 is 16 squared
keep_rows <- c()

for(i in 1:length(df2$mean_by_activity)) {
  if((df2$mean_by_activity[i])**2 > 256){
    keep_rows <- c(keep_rows, i)
    print(i)
  }
}

# Create new data frame that has only the rows with the greater percentage change
# Remove the "other" condition as it is uninformative
df3 <- df2[keep_rows,]
df3 <- df3[df3$activity != "Other",]


# Put the income levels in order and re-label them for the sake of the graph legend
df3$income <- factor(df3$income,
                     levels = c("high", "med", "low"),
                     labels = c("High", "Medium", "Low"))

# Plot the graph 
ggplot(df3, aes(y = time_ch, x = activity, fill = income)) +
  geom_col(position = position_dodge()) +
  geom_text(aes(label = paste(time_ch),
                vjust = ifelse(time_ch >= 0, -0.4, 1.1)),
            position = position_dodge(0.9),
            size = 3) +
  scale_y_continuous(limits = c(-120, 430)) +
  scale_x_discrete(labels=c("Gardening/DIY",
                            "Keep fit",
                            "Study",
                            "Travel/
                            transport",
                            "Working
                            (from home)",
                            "Working
                            (not from home)")) +
  scale_fill_manual(values = c("High" = "lightslateblue",
                               "Medium" = "skyblue1",
                               "Low" = "paleturquoise1")) +
  theme(plot.title = element_text (size = 20, face = "bold"),
        plot.subtitle = element_text(face = "italic"),
        plot.caption = element_text(vjust = 30),
        axis.text.x = element_text(angle = 45, hjust=1, size = 10),
        axis.title.x = element_text(size = 15, vjust = 20),
        axis.title.y = element_text(size = 15),
        axis.text.y = element_text(size = 10),
        legend.title = element_text(size=18),
        legend.text = element_text(size=11)) +
  labs(y = "Percentage change in time spent on
       activities (%)",
       x = "Activity", 
       fill = "Income",
       title = "Behaviour change: 2014/15 compared to 2020",
       subtitle = "A bar chart showing the percentage change in time spent doing specified activities, arranged by income.",
       caption = "Source: Office of National Statistics")
ggsave(here("graph", "bar_chart.png"))
  
