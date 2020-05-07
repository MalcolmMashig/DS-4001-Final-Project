
library(tidyverse)
library(ggplot2)
library(gridExtra)

# First run complete data wrange file

source("CONSECUTIVE-import-wrangle.R")

careers_clean <- fangraphs_clean

# 1. Fastball velocity over time (seasons) - how has it changed
years <- seq(2002,2019,1)

## function to get 25th percentile
first_func <- function(year){
  df <- filter(careers_clean, Season==year)
  df <- filter(df, IP >= 30)
  fb <- df$FBv
  quantile(fb, 0.25)
}

## 50th percentile function
med_func <- function(year){
  df <- filter(careers_clean, Season==year)
  df <- filter(df, IP >= 30)
  fb <- df$FBv
  quantile(fb, 0.5)
}

## 75th percentile function
third_func <- function(year){
  df <- filter(careers_clean, Season==year)
  df <- filter(df, IP >= 30)
  fb <- df$FBv
  quantile(fb, 0.75)
}

## 90th percentile function
ninety_func <- function(year){
  df <- filter(careers_clean, Season==year)
  df <- filter(df, IP >= 30)
  fb <- df$FBv
  quantile(fb, 0.9)
}

## applying each function to the data
first <- sapply(years, first_func)
first <- as.numeric(first)
second <- sapply(years, med_func)
second <- as.numeric(second)
third <- sapply(years, third_func)
third <- as.numeric(third)
ninety <- sapply(years, ninety_func)
ninety <- as.numeric(ninety)

## preparing data for DF and plot
years3 <- rep(years, 4)
vel <- c(first, second, third, ninety)
t5 <- rep("25th", 18)
f0 <- rep("50th", 18)
s5 <- rep("75th", 18)
n0 <-  rep("90th", 18)
percentile <- c(t5, f0, s5, n0)

## DF to plot
percentiles <- data.frame("Year" = years3, "Velocity" = vel, "Percentile"  = percentile)

## plot lines grouped by percentile
plot1 <- ggplot(percentiles, aes(x=Year, y=Velocity, color = Percentile)) + geom_line() + geom_point() +
  labs(title="Fastball Velocity Percentiles by Year", y="Fastball Velocity") +
  scale_x_continuous(breaks=seq(2002,2018,2)) + scale_y_continuous(breaks=seq(87, 95, 1))
# plot1

## CONCLUSION: It appears that fastball velocities across the spectrum of pitchers are going up together.
## Therefore, standardization will probably be sufficient. However,  it looks like the 90th percentile may
## be separating itself post 2016 - something to maybe look into.

# 2. Fastball velocity distributions over time

## get dataframe and  histogram for each  year
df2002 <- filter(careers_clean, Season==2002)
df2002 <- filter(df2002, IP >= 30)
plot2002 <- ggplot(df2002, aes(x=FBv)) + geom_histogram(color='black', fill='white',bins=20) + 
  labs(title="2002 Fastball Velocity", x="Velocity") + theme(plot.title = element_text(hjust = 0.5))
# plot2002

df2010 <- filter(careers_clean, Season==2010)
df2010 <- filter(df2010, IP >= 30)
plot2010 <- ggplot(df2010, aes(x=FBv)) + geom_histogram(color='black', fill='white',bins=20) + 
  labs(title="2010 Fastball Velocity", x="Velocity") + theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks=seq(75,95,5))
# plot2010

df2015 <- filter(careers_clean, Season==2015)
df2015 <- filter(df2015, IP >= 30)
plot2015 <- ggplot(df2015, aes(x=FBv)) + geom_histogram(color='black', fill='white',bins=20) + 
  labs(title="2015 Fastball Velocity", x="Velocity") + theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks=seq(75,95,5))
# plot2015

df2019 <- filter(careers_clean, Season==2019)
df2019 <- filter(df2019, IP >= 30)
plot2019 <- ggplot(df2019, aes(x=FBv)) + geom_histogram(color='black', fill='white',bins=20) + 
  labs(title="2019 Fastball Velocity", x="Velocity") + theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks=seq(75,95,5))
# plot2019

# hists <- grid.arrange(plot2002, plot2010, plot2015, plot2019)
# hists

# 3. has xFip changed over time

## Same analysis/process as #1
years <- seq(2002,2019,1)

## function to get 25th percentile
first_func <- function(year){
  df <- filter(careers_clean, Season==year)
  df <- filter(df, IP >= 100)
  fip <- df$xFIP
  quantile(fip, 0.75)
}

## 50th percentile function
med_func <- function(year){
  df <- filter(careers_clean, Season==year)
  df <- filter(df, IP >= 100)
  fip <- df$xFIP
  quantile(fip, 0.5)
}

## 75th percentile function
third_func <- function(year){
  df <- filter(careers_clean, Season==year)
  df <- filter(df, IP >= 100)
  fip <- df$xFIP
  quantile(fip, 0.25)
}

## 90th percentile function
ninety_func <- function(year){
  df <- filter(careers_clean, Season==year)
  df <- filter(df, IP >= 100)
  fip <- df$xFIP
  quantile(fip, 0.1)
}

## applying each function to the data
first <- sapply(years, first_func)
first <- as.numeric(first)
second <- sapply(years, med_func)
second <- as.numeric(second)
third <- sapply(years, third_func)
third <- as.numeric(third)
ninety <- sapply(years, ninety_func)
ninety <- as.numeric(ninety)

## preparing data for DF and plot
years3 <- rep(years, 4)
xfip <- c(first, second, third, ninety)
t5 <- rep("25th", 18)
f0 <- rep("50th", 18)
s5 <- rep("75th", 18)
n0 <-  rep("90th", 18)
percentile <- c(t5, f0, s5, n0)

## DF to plot
percentiles <- data.frame("Year" = years3, "xFIP" = xfip, "Percentile"  = percentile)

## plot lines grouped by percentile
plot3 <- ggplot(percentiles, aes(x=Year, y=xFIP, color = Percentile)) + geom_line() + geom_point() +
  labs(title="xFIP Percentiles by Year", y="xFIP") +
  scale_x_continuous(breaks=seq(2002,2018,2)) 
# plot3

## Really interesting - seems like FBv the league tends to go in the same direction - fell off hard from 2006 to
## 2014 or so, then jumped back up. Either random variation, or home run boom has hurt pitchers in xFIP (since
## home run rate is one of the factors in it)

