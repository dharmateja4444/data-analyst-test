######################################
###########  1 - Packages ############
######################################
#install.packages("tidyverse")
#install.packages("dplyr")
require(tidyverse)
library(dplyr)
library(shiny)
library(shinydashboard)
library(dplyr)
library(glue)
library(reactable)
library(purrr)
library(reshape)
library(ggplot2)

######################################
###2 - Data Import and Cleaning#######
######################################

### Reading csv files#######
df_person <-read.csv("https://raw.githubusercontent.com/SamKnows/data-analyst-test/master/data/details_for_each_person.csv")
df_download <- read.csv("https://raw.githubusercontent.com/SamKnows/data-analyst-test/master/data/download_speed_measurements.csv")
df_upload <- read.csv("https://raw.githubusercontent.com/SamKnows/data-analyst-test/master/data/upload_speed_measurements.csv")

#### filtering people in the cities 'Samsville' and 'Databury'###
df_person <- df_person %>% filter(city == "Samsville" | city == "Databury")

#####filtering did_test_complete_successfully are TRUE and time_of_measurement from January 2021 #####
df_download <- df_download %>% filter(did_test_complete_successfully == TRUE & time_of_measurement >= as.Date("2021-01-01"))
df_upload <- df_upload %>% filter(did_test_complete_successfully == TRUE & time_of_measurement >= as.Date("2021-01-01"))

######Joining tables using person_id as primary key######
df_download_person = merge(x = df_person, y = df_download, by = "person_id")
df_upload_person = merge(x = df_person, y = df_upload, by = "person_id")

# checking na values in data frame
sum(is.na(df_download_person))
sum(is.na(df_upload_person))

########calculating a single average download and upload speed for each person#######
df_download_person <- df_download_person %>%
  group_by(person_id) %>%
  summarise_at(vars(measured_download_speed_in_Mbps), list(average_download_speed = mean))

df_upload_person <- df_upload_person %>%
  group_by(person_id) %>%
  summarise_at(vars(measured_upload_speed_in_Mbps), list(average_upload_speed = mean))

####joining required variables in df dataframe ########
df = merge(x= df_download_person, y = df_upload_person, by = "person_id")
df = merge(x= df_person, y=df, by = "person_id")

######################################
######## 3 - Data Quality ############
######################################

# statistics of data 
summary(df)

# checking na values in data frame
sum(is.na(df))

##### Checking outliers in the data
attach(df)
summary(df$average_download_speed)
summary(df$average_upload_speed)

# visualization of data
boxplot(df$average_download_speed,df$average_upload_speed)
hist(df$average_download_speed)
hist(df$average_upload_speed)

# Checking data following normality
qqnorm(df$average_download_speed)
qqline(df$average_download_speed)
qqnorm(df$average_upload_speed)
qqline(df$average_upload_speed)


##### Checking outliers of VDSL connection
df_VDSL <- df %>% filter(type_of_broadband_connection == "VDSL")

# statistics of VDSL connection
attach(df_VDSL)
summary(df_VDSL$average_download_speed)
summary(df_VDSL$average_upload_speed)

# visualization
boxplot(df_VDSL$average_download_speed,df_VDSL$average_upload_speed)
hist(df_VDSL$average_download_speed)
hist(df_VDSL$average_upload_speed)

# Checking data following normality
qqnorm(df_VDSL$average_download_speed)
qqline(df_VDSL$average_download_speed)
qqnorm(df_VDSL$average_upload_speed)
qqline(df_VDSL$average_upload_speed)


##### Checking outliers of ADSL connection
df_ADSL <- df %>% filter(type_of_broadband_connection == "ADSL")

# statistics of ADSL connection
attach(df_ADSL)
summary(df_ADSL$average_download_speed)
summary(df_ADSL$average_upload_speed)

# visualization  
boxplot(df_ADSL$average_download_speed,df_ADSL$average_upload_speed)
hist(df_ADSL$average_download_speed)
hist(df_ADSL$average_upload_speed)

# Checking data following normality
qqnorm(df_ADSL$average_download_speed)
qqline(df_ADSL$average_download_speed)
qqnorm(df_ADSL$average_upload_speed)
qqline(df_ADSL$average_upload_speed)

#####Removing Outliers for ASDL type of connection 
boxplot.stats(df_ADSL$average_upload_speed)$out
boxplot.stats(df_ADSL$average_download_speed)$out
df_ADSL <- df_ADSL %>% filter(average_upload_speed < 72 & average_download_speed < 182)

#validating outliers in ADSL connection 
boxplot(df_ADSL$average_download_speed,df_ADSL$average_upload_speed)
hist(df_ADSL$average_download_speed)
hist(df_ADSL$average_upload_speed)

##### Checking outliers of Fibre connection
df_Fibre <- df %>% filter(type_of_broadband_connection == "Fibre")

# statistics of Fibre connection 
attach(df_Fibre)
summary(df_Fibre$average_download_speed)
summary(df_Fibre$average_upload_speed)

# visualization
boxplot(df_Fibre$average_download_speed,df_Fibre$average_upload_speed)
hist(df_Fibre$average_download_speed)
hist(df_Fibre$average_upload_speed)

# Checking data following normality
qqnorm(df_Fibre$average_download_speed)
qqline(df_Fibre$average_download_speed)
qqnorm(df_Fibre$average_upload_speed)
qqline(df_Fibre$average_upload_speed)

#Removing Outliers for Fiber type of connection 
boxplot.stats(df_Fibre$average_upload_speed)$out
boxplot.stats(df_Fibre$average_download_speed)$out
df_Fibre <- df_Fibre %>% filter(average_upload_speed > 0.7 & average_download_speed > 9.1)

#validating outliers in fibre connection 
boxplot(df_Fibre$average_download_speed,df_Fibre$average_upload_speed)
hist(df_Fibre$average_download_speed)
hist(df_Fibre$average_upload_speed)

#final dataset  
df_final <- do.call("rbind", list(df_Fibre, df_ADSL, df_VDSL))

#######################################
#4-Data summarization and presentation#
#######################################

## filtering data with ISPs 
df_Fibrelicious<- df_final %>% filter(name_of_isp == "Fibrelicious")
df_Useus<- df_final %>% filter(name_of_isp == "Useus") 

## filtering data with  city name
df_Fibrelicious_Samsville <- df_Fibrelicious %>% filter(city == "Samsville")
df_Fibrelicious_Databury <- df_Fibrelicious %>% filter(city == "Databury")
df_Useus_Samsville <- df_Useus %>% filter(city == "Samsville")
df_Useus_Databury <- df_Useus %>% filter(city == "Databury")

## calculation of average upload speeds with respect to city and ISP
df_Fibrelicious_Samsville_upload<-df_Fibrelicious_Samsville %>%
  group_by(type_of_broadband_connection) %>%
  summarise_at(vars(average_upload_speed), list(average_upload_speed = mean)) 

df_Fibrelicious_Databury_upload<-df_Fibrelicious_Databury %>%
  group_by(type_of_broadband_connection) %>%
  summarise_at(vars(average_upload_speed), list(average_upload_speed = mean)) 

df_Useus_Samsville_upload<-df_Useus_Samsville %>%
  group_by(type_of_broadband_connection) %>%
  summarise_at(vars(average_upload_speed), list(average_upload_speed = mean))

df_Useus_Databury_download<-df_Useus_Databury %>%
  group_by(type_of_broadband_connection) %>%
  summarise_at(vars(average_download_speed), list(average_download_speed = mean))

## calculation of average download speeds with respect to city and ISP
df_Fibrelicious_Samsville_download<-df_Fibrelicious_Samsville %>%
  group_by(type_of_broadband_connection) %>%
  summarise_at(vars(average_download_speed), list(average_download_speed = mean))

df_Fibrelicious_Databury_download<-df_Fibrelicious_Databury %>%
  group_by(type_of_broadband_connection) %>%
  summarise_at(vars(average_download_speed), list(average_download_speed = mean)) 

df_Useus_Samsville_download<-df_Useus_Samsville %>%
  group_by(type_of_broadband_connection) %>%
  summarise_at(vars(average_download_speed), list(average_download_speed = mean)) 

df_Useus_Databury_upload<-df_Useus_Databury %>%
  group_by(type_of_broadband_connection) %>%
  summarise_at(vars(average_upload_speed), list(average_upload_speed = mean)) 

#merging data frames
df_Fibrelicious_Samsville_Avg = merge(x = df_Fibrelicious_Samsville_upload, y = df_Fibrelicious_Samsville_download, by = "type_of_broadband_connection")
df_Useus_Samsville_Avg = merge(x = df_Useus_Samsville_upload, y = df_Useus_Samsville_download, by = "type_of_broadband_connection")
df_Fibrelicious_Databury_Avg = merge(x = df_Fibrelicious_Databury_upload, y = df_Fibrelicious_Databury_download, by = "type_of_broadband_connection")
df_Useus_Databury_Avg = merge(x = df_Useus_Databury_upload, y = df_Useus_Databury_download, by = "type_of_broadband_connection")


########################
######dashboard#########
########################

dfm <- melt(df_Fibrelicious_Samsville_Avg[,c('type_of_broadband_connection',
                                             'average_download_speed',
                                             'average_upload_speed')],id.vars = 1)
dfm1 <- melt(df_Useus_Samsville_Avg[,c('type_of_broadband_connection',
                                       'average_download_speed',
                                       'average_upload_speed')],id.vars = 1)
dfm2 <- melt(df_Fibrelicious_Databury_Avg[,c('type_of_broadband_connection',
                                             'average_download_speed',
                                             'average_upload_speed')],id.vars = 1)
dfm3 <- melt(df_Useus_Databury_Avg[,c('type_of_broadband_connection',
                                      'average_download_speed',
                                      'average_upload_speed')],id.vars = 1)

####User Interface
ui <- dashboardPage(
  dashboardHeader(title ="Dashboard"),
  dashboardSidebar(
    selectInput(inputId = "city",
                label = "Choose the City",
                list("Samsville","Databury"),
                selected ="Samsville")
  ),
  dashboardBody(
    h1("Boardband Speed  Comparsion"),
    h3("Fibrelicious Internet Speed"),
    plotOutput("Fibrelicious_Avg_Speed"),
    h3("Useus Internet Speed"),
    plotOutput("Useus_Avg_Speed")
  ))


# Defining server logic 
server <- function(input, output, session) {
  output$Fibrelicious_Avg_Speed = renderPlot(
    { 
      if(input$city=="Samsville")
      {
        ggplot(data= dfm,mapping=aes(x=type_of_broadband_connection, y=value))+
          geom_bar(aes(fill = variable),stat='identity',position = "dodge")+
          geom_text(mapping=aes(label=round(value, digits=2)),cex=4,vjust=0)}
      else{
        ggplot(data= dfm2,mapping=aes(x=type_of_broadband_connection, y=value))+
          geom_bar(aes(fill = variable),stat='identity',position = "dodge")+
          geom_text(mapping=aes(label=round(value, digits=2)),cex=4,vjust=0)
      }
    }
  )
  output$Useus_Avg_Speed = renderPlot(
    { if(input$city=="Samsville")
    {
      ggplot(data= dfm1, mapping=aes(x=type_of_broadband_connection, y=value))+
        geom_bar(aes(fill = variable),stat='identity',position = "dodge")+
        geom_text(mapping=aes(label=round(value, digits=2)),cex=4,vjust=0)}
      else{
        ggplot(data= dfm3, mapping=aes(x=type_of_broadband_connection, y=value))+
          geom_bar(aes(fill = variable),stat='identity',position = "dodge")+
          geom_text(mapping=aes(label=round(value, digits=2)),cex=4,vjust=0)
      }
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

