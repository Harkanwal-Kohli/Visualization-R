---
  title: "Gun Murders USA"
  author: "Harkanwal Kohli"
---

library(dslabs) ##importing dslabs Library for datasets
library(tidyverse) ##importing library tidyverse for data manipulation
library(ggplot2)##importing library ggplot2 to plot the graphs
library(ggrepel)##importing the library ggrepel to avoid point and text overlap

data(murders) ##Loading data murders

df <- murders ## adding dataset to df

names(df) ##TO know the column names 
str(df) ##To know the structure of data frame (SUMMARY FUNCTION)
summary(df) #To know the summary of df (SUMMARY FUNCTION)

head(df,10) ##to see the top 10 entries

df_mutate <- df %>%
  mutate(murderRate = total/population*10^5) ##adding new column murder rate

USA_murder_rate <- df_mutate %>%
  summarise(USA_rate = sum(total)/sum(population)*10^6) %>%
  pull(USA_rate)

MurdersAndPopulation_plot <- df_mutate %>%
  ggplot(aes(x=population/10^6,y=total,na.rm=TRUE)) ##defining aesthetic variables to ggplot

MurdersAndPopulation_plot <- MurdersAndPopulation_plot+ ##defining the type of graph needed
  geom_point(aes(color=region))

MurdersAndPopulation_plot <- MurdersAndPopulation_plot + ##defining the axis scales
  scale_x_continuous(trans = "log10") +
  scale_y_log10() 

MurdersAndPopulation_plot <- MurdersAndPopulation_plot+ ##Defining the lables
  labs(x="Population per Million",
       y= "Murders",
       title="Average Gun Murders",
       subtitle="USA",
       caption="Year-2010")

MurdersAndPopulation_plot <- MurdersAndPopulation_plot+ ##plotting the average rate line
  geom_abline(intercept = log10(USA_murder_rate),lty=3,color="grey")

MurdersAndPopulation_plot <- MurdersAndPopulation_plot+## to avoid point and text overlap
  geom_text_repel(aes(label=abb, color=region))

MurdersAndPopulation_plot ##viewing the graph

