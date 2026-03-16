#DACSS690V HW3 Script
#Load data -----MAKE SURE TO CHANGE TO PULL FROM GITHUB
setwd("C:/Users/patri/OneDrive/Desktop/DACSS/DACSS690V/Week_3_HW")

pacman::p_load(tidyverse, dplyr, janitor, readxl, ggplot2, magrittr, psych, ggridges)
ma_school_df <- read_excel("C:/Users/patri/OneDrive/Desktop/DACSS/DACSS690V/Week_3_HW/ncesdata_MASS_202425.xlsx")
#Pre process data - check data types
ma_school_df <- ma_school_df |> 
  slice(11:1835) |> 
  row_to_names(row_number = 1) |> 
  mutate(
         Teachers = as.numeric(Teachers),#INtroducing NAs - where data was missing
         Type = as.factor(Type))

#Create two visualizations - one cat, one num
#-----Plot 1: Categorical - Type (Other/alternative, Regular, Special Education, Vocational) - Bar chart------


#type table and df
type_table <- table(ma_school_df$Type, useNA = "ifany")

type_percentages_df <- as.data.frame(prop.table(type_table)*100)
names(type_percentages_df)=c("Type","Percent")


#percent_labels <- paste0(round(type_percentages_df$Percent,1), '%')


plot_1 <- type_percentages_df |> 
  mutate(percent_labels = paste0(round(Percent, 1), '%')) |> 
  ggplot(aes(x = reorder(Type,Percent), y = Percent)) +
  geom_text(hjust= 0.5,
            color = "black",
            size = 4,
            aes(y = Percent ,
                label = percent_labels))+
  geom_col(alpha = 0.5) +
  coord_flip()+
  theme(panel.background = element_rect(fill = "white"),
    axis.title = element_blank(),
    axis.line.y.left = element_line(color = "black")) +
  labs(#title="Types of Schools",
       subtitle = "The Vast Majority of Schools in MA are Classified as Regular") +
  theme(panel.border = element_rect(color = "white", fill = NA, linewidth = 2),
        plot.background = element_rect(color = "white", linewidth = 5)) + 
  theme(axis.text.x = element_blank())

plot_1
saveRDS(plot_1, file = "plot_1.rds")



#-----------#Plot 2: Numerical - Teachers - Density plot--------------
mean_teachers <-  mean(ma_school_df$Teachers, na.rm = TRUE)
median_teachers <- median(ma_school_df$Teachers, na.rm = TRUE)


plot_2 <- ma_school_df |>
  filter(!is.na(Teachers)) |> 
  ggplot(aes(x = Teachers)) +
  geom_density(fill = "grey", color = "grey") +
  theme_minimal() +
  #coord_flip() +
  labs(
    #title = "Teachers",
    subtitle = "Most Schools in MA have less than the Average Number of Teachers",
    x = NULL,
    y = NULL
  ) + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank())+ 
  geom_vline(xintercept = median_teachers, linetype = "dashed", color = "blue", linewidth = 1)+ 
  annotate(geom = 'text',
           label="Median",
           y = 0.005,
           x= median_teachers - 15,
           color = "blue")+
  geom_vline(xintercept = mean_teachers, linetype = "dashed", color = "red", linewidth = 1)+ 
  annotate(geom = 'text',
           label="Mean",
           y = 0.02,
           x= mean_teachers + 15,
           color = "red") +
  theme(panel.grid = element_blank()) +
  scale_x_continuous(
    breaks = c(0, 35, 42, 250)
  ) + 
  theme(axis.text.x = element_text(angle = 45,
                                      size = 8,
                                      vjust = 0.5))

plot_2

saveRDS(plot_2, file = "plot_2.rds")

