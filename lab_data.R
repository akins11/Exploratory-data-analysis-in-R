# Libraries 
library(tidyverse) |> suppressPackageStartupMessages()
library(patchwork) 
library(tidytext, include.only = c("reorder_within", "scale_y_reordered"))
library(janitor, include.only = "clean_names")
library(docstring)

# Data 
data <- read_csv("Marketing-Customer-Value.csv")

# Data cleaning 
data <- data %>%
  mutate(
    Gender = case_when(Gender == "F" ~ "Female",
                       Gender == "M" ~ "Male", TRUE ~ Gender),
    Education = if_else(
      Education == "High School or Below", "Secondary or Below", Education),
    `Vehicle Size` = str_replace(`Vehicle Size`, "Medsize", "Midsize")) %>%
  select(- c(Customer, `Renew Offer Type`, `Effective To Date`)) %>%
  clean_names()


