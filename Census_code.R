library(tidyr)
library(dplyr)
library(readxl)
data1 <- read_csv("C:/Users/User/Downloads/Census2022SampleCSV (1)/Census2022sample_F18.csv")
View(data1)
names(data1)
data1 <- data1 %>% 
  filter(District == "BUF" | District == "CPT")
View(data1)
unique(data1$Province)
unique(data1$Municipality)
unique(data1$District)
unique(data1$Geo_type)

data2 <- read_csv("C:/Users/User/Downloads/Census2022SampleCSV (1)/Census2022sample_F19.csv")
View(data2)

names(data2)
data2 <- data2 %>% 
  select(QID, DERH_HSIZE, DERH_HHSEX, DERH_HHPOP, DERH_HHAGE,H01_QUARTERS, H02_MAINDWELLING, H03_TENURE,
         H04_RDP, H05_WATERPIPED, H06_WATERSOURCE,H07A_WATERSUPPLY, H08_TOILET, H09_ENERGY_COOKING,
         H10_ENERGY_LIGHTING, H11_REFUSE,H12_ELECTRIC_GAS_STOVE, A4_ADULT_HUNGER, A5_CHILD_HUNGER, HH_WGT)
View(data2)

cleaned_census_data <- read_csv("cleaned_census_data.csv")
data3 <- cleaned_census_data
View(data3)
unique(data3$DERP_PREV_MUNIC)
data3 <- data3 %>% 
  filter(DERP_PREV_MUNIC == "BUF" | DERP_PREV_MUNIC == "CPT")
data3
unique(data3$DERP_PREV_MUNIC)

help("inner_join")
combined <- full_join(data1, data2, by ="QID")
View(combined)
unique(combined$Municipality)
data <- full_join(combined, data3, by = "QID")
View(data)
unique(data$Municipality)
unique(data$DERP_PREV_MUNIC)
data <- data %>% 
  filter(DERP_USUALRES_MUNIC == "BUF" | DERP_USUALRES_MUNIC == "CPT")
data
unique(data$DERP_USUALRES_MUNIC)


unique(data$Municipality)
unique(data$DERP_PREV_MUNIC)


census <- data %>%
  select(
    Province,
    District,
    Municipality,
    H01_QUARTERS,
    H02_MAINDWELLING,
    H03_TENURE,
    H04_RDP,
    H05_WATERPIPED,
    H06_WATERSOURCE,
    H07A_WATERSUPPLY,
    H08_TOILET,
    H11_REFUSE,
    A4_ADULT_HUNGER,
    A5_CHILD_HUNGER,
    HH_WGT)

View(census)
str(census)




names(census)


#descriptive statistics
table(census$Municipality)
prop.table(table(census$Municipality))*100


library(ggplot2)
library(gmodels)
#waterpiped
census <- census %>%
  mutate(
    H05_WATERPIPED = case_when( H05_WATERPIPED == 1 ~ "Piped water inside the dwelling/house", 
                                H05_WATERPIPED == 2 ~ "Piped water inside the yard",
                                H05_WATERPIPED == 3 ~ "Piped water on community stand: distance less than 200m from dwelling", 
                                H05_WATERPIPED == 4 ~ "Piped water on community stand: distance between 200m and 500m",
                                H05_WATERPIPED == 5 ~ "Piped water on community stand: distance between 500m and 1000m from dwelling", 
                                H05_WATERPIPED == 6 ~ "Piped water on community stand: distance greater than 1000m (1km) from dwelling", 
                                H05_WATERPIPED == 7 ~ "No access to piped water",
                             TRUE ~ NA_character_))
View(census)   

census <- census %>%
  mutate(
    H08_TOILET = case_when( H08_TOILET == 1 ~ "Flush toilet connected to a public sewerage system ", 
                            H08_TOILET == 2 ~ "Flush toilet connected to a septic tank or conservancy tank",
                            H08_TOILET == 3 ~ "Chemical toilet", 
                            H08_TOILET == 4 ~ "Pit latrine/toilet with ventilation pipe",
                            H08_TOILET == 5 ~ "Pit latrine/toilet without ventilation pipe", 
                            H08_TOILET == 6 ~ "Ecological toilet (e.g., urine diversion, enviroloo, etc.)", 
                            H08_TOILET == 7 ~ " Bucket toilet (collected by municipality)",
                            H08_TOILET == 8 ~ "Bucket toilet (emptied by household) ",
                            H08_TOILET == 9 ~ "None",
                            H08_TOILET == 10 ~ "Other",
                                TRUE ~ NA_character_))

View(census)
unique(census$H11_REFUSE)
census <- census %>%
  mutate(
    H11_REFUSE = case_when( H11_REFUSE == 1 ~ "Removed by local authority/private company/community members at least once a week", 
                            H11_REFUSE == 2 ~ "Removed by local authority/private company/community members less often",
                            H11_REFUSE == 3 ~ "Communal refuse dump", 
                            H11_REFUSE == 4 ~ "Communal container/central collection point",
                            H11_REFUSE == 5 ~ "Own refuse dump", 
                            H11_REFUSE == 6 ~ "Dump or leave rubbish anywhere (no rubbish disposal)", 
                            H11_REFUSE == 7 ~ "Other",
                           TRUE ~ NA_character_))
View(census)


census <- census %>%
  mutate(
    A4_ADULT_HUNGER = case_when(A4_ADULT_HUNGER == 1 ~ "Never", 
                                A4_ADULT_HUNGER == 2 ~ "Seldom",
                                A4_ADULT_HUNGER == 3 ~ "Sometimes", 
                                A4_ADULT_HUNGER == 4 ~ "Often",
                                A4_ADULT_HUNGER == 5 ~ "Always", 
                                A4_ADULT_HUNGER == 6 ~ "Not applicable (no adult in the household)", 
                                A4_ADULT_HUNGER == 9 ~ " Unspecified ",
                                TRUE ~ NA_character_))
View(census)

census <- census %>%
  mutate(
    H04_RDP = case_when(H04_RDP == 1 ~ "Yes", 
                        H04_RDP == 2 ~ "No",
                        H04_RDP == 3 ~ "Do not know", 
                        H04_RDP == 8 ~ "Not applicable",
                        H04_RDP == 9 ~ "Unspecified", 
                        TRUE ~ NA_character_))

View(census)

table(census$A4_ADULT_HUNGER)
prop.table(table(census$A4_ADULT_HUNGER))*100

prop.table(table(census$Municipality,
        census$A4_ADULT_HUNGER),1)*100
chisq.test(table(census$Municipality,
        census$A4_ADULT_HUNGER))

ggplot(census,aes(x = Municipality,
           fill = A4_ADULT_HUNGER)) +
  geom_bar(position = "fill") +
  labs(title = "Adult Hunger by Municipality",y = "Proportion",fill = "Adult Hunger")

















