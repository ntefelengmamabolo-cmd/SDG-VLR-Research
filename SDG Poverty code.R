


library(readxl)
excel_sheets("C:/Users/User/Downloads/PTR 2025 Provincial Tables (Western Cape).xlsx")
table1 <- read_excel("C:/Users/User/Downloads/PTR 2025 Provincial Tables (Western Cape).xlsx",sheet = 1)
View(table1)

table2 <- read_excel("C:/Users/User/Downloads/PTR 2025 Provincial Tables (Western Cape).xlsx",sheet = 2, col_names = FALSE)
View(table2)

table3 <- read_excel("C:/Users/User/Downloads/PTR 2025 Provincial Tables (Western Cape).xlsx", sheet = 3, col_names = FALSE)
View(table3)

table4 <- read_excel("C:/Users/User/Downloads/PTR 2025 Provincial Tables (Western Cape).xlsx", sheet = 4, col_names = FALSE)
View(table4)

table5 <- read_excel("C:/Users/User/Downloads/PTR 2025 Provincial Tables (Western Cape).xlsx", sheet = 5, col_names = FALSE)
View(table5)

table6 <- read_excel("C:/Users/User/Downloads/PTR 2025 Provincial Tables (Western Cape).xlsx", sheet = 6, col_names = FALSE)
View(table6)

names(table1)
names(table2)
names(table3)
names(table4)
names(table5)
names(table6)

head(table2, 15)
head(table3, 15)
head(table4, 15)
head(table5, 15)
head(table6, 15)

library(dplyr)

table2_headcount <- table2 %>%
  slice(6:8)

View(table2_headcount)
names(table2_headcount) <- c("Category",
  "FPL_2006", "FPL_2009", "FPL_2011", "FPL_2015", "FPL_2023",
  "LBPL_2006", "LBPL_2009", "LBPL_2011", "LBPL_2015", "LBPL_2023",
  "UBPL_2006", "UBPL_2009", "UBPL_2011", "UBPL_2015", "UBPL_2023")
View(table2_headcount)
nrow(table2)
tail(table2, 10)
library(dplyr)
library(tidyr)

# Headcount
t2_headcount <- table2[6:8, ]
t2_headcount

# Poverty Gap
t2_gap <- table2[11:13, ]
t2_gap
# Severity of Poverty
t2_severity <- table2[16:18, ]
t2_severity
colnames(t2_headcount) <- c("Category",
  "FPL_2006", "FPL_2009", "FPL_2011", "FPL_2015", "FPL_2023",
  "LBPL_2006", "LBPL_2009", "LBPL_2011", "LBPL_2015", "LBPL_2023",
  "UBPL_2006", "UBPL_2009", "UBPL_2011", "UBPL_2015", "UBPL_2023")
colnames(t2_headcount)

colnames(t2_gap) <- colnames(t2_headcount)
colnames(t2_severity) <- colnames(t2_headcount)

t2_headcount$Measure <- "Headcount"
t2_gap$Measure <- "Poverty Gap"
t2_severity$Measure <- "Severity"
t2_severity$Measure
table2_clean <- bind_rows(t2_headcount,t2_gap,t2_severity)
View(table2_clean)


nrow(table3)
tail(table3, 10)
t3_headcount <- table3[6:13, ]
t3_gap <- table3[16:23, ]
t3_severity <- table3[26:33, ]

colnames(t3_headcount) <- c("Category",
  "FPL_2006", "FPL_2009", "FPL_2011", "FPL_2015", "FPL_2023",
  "LBPL_2006", "LBPL_2009", "LBPL_2011", "LBPL_2015", "LBPL_2023",
  "UBPL_2006", "UBPL_2009", "UBPL_2011", "UBPL_2015", "UBPL_2023")

colnames(t3_gap) <- colnames(t3_headcount)
colnames(t3_severity) <- colnames(t3_headcount)
t3_headcount$Measure <- "Headcount"
t3_gap$Measure <- "Poverty Gap"
t3_severity$Measure <- "Severity"
table3_clean <- bind_rows(t3_headcount,t3_gap,t3_severity)
View(table3_clean)

nrow(table4)
tail(table4, 10)

t4_headcount <- table4[6:12, ]
t4_gap <- table4[15:21, ]
t4_severity <- table4[24:30, ]
colnames(t4_headcount) <- c("Category",
  "FPL_2006", "FPL_2009", "FPL_2011", "FPL_2015", "FPL_2023",
  "LBPL_2006", "LBPL_2009", "LBPL_2011", "LBPL_2015", "LBPL_2023",
  "UBPL_2006", "UBPL_2009", "UBPL_2011", "UBPL_2015", "UBPL_2023")

colnames(t4_gap) <- colnames(t4_headcount)
colnames(t4_severity) <- colnames(t4_headcount)

t4_headcount$Measure <- "Headcount"
t4_gap$Measure <- "Poverty Gap"
t4_severity$Measure <- "Severity"
table4_clean <- bind_rows(t4_headcount,t4_gap,t4_severity)
View(table4_clean)

nrow(table5)
tail(table5, 10)

t5_headcount <- table5[6:8, ]
t5_gap <- table5[11:13, ]
t5_severity <- table5[16:18, ]
colnames(t5_headcount) <- c("Category",
                            "FPL_2006", "FPL_2009", "FPL_2011", "FPL_2015", "FPL_2023",
                            "LBPL_2006", "LBPL_2009", "LBPL_2011", "LBPL_2015", "LBPL_2023",
                            "UBPL_2006", "UBPL_2009", "UBPL_2011", "UBPL_2015", "UBPL_2023")

colnames(t5_gap) <- colnames(t5_headcount)
colnames(t5_severity) <- colnames(t5_headcount)

t5_headcount$Measure <- "Headcount"
t5_gap$Measure <- "Poverty Gap"
t5_severity$Measure <- "Severity"
table5_clean <- bind_rows(t5_headcount,t5_gap,t5_severity)
View(table5_clean)


nrow(table6)
tail(table6, 10)

t6_headcount <- table6[6:11, ]
t6_gap <- table6[14:19, ]
t6_severity <- table6[22:27, ]
colnames(t6_headcount) <- c("Category",
                            "FPL_2006", "FPL_2009", "FPL_2011", "FPL_2015", "FPL_2023",
                            "LBPL_2006", "LBPL_2009", "LBPL_2011", "LBPL_2015", "LBPL_2023",
                            "UBPL_2006", "UBPL_2009", "UBPL_2011", "UBPL_2015", "UBPL_2023")

colnames(t6_gap) <- colnames(t6_headcount)
colnames(t6_severity) <- colnames(t6_headcount)

t6_headcount$Measure <- "Headcount"
t6_gap$Measure <- "Poverty Gap"
t6_severity$Measure <- "Severity"
table6_clean <- bind_rows(t6_headcount,t6_gap,t6_severity)
View(table6_clean)

nrow(table1)
nrow(table2)
nrow(table3)
nrow(table4)
nrow(table5)
nrow(table6)

table2_clean$Group <- "Sex"
table3_clean$Group <- "Age"
table4_clean$Group <- "Education"
table5_clean$Group <- "Settlement"
table6_clean$Group <- "Household Size"

combined_data <- bind_rows(table2_clean, table3_clean, table4_clean, table5_clean, table6_clean)
View(combined_data)
names(combined_data)


combined_data <- combined_data %>%
  mutate(across(c(FPL_2006, FPL_2009, FPL_2011, FPL_2015, FPL_2023,
      LBPL_2006, LBPL_2009, LBPL_2011, LBPL_2015, LBPL_2023,
      UBPL_2006, UBPL_2009, UBPL_2011, UBPL_2015, UBPL_2023),
    as.numeric))

library(tidyr)
poverty_long <- combined_data %>% 
  pivot_longer(cols = c(FPL_2006, FPL_2009, FPL_2011, FPL_2015, FPL_2023, LBPL_2006, LBPL_2009,
                        LBPL_2011, LBPL_2015, LBPL_2023, UBPL_2006, UBPL_2009, UBPL_2011,
                        UBPL_2015, UBPL_2023),
               names_to = c("PovertyLine", "Year"),
               names_sep = "_",
               values_to = "Value")
View(poverty_long)

install.packages("ggplot2")
library(ggplot2)
library(dplyr)
poverty_long %>% 
  filter(Measure == "Headcount", Category == "Western Cape")
View(poverty_long)

library(dplyr)
library(ggplot2)
poverty_long %>% 
  ggplot()+
  geom_point(mapping = aes(x = Year, y = Value, group = PovertyLine, color = PovertyLine)) +
  labs(title = "Poverty Headcount in the Western Cape", x = "Year", y = "Headcount", color = "Poverty Line")
unique(poverty_long$Category)
unique(poverty_long$Measure)
unique(poverty_long$Group)
unique(poverty_long$PovertyLine)

poverty_long$Year <- factor(
  poverty_long$Year,
  levels = c("2006", "2009", "2011", "2015", "2023"))
poverty_long$Year

poverty_long %>%
  filter(Group == "Sex",
    Category == "Western Cape",
    Measure == "Headcount") %>%
  ggplot(aes(x = Year, y = Value, color = PovertyLine, group = PovertyLine)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  labs(title = "Poverty Headcount Trends in the Western Cape", x = "Year", y = "Poverty Headcount",
    color = "Poverty Line") +
  theme_minimal()


#Compare poverty headcount, poverty gap and severity over time
poverty_long %>%
  filter(Group == "Sex",Category == "Western Cape",PovertyLine == "FPL") %>%
  ggplot(aes(x = Year, y = Value, group = Measure)) +
  geom_line() +
  geom_point(size = 3) +
  facet_wrap(~ Measure, scales = "free_y") +
  labs(title = "Changes in Poverty Measures in the Western Cape", x = "Year", y = "Value" ) +
  theme_minimal()  

#Compare all years by sex
filter(Group == "Sex",Category %in% c("Male", "Female"),
  Measure == "Headcount",
  PovertyLine == "FPL") %>%
  ggplot(aes(x = Year,y = Value,color = Category,color = Category)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  labs(title = "Poverty Headcount Trends by Sex",x = "Year",y = "Poverty Headcount",color = "Sex") +
  theme_minimal()


#Compare all years by settlement type
poverty_long %>%
  filter(Group == "Settlement",
    Measure == "Headcount",
    PovertyLine == "FPL") %>%
  ggplot(aes(x = Year,y = Value,color = Category,group = Category)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  labs(title = "Poverty Headcount Trends by Settlement Type",x = "Year",y = "Poverty Headcount",
    color = "Settlement Type") +
  theme_minimal()

poverty_long %>%
  filter(Group == "Sex", Category == "Western Cape", Measure == "Headcount") %>%
  ggplot(aes(x = Year, y = Value, colour = PovertyLine,group = PovertyLine)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_text(aes(label = paste0(round(Value, 2), "%")),
            vjust = -0.8, size = 3,
            show.legend = FALSE) +
  labs(title = "Poverty Headcounts Based on the FPL, LBPL and UBPL",x = "Year",y = "Poverty Headcount",
    colour = "Poverty Line") +
  theme_minimal()










