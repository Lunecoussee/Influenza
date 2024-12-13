# Influenza

# Hypothesis 1: Does the infection history have an influence on the vaccine response? 
## H0: The infection history does not affect the vaccine response            (--> beide hypothesen zeggen hetzelfde? eentje moet aangepast worden)
## H1: The infection history does have an influence on the vaccine response

**Load and install different packages and libraries that we will need during the visualisation as well of the statistical analysis on this dataset** (--> 'as well of ...' ?)
```{r Libraries}
library(tidyverse)
library(ggplot2)
library(dplyr)
```

**Read in the dataset**
```{r}
data = read_csv('fluprint_export.csv')
```

**Load and install the ggpubr package**
```{r QQPlot_Dummy}
#load or install the ggpubr package
install.packages("ggpubr")
library(ggpubr)
```

**Shape data to a metadata set that can be used for this first hypothesis, we deleted all unnecessary columns and fused all records from the same individual to only one record**
```{r}
metadata <- data %>% select(-name, -name_formatted, -subset, -units, -data, -mesurment_id, -assay) %>% distinct() 
```

**Confirm metadata is good to work with**
```{r}
head(metadata)
```

**Verify which datatype vaccine response and infection history are**
```{r}
class(metadata$vaccine_response)
class(metadata$influenza_infection_history)
```

**Transform datatype of vaccine response and infection history to factor (categorical) and check whether the conversion succeeded**
```{r}
metadata$vaccine_response <- as.factor(metadata$vaccine_response)
metadata$influenza_infection_history <- as.factor(metadata$influenza_infection_history)
class(metadata$vaccine_response)
class(metadata$influenza_infection_history)
head(metadata)
```

**Delete all NA-values by changing 'NULL' to NA and omit all NA-values afterwards**
```{r}
metadata$vaccine_response[metadata$vaccine_response == 'NULL'] <- NA
metadata <- na.omit(metadata)
```

**Visualisation: a stacked bar plot was made**
**First, the variables in the metadata were relabeled**
```{r}
metadata <- metadata %>%
  mutate(
    vaccine_response = factor(vaccine_response, 
                              levels = c(0, 1), 
                              labels = c("bad responder", "good responder")),
    influenza_infection_history = factor(influenza_infection_history, 
                                         levels = c(0, 1), 
                                         labels = c("never infected", "ever infected"))
  )
```

**Then, the stacked bar plot was made**
```{r}
metadata %>%
  ggplot(aes(x=vaccine_response, fill=influenza_infection_history)) +
    geom_bar() +
    labs(title = "stacked Bar Plot", x = "vaccine response", fill = "infection history") +
    theme_classic()
```

**Visualisation: a grouped bar plot was made**
```{r}
ggplot(metadata, aes(x = vaccine_response, fill = influenza_infection_history)) +
  geom_bar(position = "dodge") +
  labs(title = "Grouped Bar Plot", x = "vaccine response", fill = "infection history")
```

**Visualisation: a contingency table was made**
```{r}
contingency_table <- table(metadata$vaccine_response, metadata$influenza_infection_history)
print(contingency_table)
```

**Convert contingency table to data frame for ggplot**
```{r}
table_df <- as.data.frame(table(metadata$vaccine_response, metadata$influenza_infection_history))
colnames(table_df) <- c("vaccine_response", "infection_history", "Count")
table_df
```

**Visualisation: a heatmap was created**
```{r}
ggplot(table_df, aes(x = vaccine_response , y = infection_history , fill = Count)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  labs(title = "Heatmap of vaccine response vs infection history", 
       x = "vaccine response", 
       y = "influenza infection history", 
       fill = "count")
```

**Statistical analysis: perform a fisher-test, we compare 2 categorical variables and low values are expected for the positive influenza histories.**
```{r}
fisher_test <- fisher.test(contingency_table)
print(fisher_test)
```

# Hypothesis 2: Multivariate analysis: Can differences in BMI-values and delta geo-mean have a significant influence on the vaccine response?
## H0: The BMI-values and the delta-geo-mean do not have an influence on the vaccine response.
## H1: The BMI-values and the delta-geo-mean do have an influence on the vaccine response.

```{r Libraries}
metadata_hyp2 <- data %>% select(- study_id, -gender, -race, -visit_id, -visit_year, -visit_day, -visit_age, -name, -name_formatted, -subset, -units, -data, -mesurment_id, -assay) %>% distinct()
```
```{r Libraries}
summary(metadata_hyp2)
```
```{r Libraries}
metadata$bmi <- as.numeric(metadata$bmi)
metadata$geo_mean <- as.numeric(metadata$geo_mean)
metadata$d_geo_mean <- as.numeric(metadata$d_geo_mean)
metadata$total_vaccines_received <- as.numeric(metadata$total_vaccines_received)
```
```{r}
for (var in all_var_num) {
  all_histo = metadata_hyp2 %>%
  filter(vaccine_response %in% c("0","1")) %>%
  ggplot(aes_string(x=var, fill="factor(vaccine_response)")) +
  geom_histogram()

  print(all_histo)
}
```

```{r}
for (var in all_var_num) {
  all_boxp = metadata_hyp2 %>%
  filter(vaccine_response %in% c("0","1")) %>% |
  ggplot(aes_string(x="factor(vaccine_response)", y=var, fill="factor(vaccine_response)")) +
  geom_boxplot()

  print(all_boxp)
}
```
```{r}
# We will work from now on with the two most promising values to be significant: bmi and d_geo_mean.
metadata_clean <- metadata_hyp2[, c('vaccine_response', 'bmi', 'd_geo_mean')]

metadata_without_na <- na.omit(metadata_clean)
```
```{r}
metadata_without_na$vaccine_response <- factor(metadata_without_na$vaccine_response,
  levels = c(0,1),
  labels = c("low responder", "high responder"))
```
```{r}
metadata_without_na %>%
  ggplot(aes(x=bmi, fill=vaccine_response)) +
  geom_histogram()

metadata_without_na %>%
  ggplot(aes(x=d_geo_mean, fill=vaccine_response)) +
  geom_histogram()
```
```{r}
metadata_without_na %>%
  ggplot(aes(x=vaccine_response, y=bmi, fill=vaccine_response)) +
  geom_boxplot()

metadata_without_na %>%
  ggplot(aes(x=vaccine_response, y=d_geo_mean, fill=vaccine_response)) +
  geom_boxplot()
```
```{r}
statistic_test <- glm(vaccine_response ~ bmi + d_geo_mean,
                      data = metadata_without_na,
                      family = binomial)

summary(statistic_test)
```

# Machine learning 
**First step is to get familiar with, load and explore the data. I used pandas for this: abbreviate pandas as pd**

In [2]: import pandas as pd

In [8] : *#save filepath to variable for easier access*    

fluprint_export_path = 'C:\\Users\\indra\\OneDrive\\Bureaublad\\LSAoBD.practicals\\R Project\\fluprint_export.csv'

*#read and store data in dataframe fluprint_data*

fluprint_data = pd.read_csv(fluprint_export_path)

*#get a summary of data*

fluprint_data.head()

fluprint_data.describe()

fluprint_data.describe(include='all').loc[:, ['geo_mean', 'vaccine_response', 'name_formatted']] * 

