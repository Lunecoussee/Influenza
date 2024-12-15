# Influenza

# Hypothesis 1: Testing two categorical variables against each other
## H0: The infection history does not have an influence on the vaccine response
## H1: The infection history does have an influence on the vaccine response

**Load and install different packages and libraries that we will need during the visualisation as well as the statistical analysis on this dataset**
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
                              labels = c("low responder", "high responder")),
    influenza_infection_history = factor(influenza_infection_history, 
                                         levels = c(0, 1), 
                                         labels = c("never infected", "ever infected"))
  )
```

**Then, the stacked bar plot was made**
```{r}
metadata %>%
  ggplot(aes(x=influenza_infection_history, fill=vaccine_response)) +
    geom_bar() +
    labs(title = "Stacked Bar Plot: Impact of Infection History on Vaccine Response", x = "infection history", fill = "vaccine response") +
    theme_classic()
```

**Visualisation: a grouped bar plot was made**
```{r}
ggplot(metadata, aes(x = influenza_infection_history, fill = vaccine_response)) +
  geom_bar(position = "dodge") +
  labs(title = "Grouped Bar Plot: Impact of Infection History on Vaccine Response", x = "infection history", fill = "vaccine response")
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
# Create heatmap
ggplot(table_df, aes(x = infection_history , y = vaccine_response , fill = Count)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  labs(title = "Heatmap of infection history vs vaccine response", 
       x = "influenza infection history", 
       y = "vaccine response", 
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

**A new metadataset was created for the second hypothesis and again all records of the same individual were fused**
```{r Libraries}
metadata_hyp2 <- data %>% select(- study_id, -gender, -race, -visit_id, -visit_year, -visit_day, -visit_age, -name, -name_formatted, -subset, -units, -data, -mesurment_id, -assay) %>% distinct()
```

**Take a look what our new metadataset looks like**
```{r Libraries}
summary(metadata_hyp2)
```

**First transform all variables that are character to numeric variables**
```{r Libraries}
metadata_hyp2$bmi <- as.numeric(metadata_hyp2$bmi)
metadata_hyp2$geo_mean <- as.numeric(metadata_hyp2$geo_mean)
metadata_hyp2$d_geo_mean <- as.numeric(metadata_hyp2$d_geo_mean)
metadata_hyp2$total_vaccines_received <- as.numeric(metadata_hyp2$total_vaccines_received)
```
**Check the amount of missing values in these variables**
```{r}
colSums(is.na(metadata_hyp2))
```
**Group these variables together under 1 variable**
```{r}
all_var_num <- c("bmi", "geo_mean", "d_geo_mean", "total_vaccines_received")
```

**Create a histogram for all variables that are left in the metadataset and search for the variable that is most likely to have a significant correlation with the vaccine response**
```{r}
for (var in all_var_num) {
  all_histo = metadata_hyp2 %>%
  filter(vaccine_response %in% c("0","1")) %>%
  ggplot(aes_string(x=var, fill="factor(vaccine_response)")) +
  geom_histogram() +
  theme_classic() +
  labs(title=paste("Histogram of",var))

  print(all_histo)
}
```

**It was difficult to evaluate on these histograms whether there could be a relationship between one of the variables and the vaccine response, therefore, we also created boxplots of the different variables and the vaccine response for further visualization**
```{r}
for (var in all_var_num) {
  all_boxp = metadata_hyp2 %>%
  filter(vaccine_response %in% c("0","1")) %>% 
  ggplot(aes_string(x="factor(vaccine_response)", y=var, fill="factor(vaccine_response)")) +
  geom_boxplot() +
  theme_classic() +
  labs(title=paste("Boxplot of", var))

  print(all_boxp)
}
```

**We will work from now on with the two most promising values to be significant: bmi and d_geo_mean. All NA values were omitted from the metadataset**
```{r}
metadata_clean <- metadata_hyp2[, c('vaccine_response', 'bmi', 'd_geo_mean')]

metadata_without_na <- na.omit(metadata_clean)
```

**Transform the vaccine response levels 0 & 1 to a factor while labelling the 2 levels with low & high responders**
```{r}
metadata_without_na$vaccine_response <- factor(metadata_without_na$vaccine_response,
  levels = c(0,1),
  labels = c("low responder", "high responder"))
```

**Create for BMI as well as d_geo_mean a publication proof histogram**
```{r}
metadata_without_na %>%
  ggplot(aes(x=bmi, fill=vaccine_response)) +
  geom_histogram() +
  theme_classic() +
  labs(title=paste("Histogram of bmi without NA-values"))

metadata_without_na %>%
  ggplot(aes(x=d_geo_mean, fill=vaccine_response)) +
  geom_histogram() +
  theme_classic() +
  labs(title=paste("Histogram of d_geo_mean without NA-values"))
```

**Create for BMI as well as d_geo_mean a publication proof boxplot**
```{r}
metadata_without_na %>%
  ggplot(aes(x=vaccine_response, y=bmi, fill=vaccine_response)) +
  geom_boxplot() +
  theme_classic() +
  labs(title=paste("Boxplot of bmi without NA-values"))

metadata_without_na %>%
  ggplot(aes(x=vaccine_response, y=d_geo_mean, fill=vaccine_response)) +
  geom_boxplot() +
  theme_classic() +
  labs(title=paste("Boxplot of d_geo_mean without NA-values"))
```

**Perform a multiple logistic regression**
```{r}
statistic_test <- glm(vaccine_response ~ bmi + d_geo_mean,
                      data = metadata_without_na,
                      family = binomial)

summary(statistic_test)
```

**Determine the 95% confidence interval**
```{r}
confint(model_no_missing)
```
