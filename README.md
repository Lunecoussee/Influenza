# Influenza

# Hypothesis 1: Does the infection history has an influence on the vaccine response?
## H0: The infection history does not affect the vaccine response            
## H1: The infection history does have an influence on the vaccine response

```{r Libraries}
library(tidyverse)
library(ggplot2)
```
```{r}
data = read_csv('fluprint_export.csv')
```
```{r QQPlot_Dummy}
#load or install the ggpubr package
install.packages("ggpubr")
library(ggpubr)
```
```{r}
metadata <- data %>% select(-name, -name_formatted, -subset, -units, -data, -mesurment_id, -assay) %>% distinct() 
```
```{r}
head(metadata)
```
```{r}
class(metadata$vaccine_response)
class(metadata$influenza_infection_history)
```
```{r}
metadata$vaccine_response[metadata$vaccine_response == 'NULL'] <- NA
metadata <- na.omit(metadata)
```
```{r}
metadata$vaccine_response <- as.factor(metadata$vaccine_response)
metadata$influenza_infection_history <- as.factor(metadata$influenza_infection_history)
class(metadata$vaccine_response)
class(metadata$influenza_infection_history)
head(metadata)
```
```{r}
metadata %>%
  ggplot(aes(x=vaccine_response, fill=influenza_infection_history)) +
    geom_bar() +
    labs(title = "stacked Bar Plot", x = "vaccine response", fill = "infection history") +
    theme_classic()
```
```{r}
ggplot(metadata, aes(x = vaccine_response, fill = influenza_infection_history)) +
  geom_bar(position = "dodge") +
  labs(title = "Grouped Bar Plot", x = "vaccine response", fill = "infection history")
```
```{r}
contingency_table <- table(metadata$vaccine_response, metadata$influenza_infection_history)
print(contingency_table)
```
```{r}
# Convert contingency table to data frame for ggplot
table_df <- as.data.frame(table(metadata$vaccine_response, metadata$influenza_infection_history))
colnames(table_df) <- c("vaccine_response", "infection_history", "Count")
table_df
```
```{r}
# Create heatmap
ggplot(table_df, aes(x = vaccine_response , y = infection_history , fill = Count)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  labs(title = "Heatmap of vaccine response vs infection history", 
       x = "vaccine response", 
       y = "influenza infection history", 
       fill = "count")
```
```{r}
#fisher-test want lage values verwacht voor de positieve influenza histories. Ook omdat we 2 categorische variabelen vergelijken (niet meer, anders chi kwadraat test).
fisher_test <- fisher.test(contingency_table)
print(fisher_test)
```

# Hypothesis 2: Multivariate analysis: Can differences in BMI-values and delta geo-mean-have a significant influence on the vaccine response?
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
