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

fluprint_data.describe(include='all').loc[:, ['geo_mean', 'vaccine_response', 'name_formatted']]

In [38] : fluprint_data.columns

**I want to select the high responders via the seroconversion (4-fold or greater rise in HAI titer) AND seroprotection (GMT ≥ 40). To do this, we need the fold change of the geo_mean (geo_mean_fold_change) and this would have to be higher than 4.**

In [58] : fluprint_data['geo_mean_before'] = fluprint_data['geo_mean'] - fluprint_data['d_geo_mean']

fluprint_data['geo_mean_fold_change'] = fluprint_data['geo_mean'] / fluprint_data['geo_mean_before']

fluprint_data.loc[:, ['geo_mean', 'd_geo_mean', 'geo_mean_before', 'geo_mean_fold_change', 'vaccine_response']]

In [74] : high_responders = fluprint_data[(fluprint_data['geo_mean'] >= 40) & (fluprint_data['geo_mean_fold_change'] >= 4)]				          

high_responders

In [78] : high_responders = fluprint_data[fluprint_data['vaccine_response'] == 1] 

num_high_responders = len(high_responders)			             

num_high_responders

In [86] : high_responders.loc[:, ['geo_mean', 'd_geo_mean', 'geo_mean_before', 'geo_mean_fold_change', 'vaccine_response']]

In [100] : high_responders = fluprint_data[(fluprint_data['geo_mean'] >= 40) & (fluprint_data['d_geo_mean'] >= 4)] 					              

num_high_responders = len(high_responders)			              

num_high_responders

**What is said in the article about the high responders doesn't match with the dataset, I wanted to test this. I will just continue working with what the dataset says that high responders are, namely vaccine response = 1**

**I will start cleaning up the dataset and look at the immune cells we could use for the actual machine learning.**

In [108] : fluprint_data.dropna(subset=['vaccine_response'], inplace=True)			  

fluprint_data

*#check wether there really are no NA-values anymore in the column vaccine_response*

In [110] : print(fluprint_data['vaccine_response'].isna().sum())

*#count the number of unique immune cells*

In [142] : filtered_cell_types = fluprint_data[fluprint_data['units'] == '% of Parent']	

unique_cell_types = filtered_cell_types['name_formatted'].unique()	

unique_cell_type_count = len(unique_cell_types)				

unique_cell_type_count

*#check what the most named cell type is, maybe there is a correlation between this cell type and vaccine reponse*

In [148] : most_named_cell_type = filtered_names['name_formatted'].value_counts().idxmax()  

most_named_count = filtered_names['name_formatted'].value_counts().max()    

print(f"The most named cell type is: {most_named_cell_type}")

print(f"It appears {most_named_count} times in the dataset.")

*#then count for every cell type how often it is named*

In [154] : cell_type_count = filtered_names['name_formatted'].value_counts()		    

cell_type_count

*#now see if there are cell types that have a higher count in either low or high responders, again looking for a possible correlation*

In [196] : cell_type_response = filtered_names.groupby(['name_formatted', 'vaccine_response']).size().reset_index(name='count')		    

cell_type_response

*#the most named cell type was CD8_pos_T_cells, so maybe we can see a correlation here*

In [198] : cell_type_response.loc[cell_type_response['name_formatted'] == 'CD8_pos_T_cells']

*#maybe it's not about how often it is named, but about the abundance, so check this*

*#get the index of the cell type with highest ‘data’ value*

In [204] : most_abundant_cell_type = filtered_names['data'].idxmax()	

*#get its data*
 	
most_abundant_value = filtered_names.loc[most_abundant_cell_type, 'data'] 

*#get the name of the cell type*

cell_type = filtered_names.loc[most_abundant_cell_type, 'name_formatted']							       

print(f"The most abundant cell type is: {cell_type}")				        

print(f"It's value is {most_abundant_value}.")

In [206] : cell_type_response.loc[cell_type_response['name_formatted'] == 'IFNa_B_cells']

**No satisfactory results, so start with the data quality assessment for the machine learning.**

*#load and install different packages and libraries that we will need*

In [231] : import numpy as np

import matplotlib.pyplot as plt

import seaborn as sns

from scipy import stats

In [239] : def assess_data_quality(fluprint_data):

*#check for duplicate rows*

print("\nDuplicate rows:", fluprint_data.duplicated().sum())

*#check data types*

print("\nData types:\n", fluprint_data.dtypes)

*#perform data quality assessment*

assess_data_quality(fluprint_data)

In [263] : fluprint_data['vaccine_response'] = fluprint_data['vaccine_response'].astype('int64')

print(fluprint_data.dtypes)

*#make a new, smaller dataframe to work with*

In [290] : columns_to_keep = ['donor_id', 'vaccine_response', 'name_formatted', 'subset', 'units', 'data']

fluprint_clean_data = fluprint_data[columns_to_keep]

fluprint_clean_data

*#we only want the immune cells and not the chemokines*

In [294] : fluprint_clean = fluprint_clean_data[fluprint_clean_data['units'] == '% of Parent']

fluprint_clean

*#check wether the number of rows in the new dataframe is the same as in the original and we didn't accidently delete necessary columns*

In [301] : count = (fluprint_data['units'] == '% of Parent').sum()

print(f"Number of rows where 'unit' is '% of Parent': {count}")

*#calculate the Z-score to check for outliers*

In [363] : z_scores = stats.zscore(fluprint_clean['data'])

outliers = (np.abs(z_scores) > 3).sum(axis=0)

print("\nNumber of outliers (Z-score > 3):\n", outliers)

*#same, but with a boxplot now to check for outliers*

In [397] : plt.figure(figsize=(8, 6))

fluprint_clean.boxplot(column=['data'])

plt.title('Box Plot for data')

plt.show()

*#we calculate class balance, because when there is an inbalance, the model might be biased to predict toward the majority class*

In [365] : class_balance = fluprint_clean['vaccine_response'].value_counts(normalize=True)

print("\nClass balance:\n", class_balance)

*#using Seaborn, we make a plot to visualize class inbalance*

In [367] : plt.figure(figsize=(8, 6))

sns.countplot(x='vaccine_response', data=fluprint_clean)

plt.title('Class Distribution')

plt.show()

*#reassure there are no NA-values*

In [391] : result = fluprint_clean[fluprint_clean['data'].isnull()]

len(result)












