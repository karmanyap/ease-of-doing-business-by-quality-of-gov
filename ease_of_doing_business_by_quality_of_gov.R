
## EDA for Ease of Doing Business by Quality of Government

df <- read.csv("https://raw.githubusercontent.com/karmanyap/ease-of-doing-business-by-quality-of-gov/master/qog_std_cs_jan20.csv")

# Loading the packages


library(tidyverse)
library(readr)
library(kableExtra)
library(ggplot2)
library(tidyr)
library(factoextra)
library(purrr)
library(corrplot)
library(cluster)
library(Hmisc)



## Research Question

# The QoG dataset explores the quality of government data for the year 2020 from multiple data sources. 
# From multiple categories and data sources, the focus of this research and report is on the Ease of doing business.

# The research question for this project would be: 
#"How do business regulations and enforcements across various economies affect their respective ease of doing business?"



## Describe Data

# Out of 1758 variables, this research focuses on 12 business factor varables and 2 variables identifying the economy or the country.

# The score for each of the above independent variables ranges from 0 to 100, where 0 represents the worst regulatory performance and 100 the best regulatory performance.

eda <- subset(df,select = c(ccode,cname, eob_dcp16, eob_ec16, eob_eob16, eob_gc15, eob_ge16, eob_ldri, eob_pmi15, eob_pt17, eob_ri15, eob_rp16, eob_sab, eob_tab16))


# Normalize column names

names(eda) <- make.names(names(eda))

# Number of rows

n_row <- nrow(eda)

# Number of columns

n_col <- ncol(eda)

# Run it in the current block but always comment this out for your final report

# str(eda) # commented for the output file

# Converting eob_gc15 from integer to numeric

eda$eob_gc15 <- as.numeric(eda$eob_gc15)

# str(eda) # Checking for data types # comment for output file


# run it in the current block but always comment this out for your final report

# head(eda) # commented for the output file


# Finding numeric variables
num_var <- eda %>% 
  select_if(is.numeric) %>% # recall how to check data types
  names()


# Finding character variables

chr_var <- eda %>% 
  select_if(is.character) %>%
  names()

# Finding factor variables

fac_var <- eda %>%
  select_if(is.factor) %>%
  names()


# Change all characters into factors. Recall what factors are.
eda <- eda %>% 
  mutate_if(sapply(eda, is.character), as.factor)



# Finding out number missing values?

miss_count <- table((is.na(eda)))[2]
miss_perc <- prop.table(table((is.na(eda))))[2]


# Drop rows with missing values

eda_nona <- na.omit(eda)

# New number of rows

new_n_row <- nrow(eda_nona)

# Since only 3.72% of the observations have missing values, 
# there is significant data to conduct further research by loading the data frame without missing values into a new data frame.

n_countries <- length(eda_nona$cname)



## Exploratory Data Analysis

# Within the scope of this report, only few variables are chosen for exploration.

# The following graphs show the score frequency for starting a business and trading across borders.

hist(eda_nona$eob_sab,
     main="Histogram for EOB - Starting a business", 
     xlab="Score", 
     border="black", 
     col="gray",
     xlim=c(0,100),
     las=1, 
     breaks=5)

hist(eda_nona$eob_tab16,
     main="Histogram for EOB - Trading across borders", 
     xlab="Score", 
     border="black", 
     col="gray",
     xlim=c(0,100),
     las=1, 
     breaks=5)


hist(eda_nona$eob_pmi15,
     main="Histogram for EOB - Trading across borders", 
     xlab="Score", 
     border="black", 
     col="gray",
     xlim=c(0,100),
     las=1, 
     breaks=5)


# The following graph shows the top 10 countries by ease of doing business.

eda_nona %>%
  group_by(cname) %>%
  summarise(mean_ease_business = mean(eob_eob16)) %>%
  arrange(desc(mean_ease_business)) %>%
  head(.,n=10) %>%
  ggplot()+
  geom_bar(aes(x=reorder(cname, -mean_ease_business), y=mean_ease_business), stat= "identity") +
  geom_label(aes(x=cname, y=mean_ease_business, label = round(mean_ease_business, digits = 2))) +
  labs(title = "Top 10 Countries with ease of doing business", x = "Country", y = "Mean Score")

# The following graph shows the top 10 countries that have good policies for taxes.

eda_nona %>%
  group_by(cname) %>%
  summarise(mean_taxes = mean(eob_pt17)) %>%
  arrange(desc(mean_taxes)) %>%
  head(.,n=10) %>%
  ggplot()+
  geom_bar(aes(x=reorder(cname, -mean_taxes), y=mean_taxes), stat= "identity") +
  geom_label(aes(x=cname, y=mean_taxes, label = round(mean_taxes, digits = 2))) +
  labs(title = "Top 10 Countries with good tax policy", x = "Country", y = "Mean Score")

# The following plot shows the correlation between the variables

eda_nona_cor <- cor(eda_nona[,-c(1,2,6)], method = "pearson")

#eda_nona_cor # comment out during final print

corrplot(eda_nona_cor, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)

# The following graph shows the distribution of data for each variable.

ggplot(stack(eda_nona[-1]), aes(x = ind, y = values, fill="coral")) +
  geom_boxplot()+
  labs(title = "Distribution of data for each variable", x = "Variable Names", y = "Values")


# The data exploration reveals that there are no missing values and the variable eob_ldri Land dispute resolution index has a different data range. 

# This column will not be used in future analysis in this research.

# Also, variables eob_sab, eob_pt17, eob_tab16 and eob_dcp16 have significant frequencies of outliers and decreasing in the said order.



## Clustering

# The aim of cluster analysis is to group countries with similar scores of ease of business and to see which countries fall in each group.


rownames(eda_nona)<- eda_nona$cname

eda_cluster <- eda_nona[,-c(1,2)]

# Since the data is already standardized on a scale, there was no need to scale the data using scale().

# Using elbow, silhouette and gap statistics methods to find the optimal number of clusters, 
# it can be concluded that it is optimal to have 3 clusters for this data frame.


# Elbow method to determine optimal clusters

set.seed(123)

fviz_nbclust(eda_cluster, kmeans, method = "wss") # gives ideal kmean of 2

# silhouette method

fviz_nbclust(eda_cluster, kmeans, method = "silhouette") # gives ideal kmean of 2


# Gap Statistics method:

# compute gap statistic
set.seed(123)
gap_stat <- clusGap(eda_cluster, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)

# Print the result

#print(gap_stat, method = "firstmax") # commenting for final output

fviz_gap_stat(gap_stat) # gives ideal kmean of 4



set.seed(123)
final <- kmeans(eda_cluster, 2, nstart = 50) # using 2 clusters for kmeans

fviz_cluster(final, data = eda_cluster)


## PCA

# Looking at the data, it is observed that there are more variable than can be used to form a good analysis. To reduce the number of variables, Prinicipal Component Analysis is utilized.

# For PCA, first the correlation between variables are plotted.

# pairs(eda_cluster,cex.labels = 2, font.labels = 2) # comment for knitted file

# cor(eda_cluster)  # comment for knitted file

res <- cor(eda_cluster, method='pearson')

corrplot(res, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)


# The above graph clearly shows the correlation between some variables are high whereas others are moderate to weak.


# The next steps is to reduce the variable by fitting the data using PCA.

fit <- prcomp((eda_cluster), cor = TRUE)

summary(fit) # comment for output file

eda_new <- fit$x[,(1:4)]

head(eda_new)


# From the above importance of components, it is deduced that the 1st 4 have a cumulative proportion of 81.48% of the variance.

# This can be further explained using the graphs below.


fviz_eig(fit, addlabels = TRUE, ylim = c(0, 85))

eig.val <- get_eigenvalue(fit)

# eig.val  # comment for knitted file

# The Biplot clearly shows that the variable: eob_ri15, eob_tab16, eob_ge16 and eob_eob16 have the highest contribution to both PC1 and PC2.

biplot(fit, expand=3, xlim=c(-0.20, 0.4), ylim=c(-0.4, 0.4))


## Conclusion

# From the above analysis, it can be concluded that business regulations and enforcements across various economies 
# affect their respective ease of doing business and they can be grouped into 3 different clusters of countries 
# from the methods based on the scores of the variables considered.

# The PCA shows us that the whole analysis can be reduced to just 4 components or variables that would 
# reproduce the information with similar information making the computation faster and easier analysis.

