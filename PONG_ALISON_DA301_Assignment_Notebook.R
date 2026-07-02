## LSE Data Analytics Online Career Accelerator 
# DA301:  Advanced Analytics for Organisational Impact

###############################################################################

# Assignment 5 scenario
## Turtle Games’s sales department has historically preferred to use R when performing 
## sales analyses due to existing workflow systems. As you’re able to perform data analysis 
## in R, you will perform exploratory data analysis and present your findings by utilising 
## basic statistics and plots. You'll explore and prepare the data set to analyse sales per 
## product. The sales department is hoping to use the findings of this exploratory analysis 
## to inform changes and improvements in the team. (Note that you will use basic summary 
## statistics in Module 5 and will continue to go into more detail with descriptive 
## statistics in Module 6.)

################################################################################

## Assignment 5 objective
## Load and wrangle the data. Use summary statistics and groupings if required to sense-check
## and gain insights into the data. Make sure to use different visualisations such as scatterplots, 
## histograms, and boxplots to learn more about the data set. Explore the data and comment on the 
## insights gained from your exploratory data analysis. For example, outliers, missing values, 
## and distribution of data. Also make sure to comment on initial patterns and distributions or 
## behaviour that may be of interest to the business.

################################################################################

# Module 5 assignment: Load, clean and wrangle data using R

## It is strongly advised that you use the cleaned version of the data set that you created and 
##  saved in the Python section of the course. Should you choose to redo the data cleaning in R, 
##  make sure to apply the same transformations as you will have to potentially compare the results.
##  (Note: Manual steps included dropping and renaming the columns as per the instructions in module 1.
##  Drop ‘language’ and ‘platform’ and rename ‘remuneration’ and ‘spending_score’) 

## 1. Open your RStudio and start setting up your R environment. 
## 2. Open a new R script and import the turtle_review.csv data file, which you can download from 
##      Assignment: Predicting future outcomes. (Note: You can use the clean version of the data 
##      you saved as csv in module 1, or, can manually drop and rename the columns as per the instructions 
##      in module 1. Drop ‘language’ and ‘platform’ and rename ‘remuneration’ and ‘spending_score’) 
## 3. Import all the required libraries for the analysis and view the data. 
## 4. Load and explore the data.
##    - View the head the data.
##    - Create a summary of the new data frame.
## 5. Perform exploratory data analysis by creating tables and visualisations to better understand 
##      groupings and different perspectives into customer behaviour and specifically how loyalty 
##      points are accumulated. Example questions could include:
##    - Can you comment on distributions, patterns or outliers based on the visual exploration of the data?
##    - Are there any insights based on the basic observations that may require further investigation?
##    - Are there any groupings that may be useful in gaining deeper insights into customer behaviour?
##    - Are there any specific patterns that you want to investigate
## 6. Create
##    - Create scatterplots, histograms, and boxplots to visually explore the loyalty_points data.
##    - Select appropriate visualisations to communicate relevant findings and insights to the business.
## 7. Note your observations and recommendations to the technical and business users.

###############################################################################

# Your code here.
# Install packages
install.packages('skimr')
install.packages('DataExplorer')

# Load Libraries
library(tidyverse)
library(DataExplorer)
library(skimr)

# Import the clean turtle_reviews csv
turtle_reviews <- read.csv('turtle_reviews_clean.csv', header = TRUE)
turtle_reviews <- read.csv(file.choose(), header=TRUE)

# View the data
View(turtle_reviews)
str(turtle_reviews)

# Explore the data
# Check for missing values
turtle_reviews[is.na(turtle_reviews)]
sum(is.na(turtle_reviews))

# Both outcomes show 0 missing values or NaN values, meaning we do not have to deep-dive and fix the data

# Descriptive statistics
summary(turtle_reviews)
skim(turtle_reviews)

# Check for duplicates
duplicated(turtle_reviews)
sum(duplicated(turtle_reviews))

# Conduct exploratory analysis on data set
# Explore overall trends in data set
# Overall Customer Loyalty Points Distribution
hist(turtle_reviews$loyalty_points,
     xlab="Loyalty Points",
     ylab="No. of Customers",
     main="Histogram of Loyalty Points Distribution")

boxplot(turtle_reviews$loyalty_points,
        xlab="Loyalty Points",
        ylabel="No. of Customers",
        main="Boxplot of Loyalty Points")

boxplot.stats(turtle_reviews$loyalty_points)$stats

# There is a notable right skew in the loyalty points distribution as can be seen by the long tail extending toward the 7000 mark
# The mean (1578) is significantly higher than the median (1276), indicating that there are a few high-value outliers on the right skew who are pulling the average up
# This is further supported by the boxplot displaying outliers above the upper cap (3218)
# 75% of customers have fewer than 1751 loyalty points, highlighting how extreme those top spenders are
# The 1000 - 1500 peak confirms that the typical customer falls under the lower-loyalty point bracket 
# There are high frequencies of customers in the 0 - 1500 bins, suggesting that there are a high number of 'one-off' customers
# Would be interesting to understand why these customers aren't moving into the higher loyalty points bins

# Overall Customer Remuneration distribution 
hist(turtle_reviews$remuneration)

# summary() confirms that the minimum remuneration is 12.30k
# therefore, no customers fall into the 0-10k bin, which is why there are no customers with a remuneration score between 0 - 10k
# The 10-20k bin thus contains the lowest earners, starting at 12.30k.
# There is a right skew, with the tail extending toward the 100-120k mark, representing the highest-earning 'outlier' customers. 
# Average as confirmed in the summary() output is 47.15k
# Highest frequency of customers occur in the 30-40k bracket, followed by 40-50k, 50-60k and 10-20k bins.
# This shows that the significant portion of the customer base consists of lower-to-middle income earners
# There seems to be a large mainstream group (10 - 60k), a second upper-middle income group in the 60-100k bracket and a small high income group in the 100-200k bracket

# Overall Customer Spending Score Distribution
hist(turtle_reviews$spending_score,
     xlab="Spending Score (1-100)",
     ylab="No. of Customers",
     main="Spending Score Distribution")

boxplot(turtle_reviews$spending_score,
        xlab="Spending Score",
        ylabel="No. of Customers",
        main="Boxplot of Spending Score")

summary(turtle_reviews$spending_score)

# The distribution is multimodal, with a few peaks in the 10-20, 40-60, and 70-80 bins 
# Because the peaks on both left and right side of the distribution are somewhat 'balanced', it explains why the mean and median are both 50
# 75% of customers have a score of 73 and lower
# Low engagement spending group: (0-30) - there is a clear hump at the start in the 10-20 bin. These might be one-time buyers or occasional shoppers
# Average Spending group: (40 - 60) this is the biggest group, and they align perfectly with the mean.
# Power Spending group: (70 - 100)
# Spending scores are overall a lot more spread out, unlike the remuneration histogram. 
# Even though many customers have lower incomes, a good portion of them are still highly engaged and have very high spending scores

# Exploring relationships between remuneration/spending score/age and loyalty points accumulation
# Remuneration vs Loyalty Points
ggplot(turtle_reviews, aes(x = remuneration, y = loyalty_points)) +
  geom_point(color = "steelblue", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red") + # Adds a linear regression line
  theme_minimal() +
  labs(title = "Loyalty Points by Remuneration",
       x = "Remuneration",
       y = "Loyalty Points")

# The Remuneration vs Loyalty Points scatter plot reveals a strong positive correlation, but data points began to scatter beyond the 55k remuneration threshold
# The data diverges into 2 distinct behavioral paths: a high-velocity loyalty group and a conservative group
# This suggests that remuneration alone is an inconsistent predictor of loyalty points for high-income earners and we will need to include spending score in a multiple linear regression model to capture behavioral nuances

# Spending Score vs Loyalty Points
ggplot(turtle_reviews, aes(x = spending_score, y = loyalty_points)) +
  geom_point(color = "salmon", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red") + # Adds a linear regression line
  theme_minimal() +
  labs(title = "Loyalty Points by Spending Score",
       x = "Spending Score",
       y = "Loyalty Points")

# The Spending Score vs Loyalty Points scatter plot reveals a consistent split in customer behavior across 2 key dimensions
# Just as remuneration showed divergence at the 55k mark, the Spending Score demonstrates a clear transition at the 63-score threshold
# High-velocity earners: these customers cross the 2000 loyalty point threshold, showing an accelerated return on every point of spending score 
# Conservative earners: despite high spending scores, this group earns loyalty points at a slower rate, staying below the 2000-point mark

# Age vs Loyalty Points
ggplot(turtle_reviews, aes(x = age, y = loyalty_points)) +
  geom_point(color = "purple", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red") + # Adds a linear regression line
  theme_minimal() +
  labs(title = "Loyalty Points by Age",
       x = "Age",
       y = "Loyalty Points")

# The Age vs Loyalty Points scatter plot demonstrates a near-zero correlation between the two features. 
# This confirms that age is not a contributing factor to loyalty point accumulation
# Furthermore, the presence of high-value outliers across the entire age spectrum suggests that further analysis should focus on behavioral spending patterns rather than age-based demographic segmentation


###############################################################################
###############################################################################

# Assignment 6 scenario

## In Module 5, you were requested to redo components of the analysis using Turtle Games’s preferred 
## language, R, in order to make it easier for them to implement your analysis internally. As a final 
## task the team asked you to perform a statistical analysis and create a multiple linear regression 
## model using R to predict loyalty points using the available features in a multiple linear model. 
## They did not prescribe which features to use and you can therefore use insights from previous modules 
## as well as your statistical analysis to make recommendations regarding suitability of this model type,
## the specifics of the model you created and alternative solutions. As a final task they also requested 
## your observations and recommendations regarding the current loyalty programme and how this could be 
## improved. 

################################################################################

## Assignment 6 objective
## You need to investigate customer behaviour and the effectiveness of the current loyalty program based 
## on the work completed in modules 1-5 as well as the statistical analysis and modelling efforts of module 6.
##  - Can we predict loyalty points given the existing features using a relatively simple MLR model?
##  - Do you have confidence in the model results (Goodness of fit evaluation)
##  - Where should the business focus their marketing efforts?
##  - How could the loyalty program be improved?
##  - How could the analysis be improved?

################################################################################

## Assignment 6 assignment: Making recommendations to the business.

## 1. Continue with your R script in RStudio from Assignment Activity 5: Cleaning, manipulating, and 
##     visualising the data.
## 2. Load and explore the data, and continue to use the data frame you prepared in Module 5.
## 3. Perform a statistical analysis and comment on the descriptive statistics in the context of the 
##     review of how customers accumulate loyalty points.
##  - Comment on distributions and patterns observed in the data.
##  - Determine and justify the features to be used in a multiple linear regression model and potential
##.    concerns and corrective actions.
## 4. Create a Multiple linear regression model using your selected (numeric) features.
##  - Evaluate the goodness of fit and interpret the model summary statistics.
##  - Create a visual demonstration of the model
##  - Comment on the usefulness of the model, potential improvements and alternate suggestions that could 
##     be considered.
##  - Demonstrate how the model could be used to predict given specific scenarios. (You can create your own 
##     scenarios).
## 5. Perform exploratory data analysis by using statistical analysis methods and comment on the descriptive 
##     statistics in the context of the review of how customers accumulate loyalty points.
## 6. Document your observations, interpretations, and suggestions based on each of the models created in 
##     your notebook. (This will serve as input to your summary and final submission at the end of the course.)

################################################################################

# Your code here.
# Install necessary packages
install.packages('psych')

# Import libraries for correlation matrix
library(corrplot)
library(psych)

# Exploring correlations between loyalty points and the other features 

# Check to see values in education
table(turtle_reviews$education)

# Convert education and gender into numeric values
turtle_reviews <- turtle_reviews %>%
  mutate(
    # For gender
    gender_num = ifelse(gender == 'Male', 1, 0),
    # For education 
    education_num = case_when(
    education == 'Basic' ~ 1,
    education == 'diploma' ~ 2,
    education == 'graduate' ~ 3,
    education == 'postgraduate' ~ 4,
    education == 'PhD' ~5,
    TRUE ~ 0
  ))

# view output
str(turtle_reviews)

# Generate correlation matrix
correlation_matrix <- cor(turtle_reviews[, c('loyalty_points', 'remuneration', 'spending_score', 'age', 'gender_num', 'education_num')])

# Visualizing in a heatmap format
corPlot(correlation_matrix, cex=1)


# Correlation analysis reveals that Spending Score (r=0.67) and Remuneration (r=0.62) both have a moderately strong positive relationship with loyalty points
# The fact that correlation is not over the textbook 0.80 score confirms that loyalty point accumulations is influenced by a complex interaction of both
# All the other demographic features (age, gender, and education) exhibit zero relationships with loyalty accumulation (r values ranging from -0.05 - -0.02)
# Therefore demographic features will be excluded from the multiple linear regression model 

# Create the MLR model
model1 = lm(loyalty_points~remuneration+spending_score, data=turtle_reviews)

# Print summary statistics
summary(model1)

# Both Remuneration (t = 65.77) and Spending Score (t = 71.84) exhibit exceptionally high t-statistics.
# Combined with p-values near zero (p < 2e-16), we can confidently reject the null hypothesis
# All of the above suggests that these behavioral features are highly significant predictors of loyalty points
# The Adjusted R-squared of 0.83 indicates that the model explains 83% of the variance in loyalty point accumulation
# Thereby meaning the majority of loyalty points accumulation can be predicted using only these two variables
# The Residual Standard Error of 534.1 was a concerning metric as it represents a significant average deviation, accounting for ~34% of the mean loyalty points (1,578)
# Yet this aligns with our previous hypothesis regarding a behavioral split between customer segments  

# Plot the residuals to perform a health check on the model
# To ensure our predictions aren't consistently biased
plot(model1$residuals)
abline(h=0, col='red', lwd=2)

# In a perfect model, the errors should look like random noise
# The wavy patterns indicate that our model's accuracy is not consistent, perhaps suggesting the model to be missing a variable
# The missing variable is most likely missing from our dataset as the only features that are not already ruled out are Product ID, Summary & Review
# The far right of the plot from index 1500 onward exhibit a fan shape - this was already noted to be the behavioral split

# Verify the right-skewed distribution in loyalty points
# Specify the qqnorm function
qqnorm(turtle_reviews$loyalty_points,
       col='black',
       xlab='Z Value',
       ylab='Loyalty Points')

# Add the theoretical line for comparison
qqline(turtle_reviews$loyalty_points,
       col='red',
       lwd=2)


# Use the shapiro-wilk test to test the normality of the loyalty points data
shapiro.test(turtle_reviews$loyalty_points)

# W = 0.84307, p-value < 2.2e-16 
# the test statistic (W) is quite low for a Shapiro-Wilk test, indicating that there is a significant deviation from normality 
# Combined with a p-value far below the 0.05 threshold, our results provide strong evidence that the loyalty points follow a non-normal, skewed distribution
# This non-normal distribution is visually confirmed by the QQplot, where data points deviate sharply upward from the theoretical line beyond one standard deviation, indicating a strong right skew (hence the fanning effect)

# Install and load necessary descriptive statistics libraries
install.packages('moments')
library(moments)

# Specify the skewness and kurtosis functions
skewness(turtle_reviews$loyalty_points) # skewness greater than 1 indicates data is highly skewed. 
kurtosis(turtle_reviews$loyalty_points) # kurtosis value of 4.71 indicates the distribution has heavy tails than a normal distribution (kurtosis = 3)

# All the results above justify a log-level transformation to the loyalty points variable.
# While this analysis may not be able to find the missing variable, we may be able to stabilize the variance and improve the model robustness

# Log Transformation
turtle_reviews <- mutate(turtle_reviews,
                         logLoyaltyPoints = log(loyalty_points))

model2 <- lm(logLoyaltyPoints~remuneration+spending_score, data=turtle_reviews)

summary(model2)

plot(model2$residuals)

# Both Remuneration (52.74) and Spending Score (71.46) maintained high t-statistics after the log transformation
# With p-values (<2e-16) below p < 0.05, I remain confident in rejecting the null hypothesis
# Adjusted R-squared (0.79) has decreased slightly but is still considered a high-performing model
# Residual Standard Error value of 0.4579 (46%) indicates a consistent percentage-based error margin  

# Final notes:
# The persistent wave pattern in the residuals confirms that the model has reached its current limit with the avaiable features
# Future analysis will need to include categorical data to account for the systemic variance observed


###############################################################################
###############################################################################




