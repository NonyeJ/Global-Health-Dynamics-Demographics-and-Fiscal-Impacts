# Global-Health-Dynamics-Demographics-and-Fiscal-Impacts

## Overview 
This research provides a detailed analysis of global health systems, examining the interplay between demographics, fiscal allocation, mortality, health system equity, and efficacy from 2010 to 2021. It employs a blend of qualitative, quantitative, and domain-specific methodologies to offer a holistic view of health systems across various socioeconomic contexts. The primary aim is to identify patterns, disparities, and correlations, offering insights into global health systems to inform policies, best practices, and data scientists focusing on economics.

## Data Cleaning and Transformation
The dataset, sourced from the World Bank, encompasses 45 indicators across 23 countries. Additional data for the Power BI dashboard included four related indicators. Methods for handling missing data included Median Imputation for columns with fewer missing values and Predictive Model Imputation for those with higher percentages. This approach ensured the preservation of the original data distribution and robustness against outliers.

## EDA/Analysis
The analysis encompassed descriptive statistics to understand data distribution and properties. Statistical measures such as mean, median, mode, standard deviation, skewness, kurtosis, range, IQR, variance, and quantiles were calculated. Visual EDA included boxplots, histograms, scatterplots, and multivariate plots to understand data distribution, outliers, and relationships among variables.

## Results
Key findings from various statistical analyses include:

### Correlation Analysis:
Utilized Spearman's rank correlation and other methods to identify relationships between variables like age dependency ratios and health expenditures.
### Hypothesis Testing:
Employed tests like T-tests and Mann-Whitney U tests to explore hypotheses related to gender and location-based impacts on health expenditure.
### Regression Analysis:
Applied backward stepwise regression to identify significant predictors impacting health indicators such as mortality and life expectancy.
### Time Series Analysis:
Utilized ARIMA and ETS models to understand trends in primary indicators over the years, offering insights into the evolution of variables like health expenditure.

The results of the research weave a narrative about the intricate relationships between health systems, demographic shifts, and fiscal policies across various countries. Here's the story told by the results:

### Impact of Demographics on Health Expenditure: 
The study reveals a significant correlation between demographic factors, such as age dependency ratios, and health expenditures. Countries with a higher proportion of dependents (young and old) compared to the working-age population are likely to experience increased healthcare costs. This is a critical insight for health policy planning, as it underscores the need for health systems to adapt to demographic changes, particularly in countries with aging populations.

### Gender Differences in Health Outcomes: 
The hypothesis testing confirms significant gender-based differences in life expectancy, indicating disparities in health outcomes between males and females. This finding suggests that gender plays a crucial role in health and longevity, and health policies need to address these disparities to ensure equitable health outcomes.

### Economic Factors and Health Systems: 
The research highlights the influence of economic indicators, such as GDP, on health expenditure. The results emphasize the importance of economic strength and stability in health policy planning and the necessity for efficient allocation of resources. The study also points out that higher spending does not always equate to better health outcomes, suggesting a focus on quality over quantity in health expenditure.

### Trends in Health Expenditure and Services: 
The time series analysis shows a consistent increase in health expenditure per capita over time. This trend is crucial for forecasting future health expenditure and aids in strategic planning for healthcare services. The analysis also suggests that while there is a consistent upward trend, there are patterns in the data that could be further explored for more refined forecasting.

### Complex Interplay of Variables: 
The regression analysis illustrates the complex interplay of various predictors, such as age groups and economic variables, on health indicators like mortality and life expectancy. The results highlight the need for multifaceted approaches in health system planning that consider a range of demographic and economic factors.

## Recommendation
The study recommends focusing on specific demographic groups and fiscal strategies to improve health outcomes. It suggests that policymakers and health economists consider the impact of demographic factors like age dependency on health expenditure. The research also underscores the importance of economic considerations in health policy planning and highlights the need for health systems to adapt to changing demographic structures, especially in countries with aging populations.
