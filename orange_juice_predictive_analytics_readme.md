# Predictive Analytics for Business: Orange Juice Demand Forecasting

This repository contains the R code and final academic report for a Predictive Analytics project analyzing retail demand for orange juice. The project was completed as part of the **Predictive Analytics for Business** course at the University of Edinburgh.

## 📊 Project Overview

The goal of this project is to model and predict the sales volume (`logmove`) of three orange juice brands (Dominick's, Minute Maid, and Tropicana) across various stores. By applying Ordinary Least Squares (OLS) regression techniques, the analysis uncovers the impact of price elasticity, promotional features, and brand equity on consumer demand. 

### Key Business Insights
*   **Brand Equity Offsets Price:** Tropicana commands the highest average price (\$2.89) but maintains comparable sales volumes to the budget brand (Dominick's, \$1.75). Tropicana's customer base exhibits significantly lower price sensitivity.
*   **Promotional Power:** The presence of a promotional feature (e.g., an advertisement) is a massive driver of demand, correlating with an approximately 139.5% increase in units sold.
*   **Non-Linear Price Elasticity:** The relationship between price and sales is not strictly linear; demand becomes marginally less price-sensitive at higher price points, necessitating a quadratic model specification.

## 📁 Repository Contents

*   **`Cw1_code.R`**: The complete R script used for Data Exploration, Assumption Checking, Model Building (Simple, Multiple, Quadratic, Interaction, and Stepwise selection), and Model Evaluation.
*   **`Report.pdf`**: The comprehensive 3,400+ word final report detailing the statistical methodology, rigorous OLS assumption diagnostics, and economic interpretations of the model coefficients.
*   **`oj.csv`**: The dataset containing 1,728 observations spanning 120 weeks. Includes variables for Store, Brand, Week, Logmove (log of units sold), Price, and Feature. 

## 🛠️ Methodology & Modeling

The project takes a structured approach to econometric modeling:
1.  **Exploratory Data Analysis (EDA):** Visualizing distributions, skewness, and variance across brands and stores.
2.  **Simple Linear Regression:** Establishing a baseline relationship between `logmove` and `logprice`.
3.  **Multiple Linear Regression:** Incorporating categorical variables (Brand, Store) via dummy encoding, along with Feature and Week.
4.  **Rigorous Assumption Diagnostics:** Testing Gauss-Markov assumptions using visual plots (Q-Q, Scale-Location, ACF) and statistical tests (Curvature, Tukey, Durbin-Watson, VIF).
5.  **Advanced Specifications:** Implementing quadratic terms to capture non-linear price effects and interaction terms (`logprice * brand`) to evaluate brand-specific price elasticities.
6.  **Model Evaluation:** Using a 75/25 Train-Test split to evaluate model generalization, resulting in a robust Multiple Regression model that reduced Out-of-Sample MSE by ~52% compared to the baseline simple model.

## 💻 Prerequisites & Setup

To run the R code, you will need to install the following packages:

```R
install.packages(c("AER", "dplyr", "lmtest", "car", "ggplot2", "GGally", "MASS", "MLmetrics"))
```

### How to Run
1. Clone the repository.
2. Open `Cw1_code.R` in RStudio or your preferred R environment.
3. **Important:** Update the `setwd()` and `read.csv()` paths at the top of the script to match the directory where you saved `oj.csv` on your local machine.
4. Run the script sequentially to generate the statistical outputs and diagnostic plots (which will be saved as PDFs in your working directory).

## 🎓 Academic Context
This project demonstrates proficiency in data wrangling, econometric theory, statistical diagnostics, and the translation of mathematical models into actionable business intelligence.
