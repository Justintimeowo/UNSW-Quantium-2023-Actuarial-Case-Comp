# UNSW Quantium 2023 Actuarial Case Competition

## Case Overview

You are RiskyLending Pty Ltd, a broker who specialises in home loans and obtain profits through a commission-based arrangement with a major bank, ConnBank, who writes all the loans sold. For the past 3 years, the bank has agreed to a commission rate of 4% of the loan value on all loans which avoid clawback (loan refinances and defaults) in the first 12 months.

ConnBank recently announced a change to the commission rate, reducing it from 4% to 2.5%, and an increase in the clawback period to 18 months. RiskyLending has asked for your help in understanding the current and future state of their business, and would like to be advised on maintaining profitability going forward, with three key deliverables.

1. **Profitability**: Measure the profitability of RiskyLending across the 3 years of data available. Investigate the impact that the change in commission rate and clawback period will have on profitability and suggest short-term mitigation options
2. **Business Model Viability**: Test the viability of RiskyLending’s business model and assess the risk of its current portfolio of loans in the context of prevailing macroeconomic conditions
3. **Future Road Map**: Provide a long-term strategy for RiskyLending considering the conducted analysis. RiskyLending is eager to hear out of the box ideas

Allowed 3 weeks to develop a comprehensive presentation to be submitted.

## Data Analysis and Modelling

### Exploratory Data Analysis (EDA)

- **Objective**: To explore the dataset provide insight into customer demographics and loan characteristics that comprise of RiskyLending's portfolio
- Clean, merge and diagnose dataset to ensure it is feasible to work with
- Identify relationships and correlations between variables to understand portfolio demographics

### Predictive Modelling

- **Objective**: Develop a predictive model to predict which loan applicants are likely to clawback on the loan
- **Methodology**: A range of predictive models were tested, including Logistic Regression, Multi-nominal Logistic regression, and Random Forest Decision Tree
- **Evaluation**: The Random Forest Decision Tree model was selected due to its superior default classification, refinance classification, and none classification. The model achieved an overall classification accuracy of **96.12%** using test data

### Financial Modelling

- **Approach**: Utilise Monte Carlo Simulation to model the profitability and Poisson Process to simulate loan clawback
- **Methodology**: Simulation methodology diagram
1. Generate datasets for simulation: assigning random factors, loan characteristics, and customer demographics
2. Estimate clawback rate using historical dataset
3. Use predictive clawback model to determine which applicants clawback
4. Set commission rate and clawback period
5. Use Poisson Process to simulate loan clawback
6. Compute profitability and use Monte Carlo Simulation using 5000 simulations

### Macroeconomic Modelling and Forecasting

- **Objective**: Develop a model to understand and forecast the demand for loans & clawback rate on RiskyLending's portfolio
- **Methodology**: Created linear regression models to determine relationships between macroeconomic variables and demand for loans & clawback rate using economic data. Utilised estimated macroeconomic variables from Factset to forecast growth rates in demand for loans and clawback. Monte Carlo Simulations was then performed with these forecasted macroeconomic impacts.

## Key Achievements

### Recognition and Challenges

- Submitted PowerPoint presentation which summarises analysis, modelling, and recommendations in a strong 2-person team (originally a 3-person team)
- Made it through semi-finals where we presented to 2 Actuarial Academics from the Risk and Actuarial Business School
- Moved through to the finals where we presented to 3 senior consultants in the financial services field at Quantium headquarters
- Placed 2nd in the case competition
- **Tools Used**: The analysis was primarily conducted using R and Excel, leveraging its statistical and data analysis capabilities as well as data visualisations.
