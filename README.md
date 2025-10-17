# Multiple Linear Regression on Dairy Cow Health Data

## Project Summary

**The Problem:**  
Mastitis — a common udder infection — is one of the biggest challenges in the dairy industry, reducing both milk quality and farm profitability.  
This project aimed to identify the main risk factors contributing to mastitis in dairy cows using real-world farm data.

**Our Approach:**  
We analyzed data from three dairy farms in New Zealand.  
Using *multiple linear regression*, a statistical method that examines relationships between variables, we built a model to understand which factors most strongly affect **somatic cell count (SCC)** — a key indicator of mastitis.

**Key Findings:**  
1. **Age:** Older cows are more likely to develop mastitis.  
2. **Time to Conceive:** Cows that take longer to become pregnant show higher SCC levels.  
3. **Farm Environment:** The specific farm significantly influences SCC, suggesting that management practices play an important role in preventing mastitis.

**Impact and Value:**  
This project provides actionable insights for dairy farmers.  
Knowing that older cows and those with reproductive challenges are more vulnerable allows for better-targeted preventative care.  
It also emphasizes the value of *data-driven herd management* to improve both animal health and business outcomes.  
The entire analysis is fully reproducible and transparent, ensuring trustworthy results.

---

## Project Overview

This project was conducted as part of the **QMET306/608 assessment in Lincoln University while I was study Experimentation**.  
It applies multiple linear regression to identify key factors associated with **average individual somatic cell count (ISCC)** in dairy cows — an important indicator of mastitis.  
The data come from a study across **three dairy farms in Manawatu, New Zealand**.

---

## Dataset

The dataset used for this analysis is **`iscc.xlsx`**, which contains the following variables:

**Response Variable:**  
- `averageiscc`: Average individual somatic cell count (×1,000 cells per ml)

**Explanatory Variables:**  
- `age`: Age of the cow (years)  
- `farmid`: Farm identifier  
- `lactationno`: Lactation number  
- `timetocon`: Days from planned start of mating to conception  
- `treatment`: Treatment with a monensin rumen capsule (Yes/No)  
- `noservices`: Number of matings  

---

## Analysis and Methods

The analysis was carried out using **`analysis.R`**, following these main steps:

1. **Data Cleaning and Transformation** – including log-transformation of skewed variables  
2. **Exploratory Data Analysis** – visualized relationships using a scatterplot matrix  
3. **Model Building** – applied stepwise selection using `stepAIC` to identify the best-fit model  
4. **Diagnostics and Validation** – performed residual checks and multicollinearity testing (VIF)

---

## Skills Used

*   **Programming Languages:** R
*   **Statistical Methods:** Multiple Linear Regression, Stepwise Selection (stepAIC), ANOVA, Residual Analysis, VIF Check, Data Transformation (Log-transformation)
*   **R Packages:** `ggplot2`, `car`, `lmtest`, `AICcmodavg`, `tidyr`, `dplyr`, `ggpubr`, `EnvStats`, `bestNormalize`, `MASS`, `openxlsx`, `writexl`, `GGally`, `reshape2`
*   **Data Analysis Workflow:** Data Cleaning, Exploratory Data Analysis (EDA), Model Building, Post-estimation Diagnostics, Reproducible Research
*   **Domain Knowledge:** Dairy Industry, Mastitis, Somatic Cell Count, Experimental Design (implied by QMET608 context)

---

## Requirements

To reproduce this analysis, install **R** and the following R packages:

`ggplot2`, `car`, `lmtest`, `AICcmodavg`, `tidyr`, `dplyr`, `ggpubr`,  
`EnvStats`, `bestNormalize`, `MASS`, `openxlsx`, `writexl`,  
`GGally`, `reshape2`

---

## Usage

1. Install **R** and **RStudio** on your system.  
2. Install the required R packages listed above.  
3. Set the working directory to this project’s folder, or open `analysis.R` in RStudio.  
4. Run the `analysis.R` script (or Knit the `report.Rmd` file).  
5. The script will automatically load the `iscc.xlsx` data, perform the full analysis, and generate all outputs.