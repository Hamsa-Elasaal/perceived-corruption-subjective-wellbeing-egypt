# The Influence of Perceived Corruption on Subjective Well-Being in Egypt

## Project Overview
This graduation research project examines the relationship between perceived corruption and subjective well-being in Egypt. Perceived corruption is represented through survey measures related to bribery and nepotism (wasta), while subjective well-being is examined through two ordinal outcomes: happiness and life satisfaction.

The study uses Wave 7 of the World Values Survey (WVS), Egypt 2018, and a nationally representative analytical sample of 1,144 Egyptian adults after data cleaning.

## Research Questions
- How are perceptions of bribery and nepotism associated with happiness and life satisfaction in Egypt?
- Do these relationships remain after accounting for socioeconomic and demographic characteristics?
- How are happiness and life satisfaction related when modeled jointly?

## Data Source
The analysis is based on Egypt data from Wave 7 of the World Values Survey. The original respondent-level dataset is not included in this repository. See [DATA-SOURCE.md](DATA-SOURCE.md) for source and access information.

## Methodology in the Final Report
The final report presents:
- Separate ordinal logistic regression models for happiness and life satisfaction, estimated within the Vector Generalized Linear Model (VGLM) framework using the VGAM package in R.
- Assessment of model assumptions, including the proportional-odds assumption, with partial proportional-odds specifications where appropriate.
- Average marginal effects and model-fit evaluation.
- A bivariate ordered probit model to jointly examine happiness and life satisfaction while accounting for their residual correlation.

The report estimates a polychoric correlation of 0.569 (SE = 0.023) between happiness and life satisfaction, and a residual correlation of 0.322 (SE = 0.038) in the bivariate ordered probit model.

## Exploratory and Additional R Analyses
The R script also contains additional model specifications and analyses beyond those reported as final findings. These include path-analysis specifications and mediation analyses, as well as other supplementary model explorations.

Path Analysis and Mediation Analysis were implemented in R, but they did not yield usable results for inclusion in the final report. They are documented as attempted analyses in the code, not presented as established findings.

## Challenges and Limitations
The final report identifies the following limitations:
1. Some variables contained sparse response categories. Categories were collapsed or merged during data cleaning to support stable model estimation and avoid convergence problems. This may have reduced detail and variability in some variables.
2. The cleaned analytical sample contained 1,144 respondents. The report describes this sample as nationally representative and sufficient for the analyses conducted; a larger sample could nevertheless provide greater statistical power and allow more detailed modeling of less frequent categories.
3. The study uses cross-sectional data from a single WVS wave. Its results describe associations and do not establish direct causal relationships.

## My Contribution
My contribution to the group project included:
- **Data Cleaning and Preparation:** Cleaning and preparing the WVS Egypt 2018 data, including handling missing values, recoding variables, and organizing the analytical dataset.
- **Descriptive Exploration:** Contributing to the exploration of variable distributions and descriptive characteristics.
- **Variable Selection and Correlation Exploration:** Working with the team to consider variable availability and examine relationships among variables using correlation analysis.
- **Results Review and Interpretation:** Reviewing and interpreting analytical results with the team.
- **Research Documentation:** Reviewing sections drafted by other team members and helping maintain consistency between the analysis, interpretation, and final report.

## Project Team
**Prepared by:**
- Hamsa Hani Samir
- Merna Sameh Mahmoud
- Nouran Tarek Youssef
- Safa Aymen Mohamed
- Salma Saeed Fathi Saeed

**Supervisor:** Dr. sherouk Moawad

**Institution:** Cairo University, Faculty of Economics and Political Science, Statistics Department, English Section

**Academic Year:** 2025/2026

## Repository Contents
- [graduation_project_analysis.R](graduation_project_analysis.R) — R script containing data preparation and statistical analyses, including supplementary attempted analyses.
- [The Influence of Perceived Corruption on Subjective Well-Being in Egypt.pdf](The%20Influence%20of%20Perceived%20Corruption%20on%20Subjective%20Well-Being%20in%20Egypt.pdf) — final graduation research report (PDF).
- [DATA-SOURCE.md](DATA-SOURCE.md) — information about the original WVS data source.

---
This repository documents an academic group project. The final report and R script should be read together, noting that not every analysis implemented in the script was retained in the final report.
