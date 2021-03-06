# MEPS-Bootstrap-EXPND-SF6D

**Summary**

Here, we develop novel statistical codes to bootstrap weighted AHRQ Medical Expenditure Panel Survey (MEPS) survey data to incorporate parameter uncertainty around regression equations that predict annual health care expenditures and health-state preference weights based on diabetes, cardiovascular diagnoses, and other individual characteristics.

MEPS comprises of publicly available, de-identified data from a nationally representative sample of families and individuals subsampled from the prior year’s National Health Interview Survey (NHIS) conducted by the National Center for Health Statistics. Each panel consists of two years with 5 rounds of interviews for each individual.

We define the first year as “baseline” and second year as the one-year “outcome” period. For estimation of annual expenditures and SF-6D utility scores, we use multivariable regression analyses optimized for distribution and link function accounting for the survey sampling design.

Medical expenditures and out-of-pocket costs are adjusted to reflect the most recent financial year using the Personal Consumer Expenditures and Consumer Price Index, respectively. Generic preference-weighted quality of life (“utility”) scores are calculated from the individual-level SF-12 questionnaire data using the Short Form (SF)-6D health utility index.

**References**

1. Klein S, Morey JR, Jiang S, Pai A, Mancini D, Lala A, Ferket BS. Estimated health care utilization and expenditures in individuals with heart failure from the Medical Expenditure Panel Survey. Circ Heart Fail 2021 May 13;CIRCHEARTFAILURE120007763. doi: 10.1161/CIRCHEARTFAILURE.120.007763.

2. Morey JR, Jiang S, Klein S, Max W, Masharani U, Fleischmann KE, Hunink MG, Ferket BS. Estimating health utility scores and expenditures for cardiovascular disease from the Medical Expenditure Panel Survey. Circ Cardiovasc Qual Outcomes. 2021 Mar 25:CIRCOUTCOMES120006769. doi: 10.1161/CIRCOUTCOMES.120.006769.

**Acknowledgments**

Research reported in this repository was supported by the National Heart, Lung, And Blood Institute (NHLBI) of the National Institutes of Health under award number: R01HL153456. The content is solely the responsibility of the authors and does not necessarily represent the official views of the National Institutes of Health.

This work has not been peer-reviewed. The authors have no competing interests to declare.