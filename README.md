# Anti-Cartel Policy Preferences: Discrete Choice Experiment

## Overview

This repository contains the R analysis developed as part of my Master's dissertation in Business Intelligence (BDEEM) at Université Marie et Louis Pasteur.

The research investigates how individuals evaluate different anti-cartel enforcement policies through a Discrete Choice Experiment (DCE). Participants were asked to choose between alternative policy configurations combining different levels and mechanisms of enforcement.

The analysis covers the complete workflow from data cleaning and restructuring to econometric modelling, statistical analysis and visualisation.

## Research question

How do individuals trade off different levels and mechanisms of punitiveness in anti-cartel enforcement policies?

The experiment focuses on several policy dimensions, including:

- administrative fines
- evidence transmission to civil courts
- liability and compensation mechanisms associated with cooperating firms

## Methodology

The analysis was conducted in R and includes:

- cleaning and quality control of survey responses
- restructuring DCE data into long format
- construction of experimental policy variables
- descriptive statistics
- conditional logit models
- interaction effects
- robust standard errors clustered by respondent
- odds ratios and 95% confidence intervals
- model comparison
- data visualisation

The final analytical sample contains 458 respondents completing 12 choice tasks each, resulting in 5,496 observed choices.

## Tools

R · dplyr · tidyr · ggplot2 · survival · readxl · readr

## Repository structure

- `analysis.R`: complete R script used for the dissertation analysis
- `Figures/`: selected visual outputs from the analysis

## Data availability

The original survey data are not included in this repository in order to protect participant privacy and comply with research data confidentiality requirements.

The analysis uses data collected through LimeSurvey and participant recruitment conducted through Prolific.

## Key output

The conditional logit analysis examines how the different characteristics of anti-cartel policies influence the probability that a policy configuration is selected.

Selected results and visualisations from this project are also presented in my professional portfolio.

## Author

Sabilia Djamaldiev  
Master Business Intelligence  
Behavioral & Digital Economics for Effective Management (BDEEM)  
Université Marie et Louis Pasteur
