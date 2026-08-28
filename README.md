Canine UPEC Growth and Biofilm Phenotyping Analysis
Overview

This repository contains the R code used for analysis and visualization of growth kinetics and biofilm formation in a collection of canine uropathogenic Escherichia coli (UPEC) isolates evaluated under different in vitro growth conditions.

The analyses compare bacterial phenotypes across three culture media:

LB — Luria-Bertani broth
M9 — M9 minimal medium
AUM — artificial urine medium

The repository includes code for growth-curve analysis, Growthcurver parameter estimation, principal component analysis (PCA), statistical comparison of growth and biofilm phenotypes, Bayesian mixture modeling of biofilm distributions, and generation of publication figures and summary tables.

Repository Contents

The R scripts in this repository perform the following analyses.

Growth Curve Visualization

Generates overlapping OD600 growth curves for individual isolates in LB, M9, and AUM.

Analyses include:

Growth trajectories from 0–10 h
Raw OD600 visualization
log10-transformed OD600 visualization
Exclusion of blank wells
Medium-specific plotting
Export of plotting data
PNG and publication-resolution TIFF output
Growthcurver Analysis: 0–10 h

Uses the R package growthcurver to estimate growth parameters from OD600 measurements collected between 0 and 10 h.

The 24-h measurement is excluded from model fitting in this analysis.

Growth parameters include:

Parameter	Description
k	Carrying capacity
n0	Initial population size
r	Intrinsic growth rate
t_mid	Time at the midpoint of the growth curve
t_gen	Generation time
auc_l	Logistic area under the curve
auc_e	Empirical area under the curve
sigma	Residual error from model fitting

The scripts also perform PCA using the Growthcurver-derived parameters and compare multivariate growth phenotypes among LB, M9, and AUM.

Growthcurver Analysis: 0–24 h

A complementary Growthcurver analysis incorporates the 24-h OD600 measurement.

This analysis includes:

Growthcurver model fitting through 24 h
PCA of growth parameters
Medium-specific PCA plots
Global PCA across media
PERMANOVA
Multivariate dispersion testing
Kruskal-Wallis testing of PCA scores
Export of Growthcurver results, PCA scores, and PCA loadings

The 0–24 h analysis is maintained separately from the primary 0–10 h analysis so the effect of including the late stationary-phase measurement can be evaluated.

Growth and Biofilm PCA Analysis

Growthcurver-derived growth phenotypes are integrated with normalized and binary biofilm measurements.

Analyses include:

Global PCA of growth parameters
Overlay of biofilm phenotype on PCA scores
Classification of isolates as high- or low-biofilm phenotypes
Wilcoxon comparisons of PC1 and PC2 by biofilm category
Spearman correlations between normalized biofilm formation and PCA scores
PERMANOVA evaluating associations between multivariate growth phenotype and biofilm category
Biofilm Comparison Across Media

Normalized biofilm formation is compared among LB, M9, and AUM using paired analyses.

The analysis includes:

Median and interquartile range summaries
Friedman test across media
Paired Wilcoxon signed-rank tests
Holm adjustment for multiple comparisons
Violin, boxplot, and individual-observation visualization
Bayesian Mixture Modeling of Biofilm Phenotypes

Biofilm distributions within each medium are evaluated using mixture modeling.

The workflow includes two complementary approaches.

BIC-based mixture evaluation

The mclust package is used to evaluate the number and structure of Gaussian mixture components supported by the data using the Bayesian Information Criterion (BIC).

Bayesian mixture modeling

Two-component Gaussian mixture models are fitted using brms. Posterior distributions are used to estimate the intersection between the two mixture components, providing a medium-specific cutoff separating lower and higher biofilm phenotypes.

Outputs include:

Component means
Component standard deviations
Mixing proportions
Posterior cutoff estimates
95% credible intervals
Density plots showing the estimated cutoff
Growthcurver Summary Statistics

Growthcurver parameters are summarized by medium as:

Median [Q1–Q3]

The summary workflow:

Reads the three medium-specific blocks by spreadsheet position
Excludes control strains from the bulk clinical-isolate analysis
Calculates median and interquartile range for each Growthcurver parameter
Exports formatted results to Excel, Word, and LaTeX

Control strains excluded from this bulk summary are:

CFT073
11775
25922
Input Data

The scripts were developed around Excel workbooks containing growth and biofilm measurements.

Principal input files include:

$Data Paper 1 - Growth Avgs Across Media.xlsx
$Data Paper 1 - Normalized Biofilm Across Media.xlsx
Binary Biofilm.xlsx
#Combined Growthcurver Data 0-10 for JMP Upload.xlsx

Growth data are organized into separate worksheets for:

LB
M9
AUM

Some scripts rely on the original spreadsheet layout, including the position of isolate identifiers, timepoints, plate identifiers, and Growthcurver parameter blocks. Users applying the code to differently formatted data will need to modify the relevant import and reshaping steps.

Growth Measurements

Growth curves were evaluated using OD600 measurements at:

0, 2, 4, 6, 8, 10, and 24 h

The primary Growthcurver analysis uses:

0, 2, 4, 6, 8, and 10 h

A separate analysis incorporates the 24-h measurement.

Blank wells designated:

Blank_1
Blank_2
Blank_3
Blank_4

are excluded where specified in the analysis scripts.

R Packages

The analyses use the following R packages:

readxl
dplyr
tidyr
tibble
stringr
ggplot2
growthcurver
vegan
rstatix
ggpubr
brms
posterior
mclust
patchwork
writexl
openxlsx
officer
flextable
knitr

Package requirements differ among scripts.

Statistical Analyses

Statistical procedures implemented in the repository include:

Friedman tests
Paired Wilcoxon signed-rank tests
Holm correction for multiple comparisons
Kruskal-Wallis tests
Spearman rank correlations
Principal component analysis
PERMANOVA
Tests of multivariate dispersion
Gaussian mixture modeling
Bayesian Gaussian mixture modeling
Bayesian Information Criterion model comparison

Individual scripts contain the parameters and settings used for each analysis, including random seeds, permutation counts, MCMC iterations, warmup iterations, and model priors where applicable.

Reproducibility

Paths in the scripts may need to be modified to reflect the local location of the input data.

Files beginning with $ or # are literal filenames used during the original analysis and should be changed if the corresponding files are renamed.

Several scripts generate intermediate Excel files and publication figures. Output filenames are defined directly within each script.

For Bayesian analyses, model-fitting time will depend on the available hardware and the number of MCMC iterations specified in the script.

Figure Colors

Media are represented consistently throughout the analyses using:

Medium	Color	Hex
LB	Burnt orange	#CC5500
M9	Light purple	#C8A2C8
AUM	Teal	#008080

The standard display order is:

LB → M9 → AUM

Notes on Analysis Versions

Both 0–10 h and 0–24 h Growthcurver analyses are included intentionally.

The 0–10 h analysis excludes the 24-h measurement before Growthcurver fitting and PCA.

The 0–24 h analysis includes the 24-h measurement and provides a complementary analysis incorporating the late growth measurement.

These scripts should therefore be treated as distinct analysis workflows rather than interchangeable versions of the same script.

Citation

If using this repository in association with the accompanying publication, please cite the corresponding manuscript.

Citation:
[Add final manuscript citation and DOI upon publication]

Data Availability

The availability of the underlying isolate-level phenotype data should be described according to the data availability statement of the associated publication.

