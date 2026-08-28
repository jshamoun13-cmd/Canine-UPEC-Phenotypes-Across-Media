Canine UPEC Growth and Biofilm Phenotyping Analysis

```text
Overview

This repository contains the R code used for analysis and visualization of growth kinetics and biofilm formation in a collection of canine uropathogenic Escherichia coli (UPEC) isolates evaluated under different in vitro growth conditions.

Bacterial phenotypes were evaluated across three culture media:

LB — Luria-Bertani broth
M9 — M9 minimal medium
AUM — artificial urine medium

The repository contains the analysis workflows used to evaluate:

Growth-curve trajectories
Terminal growth across media
Growthcurver-derived growth parameters
Principal component analysis (PCA) of multivariate growth phenotypes
Biofilm formation across media
Bayesian mixture modeling of biofilm distributions
Associations between growth and biofilm phenotypes
Summary statistics used in manuscript tables
Publication figures and supplementary figures
Repository Contents
Main Figures
Figure 2a Terminal Growth Across Media.R

Compares terminal OD600 measurements among LB, M9, and AUM.

The script evaluates growth at:

10 h
24 h

Analyses include:

Exclusion of blank wells
Median and interquartile range summaries
Friedman tests across media
Paired Wilcoxon signed-rank tests
Holm correction for multiple comparisons
Violin, boxplot, and individual-observation visualization
Figure 2b Growthcurver Metrics 10h by Medium.R

Performs the primary Growthcurver analysis using OD600 measurements from 0–10 h.

Measurements included in model fitting are:

0, 2, 4, 6, 8, and 10 h

The 24-h measurement is excluded from this analysis.

The script:

Reshapes growth data for each medium
Excludes blank wells
Fits growth curves using the growthcurver package
Extracts Growthcurver parameters
Performs medium-specific PCA
Performs global PCA across media
Tests differences in multivariate growth phenotype among media using PERMANOVA
Evaluates multivariate dispersion
Tests PC1 and PC2 differences among media using Kruskal-Wallis tests
Generates PCA figures
Exports Growthcurver results, PCA scores, and PCA loadings

Growthcurver parameters include:

Parameter	Description
k	Carrying capacity
n0	Initial population size
r	Intrinsic growth rate
t_mid	Time at the midpoint of the fitted growth curve
t_gen	Generation time
auc_l	Logistic area under the curve
auc_e	Empirical area under the curve
sigma	Residual error from model fitting
Figure 3a Normalized Biofilm by Medium.R

Compares normalized biofilm formation among LB, M9, and AUM.

Analyses include:

Median and interquartile range summaries
Friedman test across media
Paired Wilcoxon signed-rank tests
Holm adjustment for multiple comparisons
Violin plots
Boxplots
Individual isolate observations
Mean indicators
Figure 3b BMM with Plotting by Medium.R

Evaluates the distribution of normalized biofilm formation within each growth medium using complementary frequentist and Bayesian mixture-modeling approaches.

BIC-based mixture evaluation

The mclust package is used to evaluate Gaussian mixture models and determine the number and structure of mixture components supported by the data using the Bayesian Information Criterion (BIC).

The analysis evaluates candidate models containing multiple possible component numbers and reports:

Selected number of components
Model structure
Component means
Component variances
Mixing proportions
Classification of observations into mixture components
Bayesian mixture modeling

Two-component Gaussian mixture models are fitted using brms.

Posterior distributions are used to estimate the intersection between the lower and higher mixture components, providing a medium-specific cutoff used to distinguish lower and higher biofilm phenotypes.

Outputs include:

Posterior component means
Posterior component standard deviations
Mixing proportions
Posterior cutoff estimates
Credible intervals
Density plots displaying medium-specific mixture cutoffs

Models are fitted separately for LB, M9, and AUM.

Figure 4 Biofilm vs Growth.R

Integrates Growthcurver-derived growth phenotypes with normalized and binary biofilm measurements.

The primary growth analysis uses Growthcurver parameters derived from the 0–10 h growth measurements.

Analyses include:

Global PCA of Growthcurver parameters
Integration of PCA scores with biofilm measurements
Classification of isolates into lower- and higher-biofilm phenotypes
Overlay of binary biofilm phenotype on PCA scores
Comparison of PC1 and PC2 scores by biofilm category
Spearman rank correlations between normalized biofilm formation and PCA scores
PERMANOVA evaluating associations between multivariate growth phenotype and biofilm category
Generation of publication figures summarizing growth–biofilm relationships
Supplementary Figures
Supp Figure 1 Growthcurver Metrics at 24h by Medium.R

Performs a complementary Growthcurver analysis using the complete set of OD600 measurements:

0, 2, 4, 6, 8, 10, and 24 h

This workflow is maintained separately from the primary 0–10 h analysis so that the effect of including the late growth measurement can be evaluated.

Analyses include:

Growthcurver model fitting through 24 h
Medium-specific PCA
Global PCA across media
PERMANOVA
Multivariate dispersion testing
Kruskal-Wallis testing of PC1 and PC2 scores
Export of Growthcurver results
Export of PCA scores and loadings
Generation of supplementary PCA figures

The 0–10 h and 0–24 h workflows should be considered distinct analyses rather than interchangeable versions of the same analysis.

Supp Figure 2 Combined Growth Curves.R

Generates overlapping growth curves for individual isolates in LB, M9, and AUM using measurements from 0–10 h.

The workflow includes:

Import and reshaping of medium-specific growth data
Exclusion of blank wells
Individual-isolate growth trajectories
Raw OD600 visualization
log10-transformed OD600 visualization
Medium-specific plotting
Export of processed plotting data
PNG output
Publication-resolution TIFF output

Nonpositive OD600 values are excluded from log10-transformed plots.

Manuscript Tables
Table 2 Growthcurver Summary Data 10h.R

Generates summary statistics for Growthcurver parameters derived from the primary 0–10 h analysis.

Each parameter is summarized by medium as:

Median [Q1–Q3]

The workflow:

Reads the three medium-specific Growthcurver data blocks by spreadsheet position
Identifies isolate IDs within each medium-specific block
Excludes designated control strains from the bulk clinical-isolate analysis
Calculates the median and interquartile range for each Growthcurver parameter
Reports the number of retained isolates
Performs a check to confirm control exclusion
Exports publication-ready tables

Control strains excluded from this bulk summary are:

CFT073
11775
25922

Output formats include:

Microsoft Excel
Microsoft Word
LaTeX
Input Data

The scripts were developed around Excel workbooks containing growth and biofilm measurements.

Principal input files include:

$Data Paper 1 - Growth Avgs Across Media.xlsx
$Data Paper 1 - Normalized Biofilm Across Media.xlsx
Binary Biofilm.xlsx
#Combined Growthcurver Data 0-10 for JMP Upload.xlsx
Growth Data

Growth data are organized into separate worksheets for:

LB
M9
AUM

Growth curves were evaluated using OD600 measurements at:

0, 2, 4, 6, 8, 10, and 24 h

The primary Growthcurver analysis uses:

0, 2, 4, 6, 8, and 10 h

A separate supplementary analysis incorporates the 24-h measurement.

Blank wells designated:

Blank_1
Blank_2
Blank_3
Blank_4

are excluded where specified within the analysis scripts.

Biofilm Data

Normalized biofilm measurements are organized by isolate and medium, with separate measurements for:

LB
M9
AUM

Binary biofilm classifications used in growth–biofilm analyses are supplied separately where required.

Data Structure

Several scripts rely on the structure of the original Excel workbooks, including the position or names of:

Isolate identifiers
Timepoints
Plate identifiers
Medium-specific worksheets
Growthcurver parameter blocks
Normalized biofilm columns

Users applying these scripts to differently formatted datasets will need to modify the corresponding import and reshaping steps.

Statistical Analyses

Statistical procedures implemented across the repository include:

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

Individual scripts contain the exact parameters used for each analysis, including, where applicable:

Random seeds
Number of permutations
MCMC chains
MCMC iterations
Warmup iterations
Priors
Model-fitting controls
R Packages

Analyses in this repository use packages including:

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

Package requirements vary among scripts.

Figure Conventions

Media are represented consistently throughout the analyses using the following colors:

Medium	Color	Hex
LB	Burnt orange	#CC5500
M9	Light purple	#C8A2C8
AUM	Teal	#008080

The standard display order is:

LB → M9 → AUM

Analysis Versions

Both 0–10 h and 0–24 h Growthcurver analyses are included intentionally.

Primary analysis: 0–10 h

Uses:

0, 2, 4, 6, 8, and 10 h

The 24-h measurement is excluded before Growthcurver model fitting and PCA.

Supplementary analysis: 0–24 h

Uses:

0, 2, 4, 6, 8, 10, and 24 h

This provides a complementary analysis incorporating the late growth measurement.

Because inclusion of the 24-h measurement changes the data used for nonlinear growth-curve fitting, these workflows should be treated as distinct analyses.

Reproducibility

The scripts are designed to be run with the required input files available in the working directory unless otherwise specified.

Several scripts create output directories automatically, while others write output directly to the current working directory.

Files beginning with $ or # are literal filenames used in the original analysis and are not R syntax. If these files are renamed, the corresponding file_path definitions within the scripts must also be updated.

Several scripts generate intermediate Excel files in addition to publication figures. Output filenames and directories are specified within the individual scripts.

Bayesian mixture-model fitting can be computationally intensive. Runtime will depend on available hardware and the MCMC settings specified in the analysis script.

For complete reproducibility, the R version and installed package versions used for the final analyses should be recorded with:

sessionInfo()

and the output retained with the archived version of the repository.

Recommended Execution

The scripts correspond primarily to individual manuscript figures and tables and can generally be executed independently when their required input data are available.

The principal analysis workflow is:

Growth data
    │
    ├── Figure 2a: Terminal growth comparison
    │
    ├── Figure 2b: Growthcurver analysis, 0–10 h
    │       │
    │       └── PCA and multivariate growth analysis
    │
    └── Supplementary Figure 1: Growthcurver analysis, 0–24 h

Biofilm data
    │
    ├── Figure 3a: Biofilm comparison across media
    │
    └── Figure 3b: Mixture modeling and biofilm cutoffs

Growth + biofilm data
    │
    └── Figure 4: Growth–biofilm phenotype associations

Growthcurver output
    │
    └── Table 2: Median [IQR] summary statistics

Raw growth trajectories
    │
    └── Supplementary Figure 2: Individual growth curves

Code Availability

The R scripts in this repository correspond to analyses performed for the accompanying publication. The archived release associated with the final manuscript should be used when reproducing the published analyses.

Data Availability

Availability of the underlying isolate-level phenotype data is described in the data availability statement of the associated publication.

If data files are distributed separately from this repository, they should be placed in the appropriate working directory or the file_path definitions in the scripts should be updated accordingly.

Citation

If using this repository or its code in association with the accompanying publication, please cite the corresponding manuscript.

Citation:
[Add final manuscript citation and DOI upon publication]

A permanent repository DOI may also be added here after archival of the final GitHub release (for example, through Zenodo).

License

[Add repository license; e.g., MIT License]

Contact

Questions regarding the analysis or code should be directed to the corresponding author of the associated publication.
