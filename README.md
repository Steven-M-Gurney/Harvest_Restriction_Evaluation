# Harvest restrictions fail to influence population abundance

### [Steven M. Gurney](https://linktr.ee/gurneyst), [Sonja A. Christensen](http://www.christensen-lab.org/), [Melissa J. Nichols](), [Chad M. Stewart](), [David M. Williams](), [Sarah L. Mayhew](), [Neil A. Gilbert](https://gilbertecology.com), [Dwayne R. Etter]()

### Manuscript: [Published in Ecosphere](https://doi.org/10.1002/ecs2.70358)

### Data/code DOI: [![DOI](https://zenodo.org/badge/678437269.svg)](https://doi.org/10.5281/zenodo.15716493)

#### Please contact the first author for questions about the code or data: Steven M. Gurney (steven.m.gurney@gmail.com)
__________________________________________________________________________________________________________________________________________

## Abstract

Evaluating changes in population abundance is essential to assess the efficacy of conservation actions. Antler point restrictions are a high-profile regulatory action aimed to advance male age structure in cervid populations, but there is a limited understanding on how restrictions affect population size and structure. Our study evaluated population-level effects of an antler point restriction on white-tailed deer (*Odocoileus virginianus*) abundance and sex and age composition using a before-after control-impact design. Antler point restrictions are intended to increase the abundance of legal-antlered deer, but the impact on the abundance of antlerless deer is less known. By limiting the harvest of yearling males, antler point restrictions may lead hunters to shift harvest toward antlerless deer, potentially increasing female mortality and reducing population fecundity. We conducted camera-trap surveys of deer in zones with and without antler point restrictions before and three years after the implementation of restrictions and used N-mixture models to estimate annual abundance by sex and age class. The restriction prohibited the harvest of antlered deer with fewer than four points on a single antler beam (i.e., on one side). Our results suggest that the restrictions did not influence population abundance of deer. Abundance of legal-antlered deer increased in both the restriction zone and non-restriction zone (opposing predictions), as did the abundance of females, sublegal-antlered deer, and fawns (opposing predictions). Partial controllability, or a failure of the regulation to influence realized harvest, likely explains these results, since antlerless deer harvest did not change throughout the duration of our study while slight but insignificant change in antlered harvest was observed in the restriction zone. Our results highlight potential limitations of antler point restrictions achieving desired effects for population reduction goals and the importance of independently evaluating conservation and management actions.


## Repository Directory

### [Code](./Code): Contains code for preparing study data and running study model.
*  [Data_Prep](./Code/Data_Prep) - Folder with code to prepare study data for use in the N-mixture model.
   * [Data_Processing.R](./Code/Data_Prep/Data_Processing.R) - Code to process wrangled data.
   * [Data_Wrangling.R](./Code/Data_Prep/Data_Wrangling.R) - Code to wrangle study data.
* [N_Mixture_Model_Supplement.R](./Code/N_Mixture_Model_Supplement.R) - Code to fit alternative model with year as a factor rather than linear effect
* [N_Mixture_Model.R](./Code/N_Mixture_Model.R) - Code to fit N-mixture models (main text)

### [Data](./Data): Contains data for study.
*  [Count_Data](./Data/Count_Data) - Folder with verified count data by sex-and-age class and year (2019 - 2022; CSV files). These detection histories include a column for LocationName (unique name of sampling site, synonymous with SiteID) and columns associated with 9 seperate weekly counts.
   * [Counts_Fawns_2019.csv](./Data/Count_Data/Counts_Fawns_2019.csv) - Fawn count data for 2019.
   * [Counts_Fawns_2020.csv](./Data/Count_Data/Counts_Fawns_2020.csv) - Fawn count data for 2020.
   * [Counts_Fawns_2021.csv](./Data/Count_Data/Counts_Fawns_2021.csv) - Fawn count data for 2021.
   * [Counts_Fawns_2022.csv](./Data/Count_Data/Counts_Fawns_2022.csv) - Fawn count data for 2022.
   * [Counts_Females_2019.csv](./Data/Count_Data/Counts_Females_2019.csv) - Female count data for 2019.
   * [Counts_Females_2020.csv](./Data/Count_Data/Counts_Females_2020.csv) - Female count data for 2020.
   * [Counts_Females_2021.csv](./Data/Count_Data/Counts_Females_2021.csv) - Female count data for 2021.
   * [Counts_Females_2022.csv](./Data/Count_Data/Counts_Females_2022.csv) - Female count data for 2022.
   * [Counts_Legal_2019.csv](./Data/Count_Data/Counts_Legal_2019.csv) - Legal-antlered count data for 2019.
   * [Counts_Legal_2020.csv](./Data/Count_Data/Counts_Legal_2020.csv) - Legal-antlered count data for 2020.
   * [Counts_Legal_2021.csv](./Data/Count_Data/Counts_Legal_2021.csv) - Legal-antlered count data for 2021.
   * [Counts_Legal_2022.csv](./Data/Count_Data/Counts_Legal_2022.csv) - Legal-antlered count data for 2022.
   * [Counts_Sublegal_2019.csv](./Data/Count_Data/Counts_Sublegal_2019.csv) - Sublegal-antlered count data for 2019.
   * [Counts_Sublegal_2020.csv](./Data/Count_Data/Counts_Sublegal_2020.csv) - Sublegal-antlered count data for 2020.
   * [Counts_Sublegal_2021.csv](./Data/Count_Data/Counts_Sublegal_2021.csv) - Sublegal-antlered count data for 2021.
   * [Counts_Sublegal_2022.csv](./Data/Count_Data/Counts_Sublegal_2022.csv) - Sublegal-antlered count data for 2022.
*  [Covariate_Tables](./Data/Covariate_Tables) - Folder with annual covatiate tables (2019 - 2022; CSV files). These tables contain the following columns:
    | Variable name | Meaning |
    |---------------|---------|
    | SiteID | Unique name of sampling site |
    | ClosedCover | Binary indicator for dominant land cover, open (1) or closed (0) |
    | AgCover | Percent agricultural cover |
   * [CovariateTable_2019.csv](./Data/Covariate_Tables/CovariateTable_2019.csv) - 2019 covariate table.
   * [CovariateTable_2020.csv](./Data/Covariate_Tables/CovariateTable_2020.csv) - 2020 covariate table.
   * [CovariateTable_2021.csv](./Data/Covariate_Tables/CovariateTable_2021.csv) - 2021 covariate table.
   * [CovariateTable_2022.csv](./Data/Covariate_Tables/CovariateTable_2022.csv) - 2022 covariate table.

### [Figures](./Figures): Contains figures and code to create them.
*  [Code_For_Figures](./Figures/Code_For_Figures) - Folder with scripts and data to create figures.
   * [Figure_02.R](./Figures/Code_For_Figures/Figure_02.R) - Create Figure 2 (marginal effects).
   * [Figure_03.R](./Figures/Code_For_Figures/Figure_03.R) - Create Figure 3 (differences in abundance).
   * [Figure_04.R](./Figures/Code_For_Figures/Figure_04.R) - Create Figure 4 (harvest estimates).
   * [Figure_Supplement.R](./Figures/Code_For_Figures/Figure_Supplement.R) - Create supplemental figures visualizing results for model with year as a factor.
*  [Appendix_S2_Figure_S1.png](./Figures/Appendix_S2_Figure_S1.png) - Appendix S2: Figure S1. Marginal effects of year by harvest treatment (for supplemental model with year as a factor).
*  [Appendix_S2_Figure_S2.png](./Figures/Appendix_S2_Figure_S2.png) - Appendix S2: Figure S2. Differences in abundance between 2022 and 2019 (for supplemental model with year as a factor).
*  [Figure_01.tif](./Figures/Figure_01.tif) - Figure 1. Study area (created in ArcGIS Pro).
*  [Figure_02.tif](./Figures/Figure_02.tif) - Figure 2. Marginal effects of year by harvest treatment.
*  [Figure_03.tif](./Figures/Figure_03.tif) - Figure 3. Differences in abundance between 2022 and 2019.
*  [Figure_04.tif](./Figures/Figure_04.tif) - Figure 4. MDNR estimated deer harvest.
*  [Silhouette_Fawn.png](./Figures/Silhouette_Fawn.png) - Fawn silhouette for figure annotations.
*  [Silhouette_Sublegal.png](./Figures/Silhouette_Sublegal.png) - Sublegal antlered silhouette for figure annotations.
*  [Silhouette_Female.png](./Figures/Silhouette_Female.png) - Female deer silhouette for figure annotations.
*  [Silhouette_Legal.png](./Figures/Silhouette_Legal.png) - Legal antlered silhouette for figure annotations.

### [Results](./Results): Contains results files.
*  [Differences](./Results/Differences) - Folder with results from the interpretation model for difference in abundances (2022 - 2019; by sex-and-age class). These tables contain the following columns:
    | Variable name | Meaning |
    |---------------|---------|
    | trt | Harvest-treatment index (1 = Non-APR; 2 = APR)|
    | mean | Mean estimate of difference |
    | l95 | Lower credible interval of difference estimate |
    | u95 | Upper credible interval of difference estimate |
    | trt_name | Harvest-treatment acronym (Non=APR = no restrictions; APR = restrictions) |
   * [Results_Diff_Fawns.csv](./Results/Differences/Results_Diff_Fawns.csv) - Fawn difference results (CSV file).
   * [Results_Diff_Females.csv](./Results/Differences/Results_Diff_Females.csv) - Female difference results (CSV file).
   * [Results_Diff_Legal.csv](./Results/Differences/Results_Diff_Legal.csv) - Legal difference results (CSV file).
   * [Results_Diff_Sublegal.csv](./Results/Differences/Results_Diff_Sublegal.csv) - Sublegal difference results (CSV file).
*  [Marginal_Effects](./Results/Marginal_Effects) - Folder with results from the interpretation model for marginal effects (by sex-and-class). These tables contain the following columns:
    | Variable name | Meaning |
    |---------------|---------|
    | trt | Harvest-treatment index (1 = Non-APR; 2 = APR) |
    | year | Year of study (2019 - 2022) |
    | mean | Mean estimate of effect |
    | l95 | Lower credible interval of effect estimate |
    | u95 | Upper credible interval of effect estimate |
    | trt_name | Harvest-treatment acronym (NonAPR = no restrictions; APR = restrictions) |
   * [Results_ME_Fawns.csv](./Results/Marginal_Effects/Results_ME_Fawns.csv) - Fawn marginal-effects results (CSV file).
   * [Results_ME_Females.csv](./Results/Marginal_Effects/Results_ME_Females.csv) - Female marginal-effects results (CSV file).
   * [Results_ME_Legal.csv](./Results/Marginal_Effects/Results_ME_Legal.csv) - Legal marginal-effects results (CSV file).
   * [Results_ME_Sublegal.csv](./Results/Marginal_Effects/Results_ME_Sublegal.csv) - Sublegal marginal-effects results (CSV file).
*  [N_Mixture_Model](./Results/N_Mixture_Model) - Folder with N-mixture model output for each sex-and-age class. Each of these .RData files contains 4 objects:
   * **constants**. A list of constants used in Nimble model:
     | Variable name | Meaning |
     |---------------|---------|
     | nyear | Number of years |
     | nsite | Number of sampling sites in a given year |
     | nreps | Number of repeated surveys |
     | trt | Harvest-treatment index |
     | twn | Township-pairing index |
   * **data**. A list of data used in the Nimble model:
     | Variable name | Meaning |
     |---------------|---------|
     | y | Observed count of animals (counts) |
     | year | Standardized year of study (4 years total) |
     | ag | Standardized agriculture predictor of abundance |
     | cover | Binary indicator for dominant land cover, open (1) or closed (0) |
   * **out**. A list of the MCMC chains with the posterior samples for model parameters.
   * **code**. Code for the Nimble model.
*  [Supplemental_Model_Results](./Results/Supplemental_Model_Results) - Folder with results from supplemental models with year as a factor
    * [fawns_year_factor_no_itx.RData](./Results/Supplemental_Model_Results/fawns_year_factor_no_itx.RData) Fawn results. RData with the following objects:
        * **out** A list of the MCMC chains with the posterior samples for model parameters
        * **posterior** A table with parameter posterior and calculation of marginal effects
          | Variable | Meaning |
          |----------|---------|
          | iter | Model iteration |
          | trt | Treatment: 1 = Non-APR, 2 = APR |
          | year | Year (1:4) |
          | beta2 | Posterior samples for beta2 parameter |
          | beta3 | Posterior samples for beta3 parameter |
          | beta5 | Posterior samples for beta5 parameter |
          | beta6 | Posterior samples for beta6 parameter |
          | year.lab | Year label |
          | trt.lab | Treatment label |
          | ag | % Agriculture (mean value used to calculate marginal effects |
          | lambda | Expected abundance - marginal effect for each year/treatment combination at average level of ag |
          | class | Which sex-age class |
   * [females_year_factor_no_itx.RData](./Results/Supplemental_Model_Results/females_year_factor_no_itx.RData) Female results. Same contents as above.
   * [legal_year_factor_no_itx.RData](./Results/Supplemental_Model_Results/legal_year_factor_no_itx.RData) Legal antlered results. Same contents as above.
   * [sublegal_year_factor_no_itx.RData](./Results/Supplemental_Model_Results/sublegal_year_factor_no_itx.RData) Sublegal antlered results. Same contents as above.
*  [MDNR_Harvest_Estimates.csv](./Results/MDNR_Harvest_Estimates.csv) - MDNR annual harvest estimates by study area (i.e., Zone). This table contains the following columns:
    | Variable name | Meaning |
    |---------------|---------|
    | Year | Year of the fall deer harvest |
    | Zone | Study area harvest treatment (APR or Non-APR) |
    | Class | Aggregated sex-and-age class (antlered or antlerless) |
    | Harvest | Estimated number of deer harvested |
    | 95CL | 95% confidence limit of estimated deer harvest |
