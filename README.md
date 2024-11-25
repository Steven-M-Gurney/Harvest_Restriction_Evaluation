# Harvest restrictions fail to influence population abundance

### [Steven M. Gurney](https://linktr.ee/gurneyst), [Sonja A. Christensen](http://www.christensen-lab.org/), [Melissa J. Nichols](), [Chad M. Stewart](), [Sarah L. Mayhew](), [Neil A. Gilbert](https://gilbertecology.com), [Dwayne R. Etter]()

### Data/code DOI: [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.14194669.svg)](https://doi.org/10.5281/zenodo.14194669)

#### Please contact the first author for questions about the code or data: Steven M. Gurney (steven.m.gurney@gmail.com)
__________________________________________________________________________________________________________________________________________

## Abstract

Evaluating changes in population abundance is essential to assess the efficacy of conservation actions. Antler point restrictions are a high-profile regulatory action aimed to advance male age structure in cervid populations, but there is a limited understanding on how restrictions affect population size and structure. Our study evaluated population-level effects of an antler point restriction on white-tailed deer (*Odocoileus virginianus*) abundance and sex-and-age composition using a before-after control-impact design. Antler point restrictions are intended to increase the abundance of legal-antlered deer, but the impact on the abundance of antlerless deer is less known. We conducted camera-trap surveys of deer in zones with and without antler point restrictions before and three years after the implementation of restrictions and used N-mixture models to estimate annual abundance by sex-and-age class. Our results suggest that the restrictions did not influence population abundance of deer. Abundance of legal-antlered deer increased in both the restriction zone and non-restriction zone (opposing prediction); and female, sublegal antlered, and fawn abundance increased in both the restriction zone and non-restriction zone (opposing prediction). Partial controllability, or a failure of the regulation to influence realized harvest, likely explains these results, since antlerless deer harvest did not change throughout the duration of our study while slight but insignificant change in antlered harvest was observed in the restriction zone. Our results highlight potential limitations of antler point restrictions achieving desired effects for population reduction goals and the importance of independently evaluating conservation and management actions.

## Repository Directory

### [Code](./Code): Contains code for preparing study data and running study model.
*  [A](./Code/) - .
*  [B](./Code/) - .
*  [C](./Code/) - .

### [Data](./Data): Contains data for study.
*  [Covariate_Tables](./Data/Covariate_Tables) - Folder with annual covatiate tables (2019 - 2022). These tables contain the following columns:
    | Variable name | Meaning |
    |---------------|---------|
    | SiteID | Unique name of sampling site |
    | ClosedCover | Binary indicator for dominant land cover, open (1) or closed (0) |
    | AgCover | Percent agricultural cover |
   * [CovariateTable_2019.csv](./Data/Covariate_Tables/CovariateTable_2019.csv) - 2019 covariate table.
   * [CovariateTable_2020.csv](./Data/Covariate_Tables/CovariateTable_2020.csv) - 2020 covariate table.
   * [CovariateTable_2021.csv](./Data/Covariate_Tables/CovariateTable_2021.csv) - 2021 covariate table.
   * [CovariateTable_2022.csv](./Data/Covariate_Tables/CovariateTable_2022.csv) - 2022 covariate table.

### [Figures](./Figures): Contains figures, and code to create them.
*  [Code_For_Figures](./Figures/Code_For_Figures) - Folder with scripts to create figures.
   * [Figure_02.R](./Figures/Code_For_Figures/Figure_02.R) - Create Figure 2 (marginal effects).
   * [Figure_03.R](./Figures/Code_For_Figures/Figure_03.R) - Create Figure 3 (differences in abundance).
   * [Figure_04.R](./Figures/Code_For_Figures/Figure_04.R) - Create Figure 4 (harvest estimates).
*  [Figure_01.tiff](Figures/Figure_01.tiff) - Figure 1. Study area (created in ArcGIS Pro).
*  [Figure_02.pptx](Figures/Figure_02.pptx) - Figure 2. Marginal effects of year by harvest treatment (PPTX file for annotation).
*  [Figure_02.tiff](Figures/Figure_02.tiff) - Figure 2. Marginal effects of year by harvest treatment.
*  [Figure_03.pptx](Figures/Figure_03.pptx) - Figure 3. Differences in abundance between 2022 and 2019 (PPTX file for annotation).
*  [Figure_03.tiff](Figures/Figure_03.tiff) - Figure 3. Differences in abundance between 2022 and 2019.
*  [Figure_04.pptx](Figures/Figure_04.pptx) - Figure 4. MDNR estimated deer harvest (PPTX file for annotation).
*  [Figure_04.tiff](Figures/Figure_04.tiff) - Figure 4. MDNR estimated deer harvest.

### [Results](./Results): Contains results files.
*  [Differences](./Results/Differences) - Folder with results from the interpretation model for difference in abundances (2022 - 2019; by sex-and-age class).
*  [Marginal_Effects](./Results/Marginal_Effects) - Folder with results from the interpretation model for marginal effects (by sex-and-class).
*  [NMixture_Model](./Results/NMixture_Model) - Folder with N-mixture model output for each sex-and-age class. Each of these .RData files contains 4 objects:
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
  
