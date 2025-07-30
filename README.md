# A parametric evaluation of the landscape external costs of wind and solar power plants in Italy
R scripts used to prepare the manuscript "A parametric evaluation of the landscape external costs of wind and solar power plants in Italy"

## Abstract

This article develops and applies a methodology for the parametric evaluation of the landscape external costs
associated with renewable energy installations, using two case studies. The primary innovation of the methodology
lies in the estimation of the economic value of landscape-related ecosystem services in Italy as a function of key
landscape attributes, enabling its application across diverse national landscape contexts. The monetary valuation of
landscape services was conducted through a discrete choice experiment (DCE) based on a survey administered to a 
representative sample of the Italian population (N = 2,352). The DCE results enabled the estimation of 50 unit values 
(in euros per hectare) reflecting the population’s willingness to pay for landscape ecosystem services. 
These values were differentiated according to qualitative classes of three landscape attributes: 
naturalistic quality, cultural heritage richness, and the presence of historic rural areas. 
A distinctive feature of the discrete choice experiment is its integration with the available national 
landscape cartography. This allows for the application of landscape monetary values to specific areas affected by 
visual intrusion from renewable energy installations, using spatial analysis tools. The methodology was applied to 
two case studies in Central Italy: an onshore wind farm and a utility-scale photovoltaic plant. Given the differing 
landscape contexts of the two case studies, the estimated external landscape cost of the photovoltaic plant was lower 
than that of the wind farm, both in absolute terms (106,000 euro/year vs. 232,000 euro/year) and relative to annual 
energy production (1.11 euro/MWh vs. 1.34 euro /MWh). In the context of energy planning and policy, the proposed 
methodology can be used to identify eligible areas for wind and solar power plants with an approach consistent with 
welfare economics.

## Getting Started

### GIT LFS Setup

**IMPORTANT: This repository uses Git Large File Storage (LFS) for handling large data files. You must have Git LFS installed on your system before cloning this repository.**

1.  **Install Git LFS:**
    If you don't have it, install Git LFS on your system.

    * **macOS:** `brew install git-lfs`
    * **Windows:** Download from [https://git-lfs.github.com/](https://git-lfs.github.com/)
    * **Linux (Debian/Ubuntu):** `sudo apt-get install git-lfs`

2.  **Set up Git LFS:**
    Run this command once to set up Git LFS for your user account:
    `git lfs install`

3.  **Clone the repository:**
    After installing Git LFS, you can clone the repository as you normally would. Git LFS will automatically download the large files for you.

    `git clone https://github.com/giuliomela/parametric_evaluation_landscape_external_costs.git`
    
4. If git clone does not work, try a more explicit command:

    ´git lfs clone https://github.com/giuliomela/parametric_evaluation_landscape_external_costs.git´
    
Once the repository has been cloned, large files will be donwloaded locally and the R script can be run.

### R packages

The R scripts use a certain number of R packages to run. Please run the following code to install them:

```{r}

# install_packages.R
packages_to_install <- c("tidyverse", "here", "sf", "readxl", "viridis", "knitr", "networkD3",  "edc", "rnaturalearth",
"ggspatial")


# Check and install packages
for (package in packages_to_install) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
}

```

## Directory structure

- A `visibility anaysis` folder, containing visibility analysis data (these data are stored in GIT LFS)
- A `traditional_landscapes` folder containing *.kml and *.kmz files of the traditional rural landscapes
- Two folders containing ISPRA's Nature and Landscape map files (`carta_natura` and `carta_paesaggio`)
- Two spreadsheet files containing Italian-English correspondance tables (`corr_table_it_en.xlsx`) and the rural landscape values estimated
  with the discrete choice experiment (`landscape_values_24.xlsx`)
- Three R scripts:
  - `depreciation_functions_24.R`: contains core functions to estimate the extent to which the presence of artificial
    manufacts (i.e. wind and solar plants) affect landscape values.
  - `script_results.R`: computes the main results
  - `script_plots.R`: genrates plots
- A `plot_generator.qmd` file that generates a document with all the plots with the same size and resolution used in the paper.
  
## Running the analysis

The R scripts must be run in this order:

1. `script_results.R` computes the main results by sourcing functions from `depreciation_functions_24.R`
2. `script_plots.R` generates the plots relying on the results computed by `script_results.R`
3. (Optional): The `plot_generator.qmd` can be rendered to obtain plots of the same size/resolution used in the paper

Alternatively, it is possible to run 

