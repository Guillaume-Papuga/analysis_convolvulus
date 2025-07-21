# Genetic and Ecological Analysis of *Convolvulus lanuginosus*

## Joint analysis of present–LGM distribution, phylogeography and micro-ecology

**Author:** Guillaume Papuga, Eric Imbert, Bouchaib Khadari  
**Last update:** July 2025

---

## Project description

This repository contains the full R code and supporting material for the genetic and ecological analysis of *Convolvulus lanuginosus*, a Mediterranean coastal plant species with a fragmented distribution. The project aims to integrate population genetics, species distribution modelling (SDM), and field-based ecological characterization to understand the evolutionary history, environmental specialization, and conservation relevance of populations across the species’ range.

This work supports a scientific article currently in preparation.

---

## Folder structure and script overview

This GitHub repository includes R scripts and R Markdown documents organized according to the main steps of the analysis workflow:

### Setup and data import
- `00.run_all.R` — Master script to run the full pipeline  
- `01.package_upload.R` — Load required R packages  
- `02.import_data_presence.R` — Import species presence data  
- `03.import_spatial_data.R` — Load environmental spatial layers  
- `04.convert_data_presence.R` — Format presence data for analysis  
- `05.import_data_microecology.R` — Import microecological field data  
- `06.map_for_methods.R` — Generate a map of sampled populations

### Species distribution modelling (SDM)
- `10.data_pseudoabsence.R` — Generate pseudo-absence data  
- `11.data_variable_selection.R` — Select relevant environmental variables  
- `20.single_model_SDM.R` — Fit individual SDM models  
- `21.model_averaging_sdm.R` — Ensemble modelling and projections

### Micro-ecological and genetic analyses
- `30.analyse.convolvulus.microniche.Rmd` — Micro-niche comparison of populations  
- `40.analyse.convolvulus.genetic.Rmd` — Main genetic structure and diversity analysis  
- `41.genetic_analysis.R` — Additional analysis of genetic differentiation

### Output, results and figures
- `90.results.Rmd` — Synthesis of results (text + figures)  
- `91.appendix.Rmd` / `91.appendix.html` — Supplementary material  
- `99.figures_convol.Rmd` — Figure generation script

### Project file
- `analysis_convolvulus.Rproj` — RStudio project file

---

## Citation

This repository supports a scientific article currently under preparation:

> Papuga G., Imbert E., Khadari B. (in prep). *Genetic and ecological differentiation of the Mediterranean coastal plant Convolvulus lanuginosus: implications for conservation under habitat fragmentation*.

Please contact the author before using or reproducing these materials.

---

## Contact

For questions or collaborations, please contact:  
Guillaume Papuga 

---

## License

This repository is shared for academic and non-commercial use only.  
For reuse, please cite appropriately and contact the author.