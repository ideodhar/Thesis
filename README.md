---
# Assessing the Poverty Alleviating Effects of MSMEs in India

This repository contains the scripts, data, and documentation for my undergraduate thesis, which investigates the role of Micro, Small, and Medium Enterprises (MSMEs) in alleviating poverty across Indian states. The research employs econometric methods to analyze the relationship between MSME growth and poverty reduction.

## Repository Structure

- **R Scripts**:
  - `asi_clean.R`: Prepares and cleans data from the Annual Survey of Industries (ASI).
  - `asi_heat.R`, `heatmap.R`: Generate heatmaps to visualize MSME and poverty metrics.
  - `msme_count.R`, `poverty_calc.R`: R scripts for MSME and poverty computation.
  -  `results.R`: Econometric models.
- **LaTeX Files**:
  - `sl_bsc_thesis.tex`: LaTeX file for for thesis slideshow.
  - `ref.bib`: Bibliography file for citations.
- **Data Files**:
  - `asi_msme_data_f.csv`: # of MSMEs from ASI.
  - `controls.txt`, `demography_controls.csv`: Contains data on control variables.
  - `poverty_state.csv`, `state_mpce.csv`: Data on state-level poverty metrics.
- **Outputs**:
  - `msme_plot.pdf`, `poverty_plot.pdf`: Poverty and MSME heatmaps.
- **README.md**: Overview of the repository.

## Key Highlights

- **Focus**: Analyzes the impact of MSMEs on poverty alleviation, with an emphasis on differences between Micro, Small, and Medium enterprises.
- **Datasets Used**:
  - ASI data for MSME statistics.
  - CMIE’s CPHS data for poverty headcount calculations.
  - States of India for Controls.
  - Census tech. projection for demographic controls.
- **Methodologies**:
  - Fixed-effects regression models with diagnostic tests for robust analysis.
  - White's Robust standard errors.

## Research Questions

1. What is the impact of MSMEs on poverty headcount ratios in India?
2. How do the effects vary across enterprise categories (Micro, Small, Medium)?
3. Do grouped MSME categories (Small-Medium vs. Micro-Small) show distinct patterns?

## Results

- MSMEs, particularly Small and Medium enterprises, significantly alleviate poverty.
- Micro enterprises show a complex relationship, potentially due to limited resources and productivity.

## Tools & Technologies

- **R**: Data analysis and visualization.
- **LaTeX**: Documentation and thesis formatting.
- **Statistical Models**: Fixed-effects regression with robust standard errors.

## Future Scope

- Inclusion of service-sector MSMEs in the analysis.
- Use of multidimensional poverty indices for a broader perspective.
- Investigating the role of MSME productivity and wages in poverty reduction.

## Acknowledgments

This thesis was completed under the guidance of Dr. Balu Pawde at the Gokhale Institute of Politics and Economics. I am grateful for their support and mentorship throughout this research.

## License

This repository is available under the [MIT License](LICENSE).

---
