# Assignment 4: 48-Hour Chart Hackathon

## Team Members
- Insang Lee
- Minyeong Yoon

## Project Overview
This repository contains our team submission for EPPS 6356 Assignment 4: 48-Hour Chart Hackathon.

Using the 2025 Happy Planet Index (HPI) data, we created four visualizations in R:

1. Variable-width column chart
2. Table with embedded charts
3. Bar chart comparing the Top 20 and Bottom 20 countries by HPI
4. Column chart comparing mean HPI across continents

The charts were created in R and rendered as a Quarto HTML page.

## Data
We used the Happy Planet Index public dataset covering 2006–2025.

Data file:  
`Happy-Planet-Index-2006-2025-public-data-set.xlsx`

Source:  
Happy Planet Index  
https://happyplanetindex.org/countries/

For this assignment, we filtered the dataset to observations from 2025.

## Repository Files
- `assign04.qmd` — Quarto source file containing the R code and four charts
- `assign04.html` — rendered assignment page
- `Happy-Planet-Index-2006-2025-public-data-set.xlsx` — HPI dataset
- `prompts.md` — AI prompt and revision documentation
- `synergyreport.md` — team synergy report
- `sessionInfo.txt` — session information for both team members, including R version, platform, locale, time zone, and loaded packages
- `README.md` — project documentation

## R Packages
The project uses the following R packages:

- `readxl`
- `tidyverse`
- `patchwork`
- `dplyr`
- `ggplot2`
- `gt`
- `gtExtras`
- `svglite`

## Reproducibility
To reproduce the charts:

1. Download or clone the repository.
2. Keep the HPI Excel file in the same folder as `assign04.qmd`.
3. Open `assign04.qmd` in RStudio.
4. Make sure the required R packages are installed.
5. Restart R to begin with a clean R session.
6. Render `assign04.qmd`.

Rendering `assign04.qmd` from a clean R session should reproduce all four charts using only the files included in the repository.

The `sessionInfo.txt` file records the R environments used by both team members.

## AI Use
AI tools were used to assist with R coding, troubleshooting, and visualization design. Full prompts and revisions are documented in `prompts.md`.