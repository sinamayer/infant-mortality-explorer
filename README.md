# Does Women's Empowerment Save Lives? Infant Mortality Explorer

**Live app:** https://sinamayer.shinyapps.io/infant-mortality-explorer/

Are maternal empowerment indicators stronger predictors of infant mortality than GDP per capita? This project tests that "feminist hypothesis" with World Bank data for 153 countries and presents the results in an interactive R Shiny app.

*Final project for Global Data Analytics, Rutgers University (April 2026). Author: Sina-Marie Mayer.*

📄 **[Read the 2-page executive summary (PDF)](ExecutiveSummary.pdf)**

The hypothesis builds on research showing that maternal education predicts child survival better than household income (Caldwell 1986), that rising women's education accounts for about half of the global decline in child mortality since 1970 (Gakidou et al. 2010), and that low maternal empowerment is linked to higher infant mortality across 59 low- and middle-income countries (Doku et al. 2020).

<!-- Add a screenshot of the app here: upload screenshot.png and uncomment the next line -->
<!-- ![App screenshot](screenshot.png) -->

## Key findings

| Model | Predictors | Adj. R² |
|---|---|---|
| Baseline | GDP per capita | 0.265 |
| Full | GDP per capita + female literacy, adolescent fertility, female labor force participation + health expenditure, sanitation, urbanization | 0.650 |

- Once the empowerment variables are added, **GDP per capita is no longer statistically significant** (p = 0.93).
- **Female literacy** (more education, fewer infant deaths) and the **adolescent fertility rate** (more teenage pregnancies, more deaths) are the significant empowerment predictors (both p < 0.01).
- **Female labor force participation** is not significant once literacy and fertility are controlled for, suggesting education and reproductive autonomy matter more than workforce participation as such.
- Among the structural controls, only **basic sanitation access** is significant (p < 0.05).

The simulator is designed for donors, NGOs, governments and multilateral funders who want to compare candidate interventions in a country before committing resources.

## The app

- **About:** research question, key numbers and data notes.
- **Simulator:** filter by region, pick a country to load its actual values, then move the sliders to ask "what if this country had different characteristics?" The predicted infant mortality rate from the full model updates live, and an arrow on the scatter plot shows the country moving from its actual to its what-if position.
- **World map:** infant mortality by country, with the selected country's what-if prediction shown in place of its actual value.

## Data and methods

All indicators come from the **World Bank World Development Indicators**, pulled with the `WDI` R package:

| Variable | WDI code |
|---|---|
| Infant mortality (per 1,000 live births) | SP.DYN.IMRT.IN |
| GDP per capita, PPP | NY.GDP.PCAP.PP.KD |
| Female adult literacy (%) | SE.ADT.LITR.FE.ZS |
| Adolescent fertility rate | SP.ADO.TFRT |
| Female labor force participation (%) | SL.TLF.CACT.FE.ZS |
| Health expenditure per capita (USD) | SH.XPD.CHEX.PC.CD |
| Basic sanitation access (%) | SH.STA.BASS.ZS |
| Urban population (%) | SP.URB.TOTL.IN.ZS |

- For each country, the most recent non-missing value between 2015 and 2022 is used, because several indicators are only reported every few years.
- High-income countries that don't report female literacy (near-universal coverage) are assigned 99%, so the upper income range isn't dropped from the sample.
- Regional aggregates and rows with missing or implausible values are excluded.
- Two OLS models: GDP only, and GDP plus empowerment variables and controls (controls motivated by Caldwell 1986 and Vollmer et al. 2014).

**Limitations:** cross-sectional, so associations rather than causal effects; the literacy imputation for high-income countries is an assumption; linear predictions are capped at zero in the simulator.

## Repository structure

```
01_data_and_models.R   downloads WDI data, cleans it, fits both regression models
app.R                  Shiny app
clean_data.rds         cleaned dataset used by the app
ExecutiveSummary.pdf   2-page summary of the research question, methods and findings
```

## Run it yourself

```r
install.packages(c("tidyverse", "WDI", "shiny", "bslib", "plotly", "scales"))

source("01_data_and_models.R")   # optional: rebuilds clean_data.rds from the World Bank API
shiny::runApp()
```

Re-running the data script pulls the current WDI release, so numbers can shift slightly as the World Bank revises its data. The app computes its headline numbers from the data, so they stay consistent.

## References

- Caldwell, J. C. (1986). Routes to low mortality in poor countries. *Population and Development Review*, 12(2), 171–220.
- Doku, D. T., Bhutta, Z. A., & Neupane, S. (2020). Associations of women's empowerment with neonatal, infant and under-5 mortality in low- and middle-income countries. *BMJ Global Health*, 5(2), e001558.
- Gakidou, E., Cowling, K., Lozano, R., & Murray, C. J. L. (2010). Increased educational attainment and its effect on child mortality in 175 countries between 1970 and 2009. *The Lancet*, 376(9745), 959–974.
- World Bank. (2024). World Development Indicators. https://data.worldbank.org

## Author

**Sina-Marie Mayer** · [Portfolio](https://sinamayer.github.io) · [GitHub](https://github.com/sinamayer)
