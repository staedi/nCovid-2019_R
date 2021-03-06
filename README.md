# nCovid-2019_R

Workspace for nCovid-2019 Data Wrangling, Analysis and Visulization using R.
Primarily used with ~~`Leaflet`~~, `geom_tile`.

## Primary objectives
* ~~Overlay Infections and Deaths for **State / Country** on a map (`Leaflet`)~~
* Develop a heatmap of the daily increases of infections and deaths of top-N countries/states (`geom_tile`)
* Generate summary file using raw time-series from [Johns Hopkins University Github](https://github.com/CSSEGISandData/COVID-19) to facilitate state-level data for more countries
* Interface daily South Korea data from [KCDC](http://ncov.mohw.go.kr/) to facilitate province-level data
* CSV files are now being combined into a summary data file (Statistics including per 100K populations, daily changes among other things)
* Employ Facet/arrangeGrob for to combine infections/deaths in one graphics (`arrangeGrob` chosen)
* Facilitate options to plot either daily increases or that per 100K populations

## Future Developments
* Different statistical methods (e.g., moving average, *per 100K populations*)
* Employ more relevant data
  * ~~[Apple mobility data](http://apple.com/covid19/mobility): Walking, Transport, and Driving~~
  * ~~[Google mobility data](https://www.google.com/covid19/mobility/): Transporation and general trends~~
  * [Bureau of Labor Statistics](https://www.bls.gov/covid19/home.htm): International Labor Statistics
  * [Federal Reserve of St. Louis](https://research.stlouisfed.org/resources/covid-19/): Overall economics statistics
* ~~Employ animations for mapping (`shiny` with `Leaflet`)~~
* Interactive Dashboard (`Streamlit`) **done** in [separate repository](https://github.com/staedi/Streamlit_nCov19)
