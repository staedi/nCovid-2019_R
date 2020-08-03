# nCovid-2019_R

Workspace for nCovid-2019 Data Wrangling, Analysis and Visulization using R.
Primarily used with `Leaflet`, `geom_tile` for now.

Probably extended with Animation (`plotlyr`)

Primary objectives
* Overlay Infections and Deaths for **State / Country** on a map (`Leaflet`)
* Develop a heatmap of the daily increases of infections and deaths of top-N countries/states (`geom_tile`)
* Generate summary file using raw time-series from [Johns Hopkins University Github](https://github.com/CSSEGISandData/COVID-19){:target="_blank"} to facilitate state-level data for more countries
* Employ Facet/arrangeGrob for to combine infections/deaths in one graphics (`arrangeGrob` chosen)

Future Developments
* Different statistical methods (e.g., moving average)
* Employ more relevant data
  * [Apple mobility data](http://apple.com/covid19/mobility){:target="_blank"}: Walking, Transport, and Driving
  * [Google mobility data](https://www.google.com/covid19/mobility/){:target="_blank"}: Transporation and general trends
  * [Bureau of Labor Statistics](https://www.bls.gov/covid19/home.htm){:target="_blank"}: Consumer Pricing and Job market statistics
  * [Federal Reserve of St. Louis](https://research.stlouisfed.org/resources/covid-19/){:target="_blank"}: Overall economics statistics
* Employ animations for mapping (`shiny` with `Leaflet`)
* Animate heatmap (`plotlyr`)
* Host on web (`dash`)
