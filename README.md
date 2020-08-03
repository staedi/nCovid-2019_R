# nCovid-2019_R

Workspace for nCovid-2019 Data Wrangling, Analysis and Visulization using R.
Primarily used with `Leaflet`, `geom_tile` for now.

Probably extended with Animation (`plotlyr`)

Primary objectives
* Overlay Infections and Deaths for **State / Country** on a map (`Leaflet`)
* Develop a heatmap of the time-series changing status of top-N areas (`geom_tile`)
* Generate summary file from Johns Hopkins University Github to facilitate state-level data for more countries
* Employ Facet/arrangeGrob for to combine infections/deaths in one graphics (`arrangeGrob` chosen)

Future Developments (*animations*)
* Employ animations for mapping (`shiny` with `Leaflet`)
* Animate heatmap (`plotlyr`)
* Host on web (`dash`)
