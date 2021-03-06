setwd('/Users/minpark/Documents/nCovid-2019')
# source('/Users/minpark/Documents/nCovid-2019/preprocess_nCovid-2019.R')

library(dplyr)
library(ggplot2)
# library(leaflet)
# library(spdplyr)
# library(sf)
# library(gridExtra)
# library(sp)

filename <- 'time_series_covid19.csv'

# Only US data can be remotely downloaded
get_filename <- function(filename) {
  filename <- paste0('data/',filename)
  return (filename)
}

read_file <- function(filename) {
  # Update filename list (remote download)
  filename <- get_filename(filename)
  
  # Read datafile(s)
  covid <- readr::read_csv(filename)
  
  return (covid)
}

# allow_minus: Whether to allow negative changes of infections and deaths
clean_dataset <- function(covid, allow_minus=FALSE) {
  covid <- covid %>%
    dplyr::mutate(num_states = dplyr::n(),
                  Date = as.Date(Date,format='%m/%d/%y'))
  
  # Subsetting to positive increases when allow_minus == FALSE
  if (allow_minus == FALSE) {
    covid <- covid %>%
      dplyr::mutate(i_Confirmed = dplyr::if_else(i_Confirmed<0,0,i_Confirmed),
                    i_Deaths = dplyr::if_else(i_Deaths<0,0,i_Deaths),
                    iTot_Confirmed = dplyr::if_else(iTot_Confirmed<0,0,iTot_Confirmed),
                    iTot_Deaths = dplyr::if_else(iTot_Deaths<0,0,iTot_Deaths),
                    ri_Confirmed = dplyr::if_else(ri_Confirmed<0,0,ri_Confirmed),
                    ri_Deaths = dplyr::if_else(ri_Deaths<0,0,ri_Deaths),
                    riTot_Confirmed = dplyr::if_else(riTot_Confirmed<0,0,riTot_Confirmed),
                    riTot_Deaths = dplyr::if_else(riTot_Deaths<0,0,riTot_Deaths)
      )
  }
  
  # Clean Province/State Names
  covid_groupset <- covid %>%
    dplyr::filter(adm0_a3 %in% c('ITA','ESP')) %>%
    dplyr::group_by(adm0_a3,Date,`Country/Region`) %>%
    
    # 1) Italy / Spain
    dplyr::mutate(`Province/State` = if_else(grepl('P.A. ',`Province/State`),'Trentino-Alto Adige',`Province/State`),
                  `Province/State` = if_else((`Province/State` == 'Ceuta' | `Province/State` == 'Melilla'),'Ceuta y Melilla',`Province/State`)) %>%
    # Lat / Long
    dplyr::group_by(adm0_a3,Date,`Country/Region`,`Province/State`,Tot_Confirmed,Tot_Deaths,iTot_Confirmed,iTot_Deaths,riTot_Confirmed,riTot_Deaths,num_states) %>%
    dplyr::summarize(Confirmed = sum(Confirmed),
                     Deaths = sum(Deaths),
                     i_Confirmed = sum(i_Confirmed),
                     i_Deaths = sum(i_Deaths),
                     ri_Confirmed = sum(ri_Confirmed),
                     ri_Deaths = sum(ri_Deaths),
                     #  Lat = mean(Lat),
                     #  Long = mean(Long)
    ) %>%
    # unique() %>%
    dplyr::ungroup()
  
  covid <- covid %>%
    dplyr::filter(!adm0_a3 %in% c('ITA','ESP'))
  
  covid <- bind_rows(covid,covid_groupset)
  
  # 2) Russia / Ukraine
  covid <- covid %>%
    dplyr::mutate(`Province/State` = if_else(grepl('Jewish',`Province/State`),'Yevrey',`Province/State`),
                  `Province/State` = if_else(`Province/State`=='Moscow','Moscow City',`Province/State`),
                  `Province/State` = if_else(`Province/State`=='Moscow Oblast','Moskva',`Province/State`),
                  `Province/State` = if_else(`Province/State`=='Kiev','Kiev City',`Province/State`),
                  `Province/State` = stringr::str_replace(`Province/State`,' Oblast',''),
                  `Province/State` = stringr::str_replace(`Province/State`,' Republic',''),
                  `Province/State` = stringr::str_replace(`Province/State`,' Autonomous Okrug',''),
                  `Province/State` = gsub('\\*','',`Province/State`),
                  `Country/Region` = gsub('\\*','',`Country/Region`))
  
  return (covid)
  
}

# read_spdata <- function() {
#   countries_sf <<- sf::read_sf("/Users/minpark/Documents/nCovid-2019/geospatial/ne_50m_admin_0_countries.shp")
#   states_sf <<- sf::read_sf("/Users/minpark/Documents/nCovid-2019/geospatial/ne_50m_admin_1_states_provinces.shp")
#   gadm_chile_sf <- readRDS("/Users/minpark/Documents/nCovid-2019/geospatial/gadm36_CHL_1_sf.rds")
#   gadm_china_sf <- readRDS("/Users/minpark/Documents/nCovid-2019/geospatial/gadm36_CHN_1_sf.rds")
#   gadm_colombia_sf <- readRDS("/Users/minpark/Documents/nCovid-2019/geospatial/gadm36_COL_1_sf.rds")
#   gadm_germany_sf <- readRDS("/Users/minpark/Documents/nCovid-2019/geospatial/gadm36_DEU_1_sf.rds")
#   gadm_india_sf <- readRDS("/Users/minpark/Documents/nCovid-2019/geospatial/gadm36_IND_1_sf.rds")
#   gadm_italy_sf <- readRDS("/Users/minpark/Documents/nCovid-2019/geospatial/gadm36_ITA_1_sf.rds")
#   # gadm_japan_sf <- readRDS("/Users/minpark/Documents/nCovid-2019/geospatial/gadm36_JPN_1_sf.rds")
#   gadm_korea_sf <- readRDS("/Users/minpark/Documents/nCovid-2019/geospatial/gadm36_KOR_1_sf.rds")
#   gadm_mexico_sf <- readRDS("/Users/minpark/Documents/nCovid-2019/geospatial/gadm36_MEX_1_sf.rds")
#   # gadm_netherlands_sf <- readRDS("/Users/minpark/Documents/nCovid-2019/geospatial/gadm36_NLD_1_sf.rds")
#   # gadm_nigeria_sf <- readRDS("/Users/minpark/Documents/nCovid-2019/geospatial/gadm36_NGA_1_sf.rds")
#   gadm_pakistan_sf <- readRDS("/Users/minpark/Documents/nCovid-2019/geospatial/gadm36_PAK_1_sf.rds")
#   gadm_peru_sf <- readRDS("/Users/minpark/Documents/nCovid-2019/geospatial/gadm36_PER_1_sf.rds")
#   gadm_russia_sf <- readRDS("/Users/minpark/Documents/nCovid-2019/geospatial/gadm36_RUS_1_sf.rds")
#   gadm_spain_sf <- readRDS("/Users/minpark/Documents/nCovid-2019/geospatial/gadm36_ESP_1_sf.rds")
#   # gadm_sweden_sf <- readRDS("/Users/minpark/Documents/nCovid-2019/geospatial/gadm36_SWE_1_sf.rds")
#   gadm_ukraine_sf <- readRDS("/Users/minpark/Documents/nCovid-2019/geospatial/gadm36_UKR_1_sf.rds")
#   gadm_uk_sf <- readRDS("/Users/minpark/Documents/nCovid-2019/geospatial/gadm36_GBR_1_sf.rds")
# 
#   for (iter in seq_along(ls()[grepl('gadm_',ls())])) {
#     print(paste("Loading shapefile:",ls()[grepl('gadm_',ls())][iter]))
#     if (iter == 1) {
#       gadm_sf <<- eval(parse(text=ls()[grepl('gadm_',ls())][iter]))
#       }
#     else {
#       gadm_sf <<- bind_rows(gadm_sf,eval(parse(text=ls()[grepl('gadm_',ls())][iter])))
#       }
#     }
#   }
# 
# clean_spdata <- function(spframe) {
#   name = deparse(substitute(spframe))
#   if (grepl('countries',name)) {
#     spframe <- spframe %>%
#       dplyr::filter(POP_EST != 0 & ADMIN != 'Antarctica') %>%
#       dplyr::select(NAME_EN,ADM0_A3,geometry) %>%
#       dplyr::rename("Country/Region" = "NAME_EN",
#                     "adm0_a3" = "ADM0_A3") %>%
#       dplyr::mutate(adm0_a3 = dplyr::if_else(grepl('SDS',adm0_a3),'SSD',adm0_a3),merge_type = 'country')
#     }
# 
#   else if (grepl('states',name)) {
#     spframe <- spframe %>%
#       dplyr::select(admin,adm0_a3,name_en,geometry) %>%
#       dplyr::rename("Country/Region" = "admin",
#                     "Province/State" = "name_en") %>%
#       dplyr::mutate(`Province/State` = stringr::str_replace(`Province/State`,'\'',''),
#                     `Province/State` = stringr::str_replace(`Province/State`,'[áãäå]','a'),
#                     `Province/State` = stringr::str_replace(`Province/State`,'[é]','e'),
#                     `Province/State` = stringr::str_replace(`Province/State`,'[í]','i'),
#                     `Province/State` = stringr::str_replace(`Province/State`,'[óôōö]','o'),
#                     `Province/State` = stringr::str_replace(`Province/State`,'[ü]','u'),
#                     `Province/State` = stringr::str_replace(`Province/State`,'[ñ]','n'),
#                     `Province/State` = stringr::str_replace(`Province/State`,'[ñ]','N'),
#                     merge_type = 'state')
#     }
# 
#   else if (grepl('gadm',name)) {
#     spframe <- spframe %>%
#       dplyr::select(GID_0,NAME_0,NAME_1,geometry) %>%
#       dplyr::rename("adm0_a3" = "GID_0",
#                     "Country/Region" = "NAME_0",
#                     "Province/State" = "NAME_1") %>%
#       dplyr::mutate(`Province/State` = stringr::str_replace(`Province/State`,'\'',''),
#                     `Province/State` = stringr::str_replace(`Province/State`,'[áãäå]','a'),
#                     `Province/State` = stringr::str_replace(`Province/State`,'[é]','e'),
#                     `Province/State` = stringr::str_replace(`Province/State`,'[í]','i'),
#                     `Province/State` = stringr::str_replace(`Province/State`,'[óôōö]','o'),
#                     `Province/State` = stringr::str_replace(`Province/State`,'[ü]','u'),
#                     `Province/State` = stringr::str_replace(`Province/State`,'[ñ]','n'),
#                     `Province/State` = stringr::str_replace(`Province/State`,'[ñ]','N'),
#                     merge_type = 'gadm') %>%
# 
#       dplyr::mutate(`Province/State` = dplyr::if_else(grepl('Aisen',`Province/State`),'Aysen',`Province/State`),
#                     `Province/State` = dplyr::if_else(grepl('Bio-',`Province/State`),'Biobio',`Province/State`),
#                     `Province/State` = dplyr::if_else(grepl('OHiggins',`Province/State`),'OHiggins',`Province/State`),
#                     `Province/State` = dplyr::if_else(grepl('Magallanes',`Province/State`),'Magallanes',`Province/State`),
#                     `Province/State` = dplyr::if_else(grepl('uble',`Province/State`),'Nuble',`Province/State`),
#                     `Province/State` = dplyr::if_else(grepl('de Santiago',`Province/State`),'Metropolitana',`Province/State`),
#                     `Province/State` = dplyr::if_else(grepl('Mongol',`Province/State`),'Inner Mongolia',`Province/State`),
#                     `Province/State` = dplyr::if_else(grepl('Ningxia',`Province/State`),'Ningxia',`Province/State`),
#                     `Province/State` = dplyr::if_else(grepl('Xinjiang',`Province/State`),'Xinjiang',`Province/State`),
#                     `Province/State` = dplyr::if_else(grepl('Xizang',`Province/State`),'Tibet',`Province/State`),
#                     `Province/State` = dplyr::if_else(grepl('Delhi',`Province/State`),'NCT of Delhi',`Province/State`),
# 
#                     # Italy (P.A. Bolzano / P.A. Trento -> Trentino-Alto Adige)
#                     `Province/State` = dplyr::if_else(grepl('Apulia',`Province/State`),'Puglia',`Province/State`),
#                     # `Province/State` = dplyr::if_else(grepl('Trentino-',`Province/State`),'P.A. Trento',`Province/State`),
#                     `Province/State` = dplyr::if_else(grepl('Sicily',`Province/State`),'Sicilia',`Province/State`),
#                     `Province/State` = dplyr::if_else(grepl('Naoasaki',`Province/State`),'Nagasaki',`Province/State`),
#                     `Province/State` = dplyr::if_else(grepl('Distrito Federal',`Province/State`),'Ciudad de Mexico',`Province/State`),
#                     `Province/State` = dplyr::if_else(grepl('Azad Jammu',`Province/State`),'Azad Jammu and Kashmir',`Province/State`),
#                     `Province/State` = dplyr::if_else(grepl('Baluchistan',`Province/State`),'Balochistan',`Province/State`),
#                     `Province/State` = dplyr::if_else(grepl('F.C.T.',`Province/State`),'Islamabad',`Province/State`),
#                     `Province/State` = dplyr::if_else(grepl('N.W.F.P.',`Province/State`),'Khyber Pakhtunkhwa',`Province/State`),
#                     `Province/State` = dplyr::if_else(grepl('Northern Areas',`Province/State`),'Gilgit-Baltistan',`Province/State`),
#                     `Province/State` = dplyr::if_else(grepl('Sind',`Province/State`),'Sindh',`Province/State`),
# 
#                     # Russia (Oblask, Republic) (Jewish Autonomous -> Yevrey, Moscow -> Moscow City / Moscow Oblast -> Moskva)
#                     `Province/State` = dplyr::if_else(grepl('Adygey',`Province/State`),'Adygea',`Province/State`),
#                     `Province/State` = dplyr::if_else(grepl('Altay',`Province/State`),'Altai Krai',`Province/State`),
#                     `Province/State` = dplyr::if_else(grepl('Arkhangel',`Province/State`),'Arkhangelsk',`Province/State`),
#                     `Province/State` = dplyr::if_else(grepl('Astrakhan',`Province/State`),'Astrakhan',`Province/State`),
#                     `Province/State` = dplyr::if_else(grepl('Buryat',`Province/State`),'Buryatia',`Province/State`),
#                     `Province/State` = dplyr::if_else(grepl('Chechnya',`Province/State`),'Chechen',`Province/State`), #
#                     `Province/State` = dplyr::if_else(grepl('Chukot',`Province/State`),'Chukotka',`Province/State`),
#                     `Province/State` = dplyr::if_else(grepl('Chuvash',`Province/State`),'Chuvashia',`Province/State`),
#                     `Province/State` = dplyr::if_else(grepl('Gorno-Altay',`Province/State`),'Altai',`Province/State`),
#                     `Province/State` = dplyr::if_else(grepl('Ingush',`Province/State`),'Ingushetia',`Province/State`),
#                     `Province/State` = dplyr::if_else(grepl('Kabardin-',`Province/State`),'Kabardino-Balkarian',`Province/State`),
#                     `Province/State` = dplyr::if_else(grepl('Kalmyk',`Province/State`),'Kalmykia',`Province/State`),
#                     `Province/State` = dplyr::if_else(grepl('Kamchatka',`Province/State`),'Kamchatka Krai',`Province/State`),
#                     `Province/State` = dplyr::if_else(grepl('Khabarovsk',`Province/State`),'Khabarovsk Krai',`Province/State`),
#                     `Province/State` = dplyr::if_else(grepl('Khakass',`Province/State`),'Khakassia',`Province/State`),
#                     `Province/State` = dplyr::if_else(grepl('Krasnodar',`Province/State`),'Krasnodar Krai',`Province/State`),
#                     `Province/State` = dplyr::if_else(grepl('Krasnoyarsk',`Province/State`),'Krasnoyarsk Krai',`Province/State`),
#                     `Province/State` = dplyr::if_else(grepl('Maga ',`Province/State`),'Magadan',`Province/State`),
#                     `Province/State` = dplyr::if_else(grepl('Mariy-',`Province/State`),'Mari El',`Province/State`),
#                     `Province/State` = dplyr::if_else(grepl('Nizhegorod',`Province/State`),'Nizhny Novgorod',`Province/State`),
#                     `Province/State` = dplyr::if_else(grepl('Ossetia',`Province/State`),'North Ossetia - Alania',`Province/State`),
#                     `Province/State` = dplyr::if_else(grepl('Perm',`Province/State`),'Perm Krai',`Province/State`),
#                     `Province/State` = dplyr::if_else(grepl('Primor',`Province/State`),'Primorsky Krai',`Province/State`),
#                     `Province/State` = dplyr::if_else(grepl('St. Petersburg',`Province/State`),'Saint Petersburg',`Province/State`),
#                     `Province/State` = dplyr::if_else(grepl('Sakha',`Province/State`),'Sakha (Yakutiya)',`Province/State`),
#                     `Province/State` = dplyr::if_else(grepl('Stavropol',`Province/State`),'Stavropol Krai',`Province/State`),
#                     `Province/State` = dplyr::if_else(grepl('Tuva',`Province/State`),'Tyva',`Province/State`),
#                     `Province/State` = dplyr::if_else(grepl('Yamal-Nenets',`Province/State`),'Yamalo-Nenets',`Province/State`),
#                     `Province/State` = dplyr::if_else(grepl('Zabaykal',`Province/State`),'Zabaykalsky Krai',`Province/State`),
# 
#                     # Spain (Ceuta / Melilla -> Ceuta y Melilla)
#                     `Province/State` = dplyr::if_else(grepl('Andalucia',`Province/State`),'Andalusia',`Province/State`),
#                     `Province/State` = dplyr::if_else(grepl('Asturias',`Province/State`),'Asturias',`Province/State`),
#                     `Province/State` = dplyr::if_else(grepl('Baleares',`Province/State`),'Baleares',`Province/State`),
#                     `Province/State` = dplyr::if_else(grepl('Valenciana',`Province/State`),'C. Valenciana',`Province/State`),
#                     `Province/State` = dplyr::if_else(grepl('Canarias',`Province/State`),'Canarias',`Province/State`),
#                     `Province/State` = dplyr::if_else(grepl('Cataluna',`Province/State`),'Catalonia',`Province/State`),
#                     `Province/State` = dplyr::if_else(grepl('Madrid',`Province/State`),'Madrid',`Province/State`),
#                     `Province/State` = dplyr::if_else(grepl('Murcia',`Province/State`),'Murcia',`Province/State`),
#                     `Province/State` = dplyr::if_else(grepl('Navarra',`Province/State`),'Navarra',`Province/State`),
#                     `Province/State` = dplyr::if_else(grepl('Jamtland',`Province/State`),'Jamtland Harjedalen',`Province/State`),
#                     `Province/State` = dplyr::if_else(grepl('Sodermanland',`Province/State`),'Sormland',`Province/State`),
# 
#                     # Ukraine (Oblask, Republic) (Kiev -> Kiev City)
#                     `Province/State` = dplyr::if_else(grepl('Khmelnyts',`Province/State`),'Khmelnytskyi',`Province/State`),
#                     `Province/State` = dplyr::if_else(grepl('Mykolayiv',`Province/State`),'Mykolaiv',`Province/State`),
#                     `Province/State` = dplyr::if_else(grepl('Vinnytsya',`Province/State`),'Vinnytsia',`Province/State`),
#                     `Province/State` = dplyr::if_else(grepl('Zaporizhzhya',`Province/State`),'Zaporizhia',`Province/State`),
#                     `Province/State` = dplyr::if_else(grepl('Transcarpathia',`Province/State`),'Zakarpattia',`Province/State`),
# 
#                     `Province/State` = stringr::str_replace(`Province/State`,'and Nicobar','and Nicobar Islands'),
#                     `Province/State` = stringr::str_replace(`Province/State`,'Nagar Haveli','Nagar Haveli and Daman and Diu'),
#                     `Province/State` = stringr::str_replace(`Province/State`,'Friuli-','Friuli '),
#                     `Province/State` = stringr::str_replace(`Province/State`,'-Mansiy','-Mansi'),
#                     `Province/State` = stringr::str_replace(`Province/State`,'-La Mancha',' - La Mancha'))
#     }
#   return (spframe)
#   }

merge_data <- function(allow_minus=FALSE) {
  # Preprocess Covid data
  covid <<- read_file(filename)
  covid <<- clean_dataset(covid, allow_minus)

  # # Record if it has Province/State information
  # covid <<- covid %>%
  #   dplyr::group_by(adm0_a3,`Country/Region`,Date) %>%
  #   dplyr::mutate(num_states = dplyr::n()) %>%
  #   dplyr::ungroup()
    
  # # Preprocess shape data
  # read_spdata()
  # countries_sf <<- clean_spdata(countries_sf)
  # states_sf <<- clean_spdata(states_sf)
  # gadm_sf <<- clean_spdata(gadm_sf)

  # covid$merge_type = 'country'
  covid <<- covid %>%
    dplyr::mutate(merge_type = dplyr::if_else(num_states>1 & !is.na(`Province/State`),'state','country')) %>%
    dplyr::group_by(adm0_a3,`Country/Region`,Date) %>%
    dplyr::mutate(merge_type = max(merge_type),
                  groups = dplyr::n()) %>%
    dplyr::ungroup()

  # Final setting of country-level statistics
  covid <<- covid %>%
    dplyr::group_by(adm0_a3,`Country/Region`,merge_type,Date) %>%
    dplyr::mutate(Confirmed = dplyr::if_else(merge_type=='country',Tot_Confirmed,Confirmed),
                  Deaths = dplyr::if_else(merge_type=='country',Tot_Deaths,Deaths),
                  i_Confirmed = dplyr::if_else(merge_type=='country',`iTot_Confirmed`,i_Confirmed),
                  i_Deaths = dplyr::if_else(merge_type=='country',`iTot_Deaths`,i_Deaths),
                  r_Confirmed = dplyr::if_else(merge_type=='country',rTot_Confirmed,r_Confirmed),
                  r_Deaths = dplyr::if_else(merge_type=='country',rTot_Deaths,r_Deaths),
                  ri_Confirmed = dplyr::if_else(merge_type=='country',riTot_Confirmed,ri_Confirmed),
                  ri_Deaths = dplyr::if_else(merge_type=='country',riTot_Deaths,ri_Deaths),
                  # Lat = ifelse(merge_type=='country' & groups > 1 & !is.na(`Province/State`),NA,Lat),
                  # Long = ifelse(merge_type=='country' & groups > 1 & !is.na(`Province/State`),NA,Long),
                  `Province/State` = ifelse(merge_type=='country' & groups > 1,NA,`Province/State`)
                  # Lat = min(Lat,na.rm=TRUE),
                  # Long = min(Long,na.rm=TRUE)
                  ) %>%
    dplyr::ungroup() %>%
  dplyr::group_by(adm0_a3,`Country/Region`,`Province/State`,merge_type,Date,Lat,Long,Confirmed,Deaths,Tot_Confirmed,Tot_Deaths,i_Confirmed,i_Deaths,iTot_Confirmed,iTot_Deaths) %>%
  dplyr::group_by(adm0_a3,`Country/Region`,`Province/State`,merge_type,Date,
                  Confirmed,Deaths,Tot_Confirmed,Tot_Deaths,
                  i_Confirmed,i_Deaths,iTot_Confirmed,iTot_Deaths,
                  r_Confirmed,r_Deaths,rTot_Confirmed,rTot_Deaths,
                  ri_Confirmed,ri_Deaths,riTot_Confirmed,riTot_Deaths
  ) %>%
  dplyr::summarize() %>%
  dplyr::ungroup()

  # # Merge data and shape (latest data only)
  # countries_cov <<- inner_join(countries_sf, dplyr::filter(covid,merge_type=='country' & Date==max(Date,na.rm=TRUE)), by=c("adm0_a3","merge_type")) %>%
  #   dplyr::rename("Country/Region" = "Country/Region.y") %>%
  #   subset(select=-c(`Country/Region.x`))
  # states_cov <<- inner_join(states_sf, dplyr::filter(covid,merge_type=='state' & Date==max(Date,na.rm=TRUE)), by=c("adm0_a3","Province/State","merge_type")) %>%
  #   dplyr::rename("Country/Region" = "Country/Region.y") %>%
  #   subset(select=-c(`Country/Region.x`))
  # gadm_cov <<- inner_join(gadm_sf, dplyr::filter(covid,merge_type=='gadm' & Date==max(Date,na.rm=TRUE)), by=c("adm0_a3","Province/State","merge_type")) %>%
  #   dplyr::rename("Country/Region" = "Country/Region.y") %>%
  #   subset(select=-c(`Country/Region.x`))
  # 
  # combined_cov <<- rbind(countries_cov,states_cov,gadm_cov)

  global_stat <<- covid %>%
    dplyr::filter(Date==max(Date,na.rm=TRUE)) %>%
    dplyr::group_by(adm0_a3,Tot_Confirmed,Tot_Deaths,iTot_Confirmed,iTot_Deaths) %>%
    dplyr::summarize() %>%
    dplyr::ungroup() %>%
    dplyr::summarize(total_confirmed = sum(Tot_Confirmed),
                     total_deaths = sum(Tot_Deaths),
                     inc_confirmed = sum(iTot_Confirmed),
                     inc_deaths = sum(iTot_Deaths)) %>%
    dplyr::ungroup()
  }

# plot_leaflet <- function(data, col='Confirmed') {
#   data <- data %>%
#     dplyr::filter(Date == max(Date,na.rm=TRUE))
# 
#   data$popup_text <- paste0(
#     paste0("<strong>Date:</strong> ",data$Date),
#     paste0("<br /><strong>Country/Region:</strong> ",data$`Country/Region`),
#     ifelse(is.na(data$`Province/State`),"",paste0("<br /><strong>Province/State:</strong> ",data$`Province/State`)),
#     paste0("<br /><strong>Confirmed:</strong> ",prettyNum(data$`Confirmed`,big.mark=','),
#            " / ",prettyNum(data$`Total Confirmed`,big.mark=',')),
#     paste0("<br /><strong>Deaths:</strong> ",prettyNum(data$`Deaths`,big.mark=','),
#            " / ",prettyNum(data$`Total Deaths`,big.mark=','))
#     ) %>% lapply(htmltools::HTML)
# 
#   pal <- leaflet::colorNumeric("YlOrRd",data[[col]])
# 
#   map <- data %>%
#     # dplyr::filter(Date == max(Date,na.rm=TRUE)) %>%
#     leaflet::leaflet() %>%
#     leaflet::addTiles() %>%
#     leaflet::addPolygons(
#       # fillColor = ~pal(data$Confirmed),
#       fillColor = ~pal(data[[col]]),
#       weight = 2,
#       opacity = 1,
#       color = 'white',
#       dashArray = '3',
#       fillOpacity = 0.7,
#       label = ~data$popup_text,
#       labelOptions = labelOptions(
#         style = list("font-weight" = "normal",
#                      padding = "3px 8px"),
#         textsize = "13px",
#         direction = "auto"),
#       highlight = highlightOptions(weight = 2,
#                                    color = "#666",
#                                    dashArray = "",
#                                    fillOpacity = 0.7,
#                                    bringToFront = TRUE)
#       ) %>%
#     leaflet::addLegend(pal = pal,
#     values = ~data[[col]],
#     # values = ~data$Confirmed,
#     opacity = 0.7,
#     title = paste(col,'(Country & State Level)'),
#     position = 'bottomright')
# 
#     map
#     }

plot_heatmap <- function(covid, target='detailed', type='raw', limit=25) {
  if (target == 'global') {
    if (type == 'ratio') {
      target_col <- c('riTot_Confirmed','riTot_Deaths')
      title_target <- "per 100K"
    }
    else {
      target_col <- c('iTot_Confirmed','iTot_Deaths')
      title_target <- ''
    }
    
    cand <- covid %>%
      dplyr::filter(Date==max(Date,na.rm=TRUE) & !is.na(!!(sym(target_col[1]))) & !is.na(!!(sym(target_col[2])))) %>%
      dplyr::group_by(Date) %>%
      # dplyr::distinct(adm0_a3,`Country/Region`,`Total Confirmed`,`Total Deaths`,Population) %>%
      dplyr::distinct(adm0_a3,`Country/Region`,iTot_Confirmed,iTot_Deaths) %>%
      dplyr::mutate(rn_tc=dplyr::min_rank(desc(iTot_Confirmed)),rn_td=dplyr::min_rank(desc(iTot_Deaths))) %>%
      # dplyr::filter(rn_tc<=35) %>%
      dplyr::ungroup() %>%
      dplyr::select(adm0_a3,rn_tc,rn_td)
    }

  else if (target %in% unique(covid$adm0_a3) | target=='detailed') {
    if (type == 'ratio') {
      target_col <- c('ri_Confirmed','ri_Deaths')
      title_target <- "per 100K"
    }
    else {
      target_col <- c('i_Confirmed','i_Deaths')
      title_target <- ''
    }
    
    if (target %in% unique(covid$adm0_a3)) {
      cand <- covid %>%
        dplyr::filter(Date==max(Date,na.rm=TRUE) & adm0_a3==target & !is.na(!!(sym(target_col[1]))) & !is.na(!!(sym(target_col[2])))) %>%
        dplyr::group_by(Date,adm0_a3) %>%
        # dplyr::distinct(adm0_a3,`Country/Region`,`Province/State`,Confirmed,Deaths,Population) %>%
        dplyr::distinct(adm0_a3,`Country/Region`,`Province/State`,!!(sym(target_col[1])),!!(sym(target_col[2]))) %>%
        dplyr::mutate(rn_tc=dplyr::min_rank(desc(!!(sym(target_col[1])))),rn_td=dplyr::min_rank(desc(!!(sym(target_col[2]))))) %>%
        # dplyr::filter(rn_tc<=50 | rn_td<=50) %>%
        dplyr::ungroup() %>%
        dplyr::select(adm0_a3,`Province/State`,rn_tc,rn_td)
    }
    else {
      cand <- covid %>%
        dplyr::filter(Date==max(Date,na.rm=TRUE) & merge_type != 'country' & !is.na(`Province/State`) & !is.na(!!(sym(target_col[1]))) & !is.na(!!(sym(target_col[2])))) %>%
        dplyr::group_by(Date,adm0_a3) %>%
        # dplyr::distinct(adm0_a3,`Country/Region`,`Province/State`,Confirmed,Deaths,Population) %>%
        dplyr::distinct(adm0_a3,`Country/Region`,`Province/State`,!!(sym(target_col[1])),!!(sym(target_col[2]))) %>%
        dplyr::mutate(rn_tc=dplyr::min_rank(desc(!!(sym(target_col[1])))),rn_td=dplyr::min_rank(desc(!!(sym(target_col[2]))))) %>%
        # dplyr::filter(rn_tc<=50 | rn_td<=50) %>%
        dplyr::ungroup() %>%
        dplyr::select(adm0_a3,`Province/State`,rn_tc,rn_td)
    }
  } 

  # ratio <- length(unique(covid$Date))/length(cand)

  textcol <- "grey40"
  target_text <- c('infections','casualties')
  caption_text <- 'Johns Hopkins University CSSE'
  
  htmap <<- list()

  if (target == 'global') {
    for (iter in 1:2) {
      if (iter==1) {
        cand_cty <- as.vector(cand[cand$rn_tc<=limit,'adm0_a3']$adm0_a3)
        # target_col <- 'iTot_Confirmed'
        # target_text <- 'infections'
        lvl <- covid %>%
          dplyr::filter(adm0_a3 %in% cand_cty & Date==max(Date,na.rm=TRUE)) %>%
          # dplyr::arrange(Tot_Confirmed) %>%
          dplyr::arrange(iTot_Confirmed) %>%
          dplyr::distinct(`Country/Region`)
        
        summary_stat <- c(global_stat$total_confirmed,global_stat$inc_confirmed)
        }
      else {
        cand_cty <- as.vector(cand[cand$rn_td<=limit,'adm0_a3']$adm0_a3)
        # target_col <- 'iTot_Deaths'
        # target_text <- 'casualties'
        lvl <- covid %>%
          dplyr::filter(adm0_a3 %in% cand_cty & Date==max(Date,na.rm=TRUE)) %>%
          # dplyr::arrange(`Total Confirmed`) %>%
          dplyr::arrange(iTot_Deaths) %>%
          dplyr::distinct(`Country/Region`)
        summary_stat <- c(global_stat$total_deaths,global_stat$inc_deaths)
        }

      lvl <- as.vector(lvl$`Country/Region`)

      min_val <- min(covid[covid$adm0_a3 %in% cand_cty,target_col[iter]])
      # min_val <- ifelse(min_val<0,0,min_val)
      max_val <- max(covid[covid$adm0_a3 %in% cand_cty,target_col[iter]])
      
      if (type == 'ratio') {
        breaks_seq=seq(min_val,max_val,(max_val-min_val)/5)
        label_acc = .01
      }
      else {
        breaks_seq=seq(min_val,max_val,(max_val-min_val)%/%min(5,(max_val-min_val)))
        label_acc = 1
      }
      
      htmap[[iter]] <<- covid %>%
        dplyr::filter(adm0_a3 %in% cand_cty) %>%
        dplyr::arrange(target_col[iter]) %>%
        dplyr::mutate(`Country/Region` = factor(`Country/Region`, levels = lvl)) %>%
        ggplot2::ggplot(mapping=aes(x=Date,y=`Country/Region`)) +
        ggplot2::geom_tile(aes(fill=!!(sym(target_col[iter]))),color='white',size=0.25) +
        ggplot2::labs(x="",y="",
        title=paste("Global nCovid-2019 Status as of",max(covid$Date,na.rm=TRUE),title_target),
        subtitle=paste0("Daily ",target_text[iter]," increases by country (Global total: ",scales::comma(summary_stat[1])," / increases: ",scales::comma(summary_stat[2]),")"),
        caption=paste("Source:",caption_text))+
        ggplot2::scale_y_discrete(expand=c(0,0))+
        # ggplot2::scale_fill_gradient(low='lightgray',high='steelblue',labels=scales::comma,breaks=seq(min_val,max_val,(max_val-min_val)%/%5)) +
        ggplot2::scale_fill_gradient(low='lightgray',high='steelblue',labels=scales::label_comma(accuracy=label_acc),breaks=breaks_seq) +
        ggplot2::guides(fill = ggplot2::guide_legend(reverse=TRUE,title=paste(stringr::str_to_title(target_text[iter]),'increases'))) +
        # ggplot2::coord_fixed(ratio=ratio) +
        ggplot2::scale_x_date(expand=c(0,0))+
        ggplot2::theme_grey(base_size=8) +
        ggplot2::theme(#aspect.ratio = 1,
                       # legend.position="right",legend.direction="vertical",
                       legend.title=ggplot2::element_text(colour=textcol,size=7,face="bold"),
                       legend.text=ggplot2::element_text(colour=textcol,size=6),
                       # legend.margin=ggplot2::margin(grid::unit(0,"cm")),
                       legend.key.height=grid::unit(0.5,"cm"),
                       legend.key.width=grid::unit(0.25,"cm"),
                       axis.text.x=ggplot2::element_text(colour=textcol),
                       axis.text.y=ggplot2::element_text(vjust=0.2,colour=textcol),
                       axis.ticks=element_line(size=0.4),
                       plot.background=element_blank(),
                       panel.border=element_blank(),
                       plot.margin=ggplot2::margin(0.2,0.4,0.1,0.2,"cm"),
                       plot.title=ggplot2::element_text(colour=textcol,size=9,face="bold"),
                       plot.subtitle=ggplot2::element_text(colour=textcol,size=8)
        )
      }

    g <<- do.call(gridExtra::arrangeGrob,htmap)
    print(paste("Saving heatmap of:",type,",",target))
    ggplot2::ggsave(g,filename=paste0("saved/heatmap_",target,"_",type,"_",max(covid$Date,na.rm=TRUE),".png"),dpi=200)

    }

  else {
    for (cty in unique(cand$adm0_a3)) {
      if (cty == 'KOR') {
        caption_text <- 'KCDC'
      }
      else {
        caption_text <- 'Johns Hopkins University CSSE'
      }
      for (iter in 1:2) {
        if (iter==1) {
          cand_state <- as.vector(cand[cand$rn_tc<=limit & cand$adm0_a3==cty,'Province/State']$`Province/State`)
          # target_col <- 'i_Confirmed'
          # target_text <- 'infections'
          total_cty <- max(tail(covid[covid$adm0_a3 == cty,'Tot_Confirmed'],1))
          inc_cty <- max(tail(covid[covid$adm0_a3 == cty,'iTot_Confirmed'],1))
          }
        else {
          cand_state <- as.vector(cand[cand$rn_td<=limit & cand$adm0_a3==cty,'Province/State']$`Province/State`)
          # target_col <- 'i_Deaths'
          # target_text <- 'casualties'
          total_cty <- max(tail(covid[covid$adm0_a3 == cty,'Tot_Deaths'],1))
          inc_cty <- max(tail(covid[covid$adm0_a3 == cty,'iTot_Deaths'],1))
          }
        
        lvl <- covid %>%
          dplyr::filter(adm0_a3 == cty & `Province/State` %in% cand_state & Date==max(Date,na.rm=TRUE)) %>%
          dplyr::arrange(!!sym(target_col[iter])) %>%
          dplyr::distinct(`Province/State`)
        
        lvl <- as.vector(lvl$`Province/State`)
        min_val <- min(covid[covid$adm0_a3 == cty & covid$`Province/State` %in% cand_state & !is.na(covid$`Province/State`),target_col[iter]])
        # min_val <- ifelse(min_val<0,0,min_val)
        max_val <- max(covid[covid$adm0_a3 == cty & covid$`Province/State` %in% cand_state & !is.na(covid$`Province/State`),target_col[iter]])

        if (type == 'ratio') {
          breaks_seq=seq(min_val,max_val,(max_val-min_val)/5)
          label_acc = .01
        }
        else {
          breaks_seq=seq(min_val,max_val,(max_val-min_val)%/%min(5,(max_val-min_val)))
          label_acc = 1
        }
        
        htmap[[iter]] <<- covid %>%
          dplyr::filter(adm0_a3 == cty & `Province/State` %in% cand_state) %>%
          dplyr::arrange(target_col[iter]) %>%
          dplyr::mutate(`Province/State` = factor(`Province/State`, levels = lvl)) %>%
          ggplot2::ggplot(mapping=aes(x=Date,y=`Province/State`)) +
          ggplot2::geom_tile(aes(fill=!!(sym(target_col[iter]))),color='white',size=0.25) +
          ggplot2::labs(x="",y="",
          title=paste(unique(covid[covid$adm0_a3 == cty,'Country/Region']),"nCovid-2019 Status as of",max(covid$Date,na.rm=TRUE),title_target),
          subtitle=paste0("Daily ",target_text[iter]," increases by State/Province (",unique(covid[covid$adm0_a3 == cty,'Country/Region'])," total: ",scales::comma(total_cty)," / increases: ",scales::comma(inc_cty),")"),
          caption=paste("Source:",caption_text))+
          ggplot2::scale_y_discrete(expand=c(0,0))+
          # ggplot2::scale_fill_gradient(low='lightgray',high='steelblue',labels=scales::comma,breaks=seq(min_val,max_val,(max_val-min_val)%/%5)) +
          ggplot2::scale_fill_gradient(low='lightgray',high='steelblue',labels=scales::label_comma(accuracy=label_acc),breaks=breaks_seq) +
          ggplot2::guides(fill = ggplot2::guide_legend(reverse=TRUE,title=paste(stringr::str_to_title(target_text[iter]),'increases'))) +
          # ggplot2::coord_fixed(ratio=ratio) +
          ggplot2::scale_x_date(expand=c(0,0))+
          ggplot2::theme_grey(base_size=8) +
          ggplot2::theme(#aspect.ratio = 1,
                         # legend.position="right",legend.direction="vertical",
                         legend.title=ggplot2::element_text(colour=textcol,size=7,face="bold"),
                         legend.text=ggplot2::element_text(colour=textcol,size=6),
                         # legend.margin=ggplot2::margin(grid::unit(0,"cm")),
                         legend.key.height=grid::unit(0.5,"cm"),
                         legend.key.width=grid::unit(0.25,"cm"),
                         axis.text.x=ggplot2::element_text(colour=textcol),
                         axis.text.y=ggplot2::element_text(vjust=0.2,colour=textcol),
                         axis.ticks=element_line(size=0.4),
                         plot.background=element_blank(),
                         panel.border=element_blank(),
                         # plot.margin=ggplot2::margin(0,0.5,0.5,0.5,"cm"),
                         plot.margin=ggplot2::margin(0.2,0.4,0.1,0.2,"cm"),
                         plot.title=ggplot2::element_text(colour=textcol,size=9,face="bold"),
                         plot.subtitle=ggplot2::element_text(colour=textcol,size=7)
                         )
        # ggplot2::ggsave(htmap,filename=paste0("saved/heatmap_",unique(covid[covid$adm0_a3==iter,'Country/Region']),"_",max(covid$Date,na.rm=TRUE),".png"),dpi=200)
        }
      g <<- do.call(gridExtra::arrangeGrob,htmap)

      print(paste("Saving heatmap of:",type,",",unique(covid[covid$adm0_a3==cty,'Country/Region'])))
      ggplot2::ggsave(g,filename=paste0("saved/heatmap_",unique(covid[covid$adm0_a3==cty,'Country/Region']),"_",type,"_",max(covid$Date,na.rm=TRUE),".png"),dpi=200)
    }
    }

  # ggplot2::ggsave(htmap,filename=paste0("saved/heatmap_",type,"_",max(covid$Date,na.rm=TRUE),".png"),dpi=200)
  # return (g)
  }

# merge_data()
# maps <- plot_leaflet(combined_cov)
# plot_heatmap(covid,'global')
# plot_heatmap(covid,'detailed')
