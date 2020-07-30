setwd('/Users/minpark/Documents/nCovid-2019')
library(dplyr)

file_list = list('wc'='time_series_covid19_confirmed_global.csv','wd'='time_series_covid19_deaths_global.csv','uc'='time_series_covid19_confirmed_US.csv','ud'='time_series_covid19_deaths_US.csv','geo'='UID_ISO_FIPS_LookUp_Table.csv')
git_path = 'https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/'

get_filename <- function(git_path, file_list, remote) {
  for (iter in seq_along(file_list)) {
    if (remote == "Y") {
      file_list[names(file_list[iter])] <- paste0(git_path,file_list[[iter]])
    }
  }
  return (file_list)
}

read_file <- function(git_path, file_list, remote) {
  # Update filename list (remote download)
  file_list <- get_filename(git_path,file_list,remote)
  
  # Read datafiles
  # data <- readr::read_csv(file_list$type)
  wc <<- readr::read_csv(file_list$wc) %>%
    subset(select=-c(Lat,Long))

  wd <<- readr::read_csv(file_list$wd) %>%
    subset(select=-c(Lat,Long))

  uc <<- readr::read_csv(file_list$uc) %>%
    dplyr::rename(#"adm0_a3" = "iso3",
                  "Province/State" = "Province_State",
                  "Country/Region" = "Country_Region"
                  # ,"County" = "Admin2"
                  # ,"Long" = "Long_"
                  ) %>%
    # dplyr::mutate(FIPS = stringr::str_pad(as.character(FIPS),5,pad="0")) %>%
    subset(select=-c(UID,iso2,iso3,code3,Lat,Long_,Admin2,FIPS,Combined_Key))
  
  ud <<- readr::read_csv(file_list$ud) %>%
    dplyr::rename(#"adm0_a3" = "iso3",
                  "Province/State" = "Province_State",
                  "Country/Region" = "Country_Region"
                  # ,"County" = "Admin2"
                  # ,"Long" = "Long_"
                  ) %>%
    # dplyr::mutate(FIPS = stringr::str_pad(as.character(FIPS),5,pad="0")) %>%
    subset(select=-c(UID,iso2,iso3,code3,Lat,Long_,Admin2,FIPS,Combined_Key,Population))
  
  geo <<- readr::read_csv(file_list$geo) %>%
    dplyr::rename("adm0_a3" = "iso3",
           "Province/State" = "Province_State",
           "Country/Region" = "Country_Region",
           "Long" = "Long_") %>%
    dplyr::mutate(FIPS = if_else(is.na(FIPS),'00000', stringr::str_pad(as.character(FIPS),5,pad="0"))) %>%
    dplyr::select('adm0_a3','Province/State','Country/Region','FIPS','Lat','Long')

  # return (data)
}

clean_data <- function() {
  # Clean geographic info
  geo <<- geo %>%
    dplyr::group_by(adm0_a3,`Province/State`,`Country/Region`) %>%
    dplyr::summarize(FIPS = min(FIPS), Lat = Lat[which(FIPS == min(FIPS))], Long = Long[which(FIPS == min(FIPS))]) %>%
    dplyr::group_by(adm0_a3,`Province/State`,`Country/Region`,FIPS,Lat,Long) %>%
    dplyr::ungroup()

  wc <<- wc %>%
    dplyr::inner_join(geo, by=c('Country/Region','Province/State')) %>%
    dplyr::filter((Lat != 0 | Long != 0) & adm0_a3 != 'USA') %>%
    subset(select=-c(FIPS))
    # dplyr::select(contains(c('Province/State','Country/Region','adm0_a3','Lat','Long','1/20','5/20')))

  wd <<- wd %>%
    dplyr::inner_join(geo, by=c('Country/Region','Province/State')) %>%
    dplyr::filter((Lat != 0 | Long != 0) & adm0_a3 != 'USA') %>%
    subset(select=-c(FIPS))
    # dplyr::select(contains(c('Province/State','Country/Region','adm0_a3','Lat','Long','1/20','5/20')))

  # type_u = grepl('^u',deparse(substitute(data)))
  uc <<- uc %>%
    group_by(`Country/Region`,`Province/State`) %>%
    dplyr::summarize_at(vars(ends_with("20")),list(~sum(.))) %>%
    dplyr::inner_join(geo, by=c('Country/Region','Province/State')) %>%
    dplyr::filter(Lat != 0 | Long != 0) %>%
    subset(select=-c(FIPS))
    # dplyr::select(contains(c('Province/State','Country/Region','adm0_a3','Lat','Long','1/20','5/20')))

  ud <<- ud %>%
    group_by(`Country/Region`,`Province/State`) %>%
    dplyr::summarize_at(vars(ends_with("20")),list(~sum(.))) %>%
    dplyr::inner_join(geo, by=c('Country/Region','Province/State')) %>%
    dplyr::filter(Lat != 0 | Long != 0) %>%
    subset(select=-c(FIPS))
    # dplyr::select(contains(c('Province/State','Country/Region','adm0_a3','Lat','Long','1/20','5/20')))
    
  # # Merge geographic info
  # # type_u = grepl('^u',deparse(substitute(data)))
  # data <- dplyr::inner_join(data, geo, by=c('Country/Region','Province/State'))

  # uc %>%
  # group_by(`Country/Region`,`Province/State`) %>%
  # dplyr::summarize_at(vars(ends_with("20")),list(~sum(.)))
  
# counties_cov <- inner_join(counties_sf, group_by(Covid, countyFIPS) %>%
#                              summarise(cases=sum(cases)), by=c("county_fips"="countyFIPS"))


  # if (type_u) {
  #   data <- dplyr::inner_join(data, geo, by=c('Country/Region','Province/State'))
  #   # dplyr::inner_join(ud, geo, by=c('Country/Region','Province/State','FIPS'))
  # }
  # else {
  #   data <- dplyr::inner_join(data, geo, by=c('Country/Region','Province/State'))
  #   # dplyr::inner_join(wd, geo, by=c('Country/Region','Province/State'))
  # }
  
  # # Cleaning data
  # data <- data %>%
  #   dplyr::filter(Lat != 0 | Long != 0) %>%
  #   subset(select=-c(FIPS))
  
  # # if (type_u) {
  # #   data <- data %>%
  # #     subset(select=-c(UID,iso2,code3,Lat,Long,Admin2,Combined_Key))
  # # }

  # # else {
  # #   data <- data %>%
  # #     subset(select=-c(Lat,Long))
  # # }

  # return (data)

  # # Cleaning data (Global)
  # wc <<- wc %>%
  #   dplyr::filter(Lat != 0 | Long != 0)
  # wd <<- wd %>%
  #   dplyr::filter(Lat != 0 | Long != 0)
  
  # # confirmed_global %>%
  # #   subset(select=-c(FIPS))
  # # deaths_global %>%
  # #   subset(select=-c(FIPS))
  
  # # Cleaning data (US)
  # uc <- uc %>%
  #   dplyr::filter(Lat != 0 | Long != 0)
  # ud <- ud %>%
  #   dplyr::filter(Lat != 0 | Long != 0)
  
  # uc <<- uc %>%
  #   subset(select=-c(UID,iso2,iso3,code3,FIPS,Admin2,Combined_Key))
  # ud <<- ud %>%
  #   subset(select=-c(UID,iso2,iso3,code3,FIPS,Admin2,Combined_Key))
  # list(confirmed_global,deaths_global,confirmed_us,deaths_us) 
}

group_data <- function(dataframe) {
  dataname = deparse(substitute(dataframe))
  if (dataname == 'confirmed') {
    dataframe <- dataframe %>%
      tidyr::gather(key=Date,value=Confirmed,-'Country/Region',-'Province/State',-adm0_a3,-Lat,-Long) %>%
      dplyr::mutate(Date = as.Date(Date,format="%m/%d/%y")) %>%
      # dplyr::filter(Date == max(Date,na.rm=TRUE) | (lubridate::day(Date)%%5) %in% c(0,5)) %>%
      dplyr::group_by(`Country/Region`,Date,adm0_a3) %>%
      dplyr::mutate('Total Confirmed' = sum(Confirmed)) %>%
      dplyr::ungroup()
  }
  else if (dataname == 'deaths') {
    dataframe <- dataframe %>%
      tidyr::gather(key=Date,value=Deaths,-'Country/Region',-'Province/State',-adm0_a3,-Lat,-Long) %>%
      dplyr::mutate(Date = as.Date(Date,format="%m/%d/%y")) %>%
      # dplyr::filter(Date == max(Date,na.rm=TRUE) | (lubridate::day(Date)%%5) %in% c(0,5)) %>%
      dplyr::group_by(`Country/Region`,Date,adm0_a3) %>%
      dplyr::mutate('Total Deaths' = sum(Deaths)) %>%
      dplyr::ungroup()
  }
  # if (dataname == 'wc') {
  #   dataframe <- dataframe %>%
  #     tidyr::gather(key=Date,value=Confirmed,-'Country/Region',-'Province/State',-adm0_a3,-FIPS) %>%
  #     dplyr::group_by(`Country/Region`,Date,adm0_a3) %>%
  #     dplyr::summarize('Total Confirmed' = sum(Confirmed))
  # }
  # else if (dataname == 'wd') {
  #   dataframe <- dataframe %>%
  #     tidyr::gather(key=Date,value=Deaths,-'Country/Region',-'Province/State',-adm0_a3,-FIPS) %>%
  #     dplyr::group_by(`Country/Region`,Date,adm0_a3) %>%
  #     dplyr::summarize('Total Deaths' = sum(Deaths))
  # }
  # else if (dataname == 'uc') {
  #   dataframe <- dataframe %>%
  #     tidyr::gather(key=Date,value=Confirmed,-'Country/Region',-'Province/State',-adm0_a3,-FIPS) %>%
  #     dplyr::group_by(`Country/Region`,`Province/State`,Date,adm0_a3) %>%
  #     dplyr::summarize('Total Confirmed' = sum(Confirmed))
  # }
  # else if (dataname == 'ud') {
  #   dataframe <- dataframe %>%
  #     tidyr::gather(key=Date,value=Deaths,-'Country/Region',-'Province/State',-adm0_a3,-FIPS) %>%
  #     dplyr::group_by(`Country/Region`,`Province/State`,Date,adm0_a3) %>%
  #     dplyr::summarize('Total Deaths' = sum(Deaths))
  # }
  return (dataframe)
}

merge_dataset <- function(wc, wd, uc, ud) {
  # Concontenate Data by type
  confirmed <- bind_rows(wc,uc)
  deaths <- bind_rows(wd,ud)

  # Transform wide-form data to long-form
  confirmed <- group_data(confirmed)
  deaths <- group_data(deaths)

  covid <- cbind(confirmed,c(deaths$Deaths)) %>%
    cbind(c(deaths$`Total Deaths`)) %>%
    tibble::as_tibble() %>%
    dplyr::rename("Deaths" = "c(deaths$Deaths)",
    "Total Deaths" = "c(deaths$`Total Deaths`)") %>%
    dplyr::ungroup()

  covid['merge_type'] = 'Country'
  covid <- covid %>%
    dplyr::mutate(merge_type = dplyr::if_else((adm0_a3=='CAN' | adm0_a3=='USA' | adm0_a3=='AUS') & !is.na(`Province/State`),'State',merge_type))
 
  covid <- covid %>%
    dplyr::mutate(merge_type = dplyr::if_else(adm0_a3=='CHN','China',merge_type))
 
  return (covid)

}

# read_file(git_path, file_list, 'N')
# clean_data()

## wc <- clean_data(wc,geo)
## wd <- clean_data(wd,geo)
## uc <- clean_data(uc,geo)
## ud <- clean_data(ud,geo)

# wc <- group_data(wc)
# wd <- group_data(wd)
# uc <- group_data(uc)
# ud <- group_data(ud)

# geo %>%
# dplyr::group_by(adm0_a3,`Province/State`,`Country/Region`) %>%
# dplyr::summarize(FIPS = min(FIPS), Lat = Lat[which(FIPS == min(FIPS))], Long = Long[which(FIPS == min(FIPS))]) %>%
# dplyr::group_by(adm0_a3,`Province/State`,`Country/Region`,FIPS,Lat,Long) %>%
# dplyr::ungroup()