setwd('/Users/minpark/Documents/nCovid-2019')
library(dplyr)

file_list = list('wc'='time_series_covid19_confirmed_global.csv','wd'='time_series_covid19_deaths_global.csv','uc'='time_series_covid19_confirmed_US.csv','ud'='time_series_covid19_deaths_US.csv','kc'='time_series_covid19_confirmed_KR.csv','kd'='time_series_covid19_deaths_KR.csv','geo'='UID_ISO_FIPS_LookUp_Table.csv')
git_path = 'https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/'

# Only US data can be remotely downloaded
get_filename <- function(git_path, file_list, remote) {
  for (iter in seq_along(file_list)) {
    if (remote == "Y" & substr(names(file_list[iter]),1,1)=='u') {
      file_list[names(file_list[iter])] <- paste0(git_path,file_list[[iter]])
    }
    else {
      file_list[names(file_list[iter])] <- paste0('data/',file_list[[iter]])
    }
  }
  return (file_list)
}

read_file <- function(git_path, file_list, remote) {
  # Update filename list (remote download)
  file_list <- get_filename(git_path,file_list,remote)
  
  # Read datafiles
  # Global data
  wc <<- readr::read_csv(file_list$wc) %>%
    subset(select=-c(Lat,Long))

  wd <<- readr::read_csv(file_list$wd) %>%
    subset(select=-c(Lat,Long))

  # South Korea data
  kc <<- readr::read_csv(file_list$kc) %>%
    subset(select=-c(Lat,Long))
  
  kd <<- readr::read_csv(file_list$kd) %>%
    subset(select=-c(Lat,Long))

  # US data
  ud <<- readr::read_csv(file_list$ud) %>%
    dplyr::rename("adm0_a3" = "iso3",
      "Province/State" = "Province_State",
      "Country/Region" = "Country_Region"
      # ,"County" = "Admin2"
      ,"Long" = "Long_"
    ) %>%
    # dplyr::mutate(FIPS = stringr::str_pad(as.character(FIPS),5,pad="0")) %>%
    subset(select=-c(UID,iso2,code3,Admin2,FIPS,Combined_Key))
  
  uc <<- readr::read_csv(file_list$uc) %>%
    dplyr::rename("adm0_a3" = "iso3",
                  "Province/State" = "Province_State",
                  "Country/Region" = "Country_Region"
                  # ,"County" = "Admin2"
                  ,"Long" = "Long_"
                  ) %>%
    # dplyr::mutate(FIPS = stringr::str_pad(as.character(FIPS),5,pad="0")) %>%
    subset(select=-c(UID,iso2,code3,Admin2,FIPS,Combined_Key)) %>%
    cbind(ud$Population) %>%
    tibble::as_tibble() %>%
    dplyr::rename("Population" = "ud$Population")
  uc <<- uc[c(1:3,length(uc),5:length(uc)-1)]
  
  geo <<- readr::read_csv(file_list$geo) %>%
    dplyr::rename("adm0_a3" = "iso3",
                  "Province/State" = "Province_State",
                  "Country/Region" = "Country_Region",
                  "Long" = "Long_") %>%
    dplyr::mutate(FIPS = if_else(is.na(FIPS),'00000', stringr::str_pad(as.character(FIPS),5,pad="0"))) %>%
    dplyr::select(FIPS,'adm0_a3','Province/State','Country/Region','Lat','Long','Population')

  # return (data)
}

clean_data <- function(cutoff) {
  # Get least recent date (reporting time gaps)
  min_latest <- as.Date(min(names(wc)[length(wc)],names(wd)[length(wd)],names(kc)[length(kc)],names(kd)[length(kd)],names(uc)[length(uc)],names(ud)[length(ud)]),'%m/%d/%y')
  max_first <- as.Date(max(names(wc)[3],names(wd)[3],names(kc)[3],names(kd)[3],names(uc)[7],names(ud)[7]),'%m/%d/%y')
  min_date <- max(min_latest - cutoff,max_first)
  
  start_date <- min_date
  end_date <- min_latest
  
  # len_cols <- min(length(wc),length(wd),length(uc),length(ud))
  
  # # Clean geographic info
  # geo <<- geo %>%
  #   dplyr::filter(adm0_a3 != 'USA') %>%
  #   dplyr::group_by(adm0_a3,`Province/State`,`Country/Region`) %>%
  #   dplyr::mutate(FIPS = min(FIPS), Lat = Lat[which(FIPS == min(FIPS))], Long = Long[which(FIPS == min(FIPS))]) %>%
  #   dplyr::group_by(adm0_a3,`Province/State`,`Country/Region`,FIPS,Lat,Long,Population) %>%
  #   dplyr::summarize() %>%
  #   dplyr::ungroup()

  wc <<- wc[,c(1:2,match(start_date,as.Date(names(wc),'%m/%d/%y')):match(end_date,as.Date(names(wc),'%m/%d/%y')))] %>%
    dplyr::inner_join(geo, by=c('Country/Region','Province/State')) %>%
    dplyr::filter((Lat != 0 | Long != 0 | `Province/State` == 'Unknown' | grepl('Quarantine',`Province/State`)) & !adm0_a3 %in% c('USA','KOR')) %>%
    subset(select=-c(FIPS,Lat,Long)) %>%
    dplyr::group_by(adm0_a3,`Country/Region`)
    # dplyr::select(contains(c('Province/State','Country/Region','adm0_a3','Lat','Long','1/20','5/20')))

  wd <<- wd[,c(1:2,match(start_date,as.Date(names(wd),'%m/%d/%y')):match(end_date,as.Date(names(wd),'%m/%d/%y')))] %>%
    dplyr::inner_join(geo, by=c('Country/Region','Province/State')) %>%
    dplyr::filter((Lat != 0 | Long != 0 | `Province/State` == 'Unknown' | grepl('Quarantine',`Province/State`)) & !adm0_a3 %in% c('USA','KOR')) %>%
    subset(select=-c(FIPS,Lat,Long))
    # dplyr::select(contains(c('Province/State','Country/Region','adm0_a3','Lat','Long','1/20','5/20')))

  kc <<- kc[,c(1:2,match(start_date,as.Date(names(kc),'%m/%d/%y')):match(end_date,as.Date(names(kc),'%m/%d/%y')))] %>%
    dplyr::inner_join(geo, by=c('Country/Region','Province/State')) %>%
    dplyr::filter((Lat != 0 | Long != 0 | `Province/State` == 'Unknown' | grepl('Quarantine',`Province/State`)) & adm0_a3 == 'KOR') %>%
    subset(select=-c(FIPS,Lat,Long))
  # dplyr::select(contains(c('Province/State','Country/Region','adm0_a3','Lat','Long','1/20','5/20')))
  
  kd <<- kd[,c(1:2,match(start_date,as.Date(names(kd),'%m/%d/%y')):match(end_date,as.Date(names(kd),'%m/%d/%y')))] %>%
    dplyr::inner_join(geo, by=c('Country/Region','Province/State')) %>%
    dplyr::filter((Lat != 0 | Long != 0 | `Province/State` == 'Unknown' | grepl('Quarantine',`Province/State`)) & adm0_a3 == 'KOR') %>%
    subset(select=-c(FIPS,Lat,Long))
  # dplyr::select(contains(c('Province/State','Country/Region','adm0_a3','Lat','Long','1/20','5/20')))
  
  # type_u = grepl('^u',deparse(substitute(data)))
  uc <<- uc[,c(1:6,match(start_date,as.Date(names(uc),'%m/%d/%y')):match(end_date,as.Date(names(uc),'%m/%d/%y')))] %>%
    # dplyr::inner_join(geo, by=c('Country/Region','Province/State','FIPS')) %>%
    dplyr::filter(Lat != 0 | Long != 0) %>%
    dplyr::group_by(`Country/Region`,`Province/State`) %>%
    dplyr::mutate(Population = sum(Population)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(adm0_a3,`Country/Region`,`Province/State`,Population) %>%
    dplyr::summarize_at(dplyr::vars(ends_with("20")),list(~sum(.)))
    # subset(select=-c(Lat,Long))
    # dplyr::select(contains(c('Province/State','Country/Region','adm0_a3','Lat','Long','1/20','5/20')))

  ud <<- ud[,c(1:6,match(start_date,as.Date(names(ud),'%m/%d/%y')):match(end_date,as.Date(names(ud),'%m/%d/%y')))] %>%
    dplyr::group_by(`Country/Region`,`Province/State`) %>%
    dplyr::filter(Lat != 0 | Long != 0) %>%
    dplyr::group_by(`Country/Region`,`Province/State`) %>%
    dplyr::mutate(Population = sum(Population)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(adm0_a3,`Country/Region`,`Province/State`,Population) %>%
    dplyr::summarize_at(dplyr::vars(ends_with("20")),list(~sum(.)))
    # dplyr::inner_join(geo, by=c('Country/Region','Province/State')) %>%
    # subset(select=-c(Lat,Long))
    # dplyr::select(contains(c('Province/State','Country/Region','adm0_a3','Lat','Long','1/20','5/20')))
  
}

group_data <- function(dataframe) {
  pop_base <- 100000
  dataname <- deparse(substitute(dataframe))
  
  # Record if it has Province/State information
  dataframe <- dataframe %>%
    dplyr::group_by(adm0_a3,`Country/Region`) %>%
    dplyr::mutate(num_states = dplyr::n()
                  ,Population = ifelse(num_states>1 & is.na(`Province/State`),0.0,Population)
                  ) %>%
    dplyr::ungroup()

  if (dataname == 'confirmed') {
    dataframe <- dataframe %>%
      # tidyr::gather(key=Date,value=Confirmed,-'Country/Region',-'Province/State',-adm0_a3,-Lat,-Long) %>%
      tidyr::gather(key=Date,value=Confirmed,-'Country/Region',-'Province/State',-adm0_a3,-Population,-num_states) %>%
      dplyr::mutate(Date = as.Date(Date,format="%m/%d/%y"),
                    Confirmed = dplyr::if_else(is.na(Confirmed),0,Confirmed)) %>%
      # dplyr::filter(Date == max(Date,na.rm=TRUE) | (lubridate::day(Date)%%5) %in% c(0,5)) %>%
      # Calculate differences between rows
      dplyr::group_by(adm0_a3,`Province/State`) %>%
      dplyr::mutate(i_Confirmed = Confirmed - dplyr::lag(Confirmed),
                    Population = max(Population)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(i_Confirmed = dplyr::if_else(is.na(i_Confirmed),Confirmed,i_Confirmed)) %>%
      dplyr::group_by(`Country/Region`,Date,adm0_a3) %>%
      dplyr::mutate('Total Confirmed' = sum(Confirmed)) %>%

      dplyr::ungroup() %>%
      # Calculate differences between rows
      dplyr::group_by(adm0_a3,`Province/State`,Population) %>%
      dplyr::mutate(iTot_Confirmed = `Total Confirmed` - dplyr::lag(`Total Confirmed`)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(iTot_Confirmed = ifelse(is.na(iTot_Confirmed),`Total Confirmed`,iTot_Confirmed),

                    rConfirmed = ifelse(is.na(Population),NA,Confirmed/Population*pop_base),
                    ri_Confirmed = ifelse(is.na(Population),NA,i_Confirmed/Population*pop_base)) %>%
      dplyr::group_by(adm0_a3) %>%
      dplyr::mutate(rTConfirmed = `Total Confirmed`/sum(Population,na.rm=TRUE)*pop_base,
                    riTot_Confirmed = iTot_Confirmed/sum(Population,na.rm=TRUE)*pop_base) %>%
      dplyr::ungroup()

  }
  else if (dataname == 'deaths') {
    dataframe <- dataframe %>%
      # tidyr::gather(key=Date,value=Deaths,-'Country/Region',-'Province/State',-adm0_a3,-Lat,-Long) %>%
      tidyr::gather(key=Date,value=Deaths,-'Country/Region',-'Province/State',-adm0_a3,-Population,-num_states) %>%
      dplyr::mutate(Date = as.Date(Date,format="%m/%d/%y"),
                    Deaths = dplyr::if_else(is.na(Deaths),0,Deaths)) %>%
      # dplyr::filter(Date == max(Date,na.rm=TRUE) | (lubridate::day(Date)%%5) %in% c(0,5)) %>%
      # Calculate differences between rows
      dplyr::group_by(adm0_a3,`Province/State`) %>%
      dplyr::mutate(i_Deaths = Deaths - dplyr::lag(Deaths),
                    Population = max(Population)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(i_Deaths = dplyr::if_else(is.na(i_Deaths),Deaths,i_Deaths)) %>%
      dplyr::group_by(`Country/Region`,Date,adm0_a3) %>%
      dplyr::mutate('Total Deaths' = sum(Deaths)) %>%
      dplyr::ungroup() %>%
      # Calculate differences between rows
      dplyr::group_by(adm0_a3,`Province/State`,Population) %>%
      dplyr::mutate(iTot_Deaths = `Total Deaths` - dplyr::lag(`Total Deaths`)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(iTot_Deaths = ifelse(is.na(iTot_Deaths),`Total Deaths`,iTot_Deaths),

                    rDeaths = ifelse(is.na(Population),NA,Deaths/Population*pop_base),
                    ri_Deaths = ifelse(is.na(Population),NA,i_Deaths/Population*pop_base)) %>%
      dplyr::group_by(adm0_a3) %>%
      dplyr::mutate(rTDeaths = `Total Deaths`/sum(Population,na.rm=TRUE)*pop_base,
                    riTot_Deaths = iTot_Deaths/sum(Population,na.rm=TRUE)*pop_base) %>%
      dplyr::ungroup()
  }

  dataframe <- dataframe %>%
    dplyr::filter(Date > min(Date,na.rm=TRUE))

  return (dataframe)
}

# allow_minus: Whether to allow negative changes of infections and deaths
merge_dataset <- function(wc, wd, kc, kd, uc, ud, allow_minus) {
  # Concontenate Data by type
  confirmed <- bind_rows(wc,kc,uc)
  deaths <- bind_rows(wd,kd,ud)

  # Transform wide-form data to long-form
  confirmed <- group_data(confirmed)
  deaths <- group_data(deaths)

  covid <- cbind(confirmed,c(deaths$Deaths)) %>%
    cbind(c(deaths$`Total Deaths`)) %>%
    cbind(c(deaths$i_Deaths)) %>%
    cbind(c(deaths$iTot_Deaths)) %>%
    cbind(c(deaths$rDeaths)) %>%
    cbind(c(deaths$rTDeaths)) %>%
    cbind(c(deaths$ri_Deaths)) %>%
    cbind(c(deaths$riTot_Deaths)) %>%
    tibble::as_tibble() %>%
    dplyr::rename("Deaths" = "c(deaths$Deaths)",
                  "Total Deaths" = "c(deaths$`Total Deaths`)",
                  "i_Deaths" = "c(deaths$i_Deaths)",
                  "iTot_Deaths" = "c(deaths$iTot_Deaths)",
                  
                  "rDeaths" = "c(deaths$rDeaths)",
                  "rTDeaths" = "c(deaths$rTDeaths)",
                  "ri_Deaths" = "c(deaths$ri_Deaths)",
                  "riTot_Deaths" = "c(deaths$riTot_Deaths)"
                  )
  
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
    dplyr::group_by(adm0_a3,Date,`Country/Region`,`Province/State`,`Total Confirmed`,`Total Deaths`,iTot_Confirmed,iTot_Deaths,riTot_Confirmed,riTot_Deaths,num_states) %>%
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