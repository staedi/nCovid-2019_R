setwd('/Users/minpark/Documents/nCovid-2019')
source('/Users/minpark/Documents/nCovid-2019/preprocess_nCovid-2019.R')

# library(dplyr)
library(ggplot2)
library(leaflet)
library(spdplyr)
library(sf)
# library(sp)

read_spdata <- function() {
    countries_sf <<- sf::read_sf("/Users/minpark/Documents/nCovid-2019/geospatial/ne_50m_admin_0_countries.shp")
    states_sf <<- sf::read_sf("/Users/minpark/Documents/nCovid-2019/geospatial/ne_50m_admin_1_states_provinces.shp")
    china_sf <<- readRDS("/Users/minpark/Documents/nCovid-2019/geospatial/gadm36_CHN_1_sf.rds")
    russia_sf <<- readRDS("/Users/minpark/Documents/nCovid-2019/geospatial/gadm36_RUS_1_sf.rds")
    # gadm_sf <<- bind_rows(china_sf,russia_sf)
}

clean_spdata <- function(spframe) {
    name = deparse(substitute(spframe))
    if (grepl('countries',name)) {
        spframe <- spframe %>%
          dplyr::filter(POP_EST != 0 & ADMIN != 'Antarctica') %>%
          dplyr::select(NAME_EN,ADM0_A3,geometry) %>%
          dplyr::rename("Country/Region" = "NAME_EN",
        "adm0_a3" = "ADM0_A3") %>%
          dplyr::mutate(adm0_a3 = dplyr::if_else(grepl('SDS',adm0_a3),'SSD',adm0_a3))
    }

    else if (grepl('states',name)) {
        spframe <- spframe %>%
          dplyr::select(admin,adm0_a3,name_en,geometry) %>%
          dplyr::rename("Country/Region" = "admin",
          "Province/State" = "name_en")
    }

    else if (grepl('china',name)) {
        spframe <- spframe %>%
          dplyr::select(GID_0,NAME_0,NAME_1,geometry) %>%
          dplyr::rename("adm0_a3" = "GID_0",
                        "Country/Region" = "NAME_0",
                        "Province/State" = "NAME_1") %>%
          dplyr::mutate(`Province/State` = dplyr::if_else(grepl('Mongol',`Province/State`),'Inner Mongolia',`Province/State`),
                        `Province/State` = dplyr::if_else(grepl('Ningxia',`Province/State`),'Ningxia',`Province/State`),
                        `Province/State` = dplyr::if_else(grepl('Xinjiang',`Province/State`),'Xinjiang',`Province/State`),
                        `Province/State` = dplyr::if_else(grepl('Xizang',`Province/State`),'Tibet',`Province/State`))
    }
    
    return (spframe)
}

merge_data <- function(remote='N') {
    # Preprocess Covid data
    read_file(git_path, file_list, remote)
    clean_data()
    # wc <- clean_data(wc,geo)
    # wd <- clean_data(wd,geo)
    # uc <- clean_data(uc,geo)
    # ud <- clean_data(ud,geo)

    covid <<- merge_dataset(wc,wd,uc,ud)

    # Preprocess shape data
    read_spdata()
    countries_sf <<- clean_spdata(countries_sf)
    states_sf <<- clean_spdata(states_sf)
    china_sf <<- clean_spdata(china_sf)

    # Merge data and shape
    countries_cov <<- inner_join(countries_sf, dplyr::filter(covid,merge_type=='Country'), by=c("adm0_a3")) %>%
        dplyr::rename("Country/Region" = "Country/Region.y") %>%
        subset(select=-c(`Country/Region.x`,merge_type))
    states_cov <<- inner_join(states_sf, dplyr::filter(covid,merge_type=='State'), by=c("adm0_a3","Province/State")) %>%
        dplyr::rename("Country/Region" = "Country/Region.y") %>%
        subset(select=-c(`Country/Region.x`,merge_type))
    china_cov <<- inner_join(china_sf, dplyr::filter(covid,merge_type=='China'), by=c("adm0_a3","Province/State")) %>%
        dplyr::rename("Country/Region" = "Country/Region.y") %>%
        subset(select=-c(`Country/Region.x`,merge_type))

    combined_cov <<- rbind(countries_cov,states_cov,china_cov)
    # combined_sf <<- rbind(countries_sf,states_sf)

    # counties_cov <- inner_join(counties_sf, group_by(Covid, countyFIPS) %>%
    #                          summarise(cases=sum(cases)), by=c("county_fips"="countyFIPS"))

    # countries_cov <- inner_join(countries_sf, group_by)

    # countries_cov = countries.merge(data[data['merge_type']=='Country'],on='adm0_a3')
    # countries_cov.rename(columns={'Country/Region_y':'Country/Region'},inplace=True)
    # countries_cov.drop(['Country/Region_x','merge_type'],axis=1,inplace=True)
    # states_cov = states.merge(data[data['merge_type']=='State'],on=['adm0_a3','Province/State'])
    # states_cov.rename(columns={'Country/Region_y':'Country/Region'},inplace=True)
    # states_cov.drop(['Country/Region_x','merge_type'],axis=1,inplace=True)
    # china_cov = china.merge(data[data['merge_type']=='China'],on=['adm0_a3','Province/State'])
    # china_cov.rename(columns={'Country/Region_y':'Country/Region'},inplace=True)
    # china_cov.drop(['Country/Region_x','merge_type'],axis=1,inplace=True)

    # # mapped = pd.concat([countries,states,china,us],sort=False)
    # mapped = countries
    # mapped_cov = pd.concat([countries_cov,states_cov,china_cov],sort=False)
    # return countries, states, china, mapped, mapped_cov
    # return (combind_cov)
}

plot_choropleth <- function() {
    combined_cov %>%
    dplyr::filter(Date == max(Date,na.rm=TRUE)) %>%
    ggplot2::ggplot() +
    ggplot2::geom_sf(mapping = aes(fill = `Confirmed`), color = NA) +
    ggplot2::geom_sf(data = countries_sf, fill = NA, color = "gray", size = 0.25) +

    # ggplot2::geom_sf(data = states, fill = NA, color = "black", size = 0.25) +
    ggplot2::coord_sf(datum = NA) +
    ggplot2::scale_fill_gradient(trans = "log", low='yellow', high='red',
                      na.value="white", breaks=c(1, max(combined_cov$`Confirmed`))) +
    ggplot2::theme_bw() + ggplot2::theme(legend.position="bottom", panel.border = element_blank())

}

plot_leaflet <- function(data) {
  data <- data %>%
    dplyr::filter(Date == max(Date,na.rm=TRUE))

  data$popup_text <- paste0(
      paste0("<strong>Date:</strong> ",data$Date),
      paste0("<br /><strong>Country/Region:</strong> ",data$`Country/Region`),
      ifelse(is.na(data$`Province/State`),"",paste0("<br /><strong>Province/State:</strong> ",data$`Province/State`)),
      paste0("<br /><strong>Confirmed:</strong> ",prettyNum(data$`Confirmed`,big.mark=','),
      " / ",prettyNum(data$`Total Confirmed`,big.mark=',')),
      paste0("<br /><strong>Deaths:</strong> ",prettyNum(data$`Deaths`,big.mark=','),
      " / ",prettyNum(data$`Total Deaths`,big.mark=','))
  ) %>% lapply(htmltools::HTML)

  pal <- leaflet::colorNumeric(
      "YlOrRd",
      data$Confirmed
  )

  map <- data %>%
    # dplyr::filter(Date == max(Date,na.rm=TRUE)) %>%
    leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::addPolygons(
        fillColor = ~pal(data$Confirmed),
        weight = 2,
        opacity = 1,
        color = 'white',
        dashArray = '3',
        fillOpacity = 0.7,
        label = ~data$popup_text,
        labelOptions = labelOptions(
            style = list("font-weight" = "normal",
            padding = "3px 8px"),
            textsize = "13px",
            direction = "auto"),
        highlight = highlightOptions(
            weight = 2,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE)
            ) %>%
    leaflet::addLegend(pal = pal,
    values = ~data$Confirmed,
    opacity = 0.7,
    title = 'Confirmed (Country & State Level)',
    position = 'bottomright')

    map
}



plot_heatmap <- function(covid, type) {
    if (type == 'global') {
        cand <- covid %>%
            dplyr::filter(Date==max(Date,na.rm=TRUE)) %>%
            dplyr::group_by(Date) %>%
            dplyr::distinct(adm0_a3,`Country/Region`,`Total Confirmed`,`Total Deaths`) %>%
            dplyr::mutate(rn_tc=dplyr::dense_rank(desc(`Total Confirmed`)),rn_td=dplyr::dense_rank(desc(`Total Deaths`))) %>%
            dplyr::filter(rn_tc<=35) %>%
            dplyr::ungroup() %>%
            dplyr::select(adm0_a3)

        cand <- as.vector(cand$adm0_a3)
    }
    else {
        if (type == 'US') {
            cand <- covid %>%
                dplyr::filter(adm0_a3 == 'USA' & Date==max(Date,na.rm=TRUE)) %>%
                dplyr::select(`Province/State`)
                }

        if (type == 'CAN') {
            cand <- covid %>%
                dplyr::filter(adm0_a3 == 'CAN' & Date==max(Date,na.rm=TRUE)) %>%
                dplyr::select(`Province/State`)
                }

        if (type == 'AUS') {
            cand <- covid %>%
                dplyr::filter(adm0_a3 == 'AUS' & Date==max(Date,na.rm=TRUE)) %>%
                dplyr::select(`Province/State`)
                }

        if (type == 'china') {
            cand <- covid %>%
                dplyr::filter(adm0_a3 == 'CHN' & Date==max(Date,na.rm=TRUE)) %>%
                dplyr::select(`Province/State`)
                }

        cand <- as.vector(cand$`Province/State`)

    }

    ratio <- length(unique(covid$Date))/length(cand)

    textcol <- "grey40"


    if (type == 'global') {
        lvl <- covid %>%
            dplyr::filter(adm0_a3 %in% cand & Date==max(Date,na.rm=TRUE)) %>%
            dplyr::arrange(`Total Confirmed`) %>%
            dplyr::distinct(`Country/Region`)

        lvl <- as.vector(lvl$`Country/Region`)

        min_val <- min(covid[covid$adm0_a3 %in% cand,'Total Confirmed'])
        max_val <- max(covid[covid$adm0_a3 %in% cand,'Total Confirmed'])

        htmap <- covid %>%
            dplyr::filter(adm0_a3 %in% cand) %>%
            dplyr::arrange(`Total Confirmed`) %>%
            dplyr::mutate(`Country/Region` = factor(`Country/Region`, levels = lvl)) %>%
            ggplot2::ggplot(mapping=aes(x=Date,y=`Country/Region`)) +
            ggplot2::geom_tile(aes(fill=`Total Confirmed`),color='white',size=0.25) +
            ggplot2::labs(x="",y="",
            title=paste0("nCovid-2019 Status as of ",max(covid$Date,na.rm=TRUE)),
            subtitle="Cumulative infections by country",
            caption="Source: Johns Hopkins University")+
            ggplot2::scale_y_discrete(expand=c(0,0))+
            ggplot2::scale_fill_gradient(low='lightgray',high='steelblue',labels=scales::comma,breaks=seq(min_val,max_val,(max_val-min_val)%/%5)) +
            ggplot2::guides(fill = ggplot2::guide_legend(reverse=TRUE)) +
            # ggplot2::coord_fixed(ratio=ratio) +
            ggplot2::scale_x_date(expand=c(0,0))+
            ggplot2::theme_grey(base_size=8) +
            ggplot2::theme(aspect.ratio = 1,
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

    else {
        lvl <- covid %>%
            dplyr::filter(`Province/State` %in% cand & Date==max(Date,na.rm=TRUE)) %>%
            dplyr::arrange(Confirmed) %>%
            dplyr::distinct(`Province/State`)

        lvl <- as.vector(lvl$`Province/State`)

        min_val = min(covid[covid$`Province/State` %in% cand,'Confirmed'])
        max_val = max(covid[covid$`Province/State` %in% cand,'Confirmed'])

        htmap <- covid %>%
            dplyr::filter(`Province/State` %in% cand) %>%
            dplyr::arrange(Confirmed) %>%
            dplyr::mutate(`Province/State` = factor(`Province/State`, levels = lvl)) %>%
            ggplot2::ggplot(mapping=aes(x=Date,y=`Province/State`)) +
            ggplot2::geom_tile(aes(fill=`Confirmed`),color='white',size=0.25) +
            ggplot2::labs(x="",y="",
            title=paste0(unique(covid[covid$`Province/State` %in% cand,'Country/Region'])," nCovid-2019 Status as of ",max(covid$Date,na.rm=TRUE)),
            subtitle="Cumulative infections by State/Province",
            caption="Source: Johns Hopkins University")+
            ggplot2::scale_y_discrete(expand=c(0,0))+
            ggplot2::scale_fill_gradient(low='lightgray',high='steelblue',labels=scales::comma,breaks=seq(min_val,max_val,(max_val-min_val)%/%5)) +
            ggplot2::guides(fill = ggplot2::guide_legend(reverse=TRUE)) +
            ggplot2::coord_fixed(ratio=ratio) +
            ggplot2::scale_x_date(expand=c(0,0))+
            ggplot2::theme_grey(base_size=8) +
            ggplot2::theme(aspect.ratio = 1,
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
    }

    ggplot2::ggsave(htmap,filename=paste0("saved/heatmap_",type,"_",max(covid$Date,na.rm=TRUE),".png"),dpi=200)
    htmap
}

merge_data()
maps <- plot_leaflet(combined_cov)
g_ht <- plot_heatmap(covid,'global')
us_ht <- plot_heatmap(covid,'US')
can_ht <- plot_heatmap(covid,'CAN')
aus_ht <- plot_heatmap(covid,'AUS')
china_ht <- plot_heatmap(covid,'china')
