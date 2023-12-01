library(shiny)
library(tidyverse)
library(shinydashboard)
library(sf)
library(leaflet)
library(DT)
library(scales)
library(formattable)
library(geojsonio)
library(htmltools)
library(lwgeom)
library(readxl)
library(shinycssloaders)
library(tmap)
library(tidyr)
library(shinyjs)
library(foreign)
library(rmarkdown)
library(knitr)
library(formattable)
library(leafem)
library(tools)
library(leafgl)
library(mapview)
library(rmapshaper)
library(leaflet.esri)
library(readxl)
library(shinyalert)
library(vctrs)
library(httr)
library(jsonlite)
#### load data ####

#read_csv("H:\\scratch\\Forecast_2045_PIA_Zones.csv")

#pm_sf<-st_read(dsn=".", "parcel_master_5_25_21") %>%
  #st_transform(crs=4326) 

mitigation_spreadsheet <- read_excel("Review of CAPCOA Mitigations.xlsx", sheet="Include")

#zipF<- "pm_6_7_21.zip"
#outDir<-getwd()
#unzip(zipF,exdir=outDir)

url <- parse_url("https://maps.trpa.org/server/rest/services/")
url$path <- paste(url$path, "Parcels/FeatureServer/0/query", sep = "/")
url$query <- list(where = "1=1",
                  outfields = 'APN',
                  returnGeometry = "true",
                  f = "geojson")
request <- build_url(url)

parcels <- st_read(request)



pb_sf<- parcels %>% st_as_sf(crs=4326)%>%
  st_transform(crs=4326) 

#pb_sf<-st_read(dsn=".", "pm_6_7_21") %>%
#  st_transform(crs=4326) 

sf_use_s2(FALSE)
#simple_pm <- rmapshaper::ms_simplify(input = as(pb_sf %>% select(APN), 'Spatial')) %>%
  #st_as_sf()


#simple_pb <- rmapshaper::ms_simplify(input = as(pb_sf %>% select(APN), 'Spatial')) %>%
#  st_as_sf()

#simple_pb_cast <- st_cast(simple_pb,"POLYGON")

#leaflet() %>% addPolygons(data=simple_pb_cast, group="Parcels") %>% addTiles() %>% groupOptions("Parcels", zoomLevels = 15:20) 

#leaflet() %>% addEsriFeatureLayer(url="https://maps.trpa.org/server/rest/services/Parcels/MapServer/0") %>% addTiles()

#pm<-read.dbf("H:\\model\\project_level_analysis\\parcel_master2.dbf")

#pm %>% mutate(BEDROOMS=as.character(BEDROOMS)) %>% filter(TRPA_LANDU %in% c("Single Family Residential")) %>% 
 # filter(!BEDROOMS %in% c("<NA>","0","N/A","4   *","<NA>"))  %>%
 # filter(!is.na(BEDROOMS)) %>%
 # mutate(BEDROOMS=as.numeric(BEDROOMS)) %>%
 # summarise(avg_bed=mean(BEDROOMS, na.rm=T))
feature_service_return<- function(fs_base_url, fs_url){
  url <- parse_url(fs_base_url)
  url$path <- paste(url$path, fs_url, sep = "/")
  url$query <- list(where = "1=1",
                    outFields = "*",
                    returnGeometry = "true",
                    f = "geojson")
  request <- build_url(url)
  
  spatial_df <- st_read(request)
  
  
  
  spatial_df<- spatial_df %>% st_as_sf(crs=4326)%>%
    st_transform(crs=4326) 
  return(spatial_df)
}

trpa_rest_service_url = "https://maps.trpa.org/server/rest/services/"
boundary_fs_url = "Boundaries/FeatureServer/4/query"
jurisdition_fs_url = "Boundaries/FeatureServer/10/query"

boundary<-feature_service_return(trpa_rest_service_url, boundary_fs_url)

#Our jurisdiction feature service isn't clipped to the basin - should we deal with this or just use the static one?
#jurisdictions<-feature_service_return(trpa_rest_service_url, jurisdition_fs_url)

#jur<-jurisdictions%>%
#  mutate(name=case_when(JURISDICTION == "City of South Lake Tahoe" ~ "South Lake Tahoe",
#                        JURISDICTION == "Carson City" ~ "Carson City County",
#                        TRUE ~ as.character(paste(toTitleCase(tolower(JURISDICTION)), "County", sep=" "))))

#COMMENT ONE OF THESE TWO OUT
jur <- st_read(".", "jurisdictions")%>% st_as_sf()  %>%
  st_transform(crs=4326) %>%
  mutate(name=case_when(COUNTY == "CSLT" ~ "South Lake Tahoe", 
                        COUNTY == "CARSON" ~ "Carson", 
                        TRUE ~ as.character(paste(toTitleCase(tolower(COUNTY)), "County", sep=" "))))





trip_rates<- read_excel("Commercial_Assessment_MC_2.0.xlsx", sheet="list_clean1") %>%
  filter(!use %in% c("Single-Family Detached","Senior Adult Housing – Attached","Congregate Care Facility (Residential Care)", "Multi-Family (low-rise, one or two levels)")) %>%
  filter(!is.na(Rate))


url <- parse_url("https://maps.trpa.org/server/rest/services/")
url$path <- paste(url$path, "Transportation_Planning/MapServer/9/query", sep = "/")
url$query <- list(where = "1=1",
                  outFields = "*",
                  returnGeometry = "true",
                  f = "geojson")
request <- build_url(url)

pia_zones <- st_read(request)

pia_zones<-pia_zones %>%
  mutate(zone_no=extract_numeric(zone_id))

res_data<-read_csv("residential_data.csv") %>%
  select(-geometry) %>%
  mutate(sos=jur_total_vmt_capita*.85)

res_data_sf<-pia_zones %>% left_join(res_data, by="zone_id")%>%
  select(zone_id,zone_vmt_per_capita, jur_total_vmt_capita, percent_threshold,cat,sos, COUNTY, zone_vmt,zone_pop)

res_data_full<-read_csv("residential_data_full_length.csv") %>%
  #select(-geometry) %>%
  mutate(sos=jur_total_vmt_capita*.85)

res_data_sf_full<-pia_zones %>% left_join(res_data_full, by="zone_id")%>%
  select(zone_id,zone_vmt_per_capita, jur_total_vmt_capita, percent_threshold,cat,sos, COUNTY, zone_vmt,zone_pop)

#write.csv(res_data_sf %>% data.frame() %>% select(-c(geometry)), "residential_table.csv", row.names=F)

#tau_data<-read_csv("tau_data.csv") %>%
 # select(-geometry)

#tau_data_sf<-pia_zones %>% left_join(tau_data, by="zone_id")

length_data<-read_csv("trip_length_data.csv")%>%
  select(-geometry)

length_data_sf<-pia_zones %>% left_join(length_data, by="zone_id") 

length_data_full<-read_csv("trip_length_data_full_length.csv")%>%
  select(-geometry)

length_data_sf_full<-pia_zones %>% left_join(length_data_full, by="zone_id") 

length_jur_full<-st_join(st_buffer(length_data_sf_full,.00001), st_buffer(jur %>% dplyr::select("COUNTY"),.00001), largest=T) %>%
  mutate(COUNTY=case_when(zone_id=="Zone 21" ~ "CSLT",
                          TRUE ~as.character(COUNTY)),
         jur_average=case_when(COUNTY=="CARSON" ~ 25.4,
                               COUNTY=="CSLT" ~ 6.35,
                               COUNTY=="DOUGLAS" ~ 11.4,
                               COUNTY=="EL DORADO" ~ 10,
                               COUNTY=="PLACER" ~ 13.2,
                               COUNTY=="WASHOE" ~ 12),
         threshold_15_below = jur_average * .85,
         percent_threshold=avg_zone_trip_length/threshold_15_below,
         cat=case_when(percent_threshold < .5 ~ "Less than 50% of threshold",
                       between(percent_threshold,.5, .75) ~ "50-75% of threshold",
                       between(percent_threshold,.751, .99999) ~ "75-100% of threshold",
                       between(percent_threshold,1, 1.25) ~ "100-125% of threshold",
                       between(percent_threshold,1.251, 1.5) ~ "125-150% of threshold",
                       percent_threshold > 1.5 ~ "Over 150% of threshold"))

length_jur<-st_join(st_buffer(length_data_sf,.00001), st_buffer(jur %>% dplyr::select("COUNTY"),.00001), largest=T) %>%
  mutate(COUNTY=case_when(zone_id=="Zone 21" ~ "CSLT",
                          TRUE ~as.character(COUNTY)),
         jur_average=case_when(COUNTY=="CARSON" ~ 13.1,
                               COUNTY=="CSLT" ~ 4.26,
                               COUNTY=="DOUGLAS" ~ 6.37,
                               COUNTY=="EL DORADO" ~ 6.69,
                               COUNTY=="PLACER" ~ 6.51,
                               COUNTY=="WASHOE" ~ 5.56),
         threshold_15_below = jur_average * .85,
         percent_threshold=avg_zone_trip_length/threshold_15_below,
         cat=case_when(percent_threshold < .5 ~ "Less than 50% of threshold",
                       between(percent_threshold,.5, .75) ~ "50-75% of threshold",
                       between(percent_threshold,.751, .99999) ~ "75-100% of threshold",
                       between(percent_threshold,1, 1.25) ~ "100-125% of threshold",
                       between(percent_threshold,1.251, 1.5) ~ "125-150% of threshold",
                       percent_threshold > 1.5 ~ "Over 150% of threshold"))


#write.csv(length_jur %>% data.frame() %>% select(-c(geometry)), "trip_length_table.csv", row.names=F)

length_jur$cat1 <- factor(length_jur$cat, levels=c("Over 150% of threshold",
                                                   "125-150% of threshold",
                                                   "100-125% of threshold",
                                                   "75-100% of threshold",
                                                   "50-75% of threshold",
                                                   "Less than 50% of threshold"))

pal_length_jur <- colorFactor(ordered = T,
                              palette = c('#6B0402','#EE3835','#FAAAA9',   '#A1CDF4' ,'#0679DF','#033663'),
                              levels=levels(length_jur$cat1))

popup_length_jur = paste0('<strong>',"Zone: ", '</strong>',length_jur$zone_id,"<br>",
                          '<strong>',"Jurisdiction: ", '</strong>',length_jur$COUNTY,"<br>",
                          '<strong>',"Avg Zone Trip Length: ", '</strong>',round(length_jur$avg_zone_trip_length,2),"<br>",
                          '<strong>',"Jurisdiction Avg Trip Length: ", '</strong>',round(length_jur$jur_average,2),"<br>",
                          '<strong>',"Jurisdiction Threshold (15% Below Regional Avg): ", '</strong>',format(length_jur$threshold_15_below,digits=3, big.mark=","),"<br>",
                          '<strong>',"Zone Percent of Threshold (15% Below Regional Avg): ", '</strong>',percent(length_jur$percent_threshold,digits=0),"<br>",
                          '<strong>',"Zone Category: ", '</strong>',length_jur$cat
)

res_data_sf$cat1 <- factor(res_data_sf$cat, levels=c("Over 150% of threshold",
                                                     "125-150% of threshold",
                                                     "100-125% of threshold",
                                                     "75-100% of threshold",
                                                     "50-75% of threshold",
                                                     "Less than 50% of threshold"))

pal <- colorFactor(ordered = T,
                   palette = c('#6B0402','#EE3835','#FAAAA9',   '#A1CDF4' ,'#0679DF','#033663'),
                   levels=levels(res_data_sf$cat1))

popup = paste0('<strong>',"Zone: ", '</strong>',res_data_sf$zone_id,"<br>",
               '<strong>',"Jurisdiction: ", '</strong>',res_data_sf$COUNTY,"<br>",
               '<strong>',"Zone VMT: ", '</strong>',formatC(res_data_sf$zone_vmt,digits=0, big.mark=","),"<br>",
               '<strong>',"Zone Residential Population: ", '</strong>',formatC(res_data_sf$zone_pop,digits=0, big.mark=","),"<br>",
               '<strong>',"Avg Zone VMT Per Resident: ", '</strong>',format(res_data_sf$zone_vmt_per_capita,digits=3, big.mark=","),"<br>",
               '<strong>',"Jurisdiction Avg VMT Per Capita: ", '</strong>',format(res_data_sf$jur_total_vmt_capita,digits=4, big.mark=","),"<br>",
               '<strong>',"Jurisdiction Threshold (15% Below Regional Avg): ", '</strong>',format(res_data_sf$sos,digits=3, big.mark=","),"<br>",
               '<strong>',"Zone Percent of Threshold (15% Below Regional Avg): ", '</strong>',percent(res_data_sf$percent_threshold,digits=0),"<br>",
               '<strong>',"Zone Category: ", '</strong>',res_data_sf$cat
)

#tc_shape <- geojson_read("https://opendata.arcgis.com/datasets/85a2e8e4bf994742a5855c1339517681_4.geojson", what="sp") %>%
 # st_as_sf(crs=4326)

tc_half<-st_read("VMT_Analysis.gdb","Town_center_Buffer_half_mile") %>% st_transform(crs=4326) %>%
  filter(!Name %in% c("NEVADA SOUTH STATELINE RESORT AREA","STATELINE/SKI RUN"))
tc_half$area <- st_area(tc_half)
tc_half <-tc_half %>%
  summarise(area = sum(area))

regional_half <-st_read("VMT_Analysis.gdb","Town_center_Buffer_half_mile") %>% st_transform(crs=4326) %>% filter(Name %in% c("NEVADA SOUTH STATELINE RESORT AREA","STATELINE/SKI RUN"))

regional_half$area <- st_area(regional_half)

regional_half <-regional_half %>%
  summarise(area = sum(area))

town_reg_half <- rbind(regional_half %>% select(-area), tc_half %>% select(-area))
town_reg_half$area <- st_area(town_reg_half)
town_reg_half <-town_reg_half %>%
  summarise(area = sum(area))

bike_buff<-st_read(".","bike_path_half_buffer") %>% st_transform(crs=4326)

walk_buff<-st_read(".","walk_quarter_buffer") %>% st_transform(crs=4326)

buffer<-st_union(walk_buff, tc_half)
buffer  <- st_difference(buffer, st_combine(regional_half)) %>% select(area) %>%
  rename(Shape=geometry)

center_bike_buffer<-rbind(town_reg_half, buffer)
center_bike_buffer$area1 <- st_area(center_bike_buffer)
center_bike_buffer <-center_bike_buffer %>%
  summarise(area1 = sum(area1))

#st_write(buffer_parcel_final, dsn=".","buffer_parcel_final", driver="ESRI Shapefile")

buffer_parcel_final<-st_read(dsn=".","buffer_parcel_final")

#a<-leaflet(buffer_parcel_final) %>% addTiles() %>% addPolygons(data=town_reg_half,  fill=F, color="red")%>% 
 # addPolygons(stroke=F, fillOpacity = .7, )

#library(htmlwidgets)
#saveWidget(a, file="tc_buffer_&_parcels.html")

#tc_half  <- st_difference(tc_half, st_combine(regional_half))

tc_walk<-st_read("VMT_Analysis.gdb","Town_center_Walkshed") %>% st_transform(crs=4326)
tc_walk$area <- st_area(tc_walk)
tc_walk <-tc_walk %>%
  summarise(area = sum(area))

transit_buffer<-sf::st_read(".","transit_buffer_2019") %>% st_as_sf() %>%
  sf::st_transform(crs=4326)
transit_buffer <- st_intersection(transit_buffer, boundary)

bonus_boundary <-sf::st_read(".","bonus_unit_boundary") %>% st_as_sf() %>%
  sf::st_transform(crs=4326)
bonus_boundary <- st_intersection(bonus_boundary, boundary)


length_data_sf$cat1 <- factor(length_data_sf$cat, 
                              levels=c("under 4 mi",
                                       "4-6 mi",
                                       "6-8 mi",
                                       "8-10 mi",
                                       "10-12 mi",
                                       "above 12 mi"))



pal1 <- colorFactor(ordered = T,
                    palette = c('#033663','#0679DF',   '#A1CDF4','#FAAAA9','#EE3835','#6B0402'),
                    levels=levels(length_data_sf$cat1))

popup1 = paste0('<strong>',"Zone: ", '</strong>',length_data_sf$zone_id,"<br>",
                '<strong>',"Zone Avg Trip Length: ", '</strong>',format(length_data_sf$avg_zone_trip_length,digits=2, big.mark=",")
)

commercial <- trip_rates %>% filter(!use %in% c("Hotel","Motel","Timeshare")) %>% distinct(use) %>% pull()

dataset <- length_jur %>% select(zone_id, zone_no, avg_zone_trip_length,threshold_15_below ) %>%
                     left_join(res_data_sf %>% data.frame() %>% select(zone_id,zone_vmt_per_capita,  sos), by="zone_id")

# table<-dataset_full %>% data.frame() %>% select(-c(zone_no, geometry)) %>%
#   mutate(avg_zone_trip_length=round(avg_zone_trip_length,2),
#          threshold_15_below=round(threshold_15_below,2),
#          zone_vmt_per_capita=round(zone_vmt_per_capita,2),
#          sos=round(sos,2)) %>%
#   rename( `Zone`=zone_id,
#                            `Avg Zone Trip Length`= avg_zone_trip_length,
#                    `Trip Length Standard of Significance`= threshold_15_below,
#                    `Avg Zone Residential VMT Per Capita` = zone_vmt_per_capita,
#                    `Residential Standard of Significance`= sos)
# 
# table %>% formattable()
# 
# write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
#   write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
# }
# 
# write.excel(table)

dataset_full <- length_jur_full %>% select(zone_id, zone_no, avg_zone_trip_length,threshold_15_below ) %>%
  left_join(res_data_sf_full %>% data.frame() %>% select(zone_id,zone_vmt_per_capita,  sos), by="zone_id")

tau_types <- c("Hotel", "Motel","Timeshare")

commercial_types <- c("Auto Parts and Service Center","Building Materials/Lumber","Drinking Place","Drive-In Bank","Fast Food Restaurant","Free-Standing Discount Store","Furniture Store","General retail","Health and Fitness Club","High Turnover Sit-Down Restaurant (<1 hr. turnover)","Hospital","Marina","Movie Theater (traditional)","Pharmacy/Drugstore","Quality Restaurant (>1 hr. turnover)","Supermarket")

office_types<- c("General Office Building (GFA of more than 5,000 sf)", "Medical –Dental Office Building","Warehouse","Light industrial" )

rec_types<- c("Golf Course","Marina","Health and Fitness Club","Public Park","Bowling Alley")

pub_serv_types <- c("Hospital", "Library","High School","Middle School/Junior High School","University/College","Recreational Community Center","Private School (K-12)","Church","Daycare Center","Elementary School")


#### --------------------------- ####

ui <- dashboardPage(skin="black", 
  dashboardHeader(title="Project Impact Analysis (PIA) Tool", titleWidth = 550),
  dashboardSidebar(width=300,
                   sidebarMenu(id = "tab",
                     menuItem("PIA Tool",
                              tabName = "trpa",icon = icon("chart-area")),
                     menuItem("User Guidelines",
                              tabName = "guide",icon = icon("info"))),
                   box(width=12,background="black",
                                 img(src='zoom_background_50th.jpg',  height = 250, width = 250),
                                 p(tags$br(),HTML("The tool provides initial screening for all project types and more detailed analysis for residential, tourist accommodation unit, and public service projects.  All non-screened commercial, recreation, and other projects will need to complete a more detailed transportation analysis.<br> <br> Follow the steps below to analyze your project. For detailed information on the PIA  framework, tool usage, and calculations select the User Guidelines tab. For questions about the project impact assessment process contact Michelle Glickert (mglickert@trpa.gov). For technical issues with the tool contact Josh Schmid (jschmid@trpa.gov). <br> <br>
                        1) Select your project type from the dropdown.<br>
                        2) Click on your project location on the map. <br>
                        3) Enter the number of proposed units<br>
                        4) Select mitigation strategies if applicable 
                        5) View the summary data at the bottom of the screen or click Export Project Info to download the information."))
  )),
  dashboardBody(tags$style(type="text/css",
                            ".shiny-output-error { visibility: hidden; }",
                            ".shiny-output-error:before { visibility: hidden;}" ,
  HTML("
      .skin-black .main-sidebar { background-color: #000000;}")),
  tabItems(
    tabItem(tabName = "trpa",
   fluidRow(useShinyjs(),
            useShinyalert(),
            div(id="reset_div", column(width=2,
           fluidRow(box(
            selectInput("analysis_type", "Analysis Type",choices=c("TRPA"#, "SB-743"
                                                                   ), selected="TRPA"), 
            textInput("proj_name","Proposed Project Name"),
            selectInput("proj_type", "Proposed Project Type",choices=c(sort(c("Mixed-Use",
                                                                               "Residential (Market-Rate)","Residential (Affordable)","Unique Project Type" ,unique(trip_rates$use))))), 
            uiOutput("custom_type"),
            uiOutput("ui_project_type"),
            uiOutput("ui_land_use1"),
            uiOutput("custom_type1"),
            uiOutput("custom_rate1"),
            uiOutput("mixed_units_1"),
           # uiOutput("ui_low_income1"),
            uiOutput("ui_mitigations1"),
            uiOutput("emp_eligible1"),
            uiOutput("emp_vmt_percent1"),
            uiOutput("ui_land_use2"),
           uiOutput("custom_type2"),
           uiOutput("custom_rate2"),
            uiOutput("mixed_units_2"),
           # uiOutput("ui_low_income2"),
            uiOutput("ui_mitigations2"),
            uiOutput("emp_eligible2"),
            uiOutput("emp_vmt_percent2"),
            uiOutput("ui_land_use3"),
           uiOutput("custom_type3"),
            uiOutput("custom_rate3"),
            uiOutput("mixed_units_3"),
           # uiOutput("ui_low_income3"),
            uiOutput("ui_mitigations3"),
            uiOutput("emp_eligible3"),
            uiOutput("emp_vmt_percent3"),
            uiOutput("custom_rate"),
           # uiOutput("ui_low_income"),
            uiOutput("ui_redevelopment"),
            uiOutput("ui_redev_land_use"),
            uiOutput("ui_redev_units"),
           uiOutput("custom_rate_redev"),
            uiOutput("ui_mitigations"),
            uiOutput("emp_eligible"),
            uiOutput("emp_vmt_percent"),
            downloadButton("report_button", "Export Project Info") %>% withSpinner(),
            radioButtons('format', '', c('PDF', 'HTML', 'Word'),inline = TRUE),
            width=12))),
    column(width=10,
           fluidRow(
             box(title="Select Project Location",
                 leafletOutput("map", height=700) %>% withSpinner(), width=12)),
           uiOutput("ui_land_use_1"),
           uiOutput("ui_land_use_2"),
           uiOutput("ui_land_use_3"),
           uiOutput("ui_tot_mixed_use"),
           fluidRow(box(actionButton("resetAll", "Clear Tool Info"), 
                        width=2))
    )
    ))),
   tabItem(tabName = "guide",
           box(width = 9, status = 'info', solidHeader = TRUE, title = "",
               uiOutput("guidelines"))
           )
   ))
)
server <- function(input, output,session) {
  observeEvent(input$resetAll, {
    reset("reset_div")
  })
  #observeEvent(input$analysis_type, {
   # reset("reset_div")
 # })
  #observeEvent(input$resetAll_743, {
    #reset("reset_div_743")
 # })
 # observeEvent(input$tab == "ed_cslt", {
    #proj_loc() <- NULL
  #})
  showModal(modalDialog(title = "TRPA Project Impact Analysis Tool",
                        HTML("<b>Tool Introduction </b><br><br> This tool provides initial screening for all project types and more detailed analysis for residential, tourist accommodation unit, and public service projects.  All non-screened commercial, recreation, and other projects will need to complete a more detailed transportation analysis. The tool is for projects that are generally consistent in size and land use type (i.e., density, mix of uses, transit accessibility, etc.) with the surrounding built environment to where the project is located. For questions about the project impact assessment process contact Melanie Sloan (msloan@trpa.gov). For technical issues with the tool contact Josh Schmid (jschmid@trpa.gov). <br><br>
<b>Tool Purpose </b><br><br>
This tool is intended to be used for the following purposes. <br><br>
1) Project screening
2) VMT analysis for residential, tourist accommodation, and public service projects that do not meet any screening criteria
3) Identification of appropriate VMT mitigations for the above project types projects that exceed VMT standards of significance
4) Calculation of mobility mitigation fees
<br><br>
<b>Basic Tool Usage </b><br><br>
1) Select your project type from the dropdown.<br>
2) Select your project location on the map. <br>
3) Enter the number of proposed units<br>
4) Select mitigation strategies if applicable<br>
5) View the summary data at the bottom of the screen or click Export Project Info to download the information <br>
* For more detailed instructions, see the user guidelines on the left<br><br
* This application is best viewed on desktop browsers and has not been optimised for mobile devices.<br><br>
<b>Terms of Use </b><br><br>
TRPA developed this tool to provide initial screening for residential, tourist accommodation unit, and public service project’s impact assessment for office, industrial, and unique project types should be conducted outside of the tool. All non-screened commercial, recreation, and other projects will need to complete a more detailed transportation analysis. By accessing this Tool, you agree to the following:<br><br>
By clicking “Accept” below and/or using the Online Project Impact Assessment Tool, you agree to the terms of this Agreement.<br><br>
TRPA shall retain all rights, titles, and interests in the Tool and its content, as well as all inventions, improvements, discoveries, methodologies, models, formats, software, algorithms, processes, procedures, designs, specifications, findings, other intellectual properties developed, gathered, compiled, or produced by TRPA in developing the Tool, or which is otherwise incorporated in the Tool. TRPA grants you only a limited, non-exclusive, non-transferable, royalty-free, license to review, promote, use, disclose, create outputs from the Tool, but only in order to evaluate the Tool’s functionality and capabilities. TRPA retains rights to all of the data contained in this tool.<br><br>
You use the Tool at your own risk. TRPA makes no promise and gives no guarantee regarding the accuracy, completeness, reliability, or functionality of the Tool. The Tool should not be used in the performance of your professional services or for any other purpose than initial screening for all project types and more detailed analysis for some project types. TRPA does not assume responsibility for any impact that the use of the Tool may have on your or any third party’s operational environment or CPU performance. TRPA does not promise that the Tool, TRPA’s server, or TRPA’s other electronic, digital, computing, or communication system, is free of bugs, viruses, Trojan horses, or other harmful, unlawful, or malicious materials. TRPA may change the data, functionality, interface, or availability of the Tool at any time in its sole discretion, but TRPA does not promise to update or debug the Tool.<br><br>
THE TOOL AND ANY ASSOCIATED DOCUMENTATION ARE MADE AVAILABLE “AS IS,” WITHOUT ANY WARRANTY OF ANY KIND. THERE IS NO WARRANTY THAT THE TOOL OR DOCUMENTATION WILL MEET YOUR EXPECTATIONS OR REQUIREMENTS, OR THAT THE OPERATION OF THE TOOL WILL BE UNINTERRUPTED OR ERROR-FREE, OR THAT THE TOOL WILL FUNCTION AS DESCRIBED IN THE DOCUMENTATION. TRPA HEREBY DISCLAIM ANY AND ALL EXPRESS OR IMPLIED WARRANTIES, INCLUDING WITHOUT LIMITATION, ANY WARRANTY OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, TITLE, OR INFRINGEMENT. IN NO EVENT WILL TRPA HAVE ANY LIABILITY TO YOU OR ANY OTHER PARTY FOR ANY LOSSES OR DAMAGES YOU MAY SUFFER DIRECTLY OR INDIRECTLY, OR FOR ANY EXPENSE OR COSTS YOU MAY INCURRED, IN CONNECTION WITH THE USE OF THE TOOL. TRPA SHALL HAVE NO LIABILITY TO YOU OR ANY OTHER PARTY FOR LOST DATA, LOST PROFITS, BUSINESS INTERUPTION, TIME, MONEY, GOODWILL AND ANY SPECIAL, INCIDENTAL, INDIRECT OR CONSEQUENTIAL DAMAGES, WHETHER IN CONTRACT, TORT, STRICT LIABILITY OR OTHERWISE, ARISING OUT OF THE USE OR INABILITY TO USE THE TOOL, EVEN IF YOU HAVE ADVISED TRPA OF THE POSSIBILITY OF SUCH LOSS OR DAMAGES. YOUR SOLE AND EXCLUSIVE REMEDY SHALL BE TO TERMINATE THE USE OF THE TOOL.<br><br>
Furthermore, in accessing TRPA’s website, server, or on the Tool, you agree not to:<br>
1. access any other website or web content through TRPA’s server;<br>
2. decompile, or reverse engineer any portion of the Tool or any other content on TRPA’s server;<br>
3. attempt unauthorized access to the Tool, TRPA’s server, or any other content, on TRPA’s server, or in any way interfere with their access control devices, integrity, or performance;<br>
4. perform load, technical security, or other vulnerability testing on any content on TRPA’s server other than the Tool;<br>
5. store, transmit, or produce libelous, slanderous, lewd, or lascivious, viruses, Trojan horses, or other harmful, unlawful, or malicious materials;<br>
6. violate any person’s privacy rights, access information of or on any person, or determine the person’s identity or location;<br>
7. violate any intellectual property rights; or<br>
8. mass download or batch data."),
footer = modalButton("I Accept")))
# observe({ 
#   identical(input$mitigations, c( "Implement CTR Program -  Required Implementation/Monitoring","Implement CTR Program - Voluntary"))  == TRUE
#   shinyalert("Oops!", "Something went wrong.", type = "error")
# })
# observeEvent(input$proj_type, { 
#   shinyalert("Oops!", "Something went wrong.", type = "error")
# })
  output$guidelines <- renderUI({
    PDFfile="PIA_Guidelines_Final.pdf"
    tags$iframe(
      src="PIA_Guidelines_Final.pdf",
      width="100%",
      height="800px")
  })
output$emp_eligible  <- renderUI({
  req(input$mitigations == "Implement CTR Program -  Required Implementation/Monitoring" | input$mitigations == "Implement CTR Program - Voluntary" | input$mitigations == "Employee Shuttle")
    sliderInput("eligible", "Percent (%) of Total Employees Eligible (if applicable)", 0, 100, 0) 
  })
output$emp_vmt_percent  <- renderUI({
  req(input$mitigations == "Implement CTR Program -  Required Implementation/Monitoring" | input$mitigations == "Implement CTR Program - Voluntary" | input$mitigations == "Employee Shuttle" | input$mitigations == "Private Shuttle")
    sliderInput("employee_vmt", "Employee Percent (%) of Total VMT (if applicable)" , 0, 100, 0) 
  })
output$emp_eligible1  <- renderUI({
  req(input$mitigations1 == "Implement CTR Program -  Required Implementation/Monitoring" | input$mitigations1 == "Implement CTR Program - Voluntary" | input$mitigations1 == "Employee Shuttle")
  sliderInput("eligible1", "Percent (%) of Total Employees Eligible (if applicable)", 0, 100, 0) 
})
output$emp_vmt_percent1  <- renderUI({
  req(input$mitigations1 == "Implement CTR Program -  Required Implementation/Monitoring" | input$mitigations1 == "Implement CTR Program - Voluntary" | input$mitigations1 == "Employee Shuttle"| input$mitigations1 == "Private Shuttle")
  sliderInput("employee_vmt1", "Employee Percent (%) of Total VMT (if applicable)" , 0, 100, 0) 
})
output$emp_eligible2  <- renderUI({
  req(input$mitigations2 == "Implement CTR Program -  Required Implementation/Monitoring" | input$mitigations2 == "Implement CTR Program - Voluntary" | input$mitigations2 == "Employee Shuttle")
  sliderInput("eligible2", "Percent (%) of Total Employees Eligible (if applicable)", 0, 100, 0) 
})
output$emp_vmt_percent2  <- renderUI({
  req(input$mitigations2 == "Implement CTR Program -  Required Implementation/Monitoring" | input$mitigations2 == "Implement CTR Program - Voluntary" | input$mitigations2 == "Employee Shuttle" | input$mitigations2 == "Private Shuttle")
  sliderInput("employee_vmt2", "Employee Percent (%) of Total VMT (if applicable)" , 0, 100, 0) 
})
output$emp_eligible3  <- renderUI({
  req(input$mitigations3 == "Implement CTR Program -  Required Implementation/Monitoring" | input$mitigations3 == "Implement CTR Program - Voluntary" | input$mitigations3 == "Employee Shuttle")
  sliderInput("eligible3", "Percent (%) of Total Employees Eligible (if applicable)", 0, 100, 0) 
})
output$emp_vmt_percent3  <- renderUI({
  req(input$mitigations3 == "Implement CTR Program -  Required Implementation/Monitoring" | input$mitigations3 == "Implement CTR Program - Voluntary" | input$mitigations3 == "Employee Shuttle" | input$mitigations3 == "Private Shuttle")
  sliderInput("employee_vmt3", "Employee Percent (%) of Total VMT (if applicable)" , 0, 100, 0) 
})
mit_choice<-reactive({
  if(input$proj_type %in% c("Residential (Market-Rate)","Residential (Affordable)")){
    c("Unbundle Parking Costs from Property Cost",
    "Traffic Calming",
    "Private Shuttle")
  }else if(input$proj_type %in% tau_types ){
    c("Traffic Calming",
      "End of Trip Facilities",
      "Employee Shuttle",
      "Private Shuttle",
      "Implement CTR Program - Voluntary",
      "Implement CTR Program -  Required Implementation/Monitoring")
  } else if(input$proj_type %in% rec_types ){
    c("Traffic Calming",
      "End of Trip Facilities",
      "Employee Shuttle",
      "Implement CTR Program - Voluntary",
      "Implement CTR Program -  Required Implementation/Monitoring")
  }else if(input$proj_type %in% commercial_types ){
    c("Traffic Calming",
      "End of Trip Facilities",
      "Employee Shuttle",
      "Implement CTR Program - Voluntary",
      "Implement CTR Program -  Required Implementation/Monitoring")
  }else if(input$proj_type %in% office_types ){
    c("Traffic Calming",
      "End of Trip Facilities",
      "Employee Shuttle",
      "Implement CTR Program - Voluntary",
      "Implement CTR Program -  Required Implementation/Monitoring")
  }else if(input$proj_type %in% pub_serv_types ){
    c("Traffic Calming",
      "End of Trip Facilities",
      "Employee Shuttle")
  }else if(input$proj_type == "Unique Project Type" ){
    c("Traffic Calming",
      "End of Trip Facilities",
      "Employee Shuttle",
      "Private Shuttle",
      "Implement CTR Program - Voluntary",
      "Implement CTR Program -  Required Implementation/Monitoring")
  }
  })
mit_choice1<-reactive({
  if(input$land_use1 %in% c("Residential (Market-Rate)","Residential (Affordable)")){
    c("Unbundle Parking Costs from Property Cost",
      "Traffic Calming",
      "Private Shuttle")
  }else if(input$land_use1 %in% tau_types ){
    c("Traffic Calming",
      "End of Trip Facilities",
      "Employee Shuttle",
      "Private Shuttle",
      "Implement CTR Program - Voluntary",
      "Implement CTR Program -  Required Implementation/Monitoring")
  } else if(input$land_use1 %in% rec_types ){
    c("Traffic Calming",
      "End of Trip Facilities",
      "Employee Shuttle",
      "Implement CTR Program - Voluntary",
      "Implement CTR Program -  Required Implementation/Monitoring")
  }else if(input$land_use1 %in% commercial_types ){
    c("Traffic Calming",
      "End of Trip Facilities",
      "Employee Shuttle",
      "Implement CTR Program - Voluntary",
      "Implement CTR Program -  Required Implementation/Monitoring")
  }else if(input$land_use1 %in% office_types ){
    c("Traffic Calming",
      "End of Trip Facilities",
      "Employee Shuttle",
      "Implement CTR Program - Voluntary",
      "Implement CTR Program -  Required Implementation/Monitoring")
  }else if(input$land_use1 %in% pub_serv_types ){
    c("Traffic Calming",
      "End of Trip Facilities",
      "Employee Shuttle")
  }else if(input$land_use1 == "Unique Project Type" ){
    c("Traffic Calming",
      "End of Trip Facilities",
      "Employee Shuttle",
      "Private Shuttle",
      "Implement CTR Program - Voluntary",
      "Implement CTR Program -  Required Implementation/Monitoring")
  }
})
mit_choice2<-reactive({
  if(input$land_use2 %in% c("Residential (Market-Rate)","Residential (Affordable)")){
    c("Unbundle Parking Costs from Property Cost",
      "Traffic Calming",
      "Private Shuttle")
  }else if(input$land_use2 %in% tau_types ){
    c("Traffic Calming",
      "End of Trip Facilities",
      "Employee Shuttle",
      "Private Shuttle",
      "Implement CTR Program - Voluntary",
      "Implement CTR Program -  Required Implementation/Monitoring")
  } else if(input$land_use2 %in% rec_types ){
    c("Traffic Calming",
      "End of Trip Facilities",
      "Employee Shuttle",
      "Implement CTR Program - Voluntary",
      "Implement CTR Program -  Required Implementation/Monitoring")
  }else if(input$land_use2 %in% commercial_types ){
    c("Traffic Calming",
      "End of Trip Facilities",
      "Employee Shuttle",
      "Implement CTR Program - Voluntary",
      "Implement CTR Program -  Required Implementation/Monitoring")
  }else if(input$land_use2 %in% office_types ){
    c("Traffic Calming",
      "End of Trip Facilities",
      "Employee Shuttle",
      "Implement CTR Program - Voluntary",
      "Implement CTR Program -  Required Implementation/Monitoring")
  }else if(input$land_use2 %in% pub_serv_types ){
    c("Traffic Calming",
      "End of Trip Facilities",
      "Employee Shuttle")
  }else if(input$land_use2 == "Unique Project Type" ){
    c("Traffic Calming",
      "End of Trip Facilities",
      "Employee Shuttle",
      "Private Shuttle",
      "Implement CTR Program - Voluntary",
      "Implement CTR Program -  Required Implementation/Monitoring")
  }
})
mit_choice3<-reactive({
  if(input$land_use3 %in% c("Residential (Market-Rate)","Residential (Affordable)")){
    c("Unbundle Parking Costs from Property Cost",
      "Traffic Calming",
      "Private Shuttle")
  }else if(input$land_use3 %in% tau_types ){
    c("Traffic Calming",
      "End of Trip Facilities",
      "Employee Shuttle",
      "Private Shuttle",
      "Implement CTR Program - Voluntary",
      "Implement CTR Program -  Required Implementation/Monitoring")
  } else if(input$land_use3 %in% rec_types ){
    c("Traffic Calming",
      "End of Trip Facilities",
      "Employee Shuttle",
      "Implement CTR Program - Voluntary",
      "Implement CTR Program -  Required Implementation/Monitoring")
  }else if(input$land_use3 %in% commercial_types ){
    c("Traffic Calming",
      "End of Trip Facilities",
      "Employee Shuttle",
      "Implement CTR Program - Voluntary",
      "Implement CTR Program -  Required Implementation/Monitoring")
  }else if(input$land_use3 %in% office_types ){
    c("Traffic Calming",
      "End of Trip Facilities",
      "Employee Shuttle",
      "Implement CTR Program - Voluntary",
      "Implement CTR Program -  Required Implementation/Monitoring")
  }else if(input$land_use3 %in% pub_serv_types ){
    c("Traffic Calming",
      "End of Trip Facilities",
      "Employee Shuttle")
  }else if(input$land_use3 == "Unique Project Type" ){
    c("Traffic Calming",
      "End of Trip Facilities",
      "Employee Shuttle",
      "Private Shuttle",
      "Implement CTR Program - Voluntary",
      "Implement CTR Program -  Required Implementation/Monitoring")
  }
})
  output$ui_mitigations <-  renderUI ({
    req(input$map_shape_click, input$proj_type)
    checkboxGroupInput(inputId="mitigations", label="Select Mitigations", selected="None",
                       choices=mit_choice())
  })
  output$ui_mitigations1 <-renderUI({
    req(input$map_shape_click,input$land_use1)
    if (input$proj_type!="Mixed-Use"){
      return()} else{
    checkboxGroupInput(inputId="mitigations1", label="Select Mitigations", selected="None",
                       choices=mit_choice1())
      }
  })
  output$ui_mitigations2  <-renderUI({
    req(input$map_shape_click,input$land_use2)
    if (input$proj_type!="Mixed-Use"){
      return()} else{
    checkboxGroupInput(inputId="mitigations2", label="Select Mitigations", selected="None",
                       choices=mit_choice2())
      }
  })
  output$ui_mitigations3  <-renderUI({
    req(input$map_shape_click,input$land_use3)
    if (input$proj_type!="Mixed-Use"){
      return()} else{
    checkboxGroupInput(inputId="mitigations3", label="Select Mitigations", selected="None",
                       choices=mit_choice3())
      }
  })
  mit_percent <- reactive({
    if(identical(input$mitigations, c("Traffic Calming"))  == TRUE){ # all types
      (1-.01)
    }else if(identical(input$mitigations, c("Unbundle Parking Costs from Property Cost"))  == TRUE){ # residential only
      (1-.026)
    } else if(identical(input$mitigations, c("Unbundle Parking Costs from Property Cost", "Traffic Calming")) == TRUE){  # residential only
      (1-.026) * (1-.01)
    }else if(identical(input$mitigations, c("Unbundle Parking Costs from Property Cost", "Traffic Calming", "Private Shuttle"))  == TRUE){ # residential only
      (1-.026) * (1-.01)* (1-(.05 * (1- (input$employee_vmt/100))))
    } else if(identical(input$mitigations, c("Unbundle Parking Costs from Property Cost",  "Private Shuttle"))  == TRUE){ # residential only
      (1-.026) *  (1-(.05 * (1- (input$employee_vmt/100))))
    }  else if(identical(input$mitigations, c( "Traffic Calming", "Private Shuttle"))  == TRUE){ # tau and resident
       (1-.01)* (1-(.05 * (1- (input$employee_vmt/100))))
    } else if(identical(input$mitigations, c( "Private Shuttle"))  == TRUE){ # tau and resident
       (1-(.05 * (1- (input$employee_vmt/100))))
    }  else if(identical(input$mitigations, c( "Employee Shuttle","Private Shuttle"))  == TRUE){ # tau only
      (1-(.05 * input$employee_vmt/100)) * (1-(.05 * (1- (input$employee_vmt/100))))
    } else if(identical(input$mitigations, c("Traffic Calming" ,"Employee Shuttle","Private Shuttle"))  == TRUE){ # tau only
      (1-(.05 * input$employee_vmt/100)) * (1-(.05 * (1- (input$employee_vmt/100)))) * (1-.01)
    } else if(identical(input$mitigations, c("Traffic Calming", "Employee Shuttle"))  == TRUE){ # tau, rec, commercial, public service
      (1-(.05 * input$employee_vmt/100)) * (1-.01)
    } else if(identical(input$mitigations, c( "Employee Shuttle"))  == TRUE){ # tau, rec, commercial, public service
      (1-(.05 * input$employee_vmt/100))
    } else if(identical(input$mitigations, c( "End of Trip Facilities"))  == TRUE){ # tau, rec, commercial, public service
      (1-.00625)
    } else if(identical(input$mitigations, c( "End of Trip Facilities","Employee Shuttle"))  == TRUE){ # tau, rec, commercial, public service
      (1-.00625) * (1-(.05 * input$employee_vmt/100))
    } else if(identical(input$mitigations, c("Traffic Calming","End of Trip Facilities","Employee Shuttle"))  == TRUE){ # tau, rec, commercial, public service
      (1-.00625) * (1-(.05 * input$employee_vmt/100)) * (1-.01)
    }else if(identical(input$mitigations, c("Traffic Calming", "End of Trip Facilities","Employee Shuttle", "Implement CTR Program -  Required Implementation/Monitoring"))  == TRUE){ # tau, rec, commercial, public service
      (1-.00625) * (1-(.05 * input$employee_vmt/100)) * (1-.01) * (1- (.21* (input$eligible/100) * (input$employee_vmt/100)))
    }else if(identical(input$mitigations, c("Traffic Calming", "End of Trip Facilities","Employee Shuttle","Implement CTR Program - Voluntary"))  == TRUE){ # tau, rec, commercial, public service
      (1-.00625) * (1-(.05 * input$employee_vmt/100)) * (1-.01) * (1- (.05* (input$eligible/100) * (input$employee_vmt/100)))
    } else if(identical(input$mitigations, c("Traffic Calming", "End of Trip Facilities","Implement CTR Program -  Required Implementation/Monitoring"))  == TRUE){ # tau, rec, commercial, public service
      (1-.00625) * (1-(.05 * input$employee_vmt/100)) * (1-.01) * (1- (.21* (input$eligible/100) * (input$employee_vmt/100)))
    } else if(identical(input$mitigations, c("Traffic Calming",  "End of Trip Facilities","Private Shuttle"))  == TRUE){ # tau, rec, commercial, public service
      (1-.00625) * (1-.01) * (1-(.05 * (1- (input$employee_vmt/100))))
    }else if(identical(input$mitigations, c("Traffic Calming", "End of Trip Facilities"))  == TRUE){ # tau, rec, commercial, public service
      (1-.00625) * (1-.01)
    } else if(identical(input$mitigations, c( "End of Trip Facilities", "Private Shuttle"))  == TRUE){ # tau, rec, commercial, public service
      (1-.00625) * (1-(.05 * (1- (input$employee_vmt/100))))
    } else if(identical(input$mitigations, c( "End of Trip Facilities","Employee Shuttle", "Private Shuttle"))  == TRUE){ # tau, rec, commercial, public service
      (1-.00625) * (1-(.05 * input$employee_vmt/100)) * (1-(.05 * (1- (input$employee_vmt/100))))
    } else if(identical(input$mitigations, c("Traffic Calming", "End of Trip Facilities", "Private Shuttle"))  == TRUE){ # tau, rec, commercial, public service
      (1-.00625) * (1-.01) * (1-(.05 * (1- (input$employee_vmt/100))))
    }else if(identical(input$mitigations, c( "Traffic Calming","Implement CTR Program - Voluntary"))  == TRUE){ # all types
      (1-.01) * (1- (.05* (input$eligible/100) * (input$employee_vmt/100)))
    } else if(identical(input$mitigations, c( "Traffic Calming","Implement CTR Program -  Required Implementation/Monitoring"))  == TRUE){ # all types
      (1-.01) * (1- (.21* (input$eligible/100) * (input$employee_vmt/100)))
    } else if(identical(input$mitigations, c( "Traffic Calming", "Private Shuttle", "Implement CTR Program - Voluntary"))  == TRUE){ # tau and resident
      (1-.01)* (1-(.05 * (1- (input$employee_vmt/100)))) * (1- (.05* (input$eligible/100) * (input$employee_vmt/100)))
    } else if(identical(input$mitigations, c( "Private Shuttle","Implement CTR Program - Voluntary"))  == TRUE){ # tau and resident
      (1-(.05 * (1- (input$employee_vmt/100)))) * (1- (.05* (input$eligible/100) * (input$employee_vmt/100)))
    }  else if(identical(input$mitigations, c( "Employee Shuttle","Private Shuttle","Implement CTR Program - Voluntary"))  == TRUE){ # tau only
      (1-(.05 * input$employee_vmt/100)) * (1-(.05 * (1- (input$employee_vmt/100)))) * (1- (.05* (input$eligible/100) * (input$employee_vmt/100)))
    } else if(identical(input$mitigations, c( "Employee Shuttle","Private Shuttle","Traffic Calming","Implement CTR Program - Voluntary"))  == TRUE){ # tau only
      (1-(.05 * input$employee_vmt/100)) * (1-(.05 * (1- (input$employee_vmt/100)))) (1-.01) * (1- (.05* (input$eligible/100) * (input$employee_vmt/100)))
    } else if(identical(input$mitigations, c("Traffic Calming","Employee Shuttle","Implement CTR Program - Voluntary"))  == TRUE){ # tau, rec, commercial, public service
      (1-(.05 * input$employee_vmt/100)) * (1-.01) * (1- (.05* (input$eligible/100) * (input$employee_vmt/100)))
    } else if(identical(input$mitigations, c( "Employee Shuttle","Implement CTR Program - Voluntary"))  == TRUE){ # tau, rec, commercial, public service
      (1-(.05 * input$employee_vmt/100)) * (1- (.05* (input$eligible/100) * (input$employee_vmt/100)))
    } else if(identical(input$mitigations, c( "End of Trip Facilities","Implement CTR Program - Voluntary"))  == TRUE){ # tau, rec, commercial, public service
      (1-.00625) * (1- (.05* (input$eligible/100) * (input$employee_vmt/100)))
    } else if(identical(input$mitigations, c( "End of Trip Facilities","Employee Shuttle","Implement CTR Program - Voluntary"))  == TRUE){ # tau, rec, commercial, public service
      (1-.00625) * (1-(.05 * input$employee_vmt/100)) * (1- (.05* (input$eligible/100) * (input$employee_vmt/100)))
   } else if(identical(input$mitigations, c( "Traffic Calming", "End of Trip Facilities","Implement CTR Program - Voluntary"))  == TRUE){ # tau, rec, commercial, public service
      (1-.00625) * (1-.01) *  (1- (.05* (input$eligible/100) * (input$employee_vmt/100)))
    } else if(identical(input$mitigations, c( "End of Trip Facilities", "Private Shuttle","Implement CTR Program - Voluntary"))  == TRUE){ # tau, rec, commercial, public service
      (1-.00625) * (1-(.05 * (1- (input$employee_vmt/100)))) * (1- (.05* (input$eligible/100) * (input$employee_vmt/100)))
    } else if(identical(input$mitigations, c( "End of Trip Facilities","Employee Shuttle", "Private Shuttle","Implement CTR Program - Voluntary"))  == TRUE){ # tau, rec, commercial, public service
      (1-.00625) * (1-(.05 * input$employee_vmt/100)) * (1-(.05 * (1- (input$employee_vmt/100)))) * (1- (.05* (input$eligible/100) * (input$employee_vmt/100)))
    } else if(identical(input$mitigations, c( "Traffic Calming", "End of Trip Facilities","Private Shuttle","Implement CTR Program - Voluntary"))  == TRUE){ # tau, rec, commercial, public service
      (1-.00625) * (1-.01) * (1-(.05 * (1- (input$employee_vmt/100)))) * (1- (.05* (input$eligible/100) * (input$employee_vmt/100)))
    } else if(identical(input$mitigations, c("Implement CTR Program - Voluntary"))  == TRUE){ # tau, rec, commercial, public service
       (1- (.05* (input$eligible/100) * (input$employee_vmt/100)))
    } else if(identical(input$mitigations, c( "Traffic Calming", "Private Shuttle", "Implement CTR Program -  Required Implementation/Monitoring"))  == TRUE){ # tau and resident
      (1-.01)* (1-(.05 * (1- (input$employee_vmt/100)))) * (1- (.21* (input$eligible/100) * (input$employee_vmt/100)))
    } else if(identical(input$mitigations, c( "Private Shuttle","Implement CTR Program -  Required Implementation/Monitoring"))  == TRUE){ # tau and resident
      (1-(.05 * (1- (input$employee_vmt/100)))) * (1- (.21* (input$eligible/100) * (input$employee_vmt/100)))
    }  else if(identical(input$mitigations, c( "Employee Shuttle","Private Shuttle","Implement CTR Program -  Required Implementation/Monitoring"))  == TRUE){ # tau only
      (1-(.05 * input$employee_vmt/100)) * (1-(.05 * (1- (input$employee_vmt/100)))) * (1- (.21* (input$eligible/100) * (input$employee_vmt/100)))
    } else if(identical(input$mitigations, c("Traffic Calming", "Employee Shuttle","Private Shuttle","Implement CTR Program -  Required Implementation/Monitoring"))  == TRUE){ # tau only
      (1-(.05 * input$employee_vmt/100)) * (1-(.05 * (1- (input$employee_vmt/100)))) (1-.01) * (1- (.21* (input$eligible/100) * (input$employee_vmt/100)))
    } else if(identical(input$mitigations, c( "Traffic Calming","Employee Shuttle","Implement CTR Program -  Required Implementation/Monitoring"))  == TRUE){ # tau, rec, commercial, public service
      (1-(.05 * input$employee_vmt/100)) * (1-.01) * (1- (.21* (input$eligible/100) * (input$employee_vmt/100)))
    } else if(identical(input$mitigations, c( "Employee Shuttle","Implement CTR Program -  Required Implementation/Monitoring"))  == TRUE){ # tau, rec, commercial, public service
      (1-(.05 * input$employee_vmt/100)) * (1- (.21* (input$eligible/100) * (input$employee_vmt/100)))
    } else if(identical(input$mitigations, c( "End of Trip Facilities","Implement CTR Program -  Required Implementation/Monitoring"))  == TRUE){ # tau, rec, commercial, public service
      (1-.00625) * (1- (.21* (input$eligible/100) * (input$employee_vmt/100)))
    } else if(identical(input$mitigations, c( "End of Trip Facilities","Employee Shuttle","Implement CTR Program -  Required Implementation/Monitoring"))  == TRUE){ # tau, rec, commercial, public service
      (1-.00625) * (1-(.05 * input$employee_vmt/100)) * (1- (.21* (input$eligible/100) * (input$employee_vmt/100)))
    } else if(identical(input$mitigations, c("Traffic Calming", "End of Trip Facilities", "Private Shuttle","Implement CTR Program -  Required Implementation/Monitoring"))  == TRUE){ # tau, rec, commercial, public service
      (1-.00625) * (1-.01) * (1-(.05 * (1- (input$employee_vmt/100)))) * (1- (.21* (input$eligible/100) * (input$employee_vmt/100)))
    } else if(identical(input$mitigations, c( "End of Trip Facilities", "Private Shuttle","Implement CTR Program -  Required Implementation/Monitoring"))  == TRUE){ # tau, rec, commercial, public service
      (1-.00625) * (1-(.05 * (1- (input$employee_vmt/100)))) * (1- (.21* (input$eligible/100) * (input$employee_vmt/100)))
    } else if(identical(input$mitigations, c( "End of Trip Facilities","Employee Shuttle", "Private Shuttle","Implement CTR Program -  Required Implementation/Monitoring"))  == TRUE){ # tau, rec, commercial, public service
      (1-.00625) * (1-(.05 * input$employee_vmt/100)) * (1-(.05 * (1- (input$employee_vmt/100)))) * (1- (.21* (input$eligible/100) * (input$employee_vmt/100)))
    } else if(identical(input$mitigations, c("Traffic Calming", "End of Trip Facilities", "Private Shuttle","Implement CTR Program -  Required Implementation/Monitoring"))  == TRUE){ # tau, rec, commercial, public service
      (1-.00625) * (1-.01) * (1-(.05 * (1- (input$employee_vmt/100)))) * (1- (.21* (input$eligible/100) * (input$employee_vmt/100)))
    } else if(identical(input$mitigations, c( "Implement CTR Program -  Required Implementation/Monitoring"))  == TRUE){ # tau, rec, commercial, public service
      (1- (.21* (input$eligible/100) * (input$employee_vmt/100)))
    } else{1}
   })
mit_percent_final <-reactive({
  if(mit_percent() < .8 & nrow(town_reg_buffer()) > 0){
    .8
  }else if(mit_percent() < .85 & nrow(town_reg_buffer()) == 0){
    .85
  } else{mit_percent()}
})
mit_percent1 <- reactive({
  if(identical(input$mitigations1, c("Traffic Calming"))  == TRUE){ # all types
    (1-.01)
  }else if(identical(input$mitigations1, c("Unbundle Parking Costs from Property Cost"))  == TRUE){ # residential only
    (1-.026)
  } else if(identical(input$mitigations1, c("Unbundle Parking Costs from Property Cost", "Traffic Calming")) == TRUE){  # residential only
    (1-.026) * (1-.01)
  }else if(identical(input$mitigations1, c("Unbundle Parking Costs from Property Cost", "Traffic Calming", "Private Shuttle"))  == TRUE){ # residential only
    (1-.026) * (1-.01)* (1-(.05 * (1- (input$employee_vmt1/100))))
  } else if(identical(input$mitigations1, c("Unbundle Parking Costs from Property Cost",  "Private Shuttle"))  == TRUE){ # residential only
    (1-.026) *  (1-(.05 * (1- (input$employee_vmt1/100))))
  }  else if(identical(input$mitigations1, c( "Traffic Calming", "Private Shuttle"))  == TRUE){ # tau and resident
    (1-.01)* (1-(.05 * (1- (input$employee_vmt1/100))))
  } else if(identical(input$mitigations1, c( "Private Shuttle"))  == TRUE){ # tau and resident
    (1-(.05 * (1- (input$employee_vmt1/100))))
  }  else if(identical(input$mitigations1, c( "Employee Shuttle","Private Shuttle"))  == TRUE){ # tau only
    (1-(.05 * input$employee_vmt1/100)) * (1-(.05 * (1- (input$employee_vmt1/100))))
  } else if(identical(input$mitigations1, c("Traffic Calming" ,"Employee Shuttle","Private Shuttle"))  == TRUE){ # tau only
    (1-(.05 * input$employee_vmt1/100)) * (1-(.05 * (1- (input$employee_vmt1/100)))) * (1-.01)
  } else if(identical(input$mitigations1, c("Traffic Calming", "Employee Shuttle"))  == TRUE){ # tau, rec, commercial, public service
    (1-(.05 * input$employee_vmt1/100)) * (1-.01)
  } else if(identical(input$mitigations1, c( "Employee Shuttle"))  == TRUE){ # tau, rec, commercial, public service
    (1-(.05 * input$employee_vmt1/100))
  } else if(identical(input$mitigations1, c( "End of Trip Facilities"))  == TRUE){ # tau, rec, commercial, public service
    (1-.00625)
  } else if(identical(input$mitigations1, c( "End of Trip Facilities","Employee Shuttle"))  == TRUE){ # tau, rec, commercial, public service
    (1-.00625) * (1-(.05 * input$employee_vmt1/100))
  } else if(identical(input$mitigations1, c("Traffic Calming","End of Trip Facilities","Employee Shuttle"))  == TRUE){ # tau, rec, commercial, public service
    (1-.00625) * (1-(.05 * input$employee_vmt1/100)) * (1-.01)
  }else if(identical(input$mitigations1, c("Traffic Calming", "End of Trip Facilities","Employee Shuttle", "Implement CTR Program -  Required Implementation/Monitoring"))  == TRUE){ # tau, rec, commercial, public service
    (1-.00625) * (1-(.05 * input$employee_vmt1/100)) * (1-.01) * (1- (.21* (input$eligible1/100) * (input$employee_vmt1/100)))
  }else if(identical(input$mitigations1, c("Traffic Calming", "End of Trip Facilities","Employee Shuttle","Implement CTR Program - Voluntary"))  == TRUE){ # tau, rec, commercial, public service
    (1-.00625) * (1-(.05 * input$employee_vmt1/100)) * (1-.01) * (1- (.05* (input$eligible1/100) * (input$employee_vmt1/100)))
  } else if(identical(input$mitigations1, c("Traffic Calming", "End of Trip Facilities","Implement CTR Program -  Required Implementation/Monitoring"))  == TRUE){ # tau, rec, commercial, public service
    (1-.00625) * (1-(.05 * input$employee_vmt1/100)) * (1-.01) * (1- (.21* (input$eligible1/100) * (input$employee_vmt1/100)))
  } else if(identical(input$mitigations1, c("Traffic Calming",  "End of Trip Facilities","Private Shuttle"))  == TRUE){ # tau, rec, commercial, public service
    (1-.00625) * (1-.01) * (1-(.05 * (1- (input$employee_vmt1/100))))
  }else if(identical(input$mitigations1, c("Traffic Calming", "End of Trip Facilities"))  == TRUE){ # tau, rec, commercial, public service
    (1-.00625) * (1-.01)
  } else if(identical(input$mitigations1, c( "End of Trip Facilities", "Private Shuttle"))  == TRUE){ # tau, rec, commercial, public service
    (1-.00625) * (1-(.05 * (1- (input$employee_vmt1/100))))
  } else if(identical(input$mitigations1, c( "End of Trip Facilities","Employee Shuttle", "Private Shuttle"))  == TRUE){ # tau, rec, commercial, public service
    (1-.00625) * (1-(.05 * input$employee_vmt1/100)) * (1-(.05 * (1- (input$employee_vmt1/100))))
  } else if(identical(input$mitigations1, c("Traffic Calming", "End of Trip Facilities", "Private Shuttle"))  == TRUE){ # tau, rec, commercial, public service
    (1-.00625) * (1-.01) * (1-(.05 * (1- (input$employee_vmt1/100))))
  }else if(identical(input$mitigations1, c( "Traffic Calming","Implement CTR Program - Voluntary"))  == TRUE){ # all types
    (1-.01) * (1- (.05* (input$eligible1/100) * (input$employee_vmt1/100)))
  } else if(identical(input$mitigations1, c( "Traffic Calming","Implement CTR Program -  Required Implementation/Monitoring"))  == TRUE){ # all types
    (1-.01) * (1- (.21* (input$eligible1/100) * (input$employee_vmt1/100)))
  } else if(identical(input$mitigations1, c( "Traffic Calming", "Private Shuttle", "Implement CTR Program - Voluntary"))  == TRUE){ # tau and resident
    (1-.01)* (1-(.05 * (1- (input$employee_vmt1/100)))) * (1- (.05* (input$eligible1/100) * (input$employee_vmt1/100)))
  } else if(identical(input$mitigations1, c( "Private Shuttle","Implement CTR Program - Voluntary"))  == TRUE){ # tau and resident
    (1-(.05 * (1- (input$employee_vmt1/100)))) * (1- (.05* (input$eligible1/100) * (input$employee_vmt1/100)))
  }  else if(identical(input$mitigations1, c( "Employee Shuttle","Private Shuttle","Implement CTR Program - Voluntary"))  == TRUE){ # tau only
    (1-(.05 * input$employee_vmt1/100)) * (1-(.05 * (1- (input$employee_vmt1/100)))) * (1- (.05* (input$eligible1/100) * (input$employee_vmt1/100)))
  } else if(identical(input$mitigations1, c( "Employee Shuttle","Private Shuttle","Traffic Calming","Implement CTR Program - Voluntary"))  == TRUE){ # tau only
    (1-(.05 * input$employee_vmt1/100)) * (1-(.05 * (1- (input$employee_vmt1/100)))) (1-.01) * (1- (.05* (input$eligible1/100) * (input$employee_vmt1/100)))
  } else if(identical(input$mitigations1, c("Traffic Calming","Employee Shuttle","Implement CTR Program - Voluntary"))  == TRUE){ # tau, rec, commercial, public service
    (1-(.05 * input$employee_vmt1/100)) * (1-.01) * (1- (.05* (input$eligible1/100) * (input$employee_vmt1/100)))
  } else if(identical(input$mitigations1, c( "Employee Shuttle","Implement CTR Program - Voluntary"))  == TRUE){ # tau, rec, commercial, public service
    (1-(.05 * input$employee_vmt1/100)) * (1- (.05* (input$eligible1/100) * (input$employee_vmt1/100)))
  } else if(identical(input$mitigations1, c( "End of Trip Facilities","Implement CTR Program - Voluntary"))  == TRUE){ # tau, rec, commercial, public service
    (1-.00625) * (1- (.05* (input$eligible1/100) * (input$employee_vmt1/100)))
  } else if(identical(input$mitigations1, c( "End of Trip Facilities","Employee Shuttle","Implement CTR Program - Voluntary"))  == TRUE){ # tau, rec, commercial, public service
    (1-.00625) * (1-(.05 * input$employee_vmt1/100)) * (1- (.05* (input$eligible1/100) * (input$employee_vmt1/100)))
  } else if(identical(input$mitigations1, c( "Traffic Calming", "End of Trip Facilities","Implement CTR Program - Voluntary"))  == TRUE){ # tau, rec, commercial, public service
    (1-.00625) * (1-.01) *  (1- (.05* (input$eligible1/100) * (input$employee_vmt1/100)))
  } else if(identical(input$mitigations1, c( "End of Trip Facilities", "Private Shuttle","Implement CTR Program - Voluntary"))  == TRUE){ # tau, rec, commercial, public service
    (1-.00625) * (1-(.05 * (1- (input$employee_vmt1/100)))) * (1- (.05* (input$eligible1/100) * (input$employee_vmt1/100)))
  } else if(identical(input$mitigations1, c( "End of Trip Facilities","Employee Shuttle", "Private Shuttle","Implement CTR Program - Voluntary"))  == TRUE){ # tau, rec, commercial, public service
    (1-.00625) * (1-(.05 * input$employee_vmt1/100)) * (1-(.05 * (1- (input$employee_vmt1/100)))) * (1- (.05* (input$eligible1/100) * (input$employee_vmt1/100)))
  } else if(identical(input$mitigations1, c( "Traffic Calming", "End of Trip Facilities","Private Shuttle","Implement CTR Program - Voluntary"))  == TRUE){ # tau, rec, commercial, public service
    (1-.00625) * (1-.01) * (1-(.05 * (1- (input$employee_vmt1/100)))) * (1- (.05* (input$eligible1/100) * (input$employee_vmt1/100)))
  } else if(identical(input$mitigations1, c("Implement CTR Program - Voluntary"))  == TRUE){ # tau, rec, commercial, public service
    (1- (.05* (input$eligible1/100) * (input$employee_vmt1/100)))
  } else if(identical(input$mitigations1, c( "Traffic Calming", "Private Shuttle", "Implement CTR Program -  Required Implementation/Monitoring"))  == TRUE){ # tau and resident
    (1-.01)* (1-(.05 * (1- (input$employee_vmt1/100)))) * (1- (.21* (input$eligible1/100) * (input$employee_vmt1/100)))
  } else if(identical(input$mitigations1, c( "Private Shuttle","Implement CTR Program -  Required Implementation/Monitoring"))  == TRUE){ # tau and resident
    (1-(.05 * (1- (input$employee_vmt1/100)))) * (1- (.21* (input$eligible1/100) * (input$employee_vmt1/100)))
  }  else if(identical(input$mitigations1, c( "Employee Shuttle","Private Shuttle","Implement CTR Program -  Required Implementation/Monitoring"))  == TRUE){ # tau only
    (1-(.05 * input$employee_vmt1/100)) * (1-(.05 * (1- (input$employee_vmt1/100)))) * (1- (.21* (input$eligible1/100) * (input$employee_vmt1/100)))
  } else if(identical(input$mitigations1, c("Traffic Calming", "Employee Shuttle","Private Shuttle","Implement CTR Program -  Required Implementation/Monitoring"))  == TRUE){ # tau only
    (1-(.05 * input$employee_vmt1/100)) * (1-(.05 * (1- (input$employee_vmt1/100)))) (1-.01) * (1- (.21* (input$eligible1/100) * (input$employee_vmt1/100)))
  } else if(identical(input$mitigations1, c( "Traffic Calming","Employee Shuttle","Implement CTR Program -  Required Implementation/Monitoring"))  == TRUE){ # tau, rec, commercial, public service
    (1-(.05 * input$employee_vmt1/100)) * (1-.01) * (1- (.21* (input$eligible1/100) * (input$employee_vmt1/100)))
  } else if(identical(input$mitigations1, c( "Employee Shuttle","Implement CTR Program -  Required Implementation/Monitoring"))  == TRUE){ # tau, rec, commercial, public service
    (1-(.05 * input$employee_vmt1/100)) * (1- (.21* (input$eligible1/100) * (input$employee_vmt1/100)))
  } else if(identical(input$mitigations1, c( "End of Trip Facilities","Implement CTR Program -  Required Implementation/Monitoring"))  == TRUE){ # tau, rec, commercial, public service
    (1-.00625) * (1- (.21* (input$eligible1/100) * (input$employee_vmt1/100)))
  } else if(identical(input$mitigations1, c( "End of Trip Facilities","Employee Shuttle","Implement CTR Program -  Required Implementation/Monitoring"))  == TRUE){ # tau, rec, commercial, public service
    (1-.00625) * (1-(.05 * input$employee_vmt1/100)) * (1- (.21* (input$eligible1/100) * (input$employee_vmt1/100)))
  } else if(identical(input$mitigations1, c("Traffic Calming", "End of Trip Facilities", "Private Shuttle","Implement CTR Program -  Required Implementation/Monitoring"))  == TRUE){ # tau, rec, commercial, public service
    (1-.00625) * (1-.01) * (1-(.05 * (1- (input$employee_vmt1/100)))) * (1- (.21* (input$eligible1/100) * (input$employee_vmt1/100)))
  } else if(identical(input$mitigations1, c( "End of Trip Facilities", "Private Shuttle","Implement CTR Program -  Required Implementation/Monitoring"))  == TRUE){ # tau, rec, commercial, public service
    (1-.00625) * (1-(.05 * (1- (input$employee_vmt1/100)))) * (1- (.21* (input$eligible1/100) * (input$employee_vmt1/100)))
  } else if(identical(input$mitigations1, c( "End of Trip Facilities","Employee Shuttle", "Private Shuttle","Implement CTR Program -  Required Implementation/Monitoring"))  == TRUE){ # tau, rec, commercial, public service
    (1-.00625) * (1-(.05 * input$employee_vmt1/100)) * (1-(.05 * (1- (input$employee_vmt1/100)))) * (1- (.21* (input$eligible1/100) * (input$employee_vmt1/100)))
  } else if(identical(input$mitigations1, c("Traffic Calming", "End of Trip Facilities", "Private Shuttle","Implement CTR Program -  Required Implementation/Monitoring"))  == TRUE){ # tau, rec, commercial, public service
    (1-.00625) * (1-.01) * (1-(.05 * (1- (input$employee_vmt1/100)))) * (1- (.21* (input$eligible1/100) * (input$employee_vmt1/100)))
  } else if(identical(input$mitigations1, c( "Implement CTR Program -  Required Implementation/Monitoring"))  == TRUE){ # tau, rec, commercial, public service
    (1- (.21* (input$eligible1/100) * (input$employee_vmt1/100)))
  } else{1}
})
mit_percent_final1 <-reactive({
  if(mit_percent1() < .8 & nrow(town_reg_buffer()) > 0){
    .8
  }else if(mit_percent1() < .85 & nrow(town_reg_buffer()) == 0){
    .85
  } else{mit_percent1()}
})
mit_percent2 <- reactive({
  if(identical(input$mitigations2, c("Traffic Calming"))  == TRUE){ # all types
    (1-.01)
  }else if(identical(input$mitigations2, c("Unbundle Parking Costs from Property Cost"))  == TRUE){ # residential only
    (1-.026)
  } else if(identical(input$mitigations2, c("Unbundle Parking Costs from Property Cost", "Traffic Calming")) == TRUE){  # residential only
    (1-.026) * (1-.01)
  }else if(identical(input$mitigations2, c("Unbundle Parking Costs from Property Cost", "Traffic Calming", "Private Shuttle"))  == TRUE){ # residential only
    (1-.026) * (1-.01)* (1-(.05 * (1- (input$employee_vmt2/100))))
  } else if(identical(input$mitigations2, c("Unbundle Parking Costs from Property Cost",  "Private Shuttle"))  == TRUE){ # residential only
    (1-.026) *  (1-(.05 * (1- (input$employee_vmt2/100))))
  }  else if(identical(input$mitigations2, c( "Traffic Calming", "Private Shuttle"))  == TRUE){ # tau and resident
    (1-.01)* (1-(.05 * (1- (input$employee_vmt2/100))))
  } else if(identical(input$mitigations2, c( "Private Shuttle"))  == TRUE){ # tau and resident
    (1-(.05 * (1- (input$employee_vmt2/100))))
  }  else if(identical(input$mitigations2, c( "Employee Shuttle","Private Shuttle"))  == TRUE){ # tau only
    (1-(.05 * input$employee_vmt2/100)) * (1-(.05 * (1- (input$employee_vmt2/100))))
  } else if(identical(input$mitigations2, c("Traffic Calming" ,"Employee Shuttle","Private Shuttle"))  == TRUE){ # tau only
    (1-(.05 * input$employee_vmt2/100)) * (1-(.05 * (1- (input$employee_vmt2/100)))) * (1-.01)
  } else if(identical(input$mitigations2, c("Traffic Calming", "Employee Shuttle"))  == TRUE){ # tau, rec, commercial, public service
    (1-(.05 * input$employee_vmt2/100)) * (1-.01)
  } else if(identical(input$mitigations2, c( "Employee Shuttle"))  == TRUE){ # tau, rec, commercial, public service
    (1-(.05 * input$employee_vmt2/100))
  } else if(identical(input$mitigations2, c( "End of Trip Facilities"))  == TRUE){ # tau, rec, commercial, public service
    (1-.00625)
  } else if(identical(input$mitigations2, c( "End of Trip Facilities","Employee Shuttle"))  == TRUE){ # tau, rec, commercial, public service
    (1-.00625) * (1-(.05 * input$employee_vmt2/100))
  } else if(identical(input$mitigations2, c("Traffic Calming","End of Trip Facilities","Employee Shuttle"))  == TRUE){ # tau, rec, commercial, public service
    (1-.00625) * (1-(.05 * input$employee_vmt2/100)) * (1-.01)
  }else if(identical(input$mitigations2, c("Traffic Calming", "End of Trip Facilities","Employee Shuttle", "Implement CTR Program -  Required Implementation/Monitoring"))  == TRUE){ # tau, rec, commercial, public service
    (1-.00625) * (1-(.05 * input$employee_vmt2/100)) * (1-.01) * (1- (.21* (input$eligible2/100) * (input$employee_vmt2/100)))
  }else if(identical(input$mitigations2, c("Traffic Calming", "End of Trip Facilities","Employee Shuttle","Implement CTR Program - Voluntary"))  == TRUE){ # tau, rec, commercial, public service
    (1-.00625) * (1-(.05 * input$employee_vmt2/100)) * (1-.01) * (1- (.05* (input$eligible2/100) * (input$employee_vmt2/100)))
  } else if(identical(input$mitigations2, c("Traffic Calming", "End of Trip Facilities","Implement CTR Program -  Required Implementation/Monitoring"))  == TRUE){ # tau, rec, commercial, public service
    (1-.00625) * (1-(.05 * input$employee_vmt2/100)) * (1-.01) * (1- (.21* (input$eligible2/100) * (input$employee_vmt2/100)))
  } else if(identical(input$mitigations2, c("Traffic Calming",  "End of Trip Facilities","Private Shuttle"))  == TRUE){ # tau, rec, commercial, public service
    (1-.00625) * (1-.01) * (1-(.05 * (1- (input$employee_vmt2/100))))
  }else if(identical(input$mitigations2, c("Traffic Calming", "End of Trip Facilities"))  == TRUE){ # tau, rec, commercial, public service
    (1-.00625) * (1-.01)
  } else if(identical(input$mitigations2, c( "End of Trip Facilities", "Private Shuttle"))  == TRUE){ # tau, rec, commercial, public service
    (1-.00625) * (1-(.05 * (1- (input$employee_vmt2/100))))
  } else if(identical(input$mitigations2, c( "End of Trip Facilities","Employee Shuttle", "Private Shuttle"))  == TRUE){ # tau, rec, commercial, public service
    (1-.00625) * (1-(.05 * input$employee_vmt2/100)) * (1-(.05 * (1- (input$employee_vmt2/100))))
  } else if(identical(input$mitigations2, c("Traffic Calming", "End of Trip Facilities", "Private Shuttle"))  == TRUE){ # tau, rec, commercial, public service
    (1-.00625) * (1-.01) * (1-(.05 * (1- (input$employee_vmt2/100))))
  }else if(identical(input$mitigations2, c( "Traffic Calming","Implement CTR Program - Voluntary"))  == TRUE){ # all types
    (1-.01) * (1- (.05* (input$eligible2/100) * (input$employee_vmt2/100)))
  } else if(identical(input$mitigations2, c( "Traffic Calming","Implement CTR Program -  Required Implementation/Monitoring"))  == TRUE){ # all types
    (1-.01) * (1- (.21* (input$eligible2/100) * (input$employee_vmt2/100)))
  } else if(identical(input$mitigations2, c( "Traffic Calming", "Private Shuttle", "Implement CTR Program - Voluntary"))  == TRUE){ # tau and resident
    (1-.01)* (1-(.05 * (1- (input$employee_vmt2/100)))) * (1- (.05* (input$eligible2/100) * (input$employee_vmt2/100)))
  } else if(identical(input$mitigations2, c( "Private Shuttle","Implement CTR Program - Voluntary"))  == TRUE){ # tau and resident
    (1-(.05 * (1- (input$employee_vmt2/100)))) * (1- (.05* (input$eligible2/100) * (input$employee_vmt2/100)))
  }  else if(identical(input$mitigations2, c( "Employee Shuttle","Private Shuttle","Implement CTR Program - Voluntary"))  == TRUE){ # tau only
    (1-(.05 * input$employee_vmt2/100)) * (1-(.05 * (1- (input$employee_vmt2/100)))) * (1- (.05* (input$eligible2/100) * (input$employee_vmt2/100)))
  } else if(identical(input$mitigations2, c( "Employee Shuttle","Private Shuttle","Traffic Calming","Implement CTR Program - Voluntary"))  == TRUE){ # tau only
    (1-(.05 * input$employee_vmt2/100)) * (1-(.05 * (1- (input$employee_vmt2/100)))) (1-.01) * (1- (.05* (input$eligible2/100) * (input$employee_vmt2/100)))
  } else if(identical(input$mitigations2, c("Traffic Calming","Employee Shuttle","Implement CTR Program - Voluntary"))  == TRUE){ # tau, rec, commercial, public service
    (1-(.05 * input$employee_vmt2/100)) * (1-.01) * (1- (.05* (input$eligible2/100) * (input$employee_vmt2/100)))
  } else if(identical(input$mitigations2, c( "Employee Shuttle","Implement CTR Program - Voluntary"))  == TRUE){ # tau, rec, commercial, public service
    (1-(.05 * input$employee_vmt2/100)) * (1- (.05* (input$eligible2/100) * (input$employee_vmt2/100)))
  } else if(identical(input$mitigations2, c( "End of Trip Facilities","Implement CTR Program - Voluntary"))  == TRUE){ # tau, rec, commercial, public service
    (1-.00625) * (1- (.05* (input$eligible2/100) * (input$employee_vmt2/100)))
  } else if(identical(input$mitigations2, c( "End of Trip Facilities","Employee Shuttle","Implement CTR Program - Voluntary"))  == TRUE){ # tau, rec, commercial, public service
    (1-.00625) * (1-(.05 * input$employee_vmt2/100)) * (1- (.05* (input$eligible2/100) * (input$employee_vmt2/100)))
  } else if(identical(input$mitigations2, c( "Traffic Calming", "End of Trip Facilities","Implement CTR Program - Voluntary"))  == TRUE){ # tau, rec, commercial, public service
    (1-.00625) * (1-.01) *  (1- (.05* (input$eligible2/100) * (input$employee_vmt2/100)))
  } else if(identical(input$mitigations2, c( "End of Trip Facilities", "Private Shuttle","Implement CTR Program - Voluntary"))  == TRUE){ # tau, rec, commercial, public service
    (1-.00625) * (1-(.05 * (1- (input$employee_vmt2/100)))) * (1- (.05* (input$eligible2/100) * (input$employee_vmt2/100)))
  } else if(identical(input$mitigations2, c( "End of Trip Facilities","Employee Shuttle", "Private Shuttle","Implement CTR Program - Voluntary"))  == TRUE){ # tau, rec, commercial, public service
    (1-.00625) * (1-(.05 * input$employee_vmt2/100)) * (1-(.05 * (1- (input$employee_vmt2/100)))) * (1- (.05* (input$eligible2/100) * (input$employee_vmt2/100)))
  } else if(identical(input$mitigations2, c( "Traffic Calming", "End of Trip Facilities","Private Shuttle","Implement CTR Program - Voluntary"))  == TRUE){ # tau, rec, commercial, public service
    (1-.00625) * (1-.01) * (1-(.05 * (1- (input$employee_vmt2/100)))) * (1- (.05* (input$eligible2/100) * (input$employee_vmt2/100)))
  } else if(identical(input$mitigations2, c("Implement CTR Program - Voluntary"))  == TRUE){ # tau, rec, commercial, public service
    (1- (.05* (input$eligible2/100) * (input$employee_vmt2/100)))
  } else if(identical(input$mitigations2, c( "Traffic Calming", "Private Shuttle", "Implement CTR Program -  Required Implementation/Monitoring"))  == TRUE){ # tau and resident
    (1-.01)* (1-(.05 * (1- (input$employee_vmt2/100)))) * (1- (.21* (input$eligible2/100) * (input$employee_vmt2/100)))
  } else if(identical(input$mitigations2, c( "Private Shuttle","Implement CTR Program -  Required Implementation/Monitoring"))  == TRUE){ # tau and resident
    (1-(.05 * (1- (input$employee_vmt2/100)))) * (1- (.21* (input$eligible2/100) * (input$employee_vmt2/100)))
  }  else if(identical(input$mitigations2, c( "Employee Shuttle","Private Shuttle","Implement CTR Program -  Required Implementation/Monitoring"))  == TRUE){ # tau only
    (1-(.05 * input$employee_vmt2/100)) * (1-(.05 * (1- (input$employee_vmt2/100)))) * (1- (.21* (input$eligible2/100) * (input$employee_vmt2/100)))
  } else if(identical(input$mitigations2, c("Traffic Calming", "Employee Shuttle","Private Shuttle","Implement CTR Program -  Required Implementation/Monitoring"))  == TRUE){ # tau only
    (1-(.05 * input$employee_vmt2/100)) * (1-(.05 * (1- (input$employee_vmt2/100)))) (1-.01) * (1- (.21* (input$eligible2/100) * (input$employee_vmt2/100)))
  } else if(identical(input$mitigations2, c( "Traffic Calming","Employee Shuttle","Implement CTR Program -  Required Implementation/Monitoring"))  == TRUE){ # tau, rec, commercial, public service
    (1-(.05 * input$employee_vmt2/100)) * (1-.01) * (1- (.21* (input$eligible2/100) * (input$employee_vmt2/100)))
  } else if(identical(input$mitigations2, c( "Employee Shuttle","Implement CTR Program -  Required Implementation/Monitoring"))  == TRUE){ # tau, rec, commercial, public service
    (1-(.05 * input$employee_vmt2/100)) * (1- (.21* (input$eligible2/100) * (input$employee_vmt2/100)))
  } else if(identical(input$mitigations2, c( "End of Trip Facilities","Implement CTR Program -  Required Implementation/Monitoring"))  == TRUE){ # tau, rec, commercial, public service
    (1-.00625) * (1- (.21* (input$eligible2/100) * (input$employee_vmt2/100)))
  } else if(identical(input$mitigations2, c( "End of Trip Facilities","Employee Shuttle","Implement CTR Program -  Required Implementation/Monitoring"))  == TRUE){ # tau, rec, commercial, public service
    (1-.00625) * (1-(.05 * input$employee_vmt2/100)) * (1- (.21* (input$eligible2/100) * (input$employee_vmt2/100)))
  } else if(identical(input$mitigations2, c("Traffic Calming", "End of Trip Facilities", "Private Shuttle","Implement CTR Program -  Required Implementation/Monitoring"))  == TRUE){ # tau, rec, commercial, public service
    (1-.00625) * (1-.01) * (1-(.05 * (1- (input$employee_vmt2/100)))) * (1- (.21* (input$eligible2/100) * (input$employee_vmt2/100)))
  } else if(identical(input$mitigations2, c( "End of Trip Facilities", "Private Shuttle","Implement CTR Program -  Required Implementation/Monitoring"))  == TRUE){ # tau, rec, commercial, public service
    (1-.00625) * (1-(.05 * (1- (input$employee_vmt2/100)))) * (1- (.21* (input$eligible2/100) * (input$employee_vmt2/100)))
  } else if(identical(input$mitigations2, c( "End of Trip Facilities","Employee Shuttle", "Private Shuttle","Implement CTR Program -  Required Implementation/Monitoring"))  == TRUE){ # tau, rec, commercial, public service
    (1-.00625) * (1-(.05 * input$employee_vmt2/100)) * (1-(.05 * (1- (input$employee_vmt2/100)))) * (1- (.21* (input$eligible2/100) * (input$employee_vmt2/100)))
  } else if(identical(input$mitigations2, c("Traffic Calming", "End of Trip Facilities", "Private Shuttle","Implement CTR Program -  Required Implementation/Monitoring"))  == TRUE){ # tau, rec, commercial, public service
    (1-.00625) * (1-.01) * (1-(.05 * (1- (input$employee_vmt2/100)))) * (1- (.21* (input$eligible2/100) * (input$employee_vmt2/100)))
  } else if(identical(input$mitigations2, c( "Implement CTR Program -  Required Implementation/Monitoring"))  == TRUE){ # tau, rec, commercial, public service
    (1- (.21* (input$eligible2/100) * (input$employee_vmt2/100)))
  } else{1}
})
mit_percent_final2<-reactive({
  if(mit_percent2() < .8 & nrow(town_reg_buffer()) > 0){
    .8
  }else if(mit_percent2() < .85 & nrow(town_reg_buffer()) == 0){
    .85
  } else{mit_percent2()}
})
mit_percent3 <- reactive({
  if(identical(input$mitigations3, c("Traffic Calming"))  == TRUE){ # all types
    (1-.01)
  }else if(identical(input$mitigations3, c("Unbundle Parking Costs from Property Cost"))  == TRUE){ # residential only
    (1-.026)
  } else if(identical(input$mitigations3, c("Unbundle Parking Costs from Property Cost", "Traffic Calming")) == TRUE){  # residential only
    (1-.026) * (1-.01)
  }else if(identical(input$mitigations3, c("Unbundle Parking Costs from Property Cost", "Traffic Calming", "Private Shuttle"))  == TRUE){ # residential only
    (1-.026) * (1-.01)* (1-(.05 * (1- (input$employee_vmt3/100))))
  } else if(identical(input$mitigations3, c("Unbundle Parking Costs from Property Cost",  "Private Shuttle"))  == TRUE){ # residential only
    (1-.026) *  (1-(.05 * (1- (input$employee_vmt3/100))))
  }  else if(identical(input$mitigations3, c( "Traffic Calming", "Private Shuttle"))  == TRUE){ # tau and resident
    (1-.01)* (1-(.05 * (1- (input$employee_vmt3/100))))
  } else if(identical(input$mitigations3, c( "Private Shuttle"))  == TRUE){ # tau and resident
    (1-(.05 * (1- (input$employee_vmt3/100))))
  }  else if(identical(input$mitigations3, c( "Employee Shuttle","Private Shuttle"))  == TRUE){ # tau only
    (1-(.05 * input$employee_vmt3/100)) * (1-(.05 * (1- (input$employee_vmt3/100))))
  } else if(identical(input$mitigations3, c("Traffic Calming" ,"Employee Shuttle","Private Shuttle"))  == TRUE){ # tau only
    (1-(.05 * input$employee_vmt3/100)) * (1-(.05 * (1- (input$employee_vmt3/100)))) * (1-.01)
  } else if(identical(input$mitigations3, c("Traffic Calming", "Employee Shuttle"))  == TRUE){ # tau, rec, commercial, public service
    (1-(.05 * input$employee_vmt3/100)) * (1-.01)
  } else if(identical(input$mitigations3, c( "Employee Shuttle"))  == TRUE){ # tau, rec, commercial, public service
    (1-(.05 * input$employee_vmt3/100))
  } else if(identical(input$mitigations3, c( "End of Trip Facilities"))  == TRUE){ # tau, rec, commercial, public service
    (1-.00625)
  } else if(identical(input$mitigations3, c( "End of Trip Facilities","Employee Shuttle"))  == TRUE){ # tau, rec, commercial, public service
    (1-.00625) * (1-(.05 * input$employee_vmt3/100))
  } else if(identical(input$mitigations3, c("Traffic Calming","End of Trip Facilities","Employee Shuttle"))  == TRUE){ # tau, rec, commercial, public service
    (1-.00625) * (1-(.05 * input$employee_vmt3/100)) * (1-.01)
  }else if(identical(input$mitigations3, c("Traffic Calming", "End of Trip Facilities","Employee Shuttle", "Implement CTR Program -  Required Implementation/Monitoring"))  == TRUE){ # tau, rec, commercial, public service
    (1-.00625) * (1-(.05 * input$employee_vmt3/100)) * (1-.01) * (1- (.21* (input$eligible3/100) * (input$employee_vmt3/100)))
  }else if(identical(input$mitigations3, c("Traffic Calming", "End of Trip Facilities","Employee Shuttle","Implement CTR Program - Voluntary"))  == TRUE){ # tau, rec, commercial, public service
    (1-.00625) * (1-(.05 * input$employee_vmt3/100)) * (1-.01) * (1- (.05* (input$eligible3/100) * (input$employee_vmt3/100)))
  } else if(identical(input$mitigations3, c("Traffic Calming", "End of Trip Facilities","Implement CTR Program -  Required Implementation/Monitoring"))  == TRUE){ # tau, rec, commercial, public service
    (1-.00625) * (1-(.05 * input$employee_vmt3/100)) * (1-.01) * (1- (.21* (input$eligible3/100) * (input$employee_vmt3/100)))
  } else if(identical(input$mitigations3, c("Traffic Calming",  "End of Trip Facilities","Private Shuttle"))  == TRUE){ # tau, rec, commercial, public service
    (1-.00625) * (1-.01) * (1-(.05 * (1- (input$employee_vmt3/100))))
  }else if(identical(input$mitigations3, c("Traffic Calming", "End of Trip Facilities"))  == TRUE){ # tau, rec, commercial, public service
    (1-.00625) * (1-.01)
  } else if(identical(input$mitigations3, c( "End of Trip Facilities", "Private Shuttle"))  == TRUE){ # tau, rec, commercial, public service
    (1-.00625) * (1-(.05 * (1- (input$employee_vmt3/100))))
  } else if(identical(input$mitigations3, c( "End of Trip Facilities","Employee Shuttle", "Private Shuttle"))  == TRUE){ # tau, rec, commercial, public service
    (1-.00625) * (1-(.05 * input$employee_vmt3/100)) * (1-(.05 * (1- (input$employee_vmt3/100))))
  } else if(identical(input$mitigations3, c("Traffic Calming", "End of Trip Facilities", "Private Shuttle"))  == TRUE){ # tau, rec, commercial, public service
    (1-.00625) * (1-.01) * (1-(.05 * (1- (input$employee_vmt3/100))))
  }else if(identical(input$mitigations3, c( "Traffic Calming","Implement CTR Program - Voluntary"))  == TRUE){ # all types
    (1-.01) * (1- (.05* (input$eligible3/100) * (input$employee_vmt3/100)))
  } else if(identical(input$mitigations3, c( "Traffic Calming","Implement CTR Program -  Required Implementation/Monitoring"))  == TRUE){ # all types
    (1-.01) * (1- (.21* (input$eligible3/100) * (input$employee_vmt3/100)))
  } else if(identical(input$mitigations3, c( "Traffic Calming", "Private Shuttle", "Implement CTR Program - Voluntary"))  == TRUE){ # tau and resident
    (1-.01)* (1-(.05 * (1- (input$employee_vmt3/100)))) * (1- (.05* (input$eligible3/100) * (input$employee_vmt3/100)))
  } else if(identical(input$mitigations3, c( "Private Shuttle","Implement CTR Program - Voluntary"))  == TRUE){ # tau and resident
    (1-(.05 * (1- (input$employee_vmt3/100)))) * (1- (.05* (input$eligible3/100) * (input$employee_vmt3/100)))
  }  else if(identical(input$mitigations3, c( "Employee Shuttle","Private Shuttle","Implement CTR Program - Voluntary"))  == TRUE){ # tau only
    (1-(.05 * input$employee_vmt3/100)) * (1-(.05 * (1- (input$employee_vmt3/100)))) * (1- (.05* (input$eligible3/100) * (input$employee_vmt3/100)))
  } else if(identical(input$mitigations3, c( "Employee Shuttle","Private Shuttle","Traffic Calming","Implement CTR Program - Voluntary"))  == TRUE){ # tau only
    (1-(.05 * input$employee_vmt3/100)) * (1-(.05 * (1- (input$employee_vmt3/100)))) (1-.01) * (1- (.05* (input$eligible3/100) * (input$employee_vmt3/100)))
  } else if(identical(input$mitigations3, c("Traffic Calming","Employee Shuttle","Implement CTR Program - Voluntary"))  == TRUE){ # tau, rec, commercial, public service
    (1-(.05 * input$employee_vmt3/100)) * (1-.01) * (1- (.05* (input$eligible3/100) * (input$employee_vmt3/100)))
  } else if(identical(input$mitigations3, c( "Employee Shuttle","Implement CTR Program - Voluntary"))  == TRUE){ # tau, rec, commercial, public service
    (1-(.05 * input$employee_vmt3/100)) * (1- (.05* (input$eligible3/100) * (input$employee_vmt3/100)))
  } else if(identical(input$mitigations3, c( "End of Trip Facilities","Implement CTR Program - Voluntary"))  == TRUE){ # tau, rec, commercial, public service
    (1-.00625) * (1- (.05* (input$eligible3/100) * (input$employee_vmt3/100)))
  } else if(identical(input$mitigations3, c( "End of Trip Facilities","Employee Shuttle","Implement CTR Program - Voluntary"))  == TRUE){ # tau, rec, commercial, public service
    (1-.00625) * (1-(.05 * input$employee_vmt3/100)) * (1- (.05* (input$eligible3/100) * (input$employee_vmt3/100)))
  } else if(identical(input$mitigations3, c( "Traffic Calming", "End of Trip Facilities","Implement CTR Program - Voluntary"))  == TRUE){ # tau, rec, commercial, public service
    (1-.00625) * (1-.01) *  (1- (.05* (input$eligible3/100) * (input$employee_vmt3/100)))
  } else if(identical(input$mitigations3, c( "End of Trip Facilities", "Private Shuttle","Implement CTR Program - Voluntary"))  == TRUE){ # tau, rec, commercial, public service
    (1-.00625) * (1-(.05 * (1- (input$employee_vmt3/100)))) * (1- (.05* (input$eligible3/100) * (input$employee_vmt3/100)))
  } else if(identical(input$mitigations3, c( "End of Trip Facilities","Employee Shuttle", "Private Shuttle","Implement CTR Program - Voluntary"))  == TRUE){ # tau, rec, commercial, public service
    (1-.00625) * (1-(.05 * input$employee_vmt3/100)) * (1-(.05 * (1- (input$employee_vmt3/100)))) * (1- (.05* (input$eligible3/100) * (input$employee_vmt3/100)))
  } else if(identical(input$mitigations3, c( "Traffic Calming", "End of Trip Facilities","Private Shuttle","Implement CTR Program - Voluntary"))  == TRUE){ # tau, rec, commercial, public service
    (1-.00625) * (1-.01) * (1-(.05 * (1- (input$employee_vmt3/100)))) * (1- (.05* (input$eligible3/100) * (input$employee_vmt3/100)))
  } else if(identical(input$mitigations3, c("Implement CTR Program - Voluntary"))  == TRUE){ # tau, rec, commercial, public service
    (1- (.05* (input$eligible3/100) * (input$employee_vmt3/100)))
  } else if(identical(input$mitigations3, c( "Traffic Calming", "Private Shuttle", "Implement CTR Program -  Required Implementation/Monitoring"))  == TRUE){ # tau and resident
    (1-.01)* (1-(.05 * (1- (input$employee_vmt3/100)))) * (1- (.21* (input$eligible3/100) * (input$employee_vmt3/100)))
  } else if(identical(input$mitigations3, c( "Private Shuttle","Implement CTR Program -  Required Implementation/Monitoring"))  == TRUE){ # tau and resident
    (1-(.05 * (1- (input$employee_vmt3/100)))) * (1- (.21* (input$eligible3/100) * (input$employee_vmt3/100)))
  }  else if(identical(input$mitigations3, c( "Employee Shuttle","Private Shuttle","Implement CTR Program -  Required Implementation/Monitoring"))  == TRUE){ # tau only
    (1-(.05 * input$employee_vmt3/100)) * (1-(.05 * (1- (input$employee_vmt3/100)))) * (1- (.21* (input$eligible3/100) * (input$employee_vmt3/100)))
  } else if(identical(input$mitigations3, c("Traffic Calming", "Employee Shuttle","Private Shuttle","Implement CTR Program -  Required Implementation/Monitoring"))  == TRUE){ # tau only
    (1-(.05 * input$employee_vmt3/100)) * (1-(.05 * (1- (input$employee_vmt3/100)))) (1-.01) * (1- (.21* (input$eligible3/100) * (input$employee_vmt3/100)))
  } else if(identical(input$mitigations3, c( "Traffic Calming","Employee Shuttle","Implement CTR Program -  Required Implementation/Monitoring"))  == TRUE){ # tau, rec, commercial, public service
    (1-(.05 * input$employee_vmt3/100)) * (1-.01) * (1- (.21* (input$eligible3/100) * (input$employee_vmt3/100)))
  } else if(identical(input$mitigations3, c( "Employee Shuttle","Implement CTR Program -  Required Implementation/Monitoring"))  == TRUE){ # tau, rec, commercial, public service
    (1-(.05 * input$employee_vmt3/100)) * (1- (.21* (input$eligible3/100) * (input$employee_vmt3/100)))
  } else if(identical(input$mitigations3, c( "End of Trip Facilities","Implement CTR Program -  Required Implementation/Monitoring"))  == TRUE){ # tau, rec, commercial, public service
    (1-.00625) * (1- (.21* (input$eligible3/100) * (input$employee_vmt3/100)))
  } else if(identical(input$mitigations3, c( "End of Trip Facilities","Employee Shuttle","Implement CTR Program -  Required Implementation/Monitoring"))  == TRUE){ # tau, rec, commercial, public service
    (1-.00625) * (1-(.05 * input$employee_vmt3/100)) * (1- (.21* (input$eligible3/100) * (input$employee_vmt3/100)))
  } else if(identical(input$mitigations3, c("Traffic Calming", "End of Trip Facilities", "Private Shuttle","Implement CTR Program -  Required Implementation/Monitoring"))  == TRUE){ # tau, rec, commercial, public service
    (1-.00625) * (1-.01) * (1-(.05 * (1- (input$employee_vmt3/100)))) * (1- (.21* (input$eligible3/100) * (input$employee_vmt3/100)))
  } else if(identical(input$mitigations3, c( "End of Trip Facilities", "Private Shuttle","Implement CTR Program -  Required Implementation/Monitoring"))  == TRUE){ # tau, rec, commercial, public service
    (1-.00625) * (1-(.05 * (1- (input$employee_vmt3/100)))) * (1- (.21* (input$eligible3/100) * (input$employee_vmt3/100)))
  } else if(identical(input$mitigations3, c( "End of Trip Facilities","Employee Shuttle", "Private Shuttle","Implement CTR Program -  Required Implementation/Monitoring"))  == TRUE){ # tau, rec, commercial, public service
    (1-.00625) * (1-(.05 * input$employee_vmt3/100)) * (1-(.05 * (1- (input$employee_vmt3/100)))) * (1- (.21* (input$eligible3/100) * (input$employee_vmt3/100)))
  } else if(identical(input$mitigations3, c("Traffic Calming", "End of Trip Facilities", "Private Shuttle","Implement CTR Program -  Required Implementation/Monitoring"))  == TRUE){ # tau, rec, commercial, public service
    (1-.00625) * (1-.01) * (1-(.05 * (1- (input$employee_vmt3/100)))) * (1- (.21* (input$eligible3/100) * (input$employee_vmt3/100)))
  } else if(identical(input$mitigations3, c( "Implement CTR Program -  Required Implementation/Monitoring"))  == TRUE){ # tau, rec, commercial, public service
    (1- (.21* (input$eligible3/100) * (input$employee_vmt3/100)))
  } else{1}
})
mit_percent_final3<-reactive({
  if(mit_percent3() < .8 & nrow(town_reg_buffer()) > 0){
    .8
  }else if(mit_percent3() < .85 & nrow(town_reg_buffer()) == 0){
    .85
  } else{mit_percent3()}
})
  datasetInput <- reactive({
    if(input$analysis_type =="TRPA"){
      dataset
    } else if(input$analysis_type=="SB-743"){
      dataset_full
    }
      })
 output$export_button<-  renderUI({
   req(input$map_shape_click,input$proj_type)
    actionButton("export_button", "Export Project Info")
  })
 observeEvent(input$export_button, {
   showModal(modalDialog(title = "See the attached PDF for your project information",
                         "Please contact TRPA at trpa@trpa.gov for additonal questions and comments about your project's approval process."))
   })
  output$ui_land_use1 <- renderUI({
    if(input$proj_type =='Mixed-Use'){
      selectInput("land_use1", "1) Proposed Project Type",choices=c(sort(c("Residential (Market-Rate)","Residential (Affordable)","Unique Project Type" , unique(trip_rates$use)))))
    } else{return(NULL)}
  })
  output$ui_land_use2 <-  renderUI({
    if(input$proj_type =='Mixed-Use'){
      selectInput("land_use2", "2) Proposed Project Type",choices=c(sort(c("Residential (Market-Rate)","Residential (Affordable)","Unique Project Type" , unique(trip_rates$use)))))
    } else{return(NULL)}
  })
  output$ui_land_use3 <-  renderUI({
    if(input$proj_type =='Mixed-Use'){
      selectInput("land_use3", "3) Proposed Project Type",choices=c(sort(c("Residential (Market-Rate)","Residential (Affordable)","Unique Project Type" , unique(trip_rates$use)))))
    } else{return(NULL)}
  })
  output$mixed_units_1  <-renderUI({
  # req(input$land_use2)
    if (input$proj_type!="Mixed-Use"){
      return()
    }else if (input$land_use1 %in% c("Residential (Market-Rate)","Residential (Affordable)")){
      numericInput("res_units1", "# of Proposed Units",
                   value = 0)
    } else if (input$land_use1 %in% c("Hotel","Motel","Timeshare")){
      numericInput("taus1", "# of Proposed TAUS",
                   value = 0)
    } else if (input$land_use1 =="Unique Project Type"){
      numericInput("input_custom_unit1", "# of Proposed Units",
                   value = 0)
    }else if (input$land_use1 =="Marina"){
      numericInput("berths1", "# of Proposed Berths",
                   value = 0)
    }else if (input$land_use1 %in% c('Developed Campground/RV Park')){
      numericInput("sites1", "# of Sites",
                   value = 0)
    }else if (input$land_use1 %in% c('Public Park')){
      numericInput("acres1", "# of Proposed Acres",
                   value = 0)
    }else if (input$land_use1 %in% c("Golf Course")){
      numericInput("holes1", "# of Holes",
                   value = 0)
    }else if (input$land_use1 %in% c("Bowling Alley")){
      numericInput("lanes1", "# of Lanes",
                   value = 0)
    }else if (input$land_use1 %in% c("Movie Theater (traditional)")){
      numericInput("screens1", "# of Screens",
                   value = 0)
    }else if (input$land_use1 %in% c("University/College","High School","Middle School/Junior High School","Elementary School","Private School (K-12)")){
      numericInput("students1", "# of Students",
                   value = 0)
    }else {
      numericInput("ksf1", "Proposed Square Feet",
                   value = 0)
    }
  })
  output$mixed_units_2 <- renderUI({
    # req(input$map_shape_click)
    if (input$proj_type!="Mixed-Use"){
      return()
    }else if (input$land_use2 %in% c("Residential (Market-Rate)","Residential (Affordable)")){
      numericInput("res_units2", "# of Proposed Units",
                   value = 0)
    } else if (input$land_use2 %in% c("Hotel","Motel","Timeshare")){
      numericInput("taus2", "# of Proposed TAUS",
                   value = 0)
    } else if (input$land_use2 =="Unique Project Type"){
      numericInput("input_custom_unit2", "# of Proposed Units",
                   value = 0)
    }else if (input$land_use2 =="Marina"){
      numericInput("berths2", "# of Proposed Berths",
                   value = 0)
    }else if (input$land_use2 %in% c('Developed Campground/RV Park')){
      numericInput("sites2", "# of Sites",
                   value = 0)
    }else if (input$land_use2 %in% c('Public Park')){
      numericInput("acres2", "# of Proposed Acres",
                   value = 0)
    }else if (input$land_use2 %in% c("Golf Course")){
      numericInput("holes2", "# of Holes",
                   value = 0)
    }else if (input$land_use2 %in% c("Bowling Alley")){
      numericInput("lanes2", "# of Lanes",
                   value = 0)
    }else if (input$land_use2 %in% c("Movie Theater (traditional)")){
      numericInput("screens2", "# of Screens",
                   value = 0)
    }else if (input$land_use2 %in% c("University/College","High School","Middle School/Junior High School","Elementary School","Private School (K-12)")){
      numericInput("students2", "# of Students",
                   value = 0)
    }else {
      numericInput("ksf2", "Proposed Square Feet",
                   value = 0)
    }
  })
  output$mixed_units_3 <- renderUI({
    # req(input$map_shape_click)
    if (input$proj_type!="Mixed-Use"){
      return()
    }else if (input$land_use3 %in% c("Residential (Market-Rate)","Residential (Affordable)")){
      numericInput("res_units3", "# of Proposed Units",
                   value = 0)
    } else if (input$land_use3 %in% c("Hotel","Motel","Timeshare")){
      numericInput("taus3", "# of Proposed TAUS",
                   value = 0)
    } else if (input$land_use3 =="Unique Project Type"){
      numericInput("input_custom_unit3", "# of Proposed Units",
                   value = 0)
    }else if (input$land_use3 =="Marina"){
      numericInput("berths3", "# of Proposed Berths",
                   value = 0)
    }else if (input$land_use3 %in% c('Developed Campground/RV Park')){
      numericInput("sites3", "# of Sites",
                   value = 0)
    }else if (input$land_use3 %in% c('Public Park')){
      numericInput("acres3", "# of Proposed Acres",
                   value = 0)
    }else if (input$land_use3 %in% c("Golf Course")){
      numericInput("holes3", "# of Holes",
                   value = 0)
    }else if (input$land_use3 %in% c("Bowling Alley")){
      numericInput("lanes3", "# of Lanes",
                   value = 0)
    }else if (input$land_use3 %in% c("Movie Theater (traditional)")){
      numericInput("screens3", "# of Screens",
                   value = 0)
    }else if (input$land_use3 %in% c("University/College","High School","Middle School/Junior High School","Elementary School","Private School (K-12)")){
      numericInput("students3", "# of Students",
                   value = 0)
    }else {
      numericInput("ksf3", "Proposed Square Feet",
                   value = 0)
    }
  })
  output$ui_project_type <- renderUI({
    # req(input$map_shape_click)
    if (is.null(input$proj_type) | input$proj_type == "Mixed-Use"){
      return()
    }else if (input$proj_type %in% c("Residential (Market-Rate)","Residential (Affordable)")){
      numericInput("res_units", "# of Proposed Units",
                   value = 0)
    } else if (input$proj_type %in% c("Hotel","Motel","Timeshare")){
      numericInput("taus", "# of Proposed TAUS",
                   value = 0)
    } else if (input$proj_type =="Unique Project Type"){
      numericInput("input_custom_unit", "# of Proposed Units",
                   value = 0)
    }else if (input$proj_type =="Marina"){
      numericInput("berths", "# of Proposed Berths",
                   value = 0)
    }else if (input$proj_type %in% c('Public Park')){
      numericInput("acres", "# of Proposed Acres",
                   value = 0)
    }else if (input$proj_type %in% c("Golf Course")){
      numericInput("holes", "# of Holes",
                   value = 0)
    }else if (input$proj_type %in% c("Bowling Alley")){
      numericInput("lanes", "# of Lanes",
                   value = 0)
    }else if (input$proj_type %in% c("Movie Theater (traditional)")){
      numericInput("screens", "# of Screens",
                   value = 0)
    }else if (input$proj_type %in% c('Developed Campground/RV Park')){
      numericInput("sites", "# of Sites",
                   value = 0)
    }else if (input$proj_type %in% c("University/College","High School","Middle School/Junior High School","Elementary School","Private School (K-12)")){
      numericInput("students", "# of Students",
                   value = 0)
    }else {
      numericInput("ksf", "Proposed Square Feet",
                   value = 0)
    }
  })
  output$ui_redev_land_use <-renderUI({
    # req(input$map_shape_click)
    if(input$proj_type=="Mixed-Use"){
      return()
    } else{
    if (input$redevelopment=="No"){
      return()
    }else if (input$redevelopment=="Yes"){
      selectInput("current_use", "Existing Land Use",choices=c(sort(c("Residential (Market-Rate)","Residential (Affordable)","Unique Project Type" ,
                                                                      unique(trip_rates$use)))))
    } else {
      return()
    }
    }
  })
  output$ui_redev_units <-renderUI({
    # req(input$map_shape_click)
    if(input$proj_type=="Mixed-Use"){
      return()
    } else{
    if (input$redevelopment=="No"){
      return()
    }else if (input$redevelopment=="Yes"){
      if (input$current_use %in% c("Residential (Market-Rate)","Residential (Affordable)")){
        numericInput("res_units_redev", "Existing Residential Units",value = 0)
      } else if(input$current_use %in% c('Auto Parts and Service Center','General retail', 'Furniture Store', 'Pharmacy/Drugstore', 'Supermarket', 'Drive-In Bank', 'High Turnover Sit-Down Restaurant (<1 hr. turnover)', 'Fast Food Restaurant', 'Quality Restaurant (>1 hr. turnover)', 'Drinking Place', 'Building Materials/Lumber', 'Free-Standing Discount Store', 'General Office Building (GFA of more than 5,000 sf)', 'Medical –Dental Office Building', 'Light industrial', 'Warehouse', 'Automobile Sales',  'Health and Fitness Club', 'Recreational Community Center',  'Church', 'Daycare Center', 'Library', 'Hospital')) {
        numericInput("ksf_redev", "Existing Square Footage",value = 0)
      }else if (input$current_use %in% c("Hotel","Motel","Timeshare")){
        numericInput("taus_redev", "Existing TAUS",value = 0)
      } else if (input$current_use =="Unique Project Type"){
        numericInput("input_custom_unit_redev", "Existing Units",value = 0)
      }else if (input$current_use =="Marina"){
        numericInput("berths_redev", "Existing Berths",value = 0)
      }else if (input$current_use %in% c('Public Park',)){
        numericInput("acres_redev", "Existing Acres",value = 0)
      }else if (input$current_use %in% c("Golf Course")){
        numericInput("holes_redev", "# of Holes",value = 0)
      }else if (input$current_use %in% c("Bowling Alley")){
        numericInput("lanes_redev", "# of Lanes",value = 0)
      }else if (input$current_use %in% c("Movie Theater (traditional)")){
        numericInput("screens_redev", "# of Screens",value = 0)
      }else if (input$current_use %in% c('Developed Campground/RV Park')){
        numericInput("sites_redev", "# of Sites",value = 0)
      }else if (input$current_use %in% c("University/College","High School","Middle School/Junior High School","Elementary School","Private School (K-12)")){
        numericInput("students_redev", "# of Students",value = 0)
        }
    }
    }
  })
  output$custom_rate_redev<- renderUI({
    if(input$proj_type=="Mixed-Use"){
      return()
    } else{
      if (input$redevelopment=="No"){
        return()}
      else if(input$current_use =="Unique Project Type"){
        numericInput("input_custom_rate_redev","Custom Rate",value=0)
      }
    }
  }
  )
  
  output$custom_rate <- renderUI({
   # req(input$map_shape_click)
    if (input$proj_type == "Unique Project Type"){
      numericInput("input_custom_rate", "Trip Rate",
                   value = 0)
    } else {
      return()
    }
  })
  output$custom_rate1 <- renderUI({
    # req(input$map_shape_click)
    if (input$land_use1 == "Unique Project Type"){
      numericInput("input_custom_rate1", "Trip Rate",
                   value = 0)
    } else {
      return()
    }
  })
  output$custom_rate2 <- renderUI({
    # req(input$map_shape_click)
    if (input$land_use2 == "Unique Project Type"){
      numericInput("input_custom_rate2", "Trip Rate",
                   value = 0)
    } else {
      return()
    }
  })
  output$custom_rate3 <- renderUI({
    # req(input$map_shape_click)
    if (input$land_use3 == "Unique Project Type"){
      numericInput("input_custom_rate3", "Trip Rate",
                   value = 0)
    } else {
      return()
    }
  })
  output$custom_type <- renderUI({
   # req(input$map_shape_click)
    if (input$proj_type == "Unique Project Type"){
      textInput("custom_proj_type","Enter Project Type")
    } else {
      return()
    }
  })
  output$custom_type1 <- renderUI({
    # req(input$map_shape_click)
    if (input$land_use1 == "Unique Project Type"){
      textInput("custom_proj_type1","Enter Project Type")
    } else {
      return()
    }
  })
  output$custom_type2 <- renderUI({
    # req(input$map_shape_click)
    if (input$land_use2 == "Unique Project Type"){
      textInput("custom_proj_type2","Enter Project Type")
    } else {
      return()
    }
  })
  output$custom_type3 <- renderUI({
    # req(input$map_shape_click)
    if (input$land_use3 == "Unique Project Type"){
      textInput("custom_proj_type3","Enter Project Type")
    } else {
      return()
    }
  })
  output$ui_redevelopment <- renderUI({
    req(input$map_shape_click, input$proj_type)
    if(input$proj_type=="Mixed-Use"){
      return()
    } else{
    radioButtons(inputId="redevelopment", label="Change in Operation", selected="No",
                       choices=c("Yes","No"))
    }
  })
  # output$ui_low_income <-renderUI({
   # req(input$proj_type)
  #   if(input$proj_type!="Residential"){return()
  #   } else{
  #     checkboxInput("low_income_screen", "Low Income Units")
  #   }
  # })
  # output$ui_low_income1 <- renderUI({
  #    #req(input$land_use1)
  #   if(input$land_use1!="Residential"){return()
  #   }else if(input$proj_type!="Mixed-Use"){return()
  #   } else{
  #     checkboxInput("low_income_screen1", "Low Income Units")
  #   }
  # })
  # output$ui_low_income2 <- renderUI({
  #   #req(input$land_use1)
  #   if(input$land_use2!="Residential"){return()
  #   }else if(input$proj_type!="Mixed-Use"){return()
  #   } else{
  #     checkboxInput("low_income_screen2", "Low Income Units")
  #   }
  # })
  # output$ui_low_income3 <- renderUI({
  #   #req(input$land_use1)
  #   if(input$land_use3!="Residential"){return()
  #   }else if(input$proj_type!="Mixed-Use"){return()
  #   } else{
  #     checkboxInput("low_income_screen3", "Low Income Units")
  #   }
  # })
  map_event <-reactive({ 
   # req(input$map_shape_click)
    input$map_shape_click$id
  })
  output$map <- renderLeaflet({
    datasetInput() %>%
      leaflet() %>% addProviderTiles("OpenStreetMap.HOT") %>%
      addPolygons(data=boundary,color="black",options = pathOptions(interactive = FALSE), fill=F) %>%
      addPolygons(data=town_reg_half , options = pathOptions(interactive = FALSE), group="Town & Regional Center 1/2 Mile Buffer", fill=F, opacity=1) %>%
      addPolygons(data=jur,options = pathOptions(interactive = FALSE), group="Jurisdictions/Subregions", fillColor = "white",fillOpacity = .5, opacity=1, color="#C6C82C") %>%
      addStaticLabels(data=jur, label=jur$name,group="Jurisdictions/Subregions" ,
                      style = list("color" = "black", 
                                   "font-weight" = "bold",
                                   "font-size" = "12px")) %>%
      addPolygons(data=bonus_boundary,options = pathOptions(interactive = FALSE), group="Bonus Unit Eligible Areas", fill=F, color="purple", opacity=1) %>%
      addPolygons(data=res_data_sf,options = pathOptions(interactive = FALSE),weight=2, fillColor=~pal(cat1), color="white", fillOpacity = 0.6,
                  label = lapply(popup, HTML), group="Zone Average Residential VMT",
                  highlightOptions = highlightOptions(stroke = 4, weight = 4, color="black", opacity=1)) %>% 
      addLegend(data=res_data_sf, pal = pal, values = ~cat1, opacity = 1, title="Zone Average Residential VMT", position="bottomright",group="Zone Average Residential VMT") %>% 
      addPolygons(data=length_jur,options = pathOptions(interactive = FALSE),weight=2, fillColor=~pal_length_jur(cat1), color="white", fillOpacity = 0.6,
                  label = lapply(popup_length_jur, HTML), group="Zone Average Trip Lengths",
                  highlightOptions = highlightOptions(stroke = 4, weight = 4, color="black", opacity=1)) %>%
      addLegend(data=length_jur, pal = pal_length_jur, values = ~cat1, opacity = 1, title="Zone Average Trip Lengths", position="bottomright", group="Zone Average Trip Lengths") %>%
      #addEsriFeatureLayer(url="https://maps.trpa.org/server/rest/services/Parcels/MapServer/0") %>% addTiles() %>% groupOptions("Parcels", zoomLevels = 15:20)  %>%
      addPolygons(layerId=~zone_id,opacity = 0, fillOpacity = 0) %>%
      addLayersControl(
        overlayGroups = c("Zone Average Residential VMT","Zone Average Trip Lengths","Town & Regional Center 1/2 Mile Buffer","Bonus Unit Eligible Areas","Jurisdictions/Subregions"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>% hideGroup(c("Zone Average Residential VMT","Zone Average Trip Lengths","Town & Regional Center 1/2 Mile Buffer","Bonus Unit Eligible Areas","Jurisdictions/Subregions"))
  })
  observeEvent(input$map_click, {
    click <- input$map_click
    text<-paste("Latitude ", round(click$lat,2), "Longtitude ", round(click$lng,2))
    proxy <- leafletProxy("map")
    ## This displays the pin drop circle
    proxy %>%
      clearGroup("new_point") %>%
      addMarkers(click$lng, click$lat, group = "new_point")
  })
  proj_loc <- reactive({
    click <- input$map_click
    points <- data.frame(lat=as.numeric(click$lat), lon=as.numeric(click$lng))
    points %>% st_as_sf(crs=4326, coords=c("lon","lat"))
      })
  in_out_region<-reactive({
    st_intersection(proj_loc(), st_buffer(boundary, 0))
  })
  town_reg_buffer<-reactive({
    st_intersection(proj_loc(), st_buffer(buffer_parcel_final, 0))
  })
  transit<-reactive({
    st_intersection(proj_loc(), st_buffer(transit_buffer, 0))
  })
tot_vmt_react<-reactive({
  if(input$proj_type != "Mixed-Use"){
    if(input$proj_type %in% c('Auto Parts and Service Center','General retail', 'Furniture Store', 'Pharmacy/Drugstore', 'Supermarket', 'Drive-In Bank', 'High Turnover Sit-Down Restaurant (<1 hr. turnover)', 'Fast Food Restaurant', 'Quality Restaurant (>1 hr. turnover)', 'Drinking Place', 'Building Materials/Lumber', 'Free-Standing Discount Store', 'General Office Building (GFA of more than 5,000 sf)', 'Medical –Dental Office Building', 'Light industrial', 'Warehouse', 'Automobile Sales',  'Health and Fitness Club', 'Recreational Community Center',  'Church', 'Daycare Center', 'Library', 'Hospital')){
      ((datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length)) * 
         ((input$ksf/1000) * (trip_rates %>% filter(use %in% input$proj_type) %>% select(Rate) %>% pull())) * mit_percent_final()) - display_vmt_redev()
    } else if(input$proj_type %in% c('Public Park')){
      (datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length) * 
        ((input$acres) * (trip_rates %>% filter(use == input$proj_type)  %>% pull(Rate))) * mit_percent_final()) - display_vmt_redev()
    }else if(input$proj_type %in% c('Golf Course')){
      (datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length) * 
         ((input$holes) * (trip_rates %>% filter(use == input$proj_type)  %>% pull(Rate))) * mit_percent_final()) - display_vmt_redev()
    }else if(input$proj_type %in% c("Bowling Alley")){
      (datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length) * 
         ((input$lanes) * (trip_rates %>% filter(use == input$proj_type)  %>% pull(Rate))) * mit_percent_final()) - display_vmt_redev()
    }else if(input$proj_type %in% c("Movie Theater (traditional)")){
      (datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length) * 
         ((input$screens) * (trip_rates %>% filter(use == input$proj_type)  %>% pull(Rate))) * mit_percent_final()) - display_vmt_redev()
    }else if(input$proj_type %in% c("University/College","High School","Middle School/Junior High School","Elementary School","Private School (K-12)")){
      (datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length) * 
         ((input$students) * (trip_rates %>% filter(use == input$proj_type)  %>% pull(Rate))) * mit_percent_final()) - display_vmt_redev()
    }else if(input$proj_type %in% c('Marina')){
      (datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length) * 
        ((input$berths) * (trip_rates %>% filter(use == input$proj_type)  %>% pull(Rate))) * mit_percent_final()) - display_vmt_redev()
    }else if(input$proj_type %in% c("Hotel","Motel","Timeshare")){
      (((datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length)) * 
         (input$taus) * (trip_rates %>% filter(use %in% input$proj_type) %>% select(Rate) %>% pull())) * mit_percent_final()) - display_vmt_redev()
    } else if(input$proj_type %in% c('Developed Campground/RV Park')){
      (((datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length)) * 
          (input$sites) * (trip_rates %>% filter(use %in% input$proj_type) %>% select(Rate) %>% pull())) * mit_percent_final()) - display_vmt_redev()
    }  else if(input$proj_type=="Residential (Affordable)"){
      (((datasetInput() %>% filter(zone_id==map_event()) %>% pull(zone_vmt_per_capita) * 2.3 * input$res_units) * 0.9) * mit_percent_final()) - display_vmt_redev()
    }else if(input$proj_type=="Residential (Market-Rate)"){
      ((datasetInput() %>% filter(zone_id==map_event()) %>% pull(zone_vmt_per_capita) * 2.3 * input$res_units) * mit_percent_final()) - display_vmt_redev()
    } else if (input$proj_type=="Unique Project Type"){
      (((datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length)) * 
         (input$input_custom_unit * input$input_custom_rate)) * mit_percent_final()) - display_vmt_redev()
    } 
  } else if(input$proj_type == "Mixed-Use"){
    if(input$land_use1 %in% c('Auto Parts and Service Center','General retail', 'Furniture Store', 'Pharmacy/Drugstore', 'Supermarket', 'Drive-In Bank', 'High Turnover Sit-Down Restaurant (<1 hr. turnover)', 'Fast Food Restaurant', 'Quality Restaurant (>1 hr. turnover)', 'Drinking Place', 'Building Materials/Lumber', 'Free-Standing Discount Store', 'General Office Building (GFA of more than 5,000 sf)', 'Medical –Dental Office Building', 'Light industrial', 'Warehouse', 'Automobile Sales',  'Health and Fitness Club', 'Recreational Community Center',  'Church', 'Daycare Center', 'Library', 'Hospital')){
      ((datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length)) * 
         (input$ksf1/1000) * (trip_rates %>% filter(use %in% input$land_use1) %>% select(Rate) %>% pull())) * mit_percent_final1()
    } else if(input$land_use1 %in% c('Public Park')){
      datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length) * 
        ((input$acres1) * (trip_rates %>% filter(use == input$land_use1)  %>% pull(Rate))) * mit_percent_final1()
    }else if(input$land_use1 %in% c('Golf Course')){
      (datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length) * 
         ((input$holes1) * (trip_rates %>% filter(use == input$land_use1)  %>% pull(Rate)))) * mit_percent_final1()
    }else if(input$land_use1 %in% c("Bowling Alley")){
      (datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length) * 
         ((input$lanes1) * (trip_rates %>% filter(use == input$land_use1)  %>% pull(Rate)))) * mit_percent_final1()
    }else if(input$land_use1 %in% c("Movie Theater (traditional)")){
      (datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length) * 
         ((input$screens1) * (trip_rates %>% filter(use == input$land_use1)  %>% pull(Rate)))) * mit_percent_final1()
    }else if(input$land_use1 %in% c("University/College","High School","Middle School/Junior High School","Elementary School","Private School (K-12)")){
      (datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length) * 
         ((input$students1) * (trip_rates %>% filter(use == input$land_use1)  %>% pull(Rate)))) * mit_percent_final1()
    }else if(input$land_use1 %in% c('Marina')){
      datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length) * 
        ((input$berths1) * (trip_rates %>% filter(use == input$land_use1)  %>% pull(Rate))) * mit_percent_final1()
    }else if(input$land_use1 %in% c("Hotel","Motel","Timeshare")){
      ((datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length)) * 
         (input$taus1) * (trip_rates %>% filter(use %in% input$land_use1) %>% select(Rate) %>% pull())) * mit_percent_final1()
    } else if(input$land_use1 %in% c('Developed Campground/RV Park')){
      (((datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length)) * 
          (input$sites1) * (trip_rates %>% filter(use %in% input$land_use1) %>% select(Rate) %>% pull()))) * mit_percent_final1()
    }  else if(input$land_use1=="Residential (Affordable)"){
      ((datasetInput() %>% filter(zone_id==map_event()) %>% pull(zone_vmt_per_capita) * 2.3 * input$res_units1) * 0.9) * mit_percent_final1()
    }else if(input$land_use1=="Residential (Market-Rate)"){
      (datasetInput() %>% filter(zone_id==map_event()) %>% pull(zone_vmt_per_capita) * 2.3 * input$res_units1) * mit_percent_final1()
    } else if (input$land_use1=="Unique Project Type"){
      ((datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length)) * 
         (input$input_custom_unit1 * input$input_custom_rate1)) * mit_percent_final1()
    } 
  }
})
tot_vmt_react_proposed<-reactive({
  if(input$proj_type != "Mixed-Use"){
    if(input$proj_type %in% c('Auto Parts and Service Center','General retail', 'Furniture Store', 'Pharmacy/Drugstore', 'Supermarket', 'Drive-In Bank', 'High Turnover Sit-Down Restaurant (<1 hr. turnover)', 'Fast Food Restaurant', 'Quality Restaurant (>1 hr. turnover)', 'Drinking Place', 'Building Materials/Lumber', 'Free-Standing Discount Store', 'General Office Building (GFA of more than 5,000 sf)', 'Medical –Dental Office Building', 'Light industrial', 'Warehouse', 'Automobile Sales',  'Health and Fitness Club', 'Recreational Community Center',  'Church', 'Daycare Center', 'Library', 'Hospital')){
      ((datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length)) * 
         ((input$ksf/1000) * (trip_rates %>% filter(use %in% input$proj_type) %>% select(Rate) %>% pull()))) 
    } else if(input$proj_type %in% c('Public Park')){
      (datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length) * 
         ((input$acres) * (trip_rates %>% filter(use == input$proj_type)  %>% pull(Rate)))) 
    }else if(input$proj_type %in% c('Golf Course')){
      (datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length) * 
         ((input$holes) * (trip_rates %>% filter(use == input$proj_type)  %>% pull(Rate)))) 
    }else if(input$proj_type %in% c("Bowling Alley")){
      (datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length) * 
         ((input$lanes) * (trip_rates %>% filter(use == input$proj_type)  %>% pull(Rate)))) 
    }else if(input$proj_type %in% c("Movie Theater (traditional)")){
      (datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length) * 
         ((input$screens) * (trip_rates %>% filter(use == input$proj_type)  %>% pull(Rate)))) 
    }else if(input$proj_type %in% c("University/College","High School","Middle School/Junior High School","Elementary School","Private School (K-12)")){
      (datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length) * 
         ((input$students) * (trip_rates %>% filter(use == input$proj_type)  %>% pull(Rate))))
    }else if(input$proj_type %in% c('Marina')){
      (datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length) * 
         ((input$berths) * (trip_rates %>% filter(use == input$proj_type)  %>% pull(Rate)))) 
    }else if(input$proj_type %in% c("Hotel","Motel","Timeshare")){
      (((datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length)) * 
          (input$taus) * (trip_rates %>% filter(use %in% input$proj_type) %>% select(Rate) %>% pull()))) 
    } else if(input$proj_type %in% c('Developed Campground/RV Park')){
      (((datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length)) * 
          (input$sites) * (trip_rates %>% filter(use %in% input$proj_type) %>% select(Rate) %>% pull()))) 
    }  else if(input$proj_type=="Residential (Affordable)"){
      (((datasetInput() %>% filter(zone_id==map_event()) %>% pull(zone_vmt_per_capita) * 2.3 * input$res_units) * 0.9)) 
    }else if(input$proj_type=="Residential (Market-Rate)"){
      ((datasetInput() %>% filter(zone_id==map_event()) %>% pull(zone_vmt_per_capita) * 2.3 * input$res_units))
    } else if (input$proj_type=="Unique Project Type"){
      (((datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length)) * 
          (input$input_custom_unit * input$input_custom_rate)))
    } 
  } else if(input$proj_type == "Mixed-Use"){
    if(input$land_use1 %in% c('Auto Parts and Service Center','General retail', 'Furniture Store', 'Pharmacy/Drugstore', 'Supermarket', 'Drive-In Bank', 'High Turnover Sit-Down Restaurant (<1 hr. turnover)', 'Fast Food Restaurant', 'Quality Restaurant (>1 hr. turnover)', 'Drinking Place', 'Building Materials/Lumber', 'Free-Standing Discount Store', 'General Office Building (GFA of more than 5,000 sf)', 'Medical –Dental Office Building', 'Light industrial', 'Warehouse', 'Automobile Sales',  'Health and Fitness Club', 'Recreational Community Center',  'Church', 'Daycare Center', 'Library', 'Hospital')){
      ((datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length)) * 
         (input$ksf1/1000) * (trip_rates %>% filter(use %in% input$land_use1) %>% select(Rate) %>% pull())) 
    } else if(input$land_use1 %in% c('Public Park')){
      datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length) * 
        ((input$acres1) * (trip_rates %>% filter(use == input$land_use1)  %>% pull(Rate)))
    }else if(input$land_use1 %in% c('Golf Course')){
      (datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length) * 
         ((input$holes1) * (trip_rates %>% filter(use == input$land_use1)  %>% pull(Rate)))) 
    }else if(input$land_use1 %in% c("Bowling Alley")){
      (datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length) * 
         ((input$lanes1) * (trip_rates %>% filter(use == input$land_use1)  %>% pull(Rate)))) 
    }else if(input$land_use1 %in% c("Movie Theater (traditional)")){
      (datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length) * 
         ((input$screens1) * (trip_rates %>% filter(use == input$land_use1)  %>% pull(Rate)))) 
    }else if(input$land_use1 %in% c("University/College","High School","Middle School/Junior High School","Elementary School","Private School (K-12)")){
      (datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length) * 
         ((input$students1) * (trip_rates %>% filter(use == input$land_use1)  %>% pull(Rate)))) 
    }else if(input$land_use1 %in% c('Marina')){
      datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length) * 
        ((input$berths1) * (trip_rates %>% filter(use == input$land_use1)  %>% pull(Rate))) 
    }else if(input$land_use1 %in% c("Hotel","Motel","Timeshare")){
      ((datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length)) * 
         (input$taus1) * (trip_rates %>% filter(use %in% input$land_use1) %>% select(Rate) %>% pull())) 
    } else if(input$land_use1 %in% c('Developed Campground/RV Park')){
      (((datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length)) * 
          (input$sites1) * (trip_rates %>% filter(use %in% input$land_use1) %>% select(Rate) %>% pull()))) 
    }  else if(input$land_use1=="Residential (Affordable)"){
      ((datasetInput() %>% filter(zone_id==map_event()) %>% pull(zone_vmt_per_capita) * 2.3 * input$res_units1) * 0.9) 
    }else if(input$land_use1=="Residential (Market-Rate)"){
      (datasetInput() %>% filter(zone_id==map_event()) %>% pull(zone_vmt_per_capita) * 2.3 * input$res_units1) 
    } else if (input$land_use1=="Unique Project Type"){
      ((datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length)) * 
         (input$input_custom_unit1 * input$input_custom_rate1))
    } 
  }
})
tot_vmt_react2<-reactive({
  if(input$proj_type != "Mixed-Use"){
    return() 
  } else if(input$proj_type == "Mixed-Use"){
    if(input$land_use2 %in% c('Auto Parts and Service Center','General retail', 'Furniture Store', 'Pharmacy/Drugstore', 'Supermarket', 'Drive-In Bank', 'High Turnover Sit-Down Restaurant (<1 hr. turnover)', 'Fast Food Restaurant', 'Quality Restaurant (>1 hr. turnover)', 'Drinking Place', 'Building Materials/Lumber', 'Free-Standing Discount Store', 'General Office Building (GFA of more than 5,000 sf)', 'Medical –Dental Office Building', 'Light industrial', 'Warehouse', 'Automobile Sales',  'Health and Fitness Club', 'Recreational Community Center',  'Church', 'Daycare Center', 'Library', 'Hospital')){
      ((datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length)) * 
         (input$ksf2/1000) * (trip_rates %>% filter(use %in% input$land_use2) %>% select(Rate) %>% pull())) * mit_percent_final2()
    } else if(input$land_use2 %in% c('Public Park')){
      datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length) * 
        ((input$acres2) * (trip_rates %>% filter(use == input$land_use2)  %>% pull(Rate))) * mit_percent_final2()
    }else if(input$land_use2 %in% c('Marina')){
      datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length) * 
        ((input$berths2) * (trip_rates %>% filter(use == input$land_use2)  %>% pull(Rate))) * mit_percent_final2()
    }else if(input$land_use2 %in% c('Golf Course')){
      (datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length) * 
         ((input$holes2) * (trip_rates %>% filter(use == input$land_use2)  %>% pull(Rate)))) * mit_percent_final2()
    }else if(input$land_use2 %in% c("Bowling Alley")){
      (datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length) * 
         ((input$lanes2) * (trip_rates %>% filter(use == input$land_use2)  %>% pull(Rate)))) * mit_percent_final()
    }else if(input$land_use2 %in% c("Movie Theater (traditional)")){
      (datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length) * 
         ((input$screens2) * (trip_rates %>% filter(use == input$land_use2)  %>% pull(Rate)))) * mit_percent_final2()
    }else if(input$land_use2 %in% c("University/College","High School","Middle School/Junior High School","Elementary School","Private School (K-12)")){
      (datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length) * 
         ((input$students2) * (trip_rates %>% filter(use == input$land_use2)  %>% pull(Rate)))) * mit_percent_final2()
    }else if(input$land_use2 %in% c("Hotel","Motel","Timeshare")){
      ((datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length)) * 
         (input$taus2) * (trip_rates %>% filter(use %in% input$land_use2) %>% select(Rate) %>% pull())) * mit_percent_final2()
    } else if(input$land_use2 %in% c('Developed Campground/RV Park')){
      (((datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length)) * 
          (input$sites2) * (trip_rates %>% filter(use %in% input$land_use2) %>% select(Rate) %>% pull()))) * mit_percent_final2()
    }  else if(input$land_use2=="Residential (Affordable)"){
      ((datasetInput() %>% filter(zone_id==map_event()) %>% pull(zone_vmt_per_capita) * 2.3 * input$res_units2) * 0.9) * mit_percent_final2()
    }else if(input$land_use2=="Residential (Market-Rate)"){
      (datasetInput() %>% filter(zone_id==map_event()) %>% pull(zone_vmt_per_capita) * 2.3 * input$res_units2) * mit_percent_final2()
    } else if (input$land_use2=="Unique Project Type"){
      ((datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length)) * 
         (input$input_custom_unit2 * input$input_custom_rate2)) * mit_percent_final2()
    } 
  }
})
tot_vmt_react2_proposed<-reactive({
  if(input$proj_type != "Mixed-Use"){
    return() 
  } else if(input$proj_type == "Mixed-Use"){
    if(input$land_use2 %in% c('Auto Parts and Service Center','General retail', 'Furniture Store', 'Pharmacy/Drugstore', 'Supermarket', 'Drive-In Bank', 'High Turnover Sit-Down Restaurant (<1 hr. turnover)', 'Fast Food Restaurant', 'Quality Restaurant (>1 hr. turnover)', 'Drinking Place', 'Building Materials/Lumber', 'Free-Standing Discount Store', 'General Office Building (GFA of more than 5,000 sf)', 'Medical –Dental Office Building', 'Light industrial', 'Warehouse', 'Automobile Sales',  'Health and Fitness Club', 'Recreational Community Center',  'Church', 'Daycare Center', 'Library', 'Hospital')){
      ((datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length)) * 
         (input$ksf2/1000) * (trip_rates %>% filter(use %in% input$land_use2) %>% select(Rate) %>% pull()))
    } else if(input$land_use2 %in% c('Public Park')){
      datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length) * 
        ((input$acres2) * (trip_rates %>% filter(use == input$land_use2)  %>% pull(Rate))) 
    }else if(input$land_use2 %in% c('Marina')){
      datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length) * 
        ((input$berths2) * (trip_rates %>% filter(use == input$land_use2)  %>% pull(Rate))) 
    }else if(input$land_use2 %in% c('Golf Course')){
      (datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length) * 
         ((input$holes2) * (trip_rates %>% filter(use == input$land_use2)  %>% pull(Rate)))) 
    }else if(input$land_use2 %in% c("Bowling Alley")){
      (datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length) * 
         ((input$lanes2) * (trip_rates %>% filter(use == input$land_use2)  %>% pull(Rate)))) 
    }else if(input$land_use2 %in% c("Movie Theater (traditional)")){
      (datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length) * 
         ((input$screens2) * (trip_rates %>% filter(use == input$land_use2)  %>% pull(Rate)))) 
    }else if(input$land_use2 %in% c("University/College","High School","Middle School/Junior High School","Elementary School","Private School (K-12)")){
      (datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length) * 
         ((input$students2) * (trip_rates %>% filter(use == input$land_use2)  %>% pull(Rate)))) 
    }else if(input$land_use2 %in% c("Hotel","Motel","Timeshare")){
      ((datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length)) * 
         (input$taus2) * (trip_rates %>% filter(use %in% input$land_use2) %>% select(Rate) %>% pull())) 
    } else if(input$land_use2 %in% c('Developed Campground/RV Park')){
      (((datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length)) * 
          (input$sites2) * (trip_rates %>% filter(use %in% input$land_use2) %>% select(Rate) %>% pull()))) 
    }  else if(input$land_use2=="Residential (Affordable)"){
      ((datasetInput() %>% filter(zone_id==map_event()) %>% pull(zone_vmt_per_capita) * 2.3 * input$res_units2) * 0.9)
    }else if(input$land_use2=="Residential (Market-Rate)"){
      (datasetInput() %>% filter(zone_id==map_event()) %>% pull(zone_vmt_per_capita) * 2.3 * input$res_units2) 
    } else if (input$land_use2=="Unique Project Type"){
      ((datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length)) * 
         (input$input_custom_unit2 * input$input_custom_rate2)) 
    } 
  }
})
tot_vmt_react3<-reactive({
  if(input$proj_type != "Mixed-Use"){
    return() 
  } else if(input$proj_type == "Mixed-Use"){
    if(input$land_use3 %in% c('Auto Parts and Service Center','General retail', 'Furniture Store', 'Pharmacy/Drugstore', 'Supermarket', 'Drive-In Bank', 'High Turnover Sit-Down Restaurant (<1 hr. turnover)', 'Fast Food Restaurant', 'Quality Restaurant (>1 hr. turnover)', 'Drinking Place', 'Building Materials/Lumber', 'Free-Standing Discount Store', 'General Office Building (GFA of more than 5,000 sf)', 'Medical –Dental Office Building', 'Light industrial', 'Warehouse', 'Automobile Sales',  'Health and Fitness Club', 'Recreational Community Center',  'Church', 'Daycare Center', 'Library', 'Hospital')){
      ((datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length)) * 
         (input$ksf3/1000) * (trip_rates %>% filter(use %in% input$land_use3) %>% select(Rate) %>% pull())) * mit_percent_final3()
    } else if(input$land_use3 %in% c('Public Park')){
      datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length) * 
        ((input$acres3) * (trip_rates %>% filter(use == input$land_use3)  %>% pull(Rate))) * mit_percent_final3()
    }else if(input$land_use3 %in% c('Marina')){
      datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length) * 
        ((input$berths3) * (trip_rates %>% filter(use == input$land_use3)  %>% pull(Rate))) * mit_percent_final3()
    }else if(input$land_use3 %in% c('Golf Course')){
      (datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length) * 
         ((input$holes3) * (trip_rates %>% filter(use == input$land_use3)  %>% pull(Rate)))) * mit_percent_final3()
    }else if(input$land_use3 %in% c("Bowling Alley")){
      (datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length) * 
         ((input$lanes3) * (trip_rates %>% filter(use == input$land_use3)  %>% pull(Rate)))) * mit_percent_final3()
    }else if(input$land_use3 %in% c("Movie Theater (traditional)")){
      (datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length) * 
         ((input$screens3) * (trip_rates %>% filter(use == input$land_use3)  %>% pull(Rate)))) * mit_percent_final3()
    }else if(input$land_use3 %in% c("University/College","High School","Middle School/Junior High School","Elementary School","Private School (K-12)")){
      (datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length) * 
         ((input$students3) * (trip_rates %>% filter(use == input$land_use3)  %>% pull(Rate)))) * mit_percent_final3()
    }else if(input$land_use3 %in% c("Hotel","Motel","Timeshare")){
      ((datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length)) * 
         (input$taus3) * (trip_rates %>% filter(use %in% input$land_use3) %>% select(Rate) %>% pull())) * mit_percent_final3()
    } else if(input$land_use3 %in% c('Developed Campground/RV Park')){
      (((datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length)) * 
          (input$sites3) * (trip_rates %>% filter(use %in% input$land_use3) %>% select(Rate) %>% pull()))) * mit_percent_final3()
    }   else if(input$land_use3=="Residential (Affordable)"){
      ((datasetInput() %>% filter(zone_id==map_event()) %>% pull(zone_vmt_per_capita) * 2.3 * input$res_units3) * 0.9) * mit_percent_final3()
    }else if(input$land_use3=="Residential (Market-Rate)"){
      (datasetInput() %>% filter(zone_id==map_event()) %>% pull(zone_vmt_per_capita) * 2.3 * input$res_units3) * mit_percent_final3()
    } else if (input$land_use3=="Unique Project Type"){
      ((datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length)) * 
         (input$input_custom_unit3 * input$input_custom_rate3)) * mit_percent_final3()
    } 
  }
})
tot_vmt_react3_proposed<-reactive({
  if(input$proj_type != "Mixed-Use"){
    return() 
  } else if(input$proj_type == "Mixed-Use"){
    if(input$land_use3 %in% c('Auto Parts and Service Center','General retail', 'Furniture Store', 'Pharmacy/Drugstore', 'Supermarket', 'Drive-In Bank', 'High Turnover Sit-Down Restaurant (<1 hr. turnover)', 'Fast Food Restaurant', 'Quality Restaurant (>1 hr. turnover)', 'Drinking Place', 'Building Materials/Lumber', 'Free-Standing Discount Store', 'General Office Building (GFA of more than 5,000 sf)', 'Medical –Dental Office Building', 'Light industrial', 'Warehouse', 'Automobile Sales',  'Health and Fitness Club', 'Recreational Community Center',  'Church', 'Daycare Center', 'Library', 'Hospital')){
      ((datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length)) * 
         (input$ksf3/1000) * (trip_rates %>% filter(use %in% input$land_use3) %>% select(Rate) %>% pull())) 
    } else if(input$land_use3 %in% c('Public Park')){
      datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length) * 
        ((input$acres3) * (trip_rates %>% filter(use == input$land_use3)  %>% pull(Rate)))
    }else if(input$land_use3 %in% c('Marina')){
      datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length) * 
        ((input$berths3) * (trip_rates %>% filter(use == input$land_use3)  %>% pull(Rate))) 
    }else if(input$land_use3 %in% c('Golf Course')){
      (datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length) * 
         ((input$holes3) * (trip_rates %>% filter(use == input$land_use3)  %>% pull(Rate)))) 
    }else if(input$land_use3 %in% c("Bowling Alley")){
      (datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length) * 
         ((input$lanes3) * (trip_rates %>% filter(use == input$land_use3)  %>% pull(Rate)))) 
    }else if(input$land_use3 %in% c("Movie Theater (traditional)")){
      (datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length) * 
         ((input$screens3) * (trip_rates %>% filter(use == input$land_use3)  %>% pull(Rate))))
    }else if(input$land_use3 %in% c("University/College","High School","Middle School/Junior High School","Elementary School","Private School (K-12)")){
      (datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length) * 
         ((input$students3) * (trip_rates %>% filter(use == input$land_use3)  %>% pull(Rate)))) 
    }else if(input$land_use3 %in% c("Hotel","Motel","Timeshare")){
      ((datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length)) * 
         (input$taus3) * (trip_rates %>% filter(use %in% input$land_use3) %>% select(Rate) %>% pull())) 
    } else if(input$land_use3 %in% c('Developed Campground/RV Park')){
      (((datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length)) * 
          (input$sites3) * (trip_rates %>% filter(use %in% input$land_use3) %>% select(Rate) %>% pull())))
    }   else if(input$land_use3=="Residential (Affordable)"){
      ((datasetInput() %>% filter(zone_id==map_event()) %>% pull(zone_vmt_per_capita) * 2.3 * input$res_units3) * 0.9) 
    }else if(input$land_use3=="Residential (Market-Rate)"){
      (datasetInput() %>% filter(zone_id==map_event()) %>% pull(zone_vmt_per_capita) * 2.3 * input$res_units3) 
    } else if (input$land_use3=="Unique Project Type"){
      ((datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length)) * 
         (input$input_custom_unit3 * input$input_custom_rate3)) 
    } 
  }
})
display_vmt_redev <- reactive({
  if(input$redevelopment == "No"){
    0 } else{tot_vmt_react_redev() }
})
tot_vmt_react_redev<-reactive({
  if(input$proj_type == "Mixed-Use"){
    return() 
  } else if(input$proj_type != "Mixed-Use"){
    if(input$current_use %in% c('Auto Parts and Service Center','General retail', 'Furniture Store', 'Pharmacy/Drugstore', 'Supermarket', 'Drive-In Bank', 'High Turnover Sit-Down Restaurant (<1 hr. turnover)', 'Fast Food Restaurant', 'Quality Restaurant (>1 hr. turnover)', 'Drinking Place', 'Building Materials/Lumber', 'Free-Standing Discount Store', 'General Office Building (GFA of more than 5,000 sf)', 'Medical –Dental Office Building', 'Light industrial', 'Warehouse', 'Automobile Sales',  'Health and Fitness Club', 'Recreational Community Center',  'Church', 'Daycare Center', 'Library', 'Hospital')){
      ((datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length)) * 
         (input$ksf_redev/1000) * (trip_rates %>% filter(use %in% input$current_use) %>% select(Rate) %>% pull()))
    } else if(input$current_use %in% c('Public Park')){
      datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length) * 
        ((input$acres_redev) * (trip_rates %>% filter(use == input$current_use)  %>% pull(Rate))) 
    }else if(input$current_use %in% c('Marina')){
      datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length) * 
        ((input$berths_redev) * (trip_rates %>% filter(use == input$current_use)  %>% pull(Rate))) 
    }else if(input$current_use %in% c('Golf Course')){
      (datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length) * 
         ((input$holes_redev) * (trip_rates %>% filter(use == input$current_use)  %>% pull(Rate))))
    }else if(input$current_use %in% c("Bowling Alley")){
      (datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length) * 
         ((input$lanes_redev) * (trip_rates %>% filter(use == input$current_use)  %>% pull(Rate))))
    }else if(input$current_use %in% c("Movie Theater (traditional)")){
      (datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length) * 
         ((input$screens_redev) * (trip_rates %>% filter(use == input$current_use)  %>% pull(Rate))))
    }else if(input$current_use %in% c("University/College","High School","Middle School/Junior High School","Elementary School","Private School (K-12)")){
      (datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length) * 
         ((input$students_redev) * (trip_rates %>% filter(use == input$current_use)  %>% pull(Rate))) )
    }else if(input$current_use %in% c("Hotel","Motel","Timeshare")){
      ((datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length)) * 
         (input$taus_redev) * (trip_rates %>% filter(use %in% input$current_use) %>% select(Rate) %>% pull()))
    } else if(input$current_use %in% c('Developed Campground/RV Park')){
      (((datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length)) * 
          (input$sites_redev) * (trip_rates %>% filter(use %in% input$current_use) %>% select(Rate) %>% pull())))
    }   else if(input$current_use=="Residential (Affordable)"){
      ((datasetInput() %>% filter(zone_id==map_event()) %>% pull(zone_vmt_per_capita) * 2.3 * input$res_units_redev) * 0.9)
    }else if(input$current_use=="Residential (Market-Rate)"){
      (datasetInput() %>% filter(zone_id==map_event()) %>% pull(zone_vmt_per_capita) * 2.3 * input$res_units_redev)
    } else if (input$current_use=="Unique Project Type"){
      ((datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length)) * 
         (input$input_custom_unit_redev * input$input_custom_rate_redev))
    } 
  }
})
display_vmt <- reactive({
  if(tot_vmt_react() < 0){
    0
  } else{tot_vmt_react()}
})
display_vmt2 <- reactive({
  tot_vmt_react2()
})
display_vmt3 <- reactive({
  tot_vmt_react3()
})
proj_sos_react<-reactive({
  if(input$proj_type != "Mixed-Use"){
 # req(input$map_shape_click)
    if (screened() =="Yes") {
      0
    } else if(input$proj_type %in% c('Auto Parts and Service Center','General retail', 'Furniture Store', 'Pharmacy/Drugstore', 'Supermarket', 'Drive-In Bank', 'High Turnover Sit-Down Restaurant (<1 hr. turnover)', 'Fast Food Restaurant', 'Quality Restaurant (>1 hr. turnover)', 'Drinking Place', 'Building Materials/Lumber', 'Free-Standing Discount Store', 'Automobile Sales', 'Health and Fitness Club',"General Office Building (GFA of more than 5,000 sf)","Medical –Dental Office Building","Light industrial","Warehouse")){
    #round(datasetInput() %>% filter(zone_id==map_event()) %>% pull(threshold_15_below) * 
           # ((input$ksf/1000) * (trip_rates %>% filter(use == input$proj_type)  %>% pull(Rate))),0)
      0
  } else if(input$proj_type %in% c('Recreational Community Center', 'Church', 'Daycare Center', 'Library', 'Hospital')){
    round(datasetInput() %>% filter(zone_id==map_event()) %>% pull(threshold_15_below) * 
     ((input$ksf/1000) * (trip_rates %>% filter(use == input$proj_type)  %>% pull(Rate))),0)
  } else  if(input$proj_type %in% c('Public Park')){
    round(datasetInput() %>% filter(zone_id==map_event()) %>% pull(threshold_15_below) * 
            ((input$acres) * (trip_rates %>% filter(use == input$proj_type)  %>% pull(Rate))),0)
  }else  if(input$proj_type %in% c('Marina')){
  #  round(datasetInput() %>% filter(zone_id==map_event()) %>% pull(threshold_15_below) * 
           # ((input$berths) * (trip_rates %>% filter(use == input$proj_type)  %>% pull(Rate))),0)
    0
  }else if(input$proj_type %in% c('Golf Course')){
  #  round(datasetInput() %>% filter(zone_id==map_event()) %>% pull(threshold_15_below) * 
           # (input$holes * trip_rates %>% filter(use %in% input$proj_type) %>% select(Rate) %>% pull()),0)
    0
  }else if(input$proj_type %in% c("Bowling Alley")){
   # round(datasetInput() %>% filter(zone_id==map_event()) %>% pull(threshold_15_below) * 
           # (input$lanes * trip_rates %>% filter(use %in% input$proj_type) %>% select(Rate) %>% pull()),0)
    0
  }else if(input$proj_type %in% c("Movie Theater (traditional)")){
   # round(datasetInput() %>% filter(zone_id==map_event()) %>% pull(threshold_15_below) * 
           # (input$screens * trip_rates %>% filter(use %in% input$proj_type) %>% select(Rate) %>% pull()),0)
    0
  }else if(input$proj_type %in% c("University/College","High School","Middle School/Junior High School","Elementary School","Private School (K-12)")){
    round(datasetInput() %>% filter(zone_id==map_event()) %>% pull(threshold_15_below) * 
            (input$students * trip_rates %>% filter(use %in% input$proj_type) %>% select(Rate) %>% pull()),0)
  }else if(input$proj_type %in% c("Hotel","Motel","Timeshare")){
    round(datasetInput() %>% filter(zone_id==map_event()) %>% pull(threshold_15_below) * 
            (input$taus * trip_rates %>% filter(use %in% input$proj_type) %>% select(Rate) %>% pull()),0)
  }else if(input$proj_type %in% c('Developed Campground/RV Park')){
    #round(datasetInput() %>% filter(zone_id==map_event()) %>% pull(threshold_15_below) * 
           # (input$sites * trip_rates %>% filter(use %in% input$proj_type) %>% select(Rate) %>% pull()),0)
    0
  }else if(input$proj_type == "Residential (Market-Rate)" | input$proj_type == "Residential (Affordable)"){
  round(datasetInput() %>% filter(zone_id==map_event()) %>% pull(sos) * 2.3 * input$res_units,0)
    }  else if (input$proj_type=="Unique Project Type") {
     # (datasetInput() %>% filter(zone_id==map_event()) %>% pull(threshold_15_below)) * 
       # (input$input_custom_unit * input$input_custom_rate)
      0
    } 
  } else if(input$proj_type == "Mixed-Use"){
    if (screened() =="Yes") {
      0
    } else if(input$land_use1 %in% c('Auto Parts and Service Center','General retail', 'Furniture Store', 'Pharmacy/Drugstore', 'Supermarket', 'Drive-In Bank', 'High Turnover Sit-Down Restaurant (<1 hr. turnover)', 'Fast Food Restaurant', 'Quality Restaurant (>1 hr. turnover)', 'Drinking Place', 'Building Materials/Lumber', 'Free-Standing Discount Store',  'Automobile Sales', 'Health and Fitness Club',"General Office Building (GFA of more than 5,000 sf)","Medical –Dental Office Building","Light industrial","Warehouse")){
      #round(datasetInput() %>% filter(zone_id==map_event()) %>% pull(threshold_15_below) * 
      # ((input$ksf/1000) * (trip_rates %>% filter(use == input$proj_type)  %>% pull(Rate))),0)
      0
    } else if(input$land_use1 %in% c('Recreational Community Center', 'Church', 'Daycare Center', 'Library', 'Hospital')){
      round(datasetInput() %>% filter(zone_id==map_event()) %>% pull(threshold_15_below) * 
              ((input$ksf1/1000) * (trip_rates %>% filter(use == input$land_use1)  %>% pull(Rate))),0)
    } else  if(input$land_use1 %in% c('Public Park')){
      round(datasetInput() %>% filter(zone_id==map_event()) %>% pull(threshold_15_below) * 
              ((input$acres1) * (trip_rates %>% filter(use == input$land_use1)  %>% pull(Rate))),0)
    }else  if(input$land_use1 %in% c('Marina')){
      #  round(datasetInput() %>% filter(zone_id==map_event()) %>% pull(threshold_15_below) * 
      # ((input$berths) * (trip_rates %>% filter(use == input$proj_type)  %>% pull(Rate))),0)
      0
    }else if(input$land_use1 %in% c('Golf Course')){
      #  round(datasetInput() %>% filter(zone_id==map_event()) %>% pull(threshold_15_below) * 
      # (input$holes * trip_rates %>% filter(use %in% input$proj_type) %>% select(Rate) %>% pull()),0)
      0
    }else if(input$land_use1 %in% c("Bowling Alley")){
      # round(datasetInput() %>% filter(zone_id==map_event()) %>% pull(threshold_15_below) * 
      # (input$lanes * trip_rates %>% filter(use %in% input$proj_type) %>% select(Rate) %>% pull()),0)
      0
    }else if(input$land_use1 %in% c("Movie Theater (traditional)")){
      # round(datasetInput() %>% filter(zone_id==map_event()) %>% pull(threshold_15_below) * 
      # (input$screens * trip_rates %>% filter(use %in% input$proj_type) %>% select(Rate) %>% pull()),0)
      0
    }else if(input$land_use1 %in% c("University/College","High School","Middle School/Junior High School","Elementary School","Private School (K-12)")){
      round(datasetInput() %>% filter(zone_id==map_event()) %>% pull(threshold_15_below) * 
              (input$students1 * trip_rates %>% filter(use %in% input$land_use1) %>% select(Rate) %>% pull()),0)
    }else if(input$land_use1 %in% c("Hotel","Motel","Timeshare")){
      round(datasetInput() %>% filter(zone_id==map_event()) %>% pull(threshold_15_below) * 
              (input$taus1 * trip_rates %>% filter(use %in% input$land_use1) %>% select(Rate) %>% pull()),0)
    }else if(input$land_use1 %in% c('Developed Campground/RV Park')){
      #round(datasetInput() %>% filter(zone_id==map_event()) %>% pull(threshold_15_below) * 
      # (input$sites * trip_rates %>% filter(use %in% input$proj_type) %>% select(Rate) %>% pull()),0)
      0
    }else if(input$land_use1 == "Residential (Market-Rate)" | input$land_use1 == "Residential (Affordable)"){
      round(datasetInput() %>% filter(zone_id==map_event()) %>% pull(sos) * 2.3 * input$res_units1,0)
    }  else if (input$land_use1=="Unique Project Type") {
      # (datasetInput() %>% filter(zone_id==map_event()) %>% pull(threshold_15_below)) * 
      # (input$input_custom_unit * input$input_custom_rate)
      0
    } 
}
})
proj_sos_react2<-reactive({
  # req(input$map_shape_click)
  if (screened2() =="Yes") {
    0
  } else if(input$land_use2 %in% c('Auto Parts and Service Center','General retail', 'Furniture Store', 'Pharmacy/Drugstore', 'Supermarket', 'Drive-In Bank', 'High Turnover Sit-Down Restaurant (<1 hr. turnover)', 'Fast Food Restaurant', 'Quality Restaurant (>1 hr. turnover)', 'Drinking Place', 'Building Materials/Lumber', 'Free-Standing Discount Store',  'Light industrial', 'Warehouse', 'Automobile Sales', 'Health and Fitness Club',"General Office Building (GFA of more than 5,000 sf)","Medical –Dental Office Building")){
    #round(datasetInput() %>% filter(zone_id==map_event()) %>% pull(threshold_15_below) * 
    # ((input$ksf/1000) * (trip_rates %>% filter(use == input$proj_type)  %>% pull(Rate))),0)
    0
  } else if(input$land_use2%in% c('Recreational Community Center', 'Church', 'Daycare Center', 'Library', 'Hospital')){
    round(datasetInput() %>% filter(zone_id==map_event()) %>% pull(threshold_15_below) * 
            ((input$ksf2/1000) * (trip_rates %>% filter(use == input$land_use2)  %>% pull(Rate))),0)
  } else  if(input$land_use2 %in% c('Public Park')){
    round(datasetInput() %>% filter(zone_id==map_event()) %>% pull(threshold_15_below) * 
            ((input$acres2) * (trip_rates %>% filter(use == input$land_use2)  %>% pull(Rate))),0)
  }else  if(input$land_use2 %in% c('Marina')){
    #  round(datasetInput() %>% filter(zone_id==map_event()) %>% pull(threshold_15_below) * 
    # ((input$berths) * (trip_rates %>% filter(use == input$proj_type)  %>% pull(Rate))),0)
    0
  }else if(input$land_use2 %in% c('Golf Course')){
    #  round(datasetInput() %>% filter(zone_id==map_event()) %>% pull(threshold_15_below) * 
    # (input$holes * trip_rates %>% filter(use %in% input$proj_type) %>% select(Rate) %>% pull()),0)
    0
  }else if(input$land_use2 %in% c("Bowling Alley")){
    # round(datasetInput() %>% filter(zone_id==map_event()) %>% pull(threshold_15_below) * 
    # (input$lanes * trip_rates %>% filter(use %in% input$proj_type) %>% select(Rate) %>% pull()),0)
    0
  }else if(input$land_use2 %in% c("Movie Theater (traditional)")){
    # round(datasetInput() %>% filter(zone_id==map_event()) %>% pull(threshold_15_below) * 
    # (input$screens * trip_rates %>% filter(use %in% input$proj_type) %>% select(Rate) %>% pull()),0)
    0
  }else if(input$land_use2%in% c("University/College","High School","Middle School/Junior High School","Elementary School","Private School (K-12)")){
    round(datasetInput() %>% filter(zone_id==map_event()) %>% pull(threshold_15_below) * 
            (input$students2 * trip_rates %>% filter(use %in% input$land_use2) %>% select(Rate) %>% pull()),0)
  }else if(input$land_use2 %in% c("Hotel","Motel","Timeshare")){
    round(datasetInput() %>% filter(zone_id==map_event()) %>% pull(threshold_15_below) * 
            (input$taus2 * trip_rates %>% filter(use %in% input$land_use2) %>% select(Rate) %>% pull()),0)
  }else if(input$land_use2 %in% c('Developed Campground/RV Park')){
    #round(datasetInput() %>% filter(zone_id==map_event()) %>% pull(threshold_15_below) * 
    # (input$sites * trip_rates %>% filter(use %in% input$proj_type) %>% select(Rate) %>% pull()),0)
    0
  }else if(input$land_use2 == "Residential (Market-Rate)" | input$land_use2 == "Residential (Affordable)"){
    #round(
      datasetInput() %>% filter(zone_id==map_event()) %>% pull(sos) * 2.3 * input$res_units2
      #,0)
  }  else if (input$land_use2=="Unique Project Type") {
    # (datasetInput() %>% filter(zone_id==map_event()) %>% pull(threshold_15_below)) * 
    # (input$input_custom_unit * input$input_custom_rate)
    0
  } 
})
proj_sos_react3<-reactive({
  # req(input$map_shape_click)
  if (screened3() =="Yes") {
    0
  } else if(input$land_use2 %in% c('Auto Parts and Service Center','General retail', 'Furniture Store', 'Pharmacy/Drugstore', 'Supermarket', 'Drive-In Bank', 'High Turnover Sit-Down Restaurant (<1 hr. turnover)', 'Fast Food Restaurant', 'Quality Restaurant (>1 hr. turnover)', 'Drinking Place', 'Building Materials/Lumber', 'Free-Standing Discount Store', 'Automobile Sales', 'Health and Fitness Club',"General Office Building (GFA of more than 5,000 sf)","Medical –Dental Office Building","Light industrial","Warehouse")){
    #round(datasetInput() %>% filter(zone_id==map_event()) %>% pull(threshold_15_below) * 
    # ((input$ksf/1000) * (trip_rates %>% filter(use == input$proj_type)  %>% pull(Rate))),0)
    0
  } else if(input$land_use3%in% c('Recreational Community Center', 'Church', 'Daycare Center', 'Library', 'Hospital')){
    round(datasetInput() %>% filter(zone_id==map_event()) %>% pull(threshold_15_below) * 
            ((input$ksf3/1000) * (trip_rates %>% filter(use == input$land_use3)  %>% pull(Rate))),0)
  } else  if(input$land_use3 %in% c('Public Park')){
    round(datasetInput() %>% filter(zone_id==map_event()) %>% pull(threshold_15_below) * 
            ((input$acres3) * (trip_rates %>% filter(use == input$land_use3)  %>% pull(Rate))),0)
  }else  if(input$land_use3 %in% c('Marina')){
    #  round(datasetInput() %>% filter(zone_id==map_event()) %>% pull(threshold_15_below) * 
    # ((input$berths) * (trip_rates %>% filter(use == input$proj_type)  %>% pull(Rate))),0)
    0
  }else if(input$land_use3 %in% c('Golf Course')){
    #  round(datasetInput() %>% filter(zone_id==map_event()) %>% pull(threshold_15_below) * 
    # (input$holes * trip_rates %>% filter(use %in% input$proj_type) %>% select(Rate) %>% pull()),0)
    0
  }else if(input$land_use3 %in% c("Bowling Alley")){
    # round(datasetInput() %>% filter(zone_id==map_event()) %>% pull(threshold_15_below) * 
    # (input$lanes * trip_rates %>% filter(use %in% input$proj_type) %>% select(Rate) %>% pull()),0)
    0
  }else if(input$land_use3 %in% c("Movie Theater (traditional)")){
    # round(datasetInput() %>% filter(zone_id==map_event()) %>% pull(threshold_15_below) * 
    # (input$screens * trip_rates %>% filter(use %in% input$proj_type) %>% select(Rate) %>% pull()),0)
    0
  }else if(input$land_use3%in% c("University/College","High School","Middle School/Junior High School","Elementary School","Private School (K-12)")){
    round(datasetInput() %>% filter(zone_id==map_event()) %>% pull(threshold_15_below) * 
            (input$students3 * trip_rates %>% filter(use %in% input$land_use3) %>% select(Rate) %>% pull()),0)
  }else if(input$land_use3 %in% c("Hotel","Motel","Timeshare")){
    round(datasetInput() %>% filter(zone_id==map_event()) %>% pull(threshold_15_below) * 
            (input$taus3 * trip_rates %>% filter(use %in% input$land_use3) %>% select(Rate) %>% pull()),0)
  }else if(input$land_use3 %in% c('Developed Campground/RV Park')){
    #round(datasetInput() %>% filter(zone_id==map_event()) %>% pull(threshold_15_below) * 
    # (input$sites * trip_rates %>% filter(use %in% input$proj_type) %>% select(Rate) %>% pull()),0)
    0
  }else if(input$land_use3 == "Residential (Market-Rate)" | input$land_use3 == "Residential (Affordable)"){
    round(datasetInput() %>% filter(zone_id==map_event()) %>% pull(sos) * 2.3 * input$res_units3,0)
  }  else if (input$land_use3=="Unique Project Type") {
    # (datasetInput() %>% filter(zone_id==map_event()) %>% pull(threshold_15_below)) * 
    # (input$input_custom_unit * input$input_custom_rate)
    0
  } 
})
output$tot_vmt <-renderValueBox({
 # req(input$map_shape_click)
  valueBox(subtitle="Total VMT", width=6, color="navy",
    value= tags$p(comma(display_vmt(), 0),
                  style = "font-size: 75%;"
    )
  )
})
output$tot_vmt2 <-renderValueBox({
  # req(input$map_shape_click)
  valueBox(subtitle="Total VMT", width=6, color="navy",
           value= tags$p(comma(display_vmt2(), 0),
                         style = "font-size: 75%;"
           )
  )
})
output$tot_vmt3 <-renderValueBox({
  # req(input$map_shape_click)
  valueBox(subtitle="Total VMT", width=6, color="navy",
           value= tags$p(comma(display_vmt3(), 0),
                         style = "font-size: 75%;"
           )
  )
})
output$tot_vmt_redev <-renderValueBox({
  # req(input$map_shape_click)
  valueBox(subtitle="Total VMT", width=6, color="navy",
           value= tags$p(comma(tot_vmt_react_redev(), 0),
                         style = "font-size: 75%;"
           )
  )
})
output$proj_sos <-renderValueBox({
#  req(input$map_shape_click)
  valueBox(subtitle="Standard of Significance VMT", width=6, color="navy",
           value=tags$p(comma(proj_sos_react(), 0),
                        style = "font-size: 75%;"
           )
  )
})
output$proj_sos2 <-renderValueBox({
  #  req(input$map_shape_click)
  valueBox(subtitle="Standard of Significance VMT", width=6, color="navy",
           value=tags$p(comma(proj_sos_react2(), 0),
                        style = "font-size: 75%;"
           )
  )
})
output$proj_sos3 <-renderValueBox({
  #  req(input$map_shape_click)
  valueBox(subtitle="Standard of Significance VMT", width=6, color="navy",
           value=tags$p(comma(proj_sos_react3(), 0),
                        style = "font-size: 75%;"
           )
  )
})
output$screened <-renderValueBox({
  valueBox(subtitle="Screened", width=6, color="black",
           value=tags$p(screened(),
           style = "font-size: 75%;")
  )
})
output$screened2 <-renderValueBox({
  valueBox(subtitle="Screened", width=6, color="black",
           value=tags$p(screened2(),
                        style = "font-size: 75%;")
  )
})
output$screened3 <-renderValueBox({
  valueBox(subtitle="Screened", width=6, color="black",
           value=tags$p(screened3(),
                        style = "font-size: 75%;")
  )
})
mit_needed <- reactive({
  if(screened() == "Yes" | tot_vmt_react() - proj_sos_react() <1){
    0
  }else{
tot_vmt_react() - proj_sos_react()
  }
})
mit_needed2 <- reactive({
  if(screened2() == "Yes" | tot_vmt_react2() - proj_sos_react2() <1){
    0
  }else{
    tot_vmt_react2() - proj_sos_react2()
  }
})
mit_needed3 <- reactive({
  if(screened3() == "Yes" | tot_vmt_react3() - proj_sos_react3() <1){
    0
  }else{
    tot_vmt_react3() - proj_sos_react3()
  }
})
output$mitigate <-renderValueBox({
  valueBox(subtitle="VMT Mitigation Needed", width=6, color="navy",
           value= tags$p(
             comma(mit_needed(), 0),
           style = "font-size: 75%;")
  )
})
output$mitigate2 <-renderValueBox({
  valueBox(subtitle="VMT Mitigation Needed", width=6, color="navy",
           value= tags$p(
             comma(mit_needed2(), 0),
             style = "font-size: 75%;")
  )
})
output$mitigate3 <-renderValueBox({
  valueBox(subtitle="VMT Mitigation Needed", width=6, color="navy",
           value= tags$p(
             comma(mit_needed3(), 0),
             style = "font-size: 75%;")
  )
})
mob_fee_calc <- reactive({
  if(input$proj_type %in% c("Residential (Market-Rate)","Residential (Affordable)","Hotel","Motel","Timeshare","Developed Campground/RV Park" ))
  {(round(display_vmt(),0) * .9) * 218
    }else if(!input$proj_type %in% c("Residential (Market-Rate)","Residential (Affordable)","Hotel","Motel","Timeshare","Developed Campground/RV Park" ))
    {(round(display_vmt(),0)*.1) * 218
    }else if(land_use1 %in% c("Residential (Market-Rate)","Residential (Affordable)","Hotel","Motel","Timeshare","Developed Campground/RV Park" ))
    {(round(display_vmt(),0) * .9) * 218
    } else if(!input$land_use1 %in% c("Residential (Market-Rate)","Residential (Affordable)","Hotel","Motel","Timeshare","Developed Campground/RV Park" ))
    {(round(display_vmt(),0)*.1) * 218
    }
})
mob_fee_calc2 <- reactive({
  if(input$proj_type !='Mixed-Use'){
    return()
  }
  else if(input$land_use2 %in% c("Residential (Market-Rate)","Residential (Affordable)","Hotel","Motel","Timeshare","Developed Campground/RV Park" ))
  {(round(display_vmt2(),0) *.9) * 218
  }else{
    (round(display_vmt2(),0)*.1) * 218
  }
})
mob_fee_calc3 <- reactive({
  if(input$proj_type !='Mixed-Use'){
    return()
  }
  else if(input$land_use3 %in% c("Residential (Market-Rate)","Residential (Affordable)","Hotel","Motel","Timeshare","Developed Campground/RV Park" ))
  {(round(display_vmt3(),0)*.9) * 218
  }else{
    (round(display_vmt3(),0)*.1) * 218
  }
})
output$mobility_fee <-renderValueBox({
  #req(input$map_shape_click)
  valueBox(subtitle="Mobility Fee", width=6, color="red",
           value= tags$p(
             dollar(mob_fee_calc()),
             style = "font-size: 70%;"
           )
  )
})
  output$mobility_fee2 <-renderValueBox({
    #req(input$map_shape_click)
    valueBox(subtitle="Mobility Fee", width=6, color="red",
             value= tags$p(
               dollar(mob_fee_calc2()),
               style = "font-size: 70%;"
             )
    )
})
  output$mobility_fee3 <-renderValueBox({
    #req(input$map_shape_click)
    valueBox(subtitle="Mobility Fee", width=6, color="red",
             value= tags$p(
               dollar(mob_fee_calc3()),
               style = "font-size: 70%;"
             )
    )
  })
  output$ui_tot_mixed_use <- renderUI({
    if(input$proj_type =='Mixed-Use'){
      fluidRow(  
        box(width=12,title="Mixed-Use Project Totals" ,
            valueBox(subtitle="Total Mixed-Use VMT", width=2, color="red",
               value= tags$p(
                 comma(mu_screen_vmt(),0),
                 style = "font-size: 70%;"
               )
      ), valueBox(subtitle="Mixed-Use Screened", width=2, color="red",
                  value= tags$p(
                    screened_mu(),
                    style = "font-size: 70%;"
                  )
      )
      # , valueBox(subtitle="Mixed-Use Screen VMT", width=2, color="red",
      #             value= tags$p(
      #              comma(mu_screen_vmt(),0),
      #               style = "font-size: 70%;"
      #             )
      # )
      ))
    } else{return(NULL)}
  })
  tot_mixed_use_vmt<-reactive({
    if (
      (input$land_use1 %in% c("Residential (Market-Rate)","Residential (Affordable)","Timeshare","Hotel","Motel") & input$land_use2 %in% c("General retail")) |
      (input$land_use1 %in% c("Residential (Market-Rate)","Residential (Affordable)","Timeshare","Hotel","Motel") & input$land_use3 %in% c("General retail")) |
      (input$land_use2 %in% c("Residential (Market-Rate)","Residential (Affordable)","Timeshare","Hotel","Motel") & input$land_use1 %in% c("General retail")) |
      (input$land_use2 %in% c("Residential (Market-Rate)","Residential (Affordable)","Timeshare","Hotel","Motel") & input$land_use3 %in% c("General retail")) |
      (input$land_use3 %in% c("Residential (Market-Rate)","Residential (Affordable)","Timeshare","Hotel","Motel") & input$land_use1 %in% c("General retail")) |
      (input$land_use2 %in% c("Residential (Market-Rate)","Residential (Affordable)","Timeshare","Hotel","Motel") & input$land_use2 %in% c("General retail"))
    ){ (tot_vmt_react3() + tot_vmt_react2() + tot_vmt_react()) * .9
    } else{tot_vmt_react3() + tot_vmt_react2() + tot_vmt_react()}
  })
  tot_mixed_use_fee<-reactive({
    if (
      (input$land_use1 %in% c("Residential (Market-Rate)","Residential (Affordable)","Timeshare","Hotel","Motel") & input$land_use2 %in% c("General retail")) |
      (input$land_use1 %in% c("Residential (Market-Rate)","Residential (Affordable)","Timeshare","Hotel","Motel") & input$land_use3 %in% c("General retail")) |
      (input$land_use2 %in% c("Residential (Market-Rate)","Residential (Affordable)","Timeshare","Hotel","Motel") & input$land_use1 %in% c("General retail")) |
      (input$land_use2 %in% c("Residential (Market-Rate)","Residential (Affordable)","Timeshare","Hotel","Motel") & input$land_use3 %in% c("General retail")) |
      (input$land_use3 %in% c("Residential (Market-Rate)","Residential (Affordable)","Timeshare","Hotel","Motel") & input$land_use1 %in% c("General retail")) |
      (input$land_use2 %in% c("Residential (Market-Rate)","Residential (Affordable)","Timeshare","Hotel","Motel") & input$land_use2 %in% c("General retail"))
    ){ ((round(tot_vmt_react3() + tot_vmt_react2() + tot_vmt_react(),0)) * .9) * 55.44
    } else{(round(tot_vmt_react3() + tot_vmt_react2() + tot_vmt_react(),0)) * 55.44}
  })
  output$ui_land_use_1  <-renderUI({
   # req(input$map_shape_click, input$proj_type)
    if(nrow(in_out_region()) == 0) {
    return()
  } else{
  fluidRow(  
    box(width=12,title="Land Use Type #1" ,
         column(width=2,valueBoxOutput("screened", width=12)),
        column(width=2,valueBoxOutput("tot_vmt", width=12)),
        column(width=2,valueBoxOutput("proj_sos", width=12)),
        column(width=2,valueBoxOutput("mitigate", width=12)),
        column(width=3,valueBoxOutput("mobility_fee", width=12))
        ),
   # column(width=2,uiOutput("export_button", width=12))
    )
  }
   # }
  })
  output$ui_land_use_2  <- renderUI({
    if(display_vmt2() ==0 | nrow(in_out_region()) == 0) {
      return()
    } else{
    fluidRow(  
      box(width=12,title="Land Use Type #2" ,
          column(width=2,valueBoxOutput("screened2", width=12)),
          column(width=2,valueBoxOutput("tot_vmt2", width=12)),
          column(width=2,valueBoxOutput("proj_sos2", width=12)),
          column(width=2,valueBoxOutput("mitigate2", width=12)),
          column(width=3,valueBoxOutput("mobility_fee2", width=12))
          ))
    }
  })
output$ui_land_use_3  <- renderUI({
  if(display_vmt3() ==0 | nrow(in_out_region()) == 0) {
    return()
  } else{
  fluidRow(  
  box(width=12,title="Land Use Type #3" ,
      column(width=2,valueBoxOutput("screened3", width=12)),
    column(width=2,valueBoxOutput("tot_vmt3", width=12)),
    column(width=2,valueBoxOutput("proj_sos3", width=12)),
    column(width=2,valueBoxOutput("mitigate3", width=12)),
    column(width=3,valueBoxOutput("mobility_fee3", width=12))
    ))
  }
  })
# observe({
#   input$proj_type
#   updateCheckboxInput(session, inputId="low_income_screen", value= FALSE)
# })
# observe({
#   input$map_click
#   updateCheckboxInput(session, inputId="low_income_screen", value= FALSE)
# })
screened <- reactive({
  if(input$proj_type == "Mixed-Use"){
    if (tot_vmt_react() <= 715)
    {print("Yes")
    }else if (between(tot_vmt_react(),715,1300) & nrow(town_reg_buffer()) > 0)
    {print("Yes")
    }else if (nrow(transit()) > 0 & input$land_use1 == "Residential (Affordable)")
    {print("Yes")
    } else{"No"
      }} else if (input$proj_type != "Mixed-Use"){
    if (tot_vmt_react() <= 715)
    {print("Yes")
    }else if (between(tot_vmt_react(),715,1300) & nrow(town_reg_buffer()) > 0)
    {print("Yes")
    }else if (nrow(transit()) > 0 & input$proj_type == "Residential (Affordable)")
    {print("Yes")
    } else{"No"
    }
      }
})
screened2 <- reactive({
  if (tot_vmt_react2() <= 715)
  {print("Yes")
  }else if (between(tot_vmt_react2(),715,1300) & nrow(town_reg_buffer()) > 0)
  {print("Yes")
  }else if (nrow(transit()) > 0 & input$land_use2 == "Residential (Affordable)")
  {print("Yes")
  } else{"No"
  }
})
screened3 <- reactive({
  if (tot_vmt_react3() <= 715)
  {print("Yes")
  }else if (between(tot_vmt_react3(),715,1300) & nrow(town_reg_buffer()) > 0)
  {print("Yes")
  }else if (nrow(transit()) > 0 & input$land_use3 == "Residential (Affordable)")
  {print("Yes")
  } else{"No"
  }
})
screened_mu <- reactive({
  if (mu_screen_vmt() <= 715)
  {print("Yes")
  }else if (between(mu_screen_vmt(),715,1300) & nrow(town_reg_buffer()) > 0)
  {print("Yes")
  }else if (between(mu_screen_vmt(),715,1300) & nrow(town_reg_buffer()) == 0)
  {print("No")
  } else if (mu_screen_vmt() > 1300)
  {print("No")
  }
})
mu_screen_vmt <- reactive({
  if((input$land_use1 == "Residential (Affordable)" |  input$land_use2 == "Residential (Affordable)" | input$land_use3 == "Residential (Affordable)") &  nrow(transit()) > 0 ){
  if(input$land_use1 == "Residential (Affordable)" & input$land_use2 != "Residential (Affordable)" & input$land_use3 != "Residential (Affordable)"){
    tot_vmt_react3() + tot_vmt_react2()
  } else if(input$land_use2 == "Residential (Affordable)" & input$land_use1 != "Residential (Affordable)" & input$land_use3 != "Residential (Affordable)"){
    tot_vmt_react3() + tot_vmt_react()
  }else if(input$land_use3 == "Residential (Affordable)" & input$land_use1 != "Residential (Affordable)" & input$land_use2 != "Residential (Affordable)"){
    tot_vmt_react() + tot_vmt_react2()
  }
  } else if((input$land_use1 == "Residential (Affordable)" |  input$land_use2 == "Residential (Affordable)" | input$land_use3 == "Residential (Affordable)") &  nrow(transit()) == 0 ) {
    tot_mixed_use_vmt()
  }
  else if(input$land_use1 != "Residential (Affordable)" &  input$land_use2 != "Residential (Affordable)" & input$land_use3 != "Residential (Affordable)") {tot_mixed_use_vmt()}
})
name_of_mit <- reactive({paste(names(mit_choice()[mit_choice()==input$mitigations]), collapse=", ")
  })
apn <- reactive({
  #withProgress(message = 'Download in Progress',
              # detail = 'This may take a minute...', value = 0, {
              #   for (i in 1:200) {
                #   incProgress(1/200)
                  # Sys.sleep(0.01)
                 #}
              # })
  st_join(proj_loc(), st_buffer(pb_sf %>% dplyr::select("APN"),0), join= st_within, prepared=F) %>% pull(APN)
})
screen_type1 <- reactive({
  if (tot_vmt_react() <= 715)
  {print("Low VMT Screen")
  }else if (between(tot_vmt_react(),715,1300) & nrow(town_reg_buffer()) > 0)
  {print("Low VMT Screen")
  }else if (between(tot_vmt_react(),715,1300) & nrow(town_reg_buffer()) == 0)
  {print("Not Applicable")
  } else if(input$proj_type %in% c("Residential (Market-Rate)","Residential (Affordable)") & nrow(transit()) > 0 ){
    print("Low Income Screen")
    } else { print("Not Applicable") }
})
units1 <- reactive({
  if(input$proj_type != "Mixed-Use"){
    return()} else if(input$proj_type=="Mixed-Use"){
  if (input$land_use1 %in% c("Residential (Market-Rate)","Residential (Affordable)")){
    "Units"} 
  else if (input$land_use1 %in% c("Hotel","Motel","Timeshare")){
    "TAUS"
  } else if (input$land_use1 =="Unique Project Type"){
    "Units"
  }else if (input$land_use1 =="Marina"){
    "Berths"
  }else if (input$land_use1 %in% c('Developed Campground/RV Park')){
    "Sites"
  }else if (input$land_use1 %in% c('Public Park')){
    "Acres"
  }else if (input$land_use1 %in% c("Golf Course")){
    "Holes"
  }else if (input$land_use1 %in% c("Bowling Alley")){
    "Lanes"
  }else if (input$land_use1 %in% c("Movie Theater (traditional)")){
    "Screens"
  }else if (input$land_use1 %in% c("University/College","High School","Middle School/Junior High School","Elementary School","Private School (K-11)")){
    "Students"
  }else {
    "Square Feet"
  }
    }
})
units2 <- reactive({
  if(input$proj_type != "Mixed-Use"){
    return()} else if(input$proj_type=="Mixed-Use"){
  if (input$land_use2 %in% c("Residential (Market-Rate)","Residential (Affordable)")){
    "Units"} 
  else if (input$land_use2 %in% c("Hotel","Motel","Timeshare")){
    "TAUS"
  } else if (input$land_use2 =="Unique Project Type"){
    "Units"
  }else if (input$land_use2 =="Marina"){
    "Berths"
  }else if (input$land_use2 %in% c('Developed Campground/RV Park')){
    "Sites"
  }else if (input$land_use2 %in% c('Public Park')){
    "Acres"
  }else if (input$land_use2 %in% c("Golf Course")){
    "Holes"
  }else if (input$land_use2 %in% c("Bowling Alley")){
    "Lanes"
  }else if (input$land_use2 %in% c("Movie Theater (traditional)")){
    "Screens"
  }else if (input$land_use2 %in% c("University/College","High School","Middle School/Junior High School","Elementary School","Private School (K-12)")){
    "Students"
  }else {
    "Square Feet"
  }
    }
})
units3 <- reactive({
  if(input$proj_type != "Mixed-Use"){
    return()} else if(input$proj_type=="Mixed-Use"){
  if (input$land_use3 %in% c("Residential (Market-Rate)","Residential (Affordable)")){
    "Units"} 
  else if (input$land_use3 %in% c("Hotel","Motel","Timeshare")){
    "TAUS"
  } else if (input$land_use3 =="Unique Project Type"){
    "Units"
  }else if (input$land_use3 =="Marina"){
    "Berths"
  }else if (input$land_use3 %in% c('Developed Campground/RV Park')){
    "Sites"
  }else if (input$land_use3 %in% c('Public Park')){
    "Acres"
  }else if (input$land_use3 %in% c("Golf Course")){
    "Holes"
  }else if (input$land_use3 %in% c("Bowling Alley")){
    "Lanes"
  }else if (input$land_use3 %in% c("Movie Theater (traditional)")){
    "Screens"
  }else if (input$land_use3 %in% c("University/College","High School","Middle School/Junior High School","Elementary School","Private School (K-12)")){
    "Students"
  }else {
    "Square Feet"
  }
    }
})
units <- reactive({
  if (input$proj_type %in% c("Residential (Market-Rate)","Residential (Affordable)")){
    "Units"} 
  else if (input$proj_type %in% c("Hotel","Motel","Timeshare")){
    "TAUS"
  } else if (input$proj_type =="Unique Project Type"){
    "Units"
  }else if (input$proj_type =="Marina"){
    "Berths"
  }else if (input$proj_type %in% c('Developed Campground/RV Park')){
    "Sites"
  }else if (input$proj_type %in% c('Public Park')){
    "Acres"
  }else if (input$proj_type %in% c("Golf Course")){
    "Holes"
  }else if (input$proj_type %in% c("Bowling Alley")){
    "Lanes"
  }else if (input$proj_type %in% c("Movie Theater (traditional)")){
    "Screens"
  }else if (input$proj_type %in% c("University/College","High School","Middle School/Junior High School","Elementary School","Private School (K-12)")){
    "Students"
  }else if (input$proj_type %in% c("Mixed-Use")){
    " "
  }else {
    "Square Feet"
  }
})
output$report_button <- output$report_button_743 <- downloadHandler(
  filename = function() {
    paste('my-report', sep = '.', switch(
      input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
    ))
  },
  content = function(file) {
    src <- normalizePath('report.Rmd')
    # temporarily switch to the temp dir, in case you do not have write
    # permission to the current working directory
    owd <- setwd(tempdir())
    on.exit(setwd(owd))
    file.copy(src, 'report.Rmd', overwrite = TRUE)
   # )
    out <- render('report.Rmd', switch(
      input$format,
      PDF = pdf_document(), HTML = html_document(), Word = word_document()),
      params=list(output1= if(input$proj_type=="Mixed-Use"){screened_mu()} else if(input$proj_type!="Mixed-Use") {screened()},
                  # output_screen_type=
                  #   if(input$proj_type=="Mixed-Use" & screened_mu() =="Yes" ){"Low VMT Screen"
                  #   } else if(input$proj_type =="Mixed-Use" & screened_mu() =="No" ){"Not Applicable"
                  #   } else if(input$proj_type !="Mixed-Use" & screened() =="Yes" ) {screen_type1()
                  #   } else if(input$proj_type !="Mixed-Use" & screened() =="No" ) {
                  #     "Not Applicable"
                  #     },
                  # output_screen_message= if(screened()=="No" | screened_mu()=="No"){
                  #   "This project did not screen and/or mitigate below the standard of significance and requires consulation with TRPA staff. Please contact TRPA at info@trpa.gov"
                  # } else if(screened()=="Yes" | screened_mu()=="Yes"){"This project screened and/or mitigated below the standard of significance and does not require any additional VMT analysis."},
                  output2= if(input$proj_type=="Mixed-Use"){tot_mixed_use_vmt()} else if(input$proj_type!="Mixed-Use") {display_vmt()},
                  output_mu_lu_1= input$land_use1,
                  output_mu_lu_2= input$land_use2,
                  output_mu_lu_3= input$land_use3,
                  output_mu_size1=paste0(input$ksf1,input$res_units1,input$taus1,input$berths1,input$sites1,input$acres1,input$holes1,input$lanes1,input$screens1,input$students1,input$input_custom_unit1),
                  output_mu_size2=paste0(input$ksf2,input$res_units2,input$taus2,input$berths2,input$sites2,input$acres2,input$holes2,input$lanes2,input$screens2,input$students2,input$input_custom_unit2),
                  output_mu_size3=paste0(input$ksf3,input$res_units3,input$taus3,input$berths3,input$sites3,input$acres3,input$holes3,input$lanes3,input$screens3,input$students3,input$input_custom_unit3),
                  output_proposed_vmt = if(input$proj_type=="Mixed-Use"){(tot_vmt_react_proposed() + tot_vmt_react2_proposed() + tot_vmt_react3_proposed())
                    }else if(input$proj_type!="Mixed-Use"){
                      tot_vmt_react_proposed()},
                  output_mitigated_vmt=if(input$proj_type=="Mixed-Use"){(tot_vmt_react_proposed() + tot_vmt_react2_proposed() + tot_vmt_react3_proposed())* ((1- mit_percent_final1()) + (1- mit_percent_final2()) + (1- mit_percent_final3()))
                  }else if(input$proj_type!="Mixed-Use"){
                    tot_vmt_react_proposed() * (1- mit_percent_final())},
                  output_redev_vmt=display_vmt_redev(),
                  output3= proj_sos_react(),
                  output4= if(input$proj_type=="Mixed-Use"){" "
                  }else if(input$proj_type!="Mixed-Use"){
                    mit_needed()},
                  output_mob_fee1= dollar(mob_fee_calc()),
                  output_mob_fee2= dollar(mob_fee_calc2()),
                  output_mob_fee3= dollar(mob_fee_calc3()),
                  
                  output6= input$proj_name,
                  output_analysis_type= input$analysis_type,
                  output7= input$proj_type,
                  output_redev_type= if(input$redevelopment=="Yes"){input$current_use} else if(input$redevelopment=="No") {"Not Applicable"},
                 output_units=units(),
                  output_units1=units1(),
                  output_units2=units2(),
                 output_units3=units3(),
                 output_note=if(input$proj_type=="Mixed-Use") {"For mixed-use projects, the tool does not currently incorporate reductions for internal trip capture. This may be incorporated in future version of the tool."} else if(input$proj_type !="Mixed-Use") {"None"},
                 output8= if(input$proj_type=="Mixed-Use") {" "} else if(input$proj_type !="Mixed-Use") {paste0(input$ksf,input$res_units,input$taus,input$berths,input$sites,input$acres,input$holes,input$lanes,input$screens,input$students,input$input_custom_unit)},
      output_length = datasetInput() %>% filter(zone_id==map_event()) %>% pull(avg_zone_trip_length),
      output_zone_id = datasetInput() %>% filter(zone_id==map_event()) %>% pull(zone_id),
      output_zone_sos = datasetInput() %>% filter(zone_id==map_event()) %>% pull(sos),
      output_ite = trip_rates %>% filter(use %in% input$proj_type) %>% select(Rate) %>% pull(),
       output9= if(input$proj_type=="Mixed-Use"){
         percent((1- mit_percent_final1()) + (1- mit_percent_final2()) + (1- mit_percent_final3()),2)
         } else if(input$proj_type!="Mixed-Use") {
           percent((1- mit_percent_final()),2)},
       output_mit_names= if(input$proj_type=="Mixed-Use"){paste(input$mitigations1,input$mitigations2,input$mitigations3, collapse=", ")} else if(input$proj_type!="Mixed-Use") {paste(input$mitigations, collapse=", ")},
       output10= if(nrow(town_reg_buffer()) > 0){
        "Yes"
      } else {
        "No"
      },
      output11= if(nrow(transit()) > 0){
        "Yes"
      } else {"No"},
      output_jur= st_join(proj_loc(), st_buffer(jur %>% dplyr::select("name"),0), join=st_within, prepared=F) %>% pull(name),
      output_apn= apn(),
      output_mob_fee_tot = dollar(sum(c(mob_fee_calc(),mob_fee_calc2(),mob_fee_calc3()), na.rm=T))
     # output12= if(input$low_income_screen == TRUE){
       # "Yes"
     # }else if(input$low_income_screen == FALSE ){
      #  "No"
    #} else if(is.null(input$low_income_screen)) {
      #  "Not Applicable"
     # }
  )
    )
    file.rename(out, file)
  }
)
}
shinyApp(ui, server)



