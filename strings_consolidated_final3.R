
##### BEFORE USING THIS CODE, PLEASE READ THE LAST SECTIONS OF THIS ( ) BLOG POST, WHICH EXPLAIN HOW TO USE IT #####

library(purrr)
library(data.table)
library(dplyr)
library(ggplot2)
library(lubridate)
library(stringi)
library(tidytransit)
library(tidyr)
library(ggmap)
library(sf)
library(rgeos)
library(scales)
library(rlist)
library(maptools)

setwd() #Set a path to your R working directory here. eg. "C:/Users/yourname/Documents/RProjects/Stringlines" This is an optional step.
#If you do not set a working directory, scroll to the end of the code and set a path for the plot save function, lest it clutter your 
#Documents folder. 

#For more on working directories, see: 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
############### parameters here ############### 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

dat_master <- read_gtfs("http://web.mta.info/developers/data/nyct/subway/google_transit.zip") #this should be the path to your GTFS file.
#You may also insert the download link to an agency GTFS feed here. 

#NOTE: for all of these fields, when leaving elements blank, do not use "". The code's function depends on it.
#Eg. leaving routes_secondary blank should look like routes_secondary <- c() not routes_secondary <- c(")

template_choice <- "Most" #Most (for most common) or Longest (for longest pattern). 

name_elim <- FALSE #Whether you want the code to reduce the number of stops shown on the Y axis. TRUE or FALSE. 
use_rt_sht <- FALSE #Should the code replace route_ids with route_short_names

route_tar <- c("2", "3") #Use route_ids , not the full route names. Run 1-22 and see dat$routes for more information. 
#You can use up to 2 routes in the route_tar argument, BUT the 2 routes must a) share at least 2 stops, and b) have the same direction_id-direction correspondances.
#For example, the J and M trains in New York run together, but a Metropolitan Ave-bound M and a Jamaica-bound J have direction_ids of 1 and 0, respectively, so you
#cannot plot both at the same time. You can, however, put them in the routes_secondary argument, provided you plan to plot directions 0 and 1.

stop_mand <- c() #If you want to ensure that the string plot includes a certain (branch of a route serving a) stop, add its stop ID here. 
#Remember to include that stop's ID in both directions if plotting a bidirectional plot! 

routes_secondary <- c() #Add any other routes you'd like to see shown on your string plot. Not many rules for what can/can't go here!

dir_tar <- c(0) #Select one or both directions to view. Possible directions are 0 and 1. What they correspond to varies by agency.

date_tar <- as.Date("2022-12-07") #Choose your sample date. This MUST lie within the start/end dates of your gtfs. Run lines 1-63 and 
#paste View(dat$.$dates_services) in the command line to see available dates, or see the dates listed on the GTFS download site. 

time_start <- period_to_seconds(hms("11:00:00")) #Start time for the plot
time_end <- period_to_seconds(hms("13:00:00")) #End time for the plot 

#Along with all these parameters, you may also wish to change the dimensions of the output plot. Use the last line of the code to do that.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
############### code begins here ############### 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

`%notin%` <- Negate(`%in%`)

gc()

dat <- dat_master
dat <- set_servicepattern(dat)

svc <- dat$.$dates_services

svc <- svc%>%
dplyr::filter(date == date_tar)

if(use_rt_sht == TRUE){
  dat$trips <- dat$trips %>%
    inner_join(.,dat$routes%>%select(route_id,route_short_name))%>%
    mutate(route_id=route_short_name)%>%
    select(-route_short_name)
}

dat$routes <- dat$routes %>%
  mutate(route_id= case_when(use_rt_sht == TRUE~route_short_name,
                             TRUE~route_id))%>%
  dplyr::filter(route_id %in% c(route_tar, routes_secondary))

dat$trips <- dat$trips %>%
dplyr::filter(route_id %in% dat$routes$route_id) %>%
dplyr::filter(service_id %in% svc$service_id)%>%
dplyr::filter(direction_id %in% dir_tar)%>%
mutate(shape_id = ifelse(shape_id == "", NA, shape_id))

lines_exc <- dat$shapes%>%
filter(shape_id%in%dat$trips$shape_id)%>%
group_by(shape_id)%>%
filter(n() > 1)%>%
shapes_as_sf(.)%>%
st_transform("EPSG:26918")%>%
mutate(geom_text = as.character(geometry))%>%
arrange(geom_text)%>%
mutate(dup = ifelse(!duplicated(geometry),1,0))%>%
mutate(dup_sum = cumsum(dup))%>%
group_by(dup_sum)%>%
mutate(parent_shape = first(shape_id))%>%
st_set_geometry(NULL)%>%
ungroup()%>%
select(shape_id,parent_shape)

dat$trips <- dat$trips%>%
left_join(.,lines_exc)%>%
mutate(shape_id = ifelse(!is.na(parent_shape),parent_shape,shape_id))%>%
ungroup()%>%
select(-parent_shape)

dat$shapes <- dat$shapes%>%
filter(shape_id%in%lines_exc$parent_shape)

dat$stop_times <- dat$stop_times %>%
inner_join(.,dat$trips %>% select(trip_id, route_id))

if("pickup_type"%in%names(dat$stop_times)){
dat$stop_times <- dat$stop_times %>%
  filter(pickup_type!=1|drop_off_type!=1|is.na(pickup_type))
}

dat$stops <- dat$stops %>%
dplyr::filter(stop_id %in% dat$stop_times$stop_id)

if("parent_station"%notin%names(dat$stops)){
dat$stops <- dat$stops %>%
  mutate(parent_station = stop_id)
}else{
dat$stops <- dat$stops %>%
  mutate(parent_station = ifelse(nchar(parent_station)==0,stop_id,parent_station))
}

shapetst <- dat$trips%>%
group_by(route_id,direction_id)%>%
summarise(count = n(), count_notblank = sum(nchar(shape_id)>0,na.rm = TRUE))%>%
mutate(rat = count_notblank/count)%>%
ungroup()%>%
group_by(route_id)%>%
slice_max(order_by=rat,with_ties = FALSE)%>%
filter(rat < .75)

rm("checkid")

if(nrow(shapetst)>0){
stptns = dat$stop_times%>%
  inner_join(.,dat$trips%>%select(trip_id,route_id,direction_id))%>%
  inner_join(.,dat$stops %>% select(stop_id, parent_station))%>%
  filter(route_id %in% shapetst$route_id)%>%
  group_by(route_id,direction_id,trip_id)%>%
  summarise(stp = paste0(parent_station,collapse = "_n_"),n_stp = n())%>%
  ungroup()%>%
  group_by(route_id,direction_id,stp,n_stp)%>%
  mutate(id = cur_group_id())

if("shapes"%in%names(dat)){
shape_crosswalk <- stptns%>%
  inner_join(.,dat$stop_times%>%select(trip_id,stop_id))%>%
  ungroup()%>%
  select(route_id,direction_id,id,n_stp,stop_id)%>%
  unique()%>%
  inner_join(.,stops_as_sf(dat$stops))%>%
  st_as_sf()%>%
  st_transform("EPSG:26918")%>%
  st_buffer(.,402.336)%>%
  st_intersection(.,shapes_as_sf(dat$shapes)%>%st_transform(st_crs("EPSG:26918")))%>%
  ungroup()%>%
  st_set_geometry(NULL)%>%
  group_by(route_id,direction_id,id,n_stp,shape_id)%>%
  summarise(n = n())%>%
  filter(n == n_stp)%>%
  ungroup()%>%
  select(route_id,direction_id,id,shape_id)

checkid <- stptns %>%
  ungroup()%>%
  select(route_id,direction_id,id)%>%
  unique()%>%
  filter(id%notin%shape_crosswalk$id)
}else{
  checkid <- stptns %>%
    ungroup()%>%
    select(route_id,direction_id,id)%>%
    unique()
  shape_crosswalk <- data.frame(route_id= character(0),direction_id = numeric(0),id = numeric(0),shape_id = character(0))

}

if(nrow(checkid)>0){
  checkid <- checkid%>%
    inner_join(.,stptns%>%ungroup()%>%select(trip_id,route_id,direction_id,id)%>%unique())%>%
    inner_join(.,dat$stop_times%>%select(trip_id,stop_id,arrival_time))%>%
    filter(!duplicated(stri_c(route_id,"_",direction_id,"_",id,"_",stop_id)))%>%
    arrange(id,arrival_time)%>%
    group_by(id)%>%
    mutate(shape_pt_sequence = row_number())%>%
    select(route_id,direction_id,id,stop_id,shape_pt_sequence)%>%
    inner_join(.,stops_as_sf(dat$stops))%>%
    st_as_sf()%>%
    st_transform("EPSG:26918")%>%
    group_by(id)%>%
    arrange(shape_pt_sequence)%>%
    summarise(do_union = FALSE)%>%
    st_cast(.,"LINESTRING")%>%
    mutate(shape_id = stri_c("ImputedFromStops",id))
  
  shps <- checkid%>%
    st_cast("POINT")%>%
    st_transform("EPSG:4326")%>%
    mutate(shape_pt_lon = unlist(map(geometry,1)),
           shape_pt_lat = unlist(map(geometry,2)))%>%
    select(-id)%>%
    mutate(shape_pt_sequence= row_number())%>%
    st_set_geometry(NULL)
  
  if("shapes"%in%names(dat)){
    dat$shapes <- dat$shapes%>%
      rbind(.,shps)
  }else{
    dat$shapes <- shps
  }
  
  checkid <- checkid %>%
    st_set_geometry(NULL)%>%
    inner_join(.,stptns%>%ungroup()%>%select(route_id,direction_id,id)%>%unique())
  
  shape_crosswalk <- shape_crosswalk%>%
    rbind(.,checkid)
}

shape_crosswalk <- shape_crosswalk%>%
  inner_join(.,stptns%>%ungroup()%>%select(trip_id,route_id,direction_id,id)%>%unique())%>%
  filter(!duplicated(trip_id))%>%
  ungroup()%>%
  select(route_id,trip_id,shape_id2 = shape_id)

dat$trips <- dat$trips%>%
  left_join(.,shape_crosswalk)%>%
  mutate(shape_id = ifelse(!is.na(shape_id2),shape_id2,shape_id))%>%
  select(-shape_id2)
}

list2env(dat,.GlobalEnv)

stop_key <- stop_times%>%
dplyr::mutate(arrival_time = period_to_seconds(hms(arrival_time)), departure_time = period_to_seconds(hms(departure_time)))%>%
dplyr::filter(arrival_time > time_start & arrival_time <= time_end)%>%
inner_join(.,trips)%>%
dplyr::filter(route_id %in% c(route_tar,routes_secondary))%>%
dplyr::filter(direction_id %in% dir_tar)%>%
pull(stop_id)%>%
unique()

stops_sf <- stops_as_sf(dat$stops)%>%
dplyr::filter(stop_id %in% stop_key)%>%
st_transform(., " +proj=lcc +lat_0=40.1666666666667 +lon_0=-74 +lat_1=41.0333333333333 +lat_2=40.6666666666667 +x_0=300000 +y_0=0 +datum=NAD83 +units=us-mi +no_defs")

if(template_choice == "Most"){
trip_key <- stops_sf %>%
  filter(case_when(length(stop_mand)>0~stop_id%in%stop_mand,
                   TRUE ~ !is.na(stop_id)))%>%
  select(stop_id)%>%
  st_set_geometry(NULL)%>%
  inner_join(.,stop_times %>% select(stop_id, trip_id, arrival_time))%>%
  dplyr::mutate(arrival_time = period_to_seconds(hms(arrival_time)))%>%
  dplyr::filter(arrival_time > time_start & arrival_time <= time_end)%>%
  inner_join(.,trips %>% select(trip_id, route_id))%>%
  dplyr::filter(route_id %in% route_tar)%>%
  select(trip_id,route_id)%>%
  unique()%>%
  inner_join(.,stop_times %>% select(stop_id, trip_id))%>%
  ungroup()%>%
  group_by(trip_id,route_id)%>%
  dplyr::summarise(ptn = stri_c(stop_id, collapse="_"))%>%
  inner_join(.,trips %>% select(trip_id, direction_id,shape_id))%>%
  dplyr::filter(!is.na(shape_id))%>%
  ungroup()%>%
  group_by(ptn)%>%
  mutate(count = n())%>%
  arrange(.,-count)%>%
  dplyr::filter(!duplicated(shape_id))%>%
  ungroup()%>%
  group_by(direction_id,route_id)%>%
  slice_max(.,count,n=1)%>%
  arrange(route_id,shape_id)%>%
  slice(n=1)%>%
  ungroup()%>%
  filter(!duplicated(shape_id))
  
}else if(template_choice == "MostStops"){
trip_key <- stops_sf %>%
  filter(case_when(length(stop_mand)>0~stop_id%in%stop_mand,
         TRUE ~ !is.na(stop_id)))%>%
  select(stop_id)%>%
  st_set_geometry(NULL)%>%
  inner_join(.,stop_times %>% select(stop_id, trip_id, arrival_time))%>%
  dplyr::mutate(arrival_time = period_to_seconds(hms(arrival_time)))%>%
  dplyr::filter(arrival_time > time_start & arrival_time <= time_end)%>%
  inner_join(.,trips %>% select(trip_id, route_id))%>%
  dplyr::filter(route_id %in% route_tar)%>%
  group_by(trip_id,route_id)%>%
  dplyr::summarise(count = n())%>%
  inner_join(.,trips %>% select(trip_id, direction_id,shape_id))%>%
  dplyr::filter(!is.na(shape_id))%>%
  arrange(.,-count)%>%
  dplyr::filter(!duplicated(shape_id))%>%
  ungroup()%>%
  group_by(direction_id,route_id)%>%
  slice_max(.,count,n=1)%>%
  arrange(shape_id)%>%
  slice(n=1)%>%
  ungroup()%>%
  filter(!duplicated(shape_id))
}else{
  trip_key <- shapes_as_sf(dat$shapes)%>%
    inner_join(.,trips %>% select(trip_id,shape_id, direction_id,route_id)%>%unique())%>%
    inner_join(.,stop_times%>%select(trip_id,stop_id))%>%
    filter(case_when(length(stop_mand)>0~stop_id%in%stop_mand,
                       TRUE ~ !is.na(stop_id)))%>%
    select(-stop_id,-trip_id)%>%
    unique()%>%
    filter(route_id%in%route_tar)%>%
    st_transform(., " +proj=lcc +lat_0=40.1666666666667 +lon_0=-74 +lat_1=41.0333333333333 +lat_2=40.6666666666667 +x_0=300000 +y_0=0 +datum=NAD83 +units=us-mi +no_defs")%>%
    mutate(length = st_length(.))%>%
    group_by(route_id,direction_id)%>%
    slice_max(order_by = length, with_ties = FALSE)%>%
    st_set_geometry(NULL)
  
}

trip_key2 <- trip_key

ssfm <- stops_sf

rtlst <<- list(0)

rttst2 <- c()

for(i in 1:length(route_tar)){
  
  trip_key <- trip_key2%>%filter(route_id==route_tar[i])
  
routes_sf <- shapes_as_sf(dat$shapes)%>%
  dplyr::filter(shape_id %in% trip_key$shape_id)%>%
  st_transform(., " +proj=lcc +lat_0=40.1666666666667 +lon_0=-74 +lat_1=41.0333333333333 +lat_2=40.6666666666667 +x_0=300000 +y_0=0 +datum=NAD83 +units=us-mi +no_defs")

stops_sf <- ssfm

rttst <- st_difference(stops_sf%>%inner_join(.,stop_times%>%
                                               dplyr::mutate(arrival_time = period_to_seconds(hms(arrival_time)))%>%
                                               dplyr::filter(arrival_time > time_start & arrival_time <= time_end)%>%
                                                           select(trip_id,stop_id)%>%
                                                           unique()%>%
                                                           inner_join(.,trips%>%select(trip_id,shape_id,direction_id))%>%
                                                           select(-trip_id)%>%
                                                           unique()%>%
                                                           filter(shape_id%in%trip_key$shape_id)),
                                   st_buffer(routes_sf[1,],.1))%>%
  rbind(st_difference(stops_sf%>%inner_join(.,stop_times%>%
                                              dplyr::mutate(arrival_time = period_to_seconds(hms(arrival_time)))%>%
                                              dplyr::filter(arrival_time > time_start & arrival_time <= time_end)%>%
                                              select(trip_id,stop_id)%>%
                                              unique()%>%
                                              inner_join(.,trips%>%select(trip_id,shape_id,direction_id))%>%
                                              select(-trip_id)%>%
                                              unique()%>%
                                              filter(shape_id%in%trip_key$shape_id)),
                      st_buffer(routes_sf[nrow(routes_sf),],.1)))%>%
  group_by(parent_station)%>%
  filter(n()<2)

rttst <- ifelse(nrow(rttst)>0,1,0)

rttst2 <- c(rttst2,rttst)
  
if(length(route_tar) > 1 | length(routes_secondary)>0){
  
  routes_sf2 <- shapes_as_sf(dat$shapes)%>%
    inner_join(.,trip_key2)%>%
    filter(route_id%in%trip_key$route_id)%>%
    st_transform(st_crs(ssfm))
  
  stops_join2 <- list(0)
  
  for(k in 1:length(unique(routes_sf2$direction_id))){
    
  stops_join <- ssfm%>% 
    left_join(.,stop_times%>%inner_join(.,trips)%>%filter(shape_id%in%trip_key2$shape_id)%>%select(stop_id,direction_id,route_id)%>%unique())%>%
    mutate(flag = ifelse(is.na(direction_id),1,0))%>%
    left_join(.,stop_times%>%inner_join(.,trips)%>%filter(route_id%in%c(route_tar,routes_secondary))%>%select(stop_id,direction_id2=direction_id,route_id2=route_id)%>%unique()%>%mutate(flag=1))%>%
    mutate(direction_id = ifelse(is.na(direction_id),direction_id2,direction_id))%>%
    mutate(route_id = ifelse(is.na(route_id),route_id2,route_id))%>%
    filter(route_id != route_tar[i])%>%
    filter(direction_id == unique(routes_sf2$direction_id)[k])%>%
    st_as_sf()
  
  stops_join <- stops_join%>%
    ungroup()%>%
    dplyr::mutate( id = row_number()) %>%
    dplyr::filter( id %in% unlist(st_intersects(routes_sf2%>%filter(direction_id == unique(routes_sf2$direction_id)[k]), 
                                                st_buffer(stops_join,0.0378788)))) %>%
    mutate(overlap=1)%>%
    mutate(route_id = route_tar[i])%>%
    mutate(direction_id = unique(routes_sf2$direction_id)[k])
  
  stops_join2 <<- list.append(stops_join2,stops_join)
  
  }
  
  stops_join2[[1]] <- NULL
  
  stops_join <- rbindlist(stops_join2)%>%
    st_as_sf()
  
}

if(rttst == 1 & length(dir_tar)>1){
  
  routes_sf_d0 <- routes_sf%>%
    inner_join(trip_key)%>%
    filter(direction_id == 0)
  
  routes_sf_d1 <- routes_sf%>%
    inner_join(trip_key)%>%
    filter(direction_id == 1)
  
  stops_sf <- stops_sf%>%
    left_join(.,stop_times%>%inner_join(.,trips)%>%filter(shape_id%in%trip_key$shape_id)%>%select(stop_id,direction_id,route_id)%>%unique())%>%
    mutate(flag = ifelse(is.na(direction_id),1,0))%>%
    left_join(.,stop_times%>%inner_join(.,trips)%>%filter(route_id%in%route_tar)%>%select(stop_id,direction_id2=direction_id,route_id)%>%unique()%>%mutate(flag=1))%>%
    mutate(direction_id = ifelse(is.na(direction_id),direction_id2,direction_id))%>%
    filter(route_id==route_tar[i])%>%
    select(-route_id)
  
  if(i == 1 & length(route_tar)>1 | length(routes_secondary)>0){
    stops_sf <- stops_sf%>%
      bind_rows(.,stops_join%>%filter(stop_id%notin%stops_sf$stop_id)%>%select(names(stops_sf)))%>%
      mutate(brk_flag = 0)
  }else{
    stops_sf <- stops_sf%>%
    mutate(brk_flag = 0)
  }
  
  stops_preserved <- stops_sf
  
  stops_sf <- stops_sf%>%
    dplyr::mutate( id = row_number()) %>%
    dplyr::filter( id %in% unlist(st_intersects(routes_sf_d0, st_buffer(stops_sf,.075)))) 
  
  stops_sf$geometry <- snapPointsToLines(as(stops_sf,Class="Spatial"), as(routes_sf_d0,Class="Spatial"), maxDist = .124)%>%
    st_as_sf()%>%pull(geometry)
  
  stops_sf <- stops_sf %>%
    dplyr::mutate(dist = gProject(as(routes_sf_d0,Class = "Spatial"), as(stops_sf, Class = "Spatial")))%>%
    ungroup()%>%
    dplyr::mutate(dist = dist-min(dist))%>%
    mutate(dist_raw= dist)%>%
    mutate(orig_shape = "d0")

  stops_preserved <- stops_preserved%>%
    filter(direction_id == 1)%>%
    ungroup()
  
  stops_preserved <- stops_preserved%>%
    dplyr::mutate( id = row_number()) %>%
    dplyr::filter( id %in% unlist(st_intersects(routes_sf_d1, st_buffer(stops_preserved,.124)))) 
  
  stops_preserved$geometry <- snapPointsToLines(as(stops_preserved,Class="Spatial"), as(routes_sf_d1,Class="Spatial"), maxDist = .4)%>%
    st_as_sf()%>%pull(geometry)
  
  stops_preserved <- stops_preserved %>%
    dplyr::mutate(dist = gProject(as(routes_sf_d1,Class = "Spatial"), as(stops_preserved, Class = "Spatial")))%>%
    dplyr::mutate(dist = dist-min(dist))%>%
    ungroup()%>%
    mutate(orig_shape = "d1")
  
  stops_sf <- stops_sf%>%
    bind_rows(.,stops_preserved)%>%
    group_by(stop_id,direction_id)%>%
    mutate(n_occur = n())%>%
    arrange(direction_id,orig_shape,dist)%>%
    ungroup()%>%
    group_by(direction_id,orig_shape)%>%
    mutate(brk_flag = case_when(n_occur == 2 & direction_id == 1 & orig_shape == "d1" & lead(n_occur)==1 |
                                   n_occur == 2 & direction_id == 1& orig_shape == "d1" & lag(n_occur)==1~1,
                                 TRUE ~ 0))%>%
    mutate(brk_flag = case_when(n_occur == 2 & direction_id == 1 & orig_shape == "d1" & lead(n_occur)==1 &
                                  n_occur == 2 & direction_id == 1& orig_shape == "d1" & lag(n_occur)==1~2,
                                TRUE ~ brk_flag))
  
  stops_sf <- stops_sf%>%
    bind_rows(.,stops_sf%>%filter(brk_flag == 2))%>%
    arrange(direction_id,orig_shape,dist)%>%
    mutate(brk_flag = ifelse(brk_flag == 2, 1, brk_flag))
  
  infix <- stops_sf%>%
    st_set_geometry(NULL)%>%
    ungroup()%>%
    filter(brk_flag == 1)%>%
    filter(!duplicated(stop_id))%>%
    mutate(brk_elim = case_when(abs(lead(dist)-dist)<.5 & abs(lag(dist)-dist)<.5 ~ 1, 
                                TRUE ~ 0))%>%
    mutate(brk_elim = case_when(row_number()%%2 == 0 & lead(brk_elim) == 1 & abs(lead(dist)-dist)<.5 ~1,
                                row_number()%%2 == 1 & lag(brk_elim) == 1 & abs(lag(dist)-dist)<.5 ~1,
                                TRUE ~ brk_elim))%>%
    mutate(brk_elim = case_when(lead(brk_elim)==1 | lag(brk_elim) == 1 ~ brk_elim,
                                TRUE ~ 0))%>%
    filter(brk_elim==0)%>%
    mutate(grp = cumsum(ifelse(row_number() %% 2 == 1, 1, 0)))%>%
    inner_join(.,stops_sf%>%filter(orig_shape == "d0")%>%ungroup()%>%
                 st_set_geometry(NULL)%>%
                 select(stop_id,direction_id,dist2=dist))%>%
    left_join(stops_preserved%>%select(stop_id,stop_name,parent_station,direction_id,dist_d1=dist,orig_shape),.)%>%
    arrange(dist_d1)%>%
    ungroup()%>%
    mutate(conflict = ifelse(row_number()[which.max(dist2)]<row_number()[which.min(dist2)],1,0))%>%
    mutate(dist_d1 = case_when(1 %in% conflict~max(dist_d1)-dist_d1,
                            TRUE ~ dist_d1))%>%
    arrange(dist_d1)%>%
    mutate(dist_raw = dist_d1)%>%
    mutate(brk_elim = ifelse(!is.na(dist2),1,0))%>%
    mutate(grp = cumsum(brk_elim))%>%
    mutate(grp = case_when(!is.na(dist2) & grp > lag(grp) & grp %% 2 == 0 ~ grp-1,
                           TRUE ~ grp))%>%
    filter(grp %% 2 == 1 | brk_flag == 1)%>%
    group_by(grp)%>%
    mutate(dist_d1 = dist_d1-min(dist_d1))%>%
    mutate(dist2 = ifelse(row_number()==1, max(dist_d1)-min(dist_d1),0))%>%
    ungroup()%>%
    mutate(dist3 = lag(dist_d1))%>%
    group_by(grp)%>%
    mutate(dist3 = case_when(row_number()==1~dist3,
                             TRUE ~ 0))%>%
    mutate(dist3 = max(dist3,na.rm=TRUE))%>%
    mutate(dist2 = case_when(row_number()==1 ~ max(dist_d1),
                             TRUE ~ 0))%>%
    ungroup()%>%
    mutate(dist2 = cumsum(dist2))%>%
    mutate(dist3 = ifelse(grp != 1 & lag(grp)!=grp, lag(dist2),dist3))
  
  stops_sf <- stops_sf%>%
    select(-brk_flag)%>%
    arrange(dist,direction_id)%>%
    filter(orig_shape == "d0")%>%
    left_join(.,infix%>%select(stop_id,stop_name,grp,direction_id,dist2,dist3)%>%
                group_by(grp)%>%
                slice_min(order_by=row_number())%>%
                st_set_geometry(NULL)%>%
                ungroup())%>%
    ungroup()%>%
    fill(dist2,.direction = "down")%>%
    mutate(dist = case_when(!is.na(dist3) ~ dist+dist3,
                            is.na(dist3) & !is.na(dist2)~dist+dist2,
                            is.na(dist3) & is.na(dist2) ~dist,
                            TRUE~ dist))
  
  stops_elim <- infix%>%filter(is.na(brk_flag))%>%pull(stop_id)
  
  infix2 <- infix%>%
    select(stop_id,stop_name,parent_station,direction_id,grp,brk_flag,dist_d1,dist_raw)%>%
    left_join(.,stops_sf%>%
                filter(!is.na(dist3))%>%
                st_set_geometry(NULL)%>%
                select(stop_id,stop_name,direction_id,grp,dist_add=dist))%>%
    arrange(grp,dist_d1)%>%
    fill(dist_add,.direction = "down")%>%
    ungroup()%>%
    mutate(dist = dist_d1+dist_add,
           brk_flag = case_when(lead(brk_flag)==1&!is.na(lead(brk_flag))~1,
                                row_number()==max(row_number())~1,
                                TRUE~0))%>%
    select(stop_id,stop_name,parent_station,direction_id,brk_flag,dist,dist_raw)
  
  stops_sf <- stops_sf%>%
    filter(direction_id==1&stop_id%notin%stops_elim|direction_id==0)%>%
    bind_rows(.,infix2)%>%
    arrange(dist)%>%
    mutate(brk_flag = ifelse(is.na(brk_flag),0,brk_flag))%>%
    group_by(stop_id)%>%
    mutate(brk_flag = max(brk_flag))%>%
    arrange(dist,direction_id)%>%
    ungroup()%>%
    filter(!duplicated(stri_c(direction_id,"-",stop_id)))%>%
    mutate(brk_flag = case_when(direction_id==lead(direction_id)&brk_flag==1&lead(brk_flag)==1~0,
                                TRUE~brk_flag))%>%
    mutate(grp = cumsum(brk_flag))%>%
    mutate(grp = case_when(is.na(flag)&lag(grp)==(grp-1)&!is.na(lead(flag))~grp-1,
                           TRUE ~ grp))%>%
    mutate(route_id=route_tar[i])%>%
    mutate(n_occur_dir = n_occur)%>%
    select(-direction_id2)%>%
    group_by(grp)%>%
    mutate(dist4 = max(dist2,na.rm = TRUE))%>%
    mutate(dist_raw = ifelse(is.na(flag),dist_raw+dist4,dist_raw))%>%
    select(-dist4)%>%
    ungroup()
             
}else{
  routes_sf <- routes_sf[1,]

stops_sf <- stops_sf%>%
  inner_join(.,stop_times%>%select(trip_id,stop_id)%>%inner_join(.,trips%>%select(trip_id,route_id))%>%select(-trip_id)%>%unique())%>%
  filter(route_id==route_tar[i])%>%
  select(-route_id)%>%
  ungroup()

if(length(route_tar)>1){
  stops_sf <- stops_sf%>%
    bind_rows(.,stops_join%>%filter(stop_id%notin%stops_sf$stop_id & overlap == 1 & route_id == route_tar[i])%>%select(colnames(stops_sf)))%>%
    ungroup()
}

stops_sf <- stops_sf%>%
  dplyr::mutate( id = row_number()) %>%
  dplyr::filter( id %in% unlist(st_intersects(routes_sf, st_buffer(stops_sf,.125))))

stops_sf$geometry <- snapPointsToLines(as(stops_sf,Class="Spatial"), as(routes_sf,Class="Spatial"), maxDist = .126)%>%
  st_as_sf()%>%pull(geometry)

stops_sf <- stops_sf %>%
  dplyr::mutate(dist = gProject(as(routes_sf,Class = "Spatial"), as(stops_sf, Class = "Spatial")))%>%
  dplyr::mutate(dist = dist-min(dist))%>%
  mutate(dist_raw = dist)%>%
  mutate(brk_flag = 0, grp = 0, grp2 = 0)%>%
  inner_join(.,stop_times%>%select(trip_id,stop_id)%>%unique()%>%
               inner_join(.,trips%>%select(trip_id,direction_id))%>%
               select(-trip_id)%>%
               unique())%>%
  mutate(route_id=route_tar[i])%>%
  arrange(direction_id,dist)%>%
  mutate(n_occur_dir = 2)%>%
  mutate(orig_shape = "db")
  
}

rtlst <<- list.append(rtlst,stops_sf)

}

rtlst[[1]]<-NULL

if(length(route_tar)>1){
  
  ntf <- stop_times%>%
    dplyr::mutate(arrival_time = period_to_seconds(hms(arrival_time)), departure_time = period_to_seconds(hms(departure_time)))%>%
    dplyr::filter(arrival_time > time_start & arrival_time <= time_end)%>%
    select(stop_id,trip_id)%>%
    inner_join(.,trips%>%select(trip_id,route_id,direction_id))%>%
    select(route_id,direction_id)%>%
    unique()%>%
    mutate(ntf = stri_c(route_id, "-", direction_id))
  
rtlst2 <- rbindlist(rtlst,fill = TRUE)%>%
  ungroup()%>%
  arrange(route_id,direction_id,dist)%>%
  group_by(stop_id)%>%
  mutate(n_occur=n_distinct(route_id))%>%
  left_join(.,stop_times%>%
              dplyr::mutate(arrival_time = period_to_seconds(hms(arrival_time)), departure_time = period_to_seconds(hms(departure_time)))%>%
              dplyr::filter(arrival_time > time_start & arrival_time <= time_end)%>%
              select(stop_id,trip_id)%>%
               inner_join(.,trips%>%select(trip_id,route_id,direction_id))%>%
              filter(route_id %in%route_tar)%>%
               select(route_id,direction_id,stop_id)%>%
               unique()%>%
              group_by(stop_id)%>%
               mutate(n_occur_sch = n()-1))%>%
  left_join(.,stop_times%>%
              dplyr::mutate(arrival_time = period_to_seconds(hms(arrival_time)), departure_time = period_to_seconds(hms(departure_time)))%>%
              dplyr::filter(arrival_time > time_start & arrival_time <= time_end)%>%
              select(stop_id,trip_id)%>%
              inner_join(.,trips%>%select(trip_id,route_id,direction_id))%>%
              filter(route_id %in%route_tar)%>%
              select(route_id,direction_id,stop_id)%>%
              unique()%>%
              group_by(stop_id)%>%
              mutate(n_occur_sch_nort = n()-1)%>%
              select(direction_id,stop_id,n_occur_sch_nort))%>%
  unique()%>%
  mutate(n_fake = ifelse(is.na(n_occur_sch),1,0))%>%
  mutate(n_occur_sch = ifelse(is.na(n_occur_sch),0,n_occur_sch))%>%
  group_by(route_id,direction_id)%>%
  mutate(n_occur = ifelse(n_occur_sch+1 < n_occur & lead(n_occur)!=2 & lag(n_occur)!=2, n_occur-1,n_occur))%>%
  mutate(n_occur_sch2 = case_when(n_occur_sch==0&n_occur==1&lag(n_occur)!=1&!is.na(lag(n_occur))~1,
                                  n_occur_sch==0&n_occur==1&is.na(lag(n_occur))~1,
                                  n_occur_sch==1&n_occur==2&lag(n_occur_sch)!=1&!is.na(lag(n_occur_sch))~1,
                                  n_occur_sch==1&n_occur==2&is.na(lag(n_occur))~2,
                                  TRUE ~ 0))%>%
  arrange(route_id,direction_id,-dist)%>%
  mutate(n_occur_sch3 = case_when(n_occur_sch==0&n_occur==1&lag(n_occur)!=1&!is.na(lag(n_occur))~1,
                                  n_occur_sch==0&n_occur==1&is.na(lag(n_occur))~1,
                                  n_occur_sch==1&n_occur==2&lag(n_occur_sch)!=1&!is.na(lag(n_occur_sch))~1,
                                  n_occur_sch==1&n_occur==2&is.na(lag(n_occur))~2,
                                  TRUE ~ 0))%>%
  mutate(n_occur_sch3 = cumsum(n_occur_sch3))%>%
  arrange(route_id,direction_id,dist)%>%
  mutate(n_occur_sch2 = cumsum(n_occur_sch2))%>%
  group_by(route_id,direction_id,n_occur_sch2)%>%
  left_join(.,ntf)%>%
  mutate(n_occur = ifelse(max(n_occur+n_occur_sch)<3&n_occur==2&!is.na(ntf),1,n_occur))%>%
  group_by(route_id,direction_id,n_occur_sch3)%>%
  mutate(n_occur = ifelse(max(n_occur+n_occur_sch)<3&n_occur==2&!is.na(ntf),1,n_occur))%>%
  ungroup()%>%
  group_by(stop_id)%>%
  mutate(n_occur = min(n_occur))%>%
  mutate(elim = ifelse(n_occur == 1 & !is.na(ntf) & n_fake == 1,1,0))%>%
  filter(elim==0)%>%
  ungroup()%>%
  select(-n_occur_sch2,-n_occur_sch3,-n_occur_sch,-elim,-n_fake)%>%
  group_by(route_id,direction_id)%>%
  mutate(brk_flag = case_when(n_occur == 2 & lead(n_occur)==1 |
                                n_occur == 2 & lag(n_occur)==1~1,
                              TRUE ~ 0))%>%
  mutate(brk_flag = case_when(n_occur == 2 & lead(n_occur)==1 &
                                n_occur == 2 & lag(n_occur)==1~2,
                              TRUE ~ brk_flag))%>%
  mutate(brk_flag = case_when(n_occur == 2 & is.na(lead(n_occur)) | n_occur == 2 & is.na(lag(n_occur)) ~0,
                              TRUE ~ brk_flag))

rtlst2 <- rtlst2%>%
  bind_rows(.,rtlst2%>%filter(brk_flag == 2))%>%
  arrange(route_id,direction_id,dist)%>%
  mutate(brk_flag = ifelse(brk_flag == 2, 1, brk_flag))

rtlst_all <- rtlst2

rtlst2 <- rtlst2%>%
  filter(route_id == route_tar[1])

rtlstall2 <- rtlst_all%>%
  filter(route_id==route_tar[2])%>%
  filter(n_occur == 1 | brk_flag == 1)%>%
  arrange(dist)%>%
  group_by(direction_id)%>%
  mutate(brk_flag = ifelse(!is.na(lag(n_occur))& lag(n_occur)==1 & brk_flag == 1 & duplicated(brk_flag), 0,brk_flag))%>%
  mutate(grp2 = cumsum(brk_flag))%>%
  mutate(grp2 = case_when(!duplicated(brk_flag)&!is.na(lag(n_occur))& lag(n_occur)==1 & brk_flag == 1~grp2-1,
                          TRUE ~ grp2))%>%
  select(-one_of("dist2"))%>%
  left_join(.,rtlst_all%>%ungroup()%>%filter(route_id == route_tar[1]&brk_flag == 1)%>%select(stop_id,direction_id,dist2=dist))%>%
  group_by(grp2,direction_id)%>%
  mutate(fail = ifelse(sum(!is.na(dist2))==0,1,0))%>%
  select(-one_of("dist3"))%>%
  left_join(.,rtlst_all%>%ungroup()%>%filter(route_id == route_tar[1])%>%select(stop_id,direction_id,dist3=dist))%>%
  mutate(dist2 = ifelse(fail==1,dist3,dist2))%>%
  mutate(dist3 = ifelse(is.na(dist3),dist2,dist3))%>%
  mutate(conflict = ifelse(row_number()[which.max(dist3)]<row_number()[which.min(dist3)],1,0))%>%
  mutate(dist = case_when(1 %in% conflict~max(dist)-dist,
                             TRUE ~ dist))%>%
  arrange(dist)%>%
  mutate(brk_flag = ifelse(n_occur == 2, 1, 0))%>%
  mutate(brk_flag = ifelse(!is.na(lag(n_occur))& lag(n_occur)==1 & brk_flag == 1 & duplicated(brk_flag), 0,brk_flag))%>%
  select(-one_of("dist3"))%>%
  mutate(pos=case_when(row_number() == min(row_number())~1,
                       row_number() == max(row_number())~2,
                       TRUE ~ 0))%>%
  ungroup()%>%
  group_by(grp2)%>%
  mutate(dist = dist - min(dist))%>%
  mutate(add_factor = dist-dist_raw)%>%
  ungroup()%>%
  mutate(add_grp = cumsum(brk_flag))%>%
  group_by(add_grp)%>%
  mutate(add_factor = first(add_factor))%>%
  ungroup()%>%
  group_by(grp)%>%
  mutate(add_factor = case_when(max(brk_flag)==0&max(is.na(orig_shape))==0~min(add_factor),
                                TRUE ~ add_factor))%>%
  mutate(dist = case_when(max(rttst2)>0~add_factor+dist_raw,
                          TRUE~dist))%>%
  ungroup()%>%
  group_by(grp2,direction_id)%>%
  mutate(dist = case_when(1 %in% conflict~min(dist)+(max(dist)-dist),
                          TRUE ~ dist))%>%
  ungroup()%>%
  group_by(grp2)%>%
  mutate(dist2 = min(dist2,na.rm=TRUE))%>%
  fill(dist2,.direction="down")%>%
  mutate(dist3 = max(dist))%>%
  ungroup()%>%
  mutate(dist3 = ifelse(!is.na(lag(dist3)),lag(dist3),0))%>%
  group_by(grp2)%>%
  mutate(dist3 = case_when(row_number()==1~dist3,
                           TRUE ~ 0))%>%
  ungroup()%>%
  mutate(dist3 = cumsum(dist3))%>%
  mutate(dist = dist+dist2+dist3)%>%
  group_by(grp2)%>%
  mutate(dist4 =max(dist))%>%
  ungroup()

rtlstall3 <- rtlstall2%>%
  filter(brk_flag==1)%>%
  mutate(from_other = 1)%>%
  select(stop_id,direction_id,from_other,pos,dist2=dist4)%>%
  inner_join(.,rtlst2%>%select(stop_id,direction_id,route_id))
  
  rtlst3 <- rtlst2 %>%
    arrange(route_id,dist,direction_id)%>%
    select(-one_of("dist2"))%>%
    left_join(.,rtlstall3)%>%
    arrange(dist)%>%
    ungroup()%>%
    mutate(add = cumsum(ifelse(!is.na((dist2)),1,0)))%>%
    group_by(add)%>%
    fill(dist2,.direction="down")%>%
    mutate(dist2=ifelse(is.na(dist2),0,dist2))%>%
    mutate(dist = dist2+(dist-min(dist)))
  
  stops_sf2 <- rtlst3%>% #change group code to collapse nocc=1 grps to join with leading/lagging nocc=2 groups where appropriate, while preventing B13/B20 matching route_id grouping issue @ FP/Foster
    bind_rows(.,rtlstall2)%>%
    st_as_sf()%>%
    ungroup()%>%
    group_by(direction_id,stop_id)%>%
    arrange(direction_id,dist)%>%
    filter(case_when(pos==1~row_number()==which.min(dist),
                     pos==2~row_number()==which.max(dist),
                     TRUE ~ !is.na(stop_id)))%>%
    select(-pos)%>%
    arrange(direction_id,dist)%>%
    ungroup()%>%
    group_by(direction_id)%>%
    mutate(grp3 = cumsum(ifelse(lag(route_id)!=route_id&!is.na(lag(route_id)),1,0)))%>%
    mutate(grp3 = ifelse(grp3%%2==1,grp3-1,grp3))%>%
    mutate(grp2 = ifelse(n_occur==2&brk_flag==1&!is.na(lag(grp2)),lag(grp2),grp2))%>%
    ungroup()%>%
    mutate(change = ifelse(lag(grp)!=grp & !is.na(grp) | lag(grp2) != grp2 & !is.na(grp2) | lag(grp3) != grp3 & !is.na(grp3), 1, 0))%>%
    mutate(n_occur = ifelse(n_occur == 2 & lead(n_occur)==1 & lag(n_occur)==1, 1, n_occur))%>%
    mutate(change = ifelse(is.na(change),0,change))%>%
    mutate(change = case_when(change == 1 & lag(direction_id)==direction_id & lag(route_id)==route_id & lag(grp)==grp & 
                                abs(abs(dist_raw-lag(dist_raw))-abs(dist-lag(dist)))<.05 ~ 0,
                              change == 1 & n_occur == 2 & abs(abs(dist_raw-lag(dist_raw))-abs(dist-lag(dist)))<.05 ~ 0,
                              lag(grp)!=grp & !is.na(grp) & change == 1 & lag(route_id)==route_id &
                                lag(direction_id)==direction_id & abs(abs(dist_raw-lag(dist_raw))-abs(dist-lag(dist)))<.05 ~ 0,
                              n_occur == 1 & lag(n_occur) == 1 & route_id != lag(route_id)~ 1, 
                              !is.na(add)&!is.na(lag(add))&add!=lag(add)~ 1,
                              TRUE ~ change))%>%
    mutate(grp = cumsum(change))%>%
    group_by(stop_id,direction_id)%>%
    mutate(grp = max(grp))%>%
    mutate(n_occur2 = n())
  
  stops_sf <- stops_sf2%>%
    ungroup()
  
}else{
  stops_sf <- rbindlist(rtlst)%>%
    st_as_sf()%>%
    group_by(stop_id)%>%
    mutate(grp = max(grp))
}
  
df <- stop_times %>%
  arrange(trip_id,arrival_time)%>%
  dplyr::filter(stop_id %in% stops_sf$stop_id)%>%
  dplyr::mutate(arrival_time = period_to_seconds(hms(arrival_time)), departure_time = period_to_seconds(hms(departure_time)))%>%
  dplyr::filter(arrival_time > time_start & arrival_time <= time_end)

if("timepoint"%in%names(df)){
  df$tpt <- df$timepoint
}else{
  df$tpt <- 0
}

df <- df%>%
  inner_join(.,stops_sf %>% st_set_geometry(NULL) %>% select(stop_id, stop_name, dist,grp))%>%
  inner_join(.,trips %>% select(trip_id,direction_id))%>%
  arrange(trip_id,arrival_time)

df_dep <- df%>%
  dplyr::filter(arrival_time != departure_time)%>%
  mutate(arrival_time=departure_time)%>%
  dplyr::filter(arrival_time > time_start & arrival_time <= time_end)%>%
  dplyr::mutate(arrival_time_t = as.POSIXct(arrival_time, origin = "1970-01-01 00:00:00", tz= "America/New_York"))%>%
  select(trip_id, stop_id, stop_sequence,stop_name, direction_id, dist, grp,tpt, arrival_time,arrival_time_t)

df<- df%>%
  dplyr::mutate(arrival_time_t = as.POSIXct(arrival_time, origin = "1970-01-01 00:00:00", tz= "America/New_York"))%>%
  select(trip_id, stop_id, stop_sequence,stop_name, direction_id, dist,grp,tpt, arrival_time,arrival_time_t)%>%
  bind_rows(.,df_dep)

key <- stop_times %>%
  dplyr::filter(trip_id %in% df$trip_id)%>%
  group_by(trip_id)%>%
  dplyr::summarise(stop_sequence = max(stop_sequence))

df2 <- inner_join(stop_times, stops %>% select(stop_id, stop_name))%>%
  inner_join(.,key, by = c("trip_id", "stop_sequence"))%>%
  select(trip_id, headsign = stop_name)%>%
  inner_join(.,trips %>% select(trip_id, route_id))

dest <- stop_times%>%
  inner_join(.,trips %>% select(route_id, trip_id))%>%
  filter(route_id %in% route_tar)%>%
  group_by(trip_id)%>%
  dplyr::summarise(stop_sequence = max(stop_sequence))%>%
  inner_join(stop_times %>% select(stop_sequence, stop_id, trip_id))

if("route_short_name"%notin%names(routes)){
  routes$route_short_name <- routes$route_id
}else{
  routes<-routes%>%
    mutate(route_short_name = ifelse(nchar(route_short_name)<1,route_id,route_short_name))
}

df <- df %>%
  inner_join(.,df2)%>%
  mutate(type = stri_c(route_id," in direction ", direction_id))%>%
  ungroup()%>%
  filter(!is.na(dist))%>%
  mutate(tst = stri_c(trip_id,"_",stop_name))%>%
  ungroup()%>%
  arrange(trip_id,arrival_time)%>%
  group_by(trip_id)%>%
  mutate(contra = ifelse(row_number()[which.max(dist)]<row_number()[which.min(dist)],1,0))%>%
  arrange(trip_id,arrival_time,dist)%>%
  group_by(trip_id, stop_id)%>%
  mutate(n = n())%>%
  filter(case_when(n > 1 & length(route_tar)>1 & contra == 1~dist == max(dist),
                   n > 1 & length(route_tar)>1 & contra == 0~dist == min(dist),
                   TRUE ~ !is.na(stop_id)))%>%
  select(-contra)%>%
  arrange(-dist)%>%
  filter(!duplicated(stri_c(stop_id,arrival_time_t)))%>%
  inner_join(.,routes%>%select(route_id,route_short_name))%>%
  arrange(stop_sequence)

stop_mdn <- stops_sf%>%
  ungroup()%>%
  group_by(direction_id)%>%
  mutate(dist = dist/max(dist))%>%
  mutate(distlag = dist - lag(dist))%>%
  filter(!is.na(distlag))%>%
  summarise(distlag= median(distlag))%>%
  ungroup()%>%
  slice_min(distlag,with_ties = FALSE)%>%
  st_set_geometry(NULL)%>%
  pull(distlag)

stops_sf2 <- stops_sf%>%
  inner_join(.,df%>%ungroup()%>%select(stop_id,tpt)%>%unique())%>%
  mutate(parent_station = ifelse(nchar(parent_station)<1,stop_id,parent_station))%>%
  group_by(parent_station)%>%
  mutate(n=n_distinct(direction_id))%>%
  ungroup()%>%
  arrange(direction_id,dist,-tpt)%>%
  ungroup()%>%
  group_by(direction_id)%>%
  mutate(elim = case_when(name_elim == TRUE & stop_mdn<.005 ~ row_number()/6,
                          name_elim == TRUE & stop_mdn<.01 ~ row_number()/5,
                          name_elim == TRUE & stop_mdn<.015 ~ row_number()/4,
                          name_elim == TRUE & stop_mdn<.02 ~ row_number()/3,
                          name_elim == TRUE & stop_mdn<.025 ~ row_number()/2,
                          TRUE ~ 1))%>%
  mutate(elim = case_when(elim %% 1 == 0~1,
                          tpt == 1~1,
                          lag(grp)!=grp~1,
                          dist == min(dist)|dist == max(dist)~1,
                          TRUE ~ 0))%>%
  mutate(elim = case_when(dist == lag(dist)~0,
                          dist != lag(dist)&
                            name_elim == TRUE&
                            stop_mdn<.015&
                            tpt==0&
                            lag(grp)==grp&
                            dist!=min(dist)&
                            dist!=max(dist)&
                            lead(elim)==1 ~ 0,
                          dist != lag(dist)&
                            name_elim == TRUE&
                            stop_mdn<.015&
                            tpt==0&
                            lag(grp)==grp&
                            dist!=min(dist)&
                            dist!=max(dist)&
                            lag(elim)==1 ~ 0,
                          TRUE ~ elim))%>%
  mutate(stop_name = ifelse(elim == 0, "",stop_name))%>%
  group_by(stop_id)%>%
  mutate(n = n_distinct(direction_id))%>%
  mutate(sbst = ifelse(n == 2, 2, direction_id))%>%
  select(-n)%>%
  ungroup()%>%
  arrange(dist)%>%
  filter(stri_c(stop_id, "-", dist)%in%stri_c(df$stop_id, "-", df$dist))

if("agency_id"%in%names(routes)){
agency <- agency%>%
  filter(agency_id%in%routes$agency_id)
}

if(exists("checkid")){
  add <- ifelse(nrow(checkid)>0," DISTANCES APPROXIMATE", "")
}else{
  add <- ""
}

p <- ggplot(df, aes(x = arrival_time_t, y = dist))+
    geom_line(data = df, aes(color = stri_c(route_short_name, "-", direction_id),
                               group =trip_id),linewidth=.65)+
    geom_line(data = df, aes(color = stri_c(route_short_name, "-", direction_id),
                                                          group = stri_c(trip_id,"_",grp)),linewidth = 2.25)+
    scale_color_brewer(palette = "Set2", name = "Route-Direction")+
    scale_size_manual(values = c(2.75,1.5))+
    theme(text = element_text(size=20),
          legend.text = element_text(size = 24), 
          legend.title = element_text(size = 26, face = "bold"), 
          plot.title = element_text(face="bold"),
          plot.background = element_rect("grey99"),
          panel.background = element_rect("grey99"),
          plot.margin = unit(c(.2,.2,.2,.2),"in"),
          panel.grid.major.x = element_line(color = "grey75", size = .8),
          panel.grid.minor.x = element_line(color = "grey85", size = .6),
          panel.grid.major.y = element_line(color = "grey85", size = .6),
          panel.grid.minor.y = element_line(color = "grey85", size = .6),
          axis.line = element_line(color = "grey70", size = .75),
          axis.ticks = element_line(color = "grey70"),
          axis.text = element_text(size = 12.5),
          plot.caption = element_text(size = 11),
          strip.text.y = element_blank())+
    guides(linetype = "none",size = "none")+
    geom_point(data = df,lwd = 1.45, alpha = .35,shape=16)+
    geom_point(data = df %>% 
                 inner_join(.,stops_sf2%>%st_set_geometry(NULL)%>%
                              select(stop_id,direction_id,stop_name2=stop_name)%>%
                              unique()%>%filter(nchar(stop_name2)>2)),
               lwd = 2.5)+
    scale_y_continuous(breaks = stops_sf2%>%filter(sbst %in% c(0,2) & nchar(stop_name)>1)%>%pull(dist), 
                       labels = stops_sf2%>%filter(sbst %in% c(0,2) & nchar(stop_name)>1)%>%pull(stop_name),
                       sec.axis = sec_axis(trans = ~ . / 1,
                                           breaks = stops_sf2%>%filter(sbst %in% c(1,2) & nchar(stop_name)>1)%>%pull(dist), 
                                           labels = stops_sf2%>%filter(sbst %in% c(1,2)& nchar(stop_name)>1)%>%pull(stop_name), name = stri_c("Stops in Direction 1", add)), 
                       minor_breaks = stops_sf2%>%filter(sbst %in% c(1,2) & nchar(stop_name)>1)%>%pull(dist))+
    scale_x_datetime(date_breaks = "1 hour", date_minor_breaks= "15 mins", labels = date_format("%H:%M"),sec.axis = dup_axis())+
    xlab("Time")+
    ylab(stri_c("Stops in Direction 0", add))+
    labs(title = paste0("String chart of ", agency$agency_name[1]," ", routes%>%filter(route_id%in%route_tar)%>%
                          pull(route_short_name)%>%unique()%>%sort()%>%stri_c(collapse = " and "), " ", routes%>%filter(route_id%in%route_tar)%>%
                          pull(route_long_name)), subtitle = paste0("Showing service from ", seconds_to_period(time_start), " to ", 
                                                              seconds_to_period(time_end), " on sample date ", date_tar),
         caption = "Chart code by Uday Schultz")

plot(p)

ggsave(stri_c("stringline","_",as.numeric(Sys.time()),".png"),device="png",
       plot = p, width = 30, height = 13, units = "in", dpi = 300) 
