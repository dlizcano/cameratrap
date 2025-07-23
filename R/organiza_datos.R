# helper functions
# function to make matrix per species per day, extracting and averaging hour 
# Modified from CI-TEAM Network code by Diego Lizcano
# May 2024
# Creates a nested list by species.
# One list is presence (detection history) another list is hour to make hour of detection as a covariate

dev.det_history.creator<-function(data,year){
  require(lubridate)
  require(hms)
  #results object
  res<-list()
  
  #get the dimensions of the matrix
  
  #list if sanpling units
  cams<-unique(data$locationID)
  cams<-sort(cams)
  rows<-length(cams)
  species<-unique(data$scientificName)
  # make some equivalents
  data$eventDate <- as_date(data$`Date_Time Captured`)
  data$time <- as_hms(ymd_hms(as_datetime(data$`Date_Time Captured`)))
  
  # start and end dates of sampling periods
  # data<-data[data$Sampling.Period==year,]
  min<-min(as.Date(as.character(data$start_date), "%Y/%m/%d"))
  max<-max(as.Date(as.character(data$end_date), "%Y/%m/%d"))
  cols<-max-min+1
  
  #sampling period
  date.header<-seq(from=min,to=max, by="days")
  
  mat<-matrix(NA,rows,cols,dimnames=list(cams,as.character(date.header)))
  
  #for all cameras, determine the open and close date and mark in the matrix
  start.dates<-tapply(as.character(data$start_date),data$locationID,unique)
  nms<-names(start.dates)
  # start.dates<-ymd(start.dates)
  names(start.dates)<-nms
  end.dates<-tapply(as.character(data$end_date),data$locationID,unique)
  # end.dates<-ymd(end.dates)
  names(end.dates)<-nms
  
  #outline the sampling periods for each camera j
  for(j in 1:length(start.dates)){
    #for each camera beginning and end of sampling
    low<-which(date.header==as.Date(as.character(start.dates[j]), format = "%Y-%m-%d"))
    hi<-which(date.header==as.Date(as.character(end.dates[j]), format = "%Y-%m-%d"))
    if(length(low)+length(hi)>0){
      indx<-seq(from=low,to=hi)
      mat[names(start.dates)[j],indx]<- 0
    } else next
  }
  mat.template<-mat 
  mat_h <- mat
  #get the species
  #species<-unique(data$bin)
  #construct the matrix for each species i
  for(i in 1:length(species)){
    indx<-which(data$scientificName==species[i])
    #dates and cameras when/where the species was photographed
    dates<-data$eventDate[indx]
    cameras<-data$locationID[indx]
    hora <- data$time[indx]
    
    dates.cameras <- data.frame(dates, cameras)
    hours.cameras <- data.frame(hora, cameras)
    #unique combination of dates and cameras 
    dates.cameras<-unique(dates.cameras)
    hours.cameras <- unique(hours.cameras)
    #fill in the obs matrix
    for(j in 1:length(indx)){
      col<-which(date.header==as.character( dates.cameras[j,1]))
      row<-which(cams==as.character( dates.cameras[j,2]))
      mat[row,col]<-1
    }
    mat.nas<-is.na(mat)
    sum.nas<-apply(mat.nas,2,sum)
    indx.nas<-which(sum.nas==rows)
    if(length(indx.nas)>0){
      mat<-mat[,-indx.nas]
    }
    
    
    #fill in the hour matrix
    for(j in 1:length(dates.cameras[,1])){
      col<-which(date.header==as.character( dates.cameras[j,1]))
      row<-which(cams==as.character( dates.cameras[j,2]))
      mat_h[row,col]<- hours.cameras[j,1]# remove hms
    }
    mat_h.nas<-is.na(mat)
    sum.nas<-apply(mat_h.nas,2,mean)# instead of sum
    indx.nas<-which(sum.nas==rows)
    if(length(indx.nas)>0){
      mat_h<-mat_h[,-indx.nas]
    }
    
    occur<-c(res,list(mat)) #lista anidada
    dete <- c(res,list(mat_h))
    res<-c(res,list(mat))
    # res<-list(occur=list(mat), hora=list(mat_h), sp=list(species)) #lista anidada
    #return the matrix to its original form
    mat<-mat.template
    
  }
  
  # names(occur)<-species
  names(dete)<-species
  #res<-lapply(res,f.dum)
  return(list(occur=occur, dete=dete))
  
}

########################################


#code to shrink the matrix to exactly 9 columns: collapsing by 3 days
f.shrink.matrix.to9<-function(matrix){
  nc<-dim(matrix)[2]
  if(!nc%%9){ # of the number of columns is exactly divisible by 9
    newc<-nc%/%9
    old.cols<-seq(1,nc,newc)
    new.matrix<-matrix(NA,nr=nrow(matrix),nc=9)
    for(i in 1:9){
      new.matrix[,i]<-apply(matrix[,old.cols[i]:(old.cols[i]+newc-1)],1,max,na.rm=T)
    }
  } else{
    rem<-nc%%9
    newc<-nc%/%9
    old.cols<-seq(1,nc-rem,newc)
    new.matrix<-matrix(NA,nr=nrow(matrix),nc=9)
    for(i in 1:8)
      new.matrix[,i]<-apply(matrix[,old.cols[i]:(old.cols[i]+newc-1)],1,max,na.rm=T)
    new.matrix[,9]<-apply(matrix[,old.cols[9]:nc],1,max,na.rm=T) 
  }
  new.matrix[new.matrix=="-Inf"]<-NA
  rownames(new.matrix)<-rownames(matrix)
  new.matrix
}


#code to shrink the hour matrix to exactly 9 columns: collapsing by 3 days
f.shrink.matrix.h.to9<-function(matrix){
  nc<-dim(matrix)[2]
  if(!nc%%9){ # of the number of columns is exactly divisible by 9
    newc<-nc%/%9
    old.cols<-seq(1,nc,newc)
    new.matrix<-matrix(NA,nr=nrow(matrix),nc=9)
    for(i in 1:9){
      new.matrix[,i]<-apply(matrix[,old.cols[i]:(old.cols[i]+newc-1)],1,mean,na.rm=T)
    }
  } else{
    rem<-nc%%9
    newc<-nc%/%9
    old.cols<-seq(1,nc-rem,newc)
    new.matrix<-matrix(NA,nr=nrow(matrix),nc=9)
    for(i in 1:8)
      new.matrix[,i]<-apply(matrix[,old.cols[i]:(old.cols[i]+newc-1)],1,mean,na.rm=T)
    new.matrix[,9]<-apply(matrix[,old.cols[9]:nc],1,mean,na.rm=T) 
  }
  new.matrix[new.matrix=="-Inf"]<-NA
  rownames(new.matrix)<-rownames(matrix)
  new.matrix
}

# load the full WCS excel file to make a dataframe linking: cameras, deployment and image
loadproject <- function(path_to_file){
  require(readxl)
  require(dplyr)
  cameras <- read_excel(path_to_file, 
                        sheet = "Cameras") |> rename("Camera Id"= "Camera id") 
  deployment <- read_excel(path_to_file, 
                        sheet = "Deployment") |> select(!c(ID)) |> 
    rename("Longitude" = "Longitude Resolution") |> 
    rename("Latitude" = "Latitude Resolution") |> 
    rename("start_date" = "Camera Deployment Begin Date") |> 
    rename("end_date" = "Camera Deployment End Date") |> 
    mutate(locationID=Point)
    
  
  image <- read_excel(path_to_file, 
                        sheet = "Image") |> 
    select(!c(ID, Location, "IUCN Identification Number", "individual Animal notes"  )) |> 
    rename("scientificName" = "Genus Species")
  
  data1 <-  cameras |> left_join(deployment) # join first two
  by <- join_by("Deployment ID") 
  data <- left_join(data1, image, by) # join by "Deployment ID"
  
  return (data)
  
} # end of loadproject



# get the sites to add covariates
get.sites <- function(path_to_file){
  require(readxl)
  require(dplyr)
  require(sf)
  
  deployment <- read_excel(path_to_file, 
                           sheet = "Deployment") |> select(!c(ID)) |> 
    rename("Longitude" = "Longitude Resolution") |> 
    rename("Latitude" = "Latitude Resolution") |> 
    rename("start_date" = "Camera Deployment Begin Date") |> 
    rename("end_date" = "Camera Deployment End Date") |> 
    rename("bait"="Bait Description") |> 
    rename("CamType"="Camera Type")  
  
  bait <- deployment |> # can prodece error if is a mix 
    distinct(Point, Longitude, Latitude, bait) 
  # count camera models
  camtypes <- deployment |> mutate(site=Point) |> distinct(Point, CamType) 
  CamTypes <-  as.data.frame(cbind(CamTypes=table(camtypes$Point),  Point=names(table(camtypes$Point))))
  a <- left_join(bait, CamTypes) |> mutate(across( c(CamTypes), as.factor) ) 
  
    # make sf
    sites <- st_as_sf(a, coords = c("Longitude","Latitude"))   #crs="EPSG:4326")
    #--- set CRS ---#
    st_crs(sites) <- 4326
  
  return (sites)
  
} 




f.det_history.creator<-function(data,year){
  #results object
  res<-list()
  require(lubridate)
  require(hms)
  #get the dimensions of the matrix
  
  #list if sanpling units
  cams<-unique(data$CT)
  cams<-sort(cams)
  rows<-length(cams)
  species<-unique(data$Species)
  # make some equivalents
  data$eventDate <- as.Date(as.character(data$eventDate), "%d/%m/%Y")
  data$time <- parse_hm(data$eventTime) # as_hms(ymd_hms(as_datetime(data$eventTime)))
  
  # start and end dates of sampling periods
  # data<-data[data$Sampling.Period==year,]
  min<-min(as.Date(as.character(data$Start), "%d/%m/%Y"))
  max<-max(dmy(becerril_2021$eventDate))# max(as.Date(as.character(data$End), "%d/%m/%Y"))
  cols<-max-min+1
  
  #sampling period
  date.header<-seq(from=min,to=max, by="days")
  mat<-matrix(NA,rows,cols,dimnames=list(cams,as.character(date.header)))
  
  #for all cameras, determine the open and close date and mark in the matrix
  start.dates<-tapply(as.Date(as.character(data$Start), "%d/%m/%Y"),data$CT,unique)
  nms<-names(start.dates)
  # start.dates<-ymd(start.dates)
  names(start.dates)<-nms
  end.dates<-tapply(as.Date(as.character(data$End), "%d/%m/%Y"),data$CT,unique)
  # end.dates<-ymd(end.dates)
  names(end.dates)<-nms
  
  #outline the sampling periods for each camera j
  for(j in 1:length(start.dates)){
    #for each camera beginning and end of sampling
    low<-which(date.header==min(start.dates[[j]])) # ojo es una lista
    hi<-which(date.header==max(end.dates[[j]])) # ojo es una lista de varios
    if(length(low)+length(hi)>0){
      indx<-seq(from=low,to=hi)
      mat[names(start.dates)[j],indx]<- 0
    } else next
  }
  mat.template<-mat
  #get the species
  #species<-unique(data$bin)
  #construct the matrix for each species i
  for(i in 1:length(species)){
    indx<-which(data$scientificName==species[i])
    #dates and cameras when/where the species was photographed
    dates<-data$eventDate[indx]
    cameras<-data$locationID[indx]
    dates.cameras<-data.frame(dates,cameras)
    #unique combination of dates and cameras 
    dates.cameras<-unique(dates.cameras)
    #fill in the matrix
    for(j in 1:length(dates.cameras[,1])){
      col<-which(date.header==as.character( dates.cameras[j,1]))
      row<-which(cams==as.character( dates.cameras[j,2]))
      mat[row,col]<-1
    }
    mat.nas<-is.na(mat)
    sum.nas<-apply(mat.nas,2,sum)
    indx.nas<-which(sum.nas==rows)
    if(length(indx.nas)>0){
      mat<-mat[,-indx.nas]
    }
    
    res<-c(res,list(mat))
    #return the matrix to its original form
    mat<-mat.template
  }
  
  names(res)<-species
  #res<-lapply(res,f.dum)
  res
  
}



##############################################
### Function to read by country
### Notice only reads deployment and image
##############################################
data_by_country <- function(country="Argentina"){
  # Identify file path string names
  pais <- paste("C:/CodigoR/WCS_2024/camera_trap/data", country, sep="/")
  recIDs <- list.files(pais,  recursive = FALSE, pattern = ".xlsx")
  i.strings <- paste0(pais, "/", recIDs, sep="")
  
  # make a list with all tables
  deployment_pais<-lapply(i.strings, function(x) read_excel(x, sheet = "Deployment", 
                                                            col_types = c("numeric", 
                                                                          "text", "text", "numeric", "numeric", 
                                                                          "text", "text", "text", "text", "text", 
                                                                          "text", "text", "text", "text", "text", 
                                                                          "text"), col_names = TRUE))
  
  image_pais<-lapply(i.strings, function(x) read_excel(x, sheet = "Image", skip = 1,
                                                       col_types = c("text", 
                                                                     "text", "text", "text", "text", "text", 
                                                                     "text", "text", "numeric", "text", 
                                                                     "numeric", "text", "text", "text", 
                                                                     "numeric", "text", "text"), col_names = TRUE))
  
  
  # extract names... 
  deployment_pais1<-list() #empty list
  # add file name at the end
  for(i in 1:length(recIDs)) {
    ExcelFile<-rep(recIDs[i],nrow(deployment_pais[[i]]))
    deployment_pais1[[i]]<-cbind(deployment_pais[[i]], ExcelFile)
    colnames(deployment_pais1[[i]])[ncol(deployment_pais)]<-"ExcelFile"
    #### 
    print(i)
    print(names(deployment_pais[[i]]))
    
  } # end loop
  
  
  
  ##############################
  # loop to checking problems...
  # remove comment to activate
  ##############################
  for(h in 1:length(recIDs)){
    archivo <- read_excel(i.strings[h], sheet = "Image", skip = 1,
                          col_types = c("text",
                                        "text", "text", "text", "text", "text",
                                        "text", "text", "numeric", "text",
                                        "numeric", "text", "text", "text",
                                        "numeric", "text", "text"), col_names = TRUE)
    print (paste("archivo: ", recIDs[h], "cols: ", dim(archivo)[2], sep=""))
    if(is.character(archivo$`Date_Time Captured`)==FALSE){
      print(paste("date problem in image sheet in file:"), recIDs[h] )
      
    }
  }
  
  
  
  ### convert to dataframe
  # deployment_Pais<- deployment_pais1 %>%  do.call(rbind, .) 
  deployment_Pais<-  bind_rows(deployment_pais1) %>% select("Deployment ID",
                                                            "Longitude Resolution",
                                                            "Latitude Resolution",
                                                            "Camera Deployment Begin Date",
                                                            "Camera Deployment End Date",
                                                            "Bait Type",
                                                            "Bait Description",
                                                            "Camera Id",
                                                            "Camera Type",
                                                            "ExcelFile") 
  # %>% write.csv(file=paste0("G:/Panama_Audubon/result/formated/pegadas/",
  
  deployment_Pais$year <- year(as.Date(deployment_Pais$`Camera Deployment Begin Date`))
  
  ## get the Project id
  
  # Make dataframe binding selected rows
  image_Pais<-  bind_rows(image_pais ) %>% select("Deployment ID",
                                                  "Photo Type",
                                                  "Genus Species",
                                                  "Date_Time Captured",
                                                  "Independent event",
                                                  "Age",
                                                  "Sex",
                                                  "Count")  |> left_join(deployment_Pais)
  
  return(image_Pais) # return the join table
  
} # end of function data_by_country


