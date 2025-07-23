


###################################
########### WI assembly ########### 
###################################

# Country, Project,	Point,	bait,	CamType, season, lat lon



library(readr)
library(camtrapR)
library(tidyverse)
path <- "C:/CodigoR/CameraTrapCesar/data/katios/"
cameras <- read_csv(paste(path, "cameras.csv", sep=""))
deployment <- read_csv(paste(path, "deployments.csv", sep=""))
images <- read_csv(paste(path, "images.csv", sep=""))
project <- read_csv(paste(path, "projects.csv", sep=""))

# join_by(project_id, camera_id, camera_name)`
cam_deploy <- cameras |> left_join(deployment) |> 
  dplyr::mutate(year=lubridate::year(start_date)) #|> filter(year== 2023)
cam_deploy_image <- images  |> 
  left_join(cam_deploy) |> 
  mutate(scientificName= paste(genus, species, sep = " ")) |> 
   mutate(deployment_id_cam=paste(deployment_id, camera_id, sep = "-")) #|> 
  # filter(year==2022)

# cam_deploy_image$year <- lubridate::year("timestamp")


# filter and make uniques
CToperation_Saldania <- cam_deploy_image |> 
  # dplyr::rename(Deployment_id="Deployment ID") |> 
  dplyr::mutate(year=lubridate::year(timestamp))|> 
  distinct(deployment_id_cam, longitude, latitude, start_date, end_date, year) #|> 
  # filter(year==2023)

# CToperation_ecu16 <- CToperation_ecu16[-29,]








# Generamos la matríz de operación de las cámaras

camop_Saldania <- cameraOperation(CTtable= CToperation_Saldania, # Tabla de operación
                               stationCol= "deployment_id_cam", # define la estación
                               setupCol= "start_date", #Columna fecha de colocación
                               retrievalCol= "end_date", #Columna fecha de retiro
                               #hasProblems= T, # Hubo fallos de cámaras
                               dateFormat= "%Y-%m-%d") #, # Formato de las fechas
#cameraCol="CT")
# sessionCol= "Year")


## remove problem species
# ind <- which(cam_deploy_image$scientificName=="NA NA")
# ind <- which(is.na(cam_deploy_image$scientificName))
#  pitalito <- pitalito[-ind,]
# ind <- which(is.na(cam_deploy_image$scientificName))
# cam_deploy_image <- cam_deploy_image[-ind,]

DetHist_list <- lapply(unique(cam_deploy_image$scientificName), FUN = function(x) {
  detectionHistory(
    recordTable         = cam_deploy_image, # Tabla de registros
    camOp                = camop_Saldania, # Matriz de operación de cámaras
    stationCol           = "deployment_id_cam",
    speciesCol           = "scientificName",
    recordDateTimeCol    = "timestamp",
    recordDateTimeFormat  = "%Y-%m-%d",
    species              = x,     # la función reemplaza x por cada una de las especies
    occasionLength       = 24, # Colapso de las historias a 10 días
    day1                 = "station", #inicie en la fecha de cada survey
    datesAsOccasionNames = FALSE,
    includeEffort        = TRUE,
    scaleEffort          = FALSE,
    #unmarkedMultFrameInput=TRUE
    timeZone             = "America/Bogota" 
  )
}
)

# names
names(DetHist_list) <- unique(cam_deploy_image$scientificName)
# Finalmente creamos una lista nueva donde estén solo las historias de detección
ylist <- lapply(DetHist_list, FUN = function(x) x$detection_history)
effortlist <- lapply(DetHist_list, FUN = function(x) x$effort)

### Danta, Puma, Oso
which(unique(cam_deploy_image$scientificName)=="Tapirus pinchaque")
which(unique(cam_deploy_image$scientificName)=="Puma concolor")
which(unique(cam_deploy_image$scientificName)=="Tremarctos ornatus")

###########################################
danta <- as.data.frame(ylist[[4]])
danta$deployment_id_cam <- rownames(as.data.frame(ylist[[4]]))
danta_effort <- as.data.frame(effortlist[[4]]) 
colnames(danta_effort) <- (c("e1","e2","e3","e4","e5"))

danta <- danta |> left_join(CToperation_Saldania) |>  
  select(!c(start_date, end_date, year)) |> 
  cbind(danta_effort) |> write.csv("C:/Users/silvi/Documents/GitHub/cameratrap/data/danta_Saldania.csv")

######################################
puma <- as.data.frame(ylist[[2]])
puma$deployment_id_cam <- rownames(as.data.frame(ylist[[2]]))
puma_effort <- as.data.frame(effortlist[[2]]) 
colnames(puma_effort) <- (c("e1","e2","e3","e4", "e5"))

puma <- puma |> left_join(CToperation_Saldania) |>  
  select(!c(start_date, end_date, year)) |> 
  cbind(puma_effort) |> write.csv("C:/Users/silvi/Documents/GitHub/cameratrap/data/puma_Saldania.csv")
############################################

oso <- as.data.frame(ylist[[3]])
oso$deployment_id_cam <- rownames(as.data.frame(ylist[[3]]))
oso_effort <- as.data.frame(effortlist[[3]]) 
colnames(oso_effort) <- (c("e1","e2","e3","e4", "e5"))

oso <- oso |> left_join(CToperation_Saldania) |>  
  select(!c(start_date, end_date, year)) |> 
  cbind(oso_effort) |> write.csv("C:/Users/silvi/Documents/GitHub/cameratrap/data/oso_Saldania.csv")



