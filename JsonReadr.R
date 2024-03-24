JsonReadr <- function(){
  #set working directory
  setwd("Jsons")
  
  #get the first item in the folder and read it in
  files <- list.files()
  if (length(files) == 0){
    stop("No Files in Jsons folder")
  }
  genshin_data <- fromJSON(file = files[1])
  
  #set up lists
  weapon_data <- genshin_data$weapons
  character_data <- genshin_data$characters
  artifact_data <- genshin_data$artifacts
  weapon_list <- rep(list(0),length(weapon_data))
  character_list <- rep(list(0),length(character_data))
  artifact_list <- rep(list(0),length(artifact_data))
  
  #Assign everything to the character class
  index <- 1
  for(i in character_data){
    chara <- make_chara_raw(i)
    character_list[[index]] <- chara
    index <- index + 1
  }
  
  #assign everything to the weapon class
  index <- 1
  for(i in weapon_data){
    weapon <- make_weapon_raw(i)
    weapon_list[[index]] <- weapon
    index <- index + 1
  }
  
  #assign everything to the artifact class
  index <- 1
  for(i in artifact_data){
    artifact <- make_artifact(i)
    artifact_list[[index]] <- artifact
    index <- index + 1
  }
}