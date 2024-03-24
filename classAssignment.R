make_chara_raw <- function(x){
  character_data <- helper_chara(x)
  validate_chara(character_data)
  class(character_data) <- "Chara_raw"
  return(character_data)
}

helper_chara_raw <- function(x){
  # Make a new character list based on attributes
  new_chara <- list(
    name = x$key,
    level = as.integer(x$level),
    con = as.integer(x$constellation),
    asc = as.integer(x$ascension),
    normal = as.integer(x$talent$auto),
    skill = as.integer(x$talent$skill),
    burst = as.integer(x$talent$burst)
  )
  return(new_chara)
}

validate_chara_raw <- function(character_data){
  # Check if the input is a list
  if (!is.list(character_data)) {
    stop("Input must be a list.")
  }
  
  # Check if the name exists and is a character
  if (!("name" %in% names(character_data)) || !is.character(character_data$name)) {
    stop("Name must be provided as a character.")
  }
  
  # Define a list of attributes and their corresponding ranges
  attributes <- c("level", "con", "asc", "normal", "skill", "burst")
  ranges <- list(level = 1:90, con = 0:6, asc = 0:6, normal = 1:10, skill = 1:10, burst = 1:10)
  
  # Check if all attributes are present and are integers within the specified ranges
  for (attr in attributes) {
    if (!(attr %in% names(character_data))) {
      stop(paste("Attribute", attr, "is missing."))
    }
    
    if (!is.integer(character_data[[attr]]) ||
        !all(character_data[[attr]] %in% ranges[[attr]])) {
      stop(paste("Attribute", attr, "must be integers within the specified range."))
    }
  }
}

is.Chara_raw <- function(x) {
  inherits(x, "Chara_raw")
}


make_weapon_raw <- function(x){
  weapon_data <- helper_weapon(x)
  validate_weapon(weapon_data)
  class(weapon_data) <- "Weapon_raw"
  return(weapon_data)
}

helper_weapon_raw <- function(x){
  # Make a new weapon list based on attributes
  new_weapon <- list(
    name = x$key,
    level = as.integer(x$level),
    asc = as.integer(x$ascension),
    ref = as.integer(x$refinement),
    assigned = x$location,
    id = as.integer(x$id)
  )
  return(new_weapon)
}

validate_weapon_raw <- function(weapon_data){
  # Check if the input is a list
  if (!is.list(weapon_data)) {
    stop("Input must be a list.")
  }
  
  # Check if the name exists and is a character
  if (!("name" %in% names(weapon_data)) || !is.character(weapon_data$name)) {
    stop("Name must be provided as a character.")
  }
  
  # Define a list of attributes and their corresponding ranges
  attributes <- c("level", "asc", "ref")
  ranges <- list(level = 1:90, asc = 0:6, ref = 1:5)
  
  # Check if all attributes are present and are integers within the specified ranges
  for (attr in attributes) {
    if (!(attr %in% names(weapon_data))) {
      stop(paste("Attribute", attr, "is missing."))
    }
    
    if (!is.integer(weapon_data[[attr]]) ||
        !all(weapon_data[[attr]] %in% ranges[[attr]])) {
      stop(paste("Attribute", attr, "must be integers within the specified range."))
    }
  }
}

is.Weapon_raw <- function(x) {
  inherits(x, "Weapon_raw")
}


make_artifact <- function(x){
  artifact_data <- helper_artifact(x)
  validate_artifact(artifact_data)
  class(artifact_data) <- "Artifact"
  return(artifact_data)
}

helper_artifact <- function(x){
  # Make a new weapon list based on attributes
  sublist = list()
  for(i in 1:length(x$substats)){
      sublist = c(sublist,list(x$substats[[i]]$key,as.double(x$substats[[i]]$value)))
  }
  new_artifact <- list(
    set = x$setKey,
    type = x$slotKey,
    rarity = as.integer(x$rarity),
    main_stat = x$mainStatKey,
    level = as.integer(x$level),
    assigned = x$location,
    id = as.integer(x$id),
    sub = sublist
  )
  return(new_artifact)
}

validate_artifact <- function(artifact_data){
  # Check if the input is a list
  if (!is.list(artifact_data)) {
    stop("Input must be a list.")
  }
}

is.Artifact <- function(x) {
  inherits(x, "Artifact")
}

