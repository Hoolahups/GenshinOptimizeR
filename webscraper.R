webscraper <- function(){
url <- "https://genshin-impact.fandom.com/wiki/Character/Level_Scaling"
html <- read_html(url)

parent_node <- html %>% html_nodes("table")

tbody <- parent_node[1] %>% html_nodes("tbody")

rows <- tbody %>% html_nodes("tr")

level_scaling <- list()

# Loop through rows and extract data from child nodes (e.g., cells)
for (i in seq_along(rows)) {
  # Skip the first row
  if (i == 1)
    next
  
  row <- rows[[i]]
  
  # Select cell nodes within the current row
  cells <- row %>% html_nodes("td")
  
  # Extract text from cell nodes
  cell_texts <- cells %>% html_text()
  
  # Remove newline characters
  remove_newline <- function(x) {
    gsub("\n", "", x)
  }
  cell_texts_clean <- remove_newline(cell_texts)
  
  # Set up for the dataframe
  column_name <- as.character(cell_texts_clean[1])
  column_values <- as.numeric(cell_texts_clean[-1])
  
  # Store column values in the list
  level_scaling[[column_name]] <- column_values
}

# Combine the columns into a dataframe
level_scaling <- as.data.frame(level_scaling)
rownames(level_scaling) <- c("HP", "ATK", "DEF")


#level up multipliers
tbody <- parent_node[2] %>% html_nodes("tbody")

rows <- tbody %>% html_nodes("tr")

level_multipliers <- list()
ID <- vector("numeric")
star4 <- vector("numeric")
star5 <- vector("numeric")

# Loop through rows and extract data from child nodes (e.g., cells)
for (i in seq_along(rows)) {
  # Skip the first row
  if (i == 1)
    next
  
  row <- rows[[i]]
  
  # Select cell nodes within the current row
  cells <- row %>% html_nodes("td")
  
  # Extract text from cell nodes
  cell_texts <- cells %>% html_text()
  
  # Remove newline characters
  remove_newline <- function(x) {
    gsub("\n", "", x)
  }
  cell_texts_clean <- remove_newline(cell_texts)
  cell_texts_clean <- as.numeric(cell_texts_clean)
  
  ID <- c(ID, cell_texts_clean[1], cell_texts_clean[4], cell_texts_clean[7])
  star4 <- c(star4, cell_texts_clean[2], cell_texts_clean[5], cell_texts_clean[8])
  star5 <- c(star5, cell_texts_clean[3], cell_texts_clean[6], cell_texts_clean[9])
}

# Combine the columns into a dataframe
level_multipliers<- data.frame("level" = ID, "star4" = star4, "star5" = star5)
level_multipliers <- level_multipliers[order(level_multipliers$level), ]

tbody <- parent_node[4] %>% html_nodes("tbody")

rows <- tbody %>% html_nodes("tr")

level_max <- list()

# Loop through rows and extract data from child nodes (e.g., cells)
for (i in seq_along(rows)) {
  # Skip the first row
  if (i == 1)
    next
  
  row <- rows[[i]]
  
  # Select cell nodes within the current row
  cells <- row %>% html_nodes("td")
  
  # Extract text from cell nodes
  cell_texts <- cells %>% html_text()
  
  # Remove newline characters
  remove_newline <- function(x) {
    gsub("\n", "", x)
  }
  cell_texts_clean <- remove_newline(cell_texts)
  
  # Set up for the dataframe
  column_name <- as.character(cell_texts_clean[1])
  column_values <- as.numeric(cell_texts_clean[-1])
  
  # Store column values in the list
  level_max[[column_name]] <- column_values
}

# Combine the columns into a dataframe
level_max <- as.data.frame(level_max)
rownames(level_max) <- c("HP", "ATK", "DEF")

ascension <- c(0, 38/182, 65/182, 101/182, 128/182, 155/182, 1)
ascension_mult <- c(0,0,1,2,2,3,4)

#Ascension stats
url <- "https://genshin-impact.fandom.com/wiki/Character/Comparison"
html <- read_html(url)

parent_node <- html %>% html_nodes("table")

tbody <- parent_node[1] %>% html_nodes("tbody")

rows <- tbody %>% html_nodes("tr")

ascension_stats <- list()

# Loop through rows and extract data from child nodes (e.g., cells)
for (i in seq_along(rows)) {
  # Skip the first row
  if (i == 1)
    next
  
  row <- rows[[i]]
  
  # Select cell nodes within the current row
  cells <- row %>% html_nodes("td")
  
  # Extract text from cell nodes
  cell_texts <- cells %>% html_text()
  
  # Remove newline characters
  remove_newline <- function(x) {
    gsub("\n", "", x)
  }
  cell_texts_clean <- remove_newline(cell_texts)
  
  # Set up for the dataframe
  column_name <- as.character(cell_texts_clean[2])
  column_values <- as.character(cell_texts_clean[6])
  
  # Store column values in the list
  ascension_stats[[column_name]] <- column_values
  
}
character_stat_list <- list(level_max,level_multipliers,level_scaling, ascension_stats, ascension, ascension_mult)
return(character_stat_list)
}