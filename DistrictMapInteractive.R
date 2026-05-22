# ============================================================================
# SETUP: Run this once to prepare your environment
# ============================================================================

# Remove the old renv setup entirely
unlink("renv")  # Delete the renv folder
file.remove("renv.lock")  # Delete the lock file
file.remove(".Rprofile")  # Delete renv's startup file

# Reinitialize renv from scratch
renv::init()  # Creates a fresh renv with current packages

# Set working directory and environment
options(download.file.method = "wininet")
setwd("C:\\Mapping Projects\\District Maps - Individual\\DistMap_28")
HOME = "C:\\Mapping Projects\\District Maps - Individual\\DistMap_28"

# Load necessary libraries
library(readxl)
library(sf)
library(stringr)
library(readr)
library(ggplot2)
library(dplyr)
library(shiny)
library(maps)
library(leaflet)
library(scales)
library(DT)
library(htmlwidgets)
library(htmltools)
library(quarto)
library(knitr)
library(rmarkdown)
library(yaml)
library(rmapshaper)
library(leafgl)
library(geojsonio)
library(RColorBrewer)

# ============================================================================
# CREATE THE QUARTO DOCUMENT WITH ALL CODE EMBEDDED
# ============================================================================

qmd_content <- '---
title: "Assessor District Map: Featuring Blocks and Section Volume"
format: html
---

```{r setup, include=FALSE}
#| echo: false
#| message: false
#| warning: false

# Load necessary libraries
library(readxl)
library(sf)
library(stringr)
library(readr)
library(ggplot2)
library(dplyr)
library(shiny)
library(maps)
library(leaflet)
library(scales)
library(DT)
library(htmlwidgets)
library(htmltools)
library(RColorBrewer)
library(rmapshaper)
library(leafgl)
library(geojsonio)

# Set working directory
setwd("C:\\\\Mapping Projects\\\\District Maps - Individual\\\\DistMap_28")

# File paths
shapefile_path1 <- "C:\\\\Mapping Projects\\\\District Maps - Individual\\\\FY28\\\\GIS\\\\FY28_District_Boundaries.shp"
shapefile_path2 <- "C:\\\\Mapping Projects\\\\District Maps - Individual\\\\FY28\\\\GIS\\\\FY28_Blocks_Dissolve_Clip.shp"
shapefile_path3 <- "C:\\\\Mapping Projects\\\\District Maps - Individual\\\\FY28\\\\GIS\\\\FY28Dists_SV_Filled.shp"

# Import the shapefiles
Dist_shapefile <- sf::st_read(shapefile_path1) %>% 
  st_transform(crs = 4326)
Blocks <- sf::st_read(shapefile_path2) %>% 
  st_transform(crs = 4326)
BSV <- sf::st_read(shapefile_path3) %>% 
  st_transform(crs = 4326)

# Select relevant columns
BSV <- BSV[, c("BSV", "geometry")]
Blocks <- Blocks[, c("BLOCK", "geometry")]

# Create labels
labels <- sprintf("District: %s", Dist_shapefile$FY28_Dist) %>% lapply(htmltools::HTML)
block_labels <- sprintf("Block: %s", Blocks$BLOCK) %>% lapply(htmltools::HTML)
bsv_labels <- sprintf("BSV: %s", BSV$BSV) %>% lapply(htmltools::HTML)

# Create color palette
spectral_11 <- RColorBrewer::brewer.pal(11, "Spectral")

create_cyclic_palette <- function(values, colors) {
  unique_vals <- unique(values)
  color_mapping <- setNames(colors[((seq_along(unique_vals) - 1) %% length(colors)) + 1], unique_vals)
  return(colorFactor(palette = color_mapping, domain = values))
}

pal <- create_cyclic_palette(Dist_shapefile$FY28_Dist, spectral_11)
```

```{r create-map}
#| echo: false

# Build the map
map <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%  # Gray base map
  
  # Districts layer (added first so it is at the bottom)
addPolygons(
  data = Dist_shapefile,
  fillColor = ~pal(FY28_Dist),
  weight = 2,
  color = "white",
  fillOpacity = 0.55,
  opacity = 1,
  dashArray = "3",
  highlightOptions = highlightOptions(
    weight = 5, color = "#666", dashArray = "", fillOpacity = 0.55, bringToFront = FALSE
  ),
  label = labels,
  labelOptions = labelOptions(
    style = list("font-weight" = "normal", padding = "3px 8px"),
    textsize = "15px",
    direction = "auto"
  ),
  group = "Districts"
) %>%
  
  # Blocks (added after districts, so labels appear on top)
  addPolygons(
    data = Blocks,
    fillColor = "white",
    fillOpacity = 0.5,
    color = "black",
    weight = 0.5,
    label = block_labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "2px 6px"),
      textsize = "12px",
      direction = "auto"
    ),
    group = "Blocks"
  ) %>%
  
  # SecVol (added last, so these labels are on top of everything)
  addPolygons(
    data = BSV,
    fillColor = "white",
    fillOpacity = 0.5,
    color = "black",
    weight = 0.5,
    label = bsv_labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "2px 6px"),
      textsize = "12px",
      direction = "auto"
    ),
    group = "SecVol"
  ) %>%
  
  # Layer control
  addLayersControl(
    overlayGroups = c("Districts", "Blocks", "SecVol"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  
  # Hide Blocks and SecVol initially
  hideGroup(c("Blocks", "SecVol"))
```

```{r display-map}
#| echo: false

# Display the map
map
```
'

# Save the Quarto document
writeLines(qmd_content, "index.qmd")

# Render to HTML
quarto::quarto_render(
  input = "index.qmd",
  output_format = "html"
)