library(sf)
library(leaflet)

# Define file paths
lines_path <- "/Users/vhinymombo/Downloads/hotosm_gab_waterways_lines_shp/hotosm_gab_waterways_lines_shp.shp"

polygons_path <- "/Users/vhinymombo/Downloads/hotosm_gab_waterways_polygons_shp/hotosm_gab_waterways_polygons_shp.shp"
points_path <- "/Users/vhinymombo/Downloads/hotosm_gab_waterways_points_shp/hotosm_gab_waterways_points_shp.shp"


# Load the shapefiles
waterways_lines <- st_read(lines_path)
waterways_polygons <- st_read(polygons_path)
waterways_points <- st_read(points_path)

# Print basic info
print(st_geometry_type(waterways_lines))    # Should be LINESTRING
print(st_geometry_type(waterways_polygons)) # Should be POLYGON or MULTIPOLYGON
print(st_geometry_type(waterways_points))   # Should be POINT



# Create leaflet map
library(leaflet)

leaflet() %>%
  # Add base map layers
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
  addProviderTiles(providers$OpenStreetMap, group = "OSM") %>%
  
  # Add waterways as lines with group
  addPolylines(data = waterways_lines, color = "blue", weight = 2, popup = ~as.character(name), group = "Waterway Lines") %>%
  
  # Add waterways as polygons with group
  addPolygons(data = waterways_polygons, color = "green", fillOpacity = 0.5, popup = ~as.character(name), group = "Waterway Polygons") %>%
  
  # Add waterways as points with group
  addCircleMarkers(data = waterways_points, color = "red", radius = 3, popup = ~as.character(name), group = "Waterway Points") %>%
  
  # Add layer control allowing toggling of both base layers and overlays
  addLayersControl(
    baseGroups = c("Satellite", "OSM"),  # User can switch between these
    overlayGroups = c("Waterway Lines", "Waterway Polygons", "Waterway Points"),  # User can toggle these on/off
    options = layersControlOptions(collapsed = FALSE)  # Keep layer control panel open
  ) %>%
  
  # Hide overlays initially so users can toggle them on
  hideGroup(c("Waterway Lines", "Waterway Polygons", "Waterway Points"))

