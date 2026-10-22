create_map <- function(bdl) {
  # Montreal boundary files -------------------------------------------------
  bb <- c(xmin = -74.0788, ymin = 45.3414, xmax = -73.3894, ymax = 45.7224)

  ## Montreal
  # Download island boundary in bbox
  mtl <- opq(bb, timeout = 100) %>%
    add_osm_feature(key = 'place', value = 'island') %>%
    osmdata_sf()
  # Grab multipolygons (large islands)
  multipolys <- mtl$osm_multipolygons
  # Grab polygons (small islands)
  polys <- mtl$osm_polygons
  polys <- st_cast(polys, "MULTIPOLYGON")
  # Combine geometries and cast as sf
  allpolys <- st_as_sf(st_union(polys, multipolys)) %>% st_union()

  ## Water
  water <- opq(bb) %>%
    add_osm_feature(key = 'natural', value = 'water') %>%
    osmdata_sf()
  mpols <- water$osm_multipolygons
  mpols <- st_cast(mpols, "MULTIPOLYGON")
  mpols <- st_as_sf(st_make_valid(mpols)) %>% st_union()

  # Montreal plot -----------------------------------------------------------

  bbi <- st_bbox(st_buffer(allpolys, 2.5))

  # transform study parks into points for easier visualization
  df <- data.frame(
    parks = c(
      "Morgan Arboretum",
      "Stoneycroft",
      "Technoparc",
      "Bois-de-Liesse"
    ),
    latitude = c(45.43058, 45.430032, 45.478082, 45.501874),
    longitude = c(-73.953867, -73.942804, -73.754177, -73.765267)
  )
  df_spatial <- st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326)
  df_spatial$formality <- c('formal', 'informal', 'informal', 'formal')

  mtl <- ggplot() +
    geom_sf(fill = '#ceb99b', data = allpolys) +
    geom_sf(fill = '#99acc3', data = mpols) +
    geom_sf(aes(colour = formality), size = 2, data = df_spatial) +
    coord_sf(
      xlim = c(bbi['xmin'], bbi['xmax']),
      ylim = c(bbi['ymin'], bbi['ymax'])
    ) +
    scale_colour_manual(values = c("#669d62", "#1e3d14")) +
    labs(colour = "") +
    theme(
      panel.border = element_rect(linewidth = 1, fill = NA),
      panel.background = element_rect(fill = '#ddc48d'),
      panel.grid = element_blank(),
      axis.text = element_text(size = 11, color = 'black'),
      axis.title = element_blank(),
      plot.background = element_rect(fill = NA, colour = NA),
      legend.position = 'top'
    ) +
    annotation_scale(location = 'br', style = 'ticks')

  # Single park plot --------------------------------------------------------

  # download park boundaries from OSM
  bdl_osm <- opq('montreal canada') %>%
    add_osm_feature(key = 'boundary', value = 'protected_area') %>%
    osmdata_sf()

  bdl_osm <- bdl_osm$osm_polygons %>%
    filter(name == "Parc-nature du Bois-de-Liesse")

  # bbox
  bba <- st_bbox(bdl_osm)

  # trails
  trails <- bdl %>% st_zm() %>% st_cast('LINESTRING')
  l_trails <- trails %>% slice(3:4) %>% mutate(type = 'large trail')
  s_trails <- trails %>% slice(1:2) %>% mutate(type = 'small trail')
  quadrats <- trails %>% slice(5:20) %>% mutate(type = 'transects')
  trails <- rbind(l_trails, s_trails, quadrats)

  boisdeliesse <- ggplot() +
    geom_sf(data = bdl_osm, fill = '#c2d6a4') +
    geom_sf(data = trails, aes(colour = type)) +
    geom_sf(fill = '#99acc3', data = mpols) +
    coord_sf(
      xlim = c(bba['xmin'], bba['xmax']),
      ylim = c(bba['ymin'], bba['ymax'])
    ) +
    scale_colour_met_d(name = 'Troy') +
    labs(colour = "") +
    theme(
      panel.border = element_rect(linewidth = 1, fill = NA),
      panel.background = element_rect(fill = '#ceb99b'),
      panel.grid = element_blank(),
      axis.text = element_text(size = 11, color = 'black'),
      axis.title = element_blank(),
      plot.background = element_rect(fill = NA, colour = NA),
      legend.position = 'top'
    ) +
    annotation_scale(location = 'br', style = 'ticks')

  # All ---------------------------------------------------------------------

  arrowA <- data.frame(x1 = 10.2, x2 = 22, y1 = 8.35, y2 = 8.5)

  full <- ggplot() +
    coord_equal(xlim = c(0, 40), ylim = c(0, 20), expand = FALSE) +
    annotation_custom(
      ggplotGrob(mtl),
      xmin = 0,
      xmax = 20,
      ymin = 0,
      ymax = 20
    ) +
    annotation_custom(
      ggplotGrob(boisdeliesse),
      xmin = 20,
      xmax = 40,
      ymin = 3,
      ymax = 18
    ) +
    geom_segment(
      aes(x = x1, y = y1, xend = x2, yend = y2),
      data = arrowA,
      linewidth = 1,
      arrow = arrow(),
      lineend = "round"
    ) +

    theme_void()

  ggsave(
    'graphics/map.png',
    full,
    width = 12,
    height = 14,
    units = 'in',
    dpi = 450
  )
}
