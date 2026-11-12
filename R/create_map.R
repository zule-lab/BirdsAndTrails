create_map <- function(bdl) {
  ## Montreal
  # download boundary from Stats Can
  land <- download_shp(
    'https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/files-fichiers/lpr_000b21a_e.zip',
    'input/land.zip'
  ) |>
    filter(PRNAME == "Quebec / Québec") |>
    st_transform(4326) |>
    st_union()

  # Montreal plot -----------------------------------------------------------

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

  bbi <- st_bbox(df_spatial)

  mtl <- ggplot() +
    geom_sf(fill = '#b1af2f', data = land) +
    geom_sf(aes(colour = formality), size = 2, data = df_spatial) +
    coord_sf(
      xlim = c(bbi['xmin'] - 0.1, bbi['xmax'] + 0.3),
      ylim = c(bbi['ymin'] - 0.1, bbi['ymax'] + 0.3)
    ) +
    scale_colour_manual(values = c("#b36a2f", "#3d3558")) +
    labs(colour = "") +
    theme(
      panel.border = element_rect(linewidth = 1, fill = NA),
      panel.background = element_rect(fill = '#bbfae7'),
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      legend.key = element_rect(fill = NA),
      plot.background = element_rect(fill = NA, colour = NA),
      legend.position = 'top'
    ) +
    annotation_scale(location = 'br', style = 'ticks')

  # Single park plot --------------------------------------------------------

  # download park boundaries from OSM
  parks <- download_shp(
    'https://donnees.montreal.ca/dataset/2e9e4d2f-173a-4c3d-a5e3-565d79baa27d/resource/c57baaf4-0fa8-4aa4-9358-61eb7457b650/download/shapefile.zip',
    'input/parks.zip'
  ) %>%
    filter(Nom == "Bois-de-Liesse") %>%
    st_union

  # bbox
  bba <- st_bbox(parks)

  # trails
  bdl <- read_sf('input/transects/BoisdeLiesse.kml')
  trails <- bdl %>% st_zm() %>% st_cast('LINESTRING')
  l_trails <- trails %>% slice(3:4) %>% mutate(type = 'large trail')
  s_trails <- trails %>% slice(1:2) %>% mutate(type = 'small trail')
  quadrats <- trails %>% slice(5:20) %>% mutate(type = 'transects')
  trails <- rbind(l_trails, s_trails, quadrats)

  ggplot() +
    #geom_sf(fill = '#99acc3', data = land) +
    geom_sf(data = parks, fill = '#c2d6a4') +
    geom_sf(data = trails, aes(colour = type)) +
    coord_sf(
      xlim = c(bba['xmin'], bba['xmax']),
      ylim = c(bba['ymin'], bba['ymax'])
    ) +
    scale_colour_met_d(name = 'Troy') +
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

  # All ---------------------------------------------------------------------

  arrowA <- data.frame(x1 = 14.75, x2 = 23, y1 = 6, y2 = 8.75)

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
    'graphics/figure_2.png',
    full,
    width = 12,
    height = 14,
    units = 'in',
    dpi = 450
  )
}
