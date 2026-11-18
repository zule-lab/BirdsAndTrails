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

  # download park boundaries from Ville de Montreal
  parks <- download_shp(
    'https://donnees.montreal.ca/dataset/2e9e4d2f-173a-4c3d-a5e3-565d79baa27d/resource/c57baaf4-0fa8-4aa4-9358-61eb7457b650/download/shapefile.zip',
    'input/parks.zip'
  ) %>%
    filter(Nom == "Bois-de-Liesse") %>%
    st_union() %>%
    st_transform(4326)

  # bbox
  bba <- st_bbox(parks)

  mtl <- ggplot() +
    geom_sf(fill = '#cdcb96', colour = NA, data = land) +
    geom_sf(aes(colour = formality), size = 4, data = df_spatial) +
    coord_sf(
      xlim = c(bbi['xmin'] - 0.1, bbi['xmax'] + 0.3),
      ylim = c(bbi['ymin'] - 0.1, bbi['ymax'] + 0.2),
      expand = FALSE
    ) +
    scale_colour_manual(values = c("#b7620f", "#707178")) +
    labs(colour = "") +
    theme_void() +
    theme(
      panel.background = element_rect(fill = '#90a1db'),
      legend.key = element_rect(fill = NA),
      legend.position = c(0.9, 0.1),
      legend.text = element_text(size = 12)
    ) +
    annotation_scale(location = 'br', style = 'ticks')

  # Single park plot --------------------------------------------------------

  # trails
  trails <- bdl %>% st_zm() %>% st_cast('LINESTRING')
  l_trails <- trails %>% slice(3:4) %>% mutate(type = 'large trail')
  s_trails <- trails %>% slice(1:2) %>% mutate(type = 'small trail')
  quadrats <- trails %>% slice(5:20) %>% mutate(type = 'transects')
  trails <- rbind(l_trails, s_trails, quadrats)

  park <- ggplot() +
    geom_sf(fill = '#cdcb96', colour = NA, data = land) +
    geom_sf(data = parks, fill = '#5a5a2e', colour = NA) +
    geom_sf(data = trails, aes(colour = type)) +
    coord_sf(
      xlim = c(bba['xmin'], bba['xmax']),
      ylim = c(bba['ymin'], bba['ymax']),
      expand = FALSE
    ) +
    labs(colour = "") +
    scale_colour_manual(values = c('#ddccf5', '#8493d2', '#b998e5')) +
    theme_void() +
    theme(
      panel.background = element_rect(fill = '#90a1db'),
      legend.key = element_rect(fill = NA),
      legend.position = c(0.8, 0.2),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 2)
    ) +
    annotation_scale(location = 'br', style = 'ticks')

  # All ---------------------------------------------------------------------

  full <- mtl |>
    ggdraw() +
    draw_plot(
      {
        park
      },
      x = 0.03,
      y = 0.55,
      width = 0.35,
      height = 0.35
    )

  ggsave(
    'graphics/map.png',
    full,
    width = 12,
    height = 14,
    units = 'in',
    dpi = 450
  )
}
