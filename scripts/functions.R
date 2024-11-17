sand_size_plotter <- function(x){
  
  packing <- packcircles::circleProgressiveLayout(x$size, sizetype = 'area')
  data <- cbind(x, packing)
  dat.gg <- circleLayoutVertices(packing, npoints = 25)
  
  plot <- ggplot() + 
    geom_polygon(data = dat.gg, aes(x, y, group = id), 
                 fill = sand_palette[names(sand_palette) == x$name[1]], 
                 colour = "black", alpha = 0.8) +
    scale_color_manual(values = sand_palette ) +
    theme_void() + 
    theme(legend.position="none") +
    labs(title = gsub(' Sand', '', data$name[1])) + 
    coord_equal() +
    theme(text = element_text(color = 'white'))
  
  return(plot)
  
}

soil_lkp_tab <- 
  data.frame(
    abbrev = c('S', 'LS', 'SL', 'SCL', 'SC', 'C', 'SiC', 'SiCL', 'SiL', 'Si', 'L', 'CL'), 
    name = c('Sand', 'Loamy Sand', 'Sandy Loam', 'Sandy Clay Loam', 'Sandy Clay', 
             'Clay', 'Silty Clay', 'Silty Clay Loam', 'Silt Loam', 'Silt', 'Loam', 'Clay Loam')
  )

# convert percent volume to proportion of volumes
#input order must be: CLAY, Silt, SAND
particle2size <- function(x){
  
  out <- vector(mode = 'numeric', length = 3)
  out[1] <- (x[1] * 0.01) * 10000
  out[2] <- round(((x[2] * 0.01) * 10000) / 25.87065, 0)
  out[3] <- round(((x[3] * 0.01) * 10000) / 373.1343, 0)
  
  names(out) <- c('Clay', 'Silt', 'Sand')
  return(out)
}

#' function for drawing a circle plot of soil constituents
#' @param x a named vector of soil texture percents which add up to 100
#' @example name_ord <- c('Clay', 'Loam', 'Sand')
#' sl <- c(15, 65, 20)
#' silt_loam <- texture_plotted(sl, 'SL')

texture_plotter <- function(x, texture){
  
  part_prcnt <- particle2size(x)
  part_mapping <- filter(particle_size, name %in% c('Clay', 'Silt', 'Medium Sand'))
  part_mapping <- part_mapping[ reorder(part_mapping$name, part_prcnt), ]
  
  texture <- part_mapping[ rep(seq_len(nrow(part_mapping)), times = part_prcnt), ] 
  texture <- texture %>% 
    dplyr::group_by(name) %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(size = sample((size_lower*1000):(size_upper*1000), 1) / 1000) 
  
  packing <- packcircles::circleProgressiveLayout(texture$size, sizetype = 'area')
  data <- cbind(texture, packing)
  dat.gg <- circleLayoutVertices(packing, npoints = 25)
  dat.gg$particle <- as.factor(rep(texture$particle, each = nrow(dat.gg)/nrow(texture)))
  
  texture_name <- as.character(soil_lkp_tab[soil_lkp_tab$abbrev == texture, 'name'])
  
  soil_plot <- ggplot() + 
    geom_polygon(data = dat.gg, aes(x, y, group = id, fill = particle), colour = "black", alpha = 0.6) +   
    scale_fill_manual(values = soil_palette) +
    theme_void() + 
    labs(
      title = texture_name, 
      subtitle = paste0(names(x), ' = ', as.numeric(x), '%', collapse = "\n")
      ) + 
    coord_equal() + 
    theme(
      legend.position="none",
      plot.background = element_rect(fill="black"),
      plot.title = element_text(color="white") , 
      plot.subtitle = element_text(color="white") ,
      text = element_text(color = 'white')
    ) 
  
  return(soil_plot)
  
}



cover_plotter <- function(cover){
  
  df <- expand.grid(x = 0:49, y = 0:49)
  df <- df[sample(1:nrow(df), size = (2500 * cover) / 100),]
  
  plot <- ggplot(df, aes(x, y)) +
    geom_tile() +
    theme_void() + 
    labs(title = paste0(round( (nrow(df) / 2500) * 100, 1), '%'))
  
  return(plot)
}
