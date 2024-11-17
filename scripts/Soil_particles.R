setwd('/home/sagesteppe/Documents/assoRted/Geomorphology-Lecture/scripts')

library(packcircles)
library(tidyverse)
library(soiltexture)
library(patchwork)
library(gt)
source('functions.R')

## Soil is composed of three major particles of different sizes. ##

# The soil triangle should be familiar to all of you. This is the one used
# by the USDA, although there are other formats globally - all are quite similar
# they track how much of three components are in the soil

particle_size <- data.frame(
  particle = c( rep('sand', times = 5), 'silt', 'clay'), 
  name = c('Very Coarse Sand', 'Coarse Sand', 'Medium Sand',
           'Fine Sand', 'Very Fine Sand', 'Silt', 'Clay'),
  size_upper = c(2.0, 1.0, 0.500, 0.2500, 0.1250, 0.002, 0.00001),
  size_lower = c(1.0, 0.5, 0.250, 0.1250, 0.0625, 0.050, 0.00200)
)

particle_size$size_midpoint = (particle_size$size_upper + particle_size$size_lower) /2

# Sandy 

soil_palette <- c('#C5CBD3', '#DDA448', '#685044')
names(soil_palette) <- c('clay', 'sand', 'silt')

############################## CREATE THE SAND PARTICLE SIZE PLOT PANEL ########################
# this fn relies on 'id' each input row becomes repeated with in dat.gg, dat.gg represents the points 
# on the circumference. 
# Sand Particle Size layout plot

sand_palette <- c('#8D7429', '#A18031','#B58C39', '#C99840', '#DDA448' )
names(sand_palette) <-
  c('Very Coarse Sand', 'Coarse Sand', 'Medium Sand', 'Fine Sand', 'Very Fine Sand')

sand_sizes <- filter(particle_size, particle == 'sand')  # reduce to just sand
sand_sizes <- sand_sizes[rep(seq_len(nrow(sand_sizes)), c(5,10,20,40,80)),] # replicate each sand type ntimes
sand_sizes <- sand_sizes %>% 
  group_by(name) %>% 
  rowwise() %>% 
  mutate(size = sample((size_lower*1000):(size_upper*1000), 1) / 1000) # go big to get relevant decimals

sand_list <- split(sand_sizes, f = sand_sizes$name)
sand_plots <- lapply(sand_list, sand_size_plotter) #  create plots

sand_plots <- sand_plots[names(sand_palette)] # arrange plots from coarse to fine

p <- cowplot::plot_grid(
  sand_plots[[1]], sand_plots[[2]], sand_plots[[3]],  
    sand_plots[[4]], sand_plots[[5]], ncol = 5) 

cowplot::save_plot('../images/soil/SandSizes.png', p)

################################################################################
#################   Create example soil textures  ##############################

# sandy loam
# sandy clay loam
# silt loam

png('../images/soil/Soil_Triangle.png', width = 720, height = 720)
par(col.main="white")
TT.plot( class.sys = "USDA.TT", 
         main = 'USDA Soil Triangle', 
         bg = 'grey10', col.axis	= 'grey60', col.main = 'white',
         grid.col = 'grey30', #class.line.col	= 'black',
         class.line.col = '#9AE5E6',
         class.lab.col = 'white',
         class.lab.show = 'full',
         col.lab	= 'grey90')
dev.off()

# all ordered - CLAY, Silt, SAND
name_ord <- c('Clay', 'Silt', 'Sand')
l <- c(20, 35, 45)
scl <- c(30, 10, 60)
sil <- c(15, 65, 20)
sl <- c(15, 15, 70)

names(l) <- name_ord
names(scl) <- name_ord
names(sil) <- name_ord
names(sl) <- name_ord
soils <- list(l, scl, sil, sl)
# soils <- lapply(soils, function(x){ names(x) <- name_ord }) need a '[' somewhere

soil_t <- lapply(soils, texture_plotter, c('l', 'scl', 'sil', 'sl'))

names(soils) <- c('l', 'scl', 'sil', 'sl') 

soils1 <- soils %>% 
  bind_rows(.id = 'soil') %>% 
  pivot_longer(!soil, names_to = 'particle', values_to = 'percent') %>% 
  mutate(particle = str_to_lower(particle))


ggplot(data = soils1, aes(y = percent, x = soil, 
                          fill = factor(particle, c('clay', 'silt', 'sand')))) +
  geom_bar(stat = 'identity') + 
  labs(fill="particle") +
  scale_fill_manual(values = soil_palette) + 
  scale_y_continuous(labels=function(x) paste0(x,"%")) + 
  theme_void() + 
  theme(
    panel.background = element_rect(fill = "black"), 
    plot.background = element_rect(fill = "black"), 
    legend.text = element_text(colour = 'white'), 
    legend.title = element_text(colour = 'white'),
    axis.text.y = element_text(color = 'white')) 

ggsave('../images/soil/Soil_Texture_Components-barplot.png')

p <- cowplot::plot_grid(soil_t[[1]], soil_t[[2]], soil_t[[3]], soil_t[[4]]) + 
  theme(plot.background = element_rect(fill = "black"))
cowplot::save_plot('../images/soil/Soil_Texture_Examples.png', p, base_width = 7,
                   base_height =  7)

textures4triangle <- data.frame(l, scl, sil, sl) |>
  t() |>
  data.frame()

png('../images/soil/Soil_Texture_Components.png')
par(col.main="white")
TT.plot(
  class.sys = "USDA.TT", 
  tri.data = textures4triangle, 
  css.names = c("Clay","Silt","Sand"),
  cex.axis = 1.25,
  css.lab = c("Clay","Silt","Sand"),
  arrows.show = TRUE, pch = 2,  cex = 2, 
  grid.show = T, class.lab.show = 'none', 
  main = 'Locations of Example Textures', 
  bg = 'grey10', col.axis	= 'grey60', col.main = 'white',
  grid.col = 'grey30', 
  class.line.col = '#9AE5E6',
  col.lab	= 'grey90', # class.line.col = 'grey40',
  col= 'red'
  )
dev.off()

rm(l, scl, sil, s)

################################################################################
#################### Alluvial fan cross section ################################

# goal have three components in base map
# a slope which is a cross section of both a alluvial fan and valley / playa
# clay, increase to maximum in playa
# silt, increase to maximum on toe of slope
# sand, decrease to lowest on playa

# the above is map 1, and shows how soil texture changes across slope
# map 2 builds on map 1, and shows an increase in salinity towards the playa
# - use geom_points on colored points to increase the concentration of flecks

# map 3 builds on map 1, and shows a decrease of large rocky fragments towards the playa
# - use geom_size  on points to reduce rocks and rock size

# map 4 combines all components. 

fan_playa <- data.frame(
  variable = rep('slope', times = 20), 
  x = 1:20, 
  y = (seq(20, 1)/10)
)

slope_shape <- data.frame(
  x = 1:30, 
  y = c(9, 8.5, 8.5, 8, 7.5, 7, 6.5, 6.6, 6, 6, 6, 5.75, 5.5, 5, 5, 5, 5, 5, 4.5, 4.25, 4, 4, 4, 3.5, 3.25, 3, 3, 3, 3, 3)
)


boulder <- data.frame(
  boulder = c(
    sample(0:1, size = 75, prob = c(0.5, 0.5), replace = TRUE), 
    sample(0:1, size = 50, prob = c(0.9, 0.1), replace = TRUE),
    sample(0:1, size = 50, prob = c(1, 0.0), replace = TRUE))
) 

cobbles <- data.frame(
  cobbles = c(
    sample(0:1, size = 75, prob = c(0.2, 0.8), replace = TRUE), 
    sample(0:1, size = 50, prob = c(0.5, 0.5), replace = TRUE),
    sample(0:1, size = 50, prob = c(0.9, 0.1), replace = TRUE))
) 

gravel <- data.frame(
  gravel = c(
    sample(0:1, size = 75, prob = c(0.2, 0.8), replace = TRUE), 
    sample(0:1, size = 50, prob = c(0.25, 0.75), replace = TRUE),
    sample(0:1, size = 50, prob = c(0.3, 0.7), replace = TRUE))
) 

gr <- jitter(sample(1:3, size = 175, replace = TRUE))
co <- jitter(sample(6:12, size = 175, replace = TRUE))
bo <- jitter(sample(16:25, size = 175, replace = TRUE))

rock_fragments <- cbind(boulder, cobbles, gravel) %>% 
  mutate(x = sort(sample(1:30, size = 175, replace = T))) %>% 
  pivot_longer(-x) %>% 
  mutate(size = vctrs::vec_interleave(bo, co, gr)) %>% 
  filter(value == 1)

rm(boulder, cobbles, gravel)

soil <- data.frame(
  clay = jitter(seq(from = 20, to = 60, length = 20)),
  silt = jitter(c(seq(from = 25, to = 60, length = 10), seq(from = 55, to = 30, length = 10))),
  sand = jitter(c(seq(from = 60, to = 30, length = 15), seq(from = 25, to = 5, length = 5)))
) %>% 
  rowwise() %>% 
  mutate(
    Rate = 100 / sum(clay, silt, sand), 
    across(clay:sand, ~ .x * Rate)) %>% 
  ungroup() %>% 
  mutate(x = 1:20) %>% 
  select(-Rate) %>% 
  pivot_longer(-x)

ggplot(soil, aes(x=x, y=value, fill=name)) + 
  theme_void() + 
  geom_area(alpha=0.6 , size=1, colour="black") + 
  scale_fill_manual('Component', values = soil_palette) +
  scale_y_continuous(labels = function(x) paste0(x*1, "%")) +
  labs(x = 'Increasing Distance from Bedrock',
       y = 'Percent Component',
       title = 'Soil Texture Along an Alluvial Fan and Playa') + 
  theme(
    legend.position = 'bottom',
    plot.title = element_text(hjust = 0.5),
    text = element_text(color = 'white'), 
 #   axis.title.x=element_blank(),
  #  axis.text.x=element_blank(),
    axis.ticks.x=element_blank(), 
  
    aspect.ratio = 6/9) 

ggsave('../images/soil/Soil_Texture_Fan.png', width = 6, height = 6, units = 'in')

loessMod10 <- loess(y ~ x, data = slope_shape, span = 0.25)
slope_shape$y <- predict(loessMod10)

ggplot(data = slope_shape, aes(x = x, y = y)) + 
  geom_area(fill="#BBAB8B", alpha = 0.4) +
  geom_line(color="#BBAB8B", size = 2) +
  labs(title = 'Cross section of Alluvial Fan into Playa') + 
  ylim(0,15) + 
  theme_void() + 
  theme(
    aspect.ratio = 6/9, 
    text = element_text(color = 'white'))

ggsave('../images/soil/Cross_Section_Fan.png', 
       width = 6, height = 6, units = 'in')

rock_frags <- left_join(rock_fragments, slope_shape, by = 'x') %>% 
  rowwise() %>% 
  mutate(x = jitter(x, amount = 0.5),
         y = jitter(sample(0.01:y, size = 1), amount = 0.5))

ggplot(data = rock_frags, aes(x = x, y = y, size = size, color = name)) + 
  geom_point(fill="#BBAB8B", alpha = 0.4) +
  geom_line(data = slope_shape, color="#BBAB8B", size = 2) +
  labs(title = 'Rock Fragments in an Alluvial Fan', 
       color = 'Fragment') + 
  scale_size_continuous(guide = "none") + 
  ylim(0,15) +
  theme_void() + 
  theme(aspect.ratio = 6/9, 
        text = element_text(color = 'white'),
        legend.position = 'bottom')

ggsave('../images/soil/Rock_Frags_Fan.png', 
       width = 6, height = 6, units = 'in')


########################       Rock Fragments       ############################

rock_sizes <- data.frame(
  name = c('Gravel', 'Cobbles', 'Stones', 'Boulders', 'Channer', 'Flagstone'), 
  size_lower = c(2,  75, 250,  600, 2, 151), 
  size_upper = c(74, 249, 599, 1000, 150, 380), 
  radial_symmetry = c(T, T, T, T, F, F)
)

rock_sizes$center <- (rock_sizes$size_lower  + rock_sizes$size_upper) / 2
rock_sizes$length <- ifelse(rock_sizes$radial_symmetry==TRUE, rock_sizes$center, rock_sizes$center/10)

y <- c('Gravelly', 'Cobbly', 'Stony', 'Bouldery')

rock_fragments <- data.frame(
  modifier = c(y, paste('Very', y), paste('Extremely', y), 
               c('Gravel', 'Cobbles', 'Stones', 'Boulders')),
  TRF_lower = c(rep(15, times = 4), rep(35, times = 4), rep(60, times = 4), rep(90, times = 4)), 
  TRF_upper = c(rep(35, times = 4), rep(60, times = 4), rep(90, times = 4), rep(100, times = 4))
)

## 

soil_palette <- c('#C5CBD3', '#DDA448', '#685044')
names(soil_palette) <- c('clay', 'sand', 'silt')

##

rf_tab <- c(y, paste('Very', y), paste('Extremely', y), c('Gravel', 'Cobbles', 'Stones', 'Boulders'))
rf_tab <- data.frame(matrix(rf_tab, nrow = 4, byrow = T))
rf_tab <- cbind(proportion = c('15 - 34', '35 - 59', '60 - 89', '90 - 100'), rf_tab)
colnames(rf_tab) <- c('proportion', 'A', 'B', 'C', 'D')

tab_exs <- data.frame(
  total_rock = c(10, 22, 50, 70),
  gravel = c(7, 15, 10, 10), 
  cobble = c(2, 5, 30, 20), 
  stone =  c(1, 2, 5, 10), 
  boulder = c(0, 0, 5, 30), 
  examples = c(
    'Total Rock < 15%',
    '15% GR >= (1.5*5% CB) + (2*2% ST) + (2.5*0% BY); 15% GR >= 5%',
    '30% CB >= (1.5*5% ST) + (2*5% BY); 30% CB >= 17.5%', 
    '10% ST >= (1.5*30% BY); 10% ST < 45% BY'
  ), 
  modifier = c('none', 'Gravelly', 'Very Cobbly', 'Extremely Bouldery')
)

rock_frag_tab <- gt(data = rf_tab)  %>%
  tab_header(
    title = "Rock Fragment Texture Modifiers"
  ) %>% 
  tab_source_note(
    source_note = 
      "Gravel (GR), Cobble (CB), Stone (ST), Boulder (BD)"
  ) %>% 
  tab_source_note(
    source_note = 
      "Channer is included with gravel (channer + gravel), and flagstone with cobbles (cobble + flagstone)."
  ) %>% 
  tab_source_note(
    source_note = md("Reproduced from National Soil Survey Handbook, Part 618. NRCS.")
  ) %>% 
  cols_align(align = "center") %>%
  cols_label(
    proportion = "Total Rock Fragment (%)",
    A = "GR >= 1.5x (CB + 2x ST + 2.5x BY)",
    B = "CB >= 1.5x (ST + 2x BY)", 
    C = 'ST >= 1.5x BY', 
    D = 'ST < 1.5x BY'
  )

gtsave(rock_frag_tab, '../images/soil/RockFragmentTextureModifiers.png')

rock_frag_exs <- gt(data = tab_exs) %>% 
  tab_source_note(
    source_note = 
      "The 'Example' refers to the final column in the table working from left to right."
  ) %>% 
  tab_header(
    title = "Examples of Texture Modifiers"
  ) %>% 
  cols_align(align = "center") %>%
  cols_label(
    total_rock = "Total Rock Fragment (%)",
    gravel = "Gravel (%)",
    cobble = "Cobble (%)", 
    stone = 'Stone (%)', 
    boulder = 'Boulder (%)', 
    modifier = 'textural modifier', 
    examples = 'Example'
  ) %>% 
  data_color(
    method = "numeric",
    palette = c("#91F9E5", "#5FDD9D")
  )

gtsave(rock_frag_exs, '../images/soil/RockFragmentTextureModifiers-Examples.png')

rm(rock_fragments, rf_tab, y, tab_exs, rock_frag_exs, rock_frag_tab)

########################   % Cover ############################################

cover <- lapply(c(10, 20, 30, 40, 50, 60, 70, 80, 90), cover_plotter)

percent_cover <- cover[[1]] + cover[[2]] + cover[[3]] + cover[[4]] + 
  cover[[5]] + cover[[6]] + cover[[7]] + cover[[8]] + cover[[9]] +
  patchwork::plot_annotation(
    subtitle = 'Percent Cover',
    theme = theme(plot.title = element_text(hjust = 0.5))
  )

ggsave('../images/soil/RockFragment_Covers.png', percent_cover)

rm(cover, percent_cover, cover_plotter)
