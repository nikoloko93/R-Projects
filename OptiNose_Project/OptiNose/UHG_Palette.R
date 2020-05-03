# library(devtools)

# devtools::install_github('cttobin/ggthemr')

# ============================================================================

# load above package if first time user of ggthemr
library(ggthemr)



#========================================== CREATE THE PALETTE ==========================================#



## UHG colors -- the first color is automatically assigned as the color for the outlines

uhg_colors <- c('#555555', # gray

                '#2d5fa7', # blue

                '#f79837', # orange

                '#72c060', # green

                '#ea4b4d', # red

                '#028ea7', # teal

                '#675da8') # purple



## Define colours for your figures with define_palette

uhg_palette <- define_palette(

  swatch = uhg_colors, # colours for plotting points and bars

  gradient = c(lower = uhg_colors[2], upper = uhg_colors[5]) #upper and lower colours for continuous colours

)



## Set the theme for your figures.

ggthemr(uhg_palette)



## !!! IMPORTANT !!!

## Note that this is a "fire-and-forget" function, meaning that once you've run it, all your figures will now use the theme.

## To remove the theme, use ggthemr_reset()





