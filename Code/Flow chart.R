################################################################################
############# This script makes the flow chart  #############
################################################################################

#load(file = "DataProcessed/Results.RData")

# create a flow chart to show inclusion/exclusion criteria

library(diagram)
par(mar = c(1,1,1,1))
openplotmat()
# set number of columns for each row
elpos <- coordinates(c (1, 3, 2, 5))

# set connections
fromto <- matrix(ncol = 2, byrow = T, 
                 data = c(1, 2, 1, 3, 1, 4, 2, 5, 5, 6, 5, 
                          7, 5, 8, 5, 9, 5, 10))

# set size factors so they can be adjusted at once
cex_fxr <- 2
rad_x <- 0.1
rad_y <- 0.05

# create arrows
nr     <- nrow(fromto)
arrpos <- matrix(ncol = 2, nrow = nr)
for (i in 1:nr)
  arrpos[i,] <- straightarrow (to = elpos[fromto[i, 2], ],
                               from = elpos[fromto[i, 1], ],
                               lwd = 1, arr.pos = 0.5, arr.length = 0.3)

# create boxes
textrect(elpos[1,], radx = rad_x*3, rady = rad_y, 
         lab = c("NORS outbreaks 1998-2015", 'N = 4059'), 
         cex = cex_fxr)
textrect(elpos[2,], radx = rad_x, rady = rad_y, 
         lab = c("Food", 'N = 3124'), 
         cex = cex_fxr)
textrect(elpos[3,], radx = rad_x, rady = rad_y, 
         lab = c("Animal Contact", 'N = 187'), 
         cex = cex_fxr)
textrect(elpos[4,], radx = rad_x*1.7, rady = rad_y*1.25, 
         lab = c("Excluded:", 
                 paste0('Environmental:N = 15'), 
                 paste0('Person to Person: N = 284'), 
                 paste0('Unknown:N = 449')),
         cex = cex_fxr)
textrect(elpos[6,], radx = rad_x*2, rady = rad_y*2, 
         lab = c("Excluded: N = 1938", 
                 paste0('Missing IFSAC:N = 1261'), 
                 paste0('Mutliple Source:N = 479'), 
                 paste0('Undertermined: N = 145'), 
                 paste0('Unclassifiable:N = 51'),
                 paste0('Non Plant/Animal:N = 12')),
         cex = cex_fxr)
textrect(elpos[7,], radx = rad_x*.9, rady = rad_y, 
         lab = c('Meat-Poultry', 'N = 536'), 
         cex = cex_fxr)

textrect(elpos[8,], radx = rad_x*.9, rady = rad_y, 
         lab = c('Eggs', 'N = 155'), 
         cex = cex_fxr)
textrect(elpos[9,], radx = rad_x*.9, rady = rad_y, 
         lab = c("Produce", "N = 182"), 
         cex = cex_fxr)
textrect(c(.8, .13), radx = rad_x*1.8, rady = rad_y*2.25, 
         lab = c("Rare:", 
                 paste0('Dairy:N = 79'), 
                 paste0('Fish:N = 19'), 
                 paste0('Game: N = 9'), 
                 paste0('Grains and Beans:N = 10'),
                 paste0('Nuts and Seeds:N = 18'), 
                 paste0('Oils and Sugars:N = 1'),
                 paste0('Aquatic Animals:N = 16')),
         cex = cex_fxr)