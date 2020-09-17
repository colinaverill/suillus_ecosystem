rm(list=ls())

#How many kg do we have right now?
lb_to_g  <- 453.592
total_lb <- 430
total_kg <- (total_lb * lb_to_g)/1000

#We desire 375 units @810g / unit.
unit_kg <- 682
needed_kg <- (375*unit_kg)/1000

#1.8kg in 1 quart of sand.
debt_kg <- needed_kg - total_kg

#what if we went 2 parts sand, 1 part soil.
#1 part is 5 quarts.
#1 quart of sand is ~1.8kg.
sand_mass <- 1.8*5
14*sand_mass
