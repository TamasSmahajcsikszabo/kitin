## character design

# libraries
library("tidyverse")

# global variables
levels <- tribble(
  ~level, ~bottom, ~top, ~bonus, ~name,
  1, 0, 1000, 1, "novice",
  2, 1001, 2000, 1.1, "apprentice",
  3, 2001, 3000, 1.2, "regular",
  4, 3001, 4000, 1.3, "master",
  5, 4001, 5000, 1.4, "champion"
)

# generic main character class
Character <- setRefClass("Character",
  fields = list(
    name = "character",
    health = "numeric",
    gender = "character",
    race = "character",
    alignment = "character",
    level = "numeric",
    experience = "numeric",
    max_health = "numeric"
  ),
  methods = list(
    heal = function(x) {
      health <<- health + x
      cat(paste(name, " has healed", x, "point of health. \n", "Now total health is", health, "\n"))
    },
    damage = function(x) {
      health <<- health - x
      cat(paste(name, " has suffered", x, "point of damage. \n", "Now total health is", health, "\n"))
      if (health <= 0) {
        cat(name, " died.", "\n")
      }
    },
    gain_experience = function(x) {
      experience <<- experience + x
      cat(paste(name, " has gained", x, "point of experience. \n", "Now total experience is", experience, "\n", "Actual level is", levels[level, ]$level, "\n", name, "is at", levels[level, ]$name, "level!", "\n"))
      actual_top <- levels[level, ]$top
      if (experience > actual_top) {
        cat(paste0("Due to bravery and unreleting fight, ", name, " has gained a level!", "\n"))
        level <<- level + 1
        cat(paste0("New level is ", level, "\n"))
        health <<- max_health
        cat(paste0(name, " has healed his full health of ", max_health, "\n"))
      }
    }
  )
)
james <- Character$new()
james$name <- "James"
james$health <- 100
james$max_health <- 150
james$alignment <- "good"
james$heal(10)
james$damage(110)
james$experience <- 1000
james$level <- 1
james$gain_experience(1000)

# TODO:
# create parent class such as being
# create child class for NPCs
# create functions for positioning, map generation, gpplot2
# items
# function for dialogue
# function for fight / fight system
# inventory
# random party creation
