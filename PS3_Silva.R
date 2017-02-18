###############################################
## PS 5625 - Applied Statistical Programming
## Problem Set 3
## Author: Patrick Cunha Silva
## Tasks: 1 - XXXXXXXXXXXX
##        2 - XXXXXXXXXXXX

# Clean session before starting
rm(list = ls())


# Create the S3 class door
door <- function(x){
   if(x!=1 & x!=2 & x!=3){
      stop("Only the values 1, 2 or 3 are allowed")
   }
   structure(x, class = "door")
}

# Example
door(1)

# Create Generic Method PlayGame S3

PlayGame <- function(x){
   UseMethod("PlayGame")
}

# Create Method PlayGame for class "door.
PlayGame.door <- function(x){
   car_door <- sample(seq(1,3), 1)
   if(x==car_door){
      print("Congratulations! You won a new car!")
   }else{
      print("Sorry, you chose the wrong door. Better luck next time!")
   }
}

# Example
PlayGame(door(3))
