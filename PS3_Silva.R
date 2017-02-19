###############################################
## PS 5625 - Applied Statistical Programming
## Problem Set 3
## Author: Patrick Cunha Silva
## Tasks: 1 - Define a new S3 Class "door".
##        2 - Create a method named "PlayGame" for S3 Class "door".
##        3 - Define a new S4 Class "door".
##        2 - Create a method named "PlayGame" for S4 Class "door".

# Clean session before starting
rm(list = ls())

###################### S3 Version ###############################

# Create the S3 class door
door <- function(x){
   # Check if x is different of 1, 2, and 3.
   if(x %in% !c(1, 2, 3, 1L, 2L, 3L)){
      stop("Only values equal to 1, 2 or 3 are allowed")
   }
   structure(x, class = "door")
}

# Examples
door(1)
door(2L)

# Create Generic Method PlayGame S3
PlayGame <- function(x){
   UseMethod("PlayGame")
}

# Create Method PlayGame for class "door.
PlayGame.door <- function(x){
   # Check if the player chose the right doors.
   if(x==sample(seq(1,3), 1)){
      # Print a congratulation message.
      print("Congratulations! You won a new car!")
   }else{
      # Print a sympathetic message to the loser.
      print("Sorry, you chose the wrong door. Better luck next time!")
   }
}

# Example
PlayGame(door(3L))
PlayGame(door(2))
PlayGame(door(1))

###################### S4 Version ###############################

# Create the S4 version of class "door"
setClass(Class = "Door",
            representation = representation(
            door_number = "numeric"),
            prototype = prototype(
            door_number = c())
         )

# Define the validity function
setValidity("Door", function(object){
   # Test if the object is an integer.
   test1 <- object@door_number %% 1 == 0
   # Test if the object is one of the possible door's numbers.
   test2 <- object@door_number %in% c(1, 2, 3, 1L, 2L, 3L)
   # Return an error message if one of the tests returns FALSE.
   if (!test1 | !test2){return("Only values equal to 1, 2 or 3 are allowed")}
   }
   )

# Define the initialize method.
setMethod("initialize", "Door", function(.Object, ...) {
   value = callNextMethod()
   validObject(value)
   return(value)
   }
   )

# Generate the function S4 Door
setGeneric("Door",
           function(x) {
              standardGeneric("Door")
           } 
           )

setMethod("Door", 
          "numeric",
          Door <- function(x) {
             # Define the slot door_number.
             return(new("Door", door_number = x))
          }
          )

# Examples
Door(1)
Door(3L)
new("Door", door_number = 2)
new("Door", door_number = 1L)
new("Door", door_number = 1)


# Generate the generic PlayGame - S4
setGeneric("PlayGame",
           function(x) {
              standardGeneric("PlayGame")
           } 
           )

# Generate the PlayGame method for "Door" - S4
setMethod("PlayGame", "Door",
          # Check if the player has the correct door.
          function(x){
             if(x@door_number==sample(c(1, 2, 3), 1)){
                # Print a congratulation message.
                print("Congratulations! You won a new car!")
             }else{
                # Print a sympathetic message to the loser.
                print("Sorry, you chose the wrong door. Better luck next time!")
             }
          }
)

# Examples:
PlayGame(Door(3))
PlayGame(Door(3L))
PlayGame(Door(2))
PlayGame(new("Door", door_number = 1))