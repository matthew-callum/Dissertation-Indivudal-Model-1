#First we create the arrays used to store the Water Vole and American Mink data
#Initially we have the Water Vole Data Array
WVole            <- array(data = 0, dim = c(30,9));
colnames(WVole)  <- c("Maximum Dispersal Distance (Cells)", "x_loc", "y_loc", "Minimum Home Range Requirement (Cells)", "Fecundity", "Litters", "Mortality", "Repr", "Death");
rownames(WVole)  <- c("WVole_1", "WVole_2", "WVole_3", "WVole_4", "WVole_5", "WVole_6", "WVole_7", "WVole_8",  "WVole_9", "WVole_10", "WVole_11", "WVole_12", "WVole_13", "WVole_14", "WVole_15",
                      "WVole_16", "WVole_17", "WVole_18", "WVole_19", "WVole_20", "WVole_21", "WVole_22", "WVole_23", "WVole_24", "WVole_25", "WVole_26", "WVole_27", "WVole_28", "WVole_29", "WVole_30");
WVole[,1]        <- sample(x = 2:10,  size = dim(WVole)[1], replace = TRUE);
WVole[,2]        <- sample(x = 1:150, size = dim(WVole)[1], replace = TRUE);
WVole[,3]        <- sample(x = 1:100, size = dim(WVole)[1], replace = TRUE);
WVole[,4]        <- sample(x = 1:2,  size = dim(WVole)[1], replace = TRUE); #This is the Minimum Home Range Requirement and I'm not sure this is right
WVole[,5]        <- sample(X = 2:6,   size = dim(WVole)[1], replace = TRUE); 
WVole[,6]        <- sample(X = 1:5,   size = dim(WVole)[1], replace = TRUE);
WVole[,7]        <- sample(x = 40:80,   size = dim(WVole)[1], replace = TRUE);

#Next we set up the American Mink array and data
AMink            <- array(data = 0, dim = c(5,5));
colnames(AMink)  <- c("x_loc", "y_loc", "rcol", "dcol");
rownames(AMink)  <- c("pred_1", "pred_2", "pred_3", "pred_4", "pred_5");
AMink[,1]        <- sample(x = 1:100, size = dim(pred)[1], replace = TRUE);
AMink[,2]        <- sample(x = 1:150, size = dim(pred)[1], replace = TRUE);



#Next we need to create the function used for movement, this variant makes the
#individuals rebound off of the edges of the area

movement <- function( xmax = 100, ymax = 150, WVole, xloc = 2, yloc = 3, Max_Dispersal = 1) {
  total_WVole  <- dim(WVole)[1]; #Get the number of individuals in WVole
  move_dists   <- Max_Dispersal/2; #Define the possible distances to move
  x_move       <- sample(x= move_dists, size = total_WVole, replace = TRUE);
  y_move       <- sample(x= move_dists, size = total_WVole, replace = TRUE);
  inds[, xloc] <- inds[, xloc] + x_move;
  inds[, yloc] <- inds[, yloc] + y_move;
  #NEW CODE FOR REFLECTING BOUNDARY IS ADDED BELOW
  for(i in 1:total_inds){                         #For each individual (i) in the array
    if(inds[i, xloc] > xmax){                     #If it moved passed the maximum xloc
      inds[i, xloc] <- xmax - 1;                  #Then move it back toward the centre
    }
    if(inds [i, xloc] < 1){                       #If it moved below 1 on xloc
      inds[i, xloc] <- 2;                         #Move it toward the centre (2)
    }
    if(inds[i, yloc] > ymax){                     #If it move passed the maximum yloc
      inds[i, yloc] <- ymax -1;                   #Then move it back toward the centre
    }
    if(inds[i, yloc] <1){                         #If it moved below 1 on yloc
      inds[i, yloc] <- 2;                         #Then move it toward the centre (2)
    }
  }
}



#Next we add the function for the Birth of new Water Vole individuals for the model
   birth <- function(WVole, Fecundity = 5, Litters = 6, Repr = 8){
    WVole_inds         <- dim(WVole)[1]; #Get the number of individuals in WVole array
    WVole_cols         <- dim(WVole)[2]; #Total WVole columns
    WVole[, Repr]      <- sum(Fecundity * Litters);
    total_off          <- sum(WVole[, Repr]);
     # We now have the total number of new offspring; now add to WVole
    new_WVole            <- array(data = 0, dim = c(total_off, Wvole_cols));
    new_WVole[,1]        <- sample(x = 2:10,  size = dim(WVole)[1], replace = TRUE);
    new_WVole[,2]        <- sample(x = 1:150, size = dim(WVole)[1], replace = TRUE);
    new_WVole[,3]        <- sample(x = 1:100, size = dim(WVole)[1], replace = TRUE);
    new_WVole[,4]        <- sample(x = 1:2,  size = dim(WVole)[1], replace = TRUE); #This is the Minimum Home Range Requirement and I'm not sure this is right
    new_WVole[,5]        <- sample(X = 2:6,   size = dim(WVole)[1], replace = TRUE); 
    new_WVole[,6]        <- sample(X = 1:5,   size = dim(WVole)[1], replace = TRUE);
    new_WVole[,7]        <- sample(x = 40:80,   size = dim(WVole)[1], replace = TRUE);
    # Our new offspring can now be attached in the inds array
    WVole <- rbind(WVole, new_WVole);
    return(WVole);
  }
  
  
  
  #Next we add the Death Function for the Water Voles
  
  death <- function(xlen = 100, ylen = 150, WVole, Death = 9, xcol = 2, ycol = 3){
    for(xdim in 1:xlen){      # For each row `xdim` of the landscape...
      for(ydim in 1:ylen){  # For each col `ydim` of the landscape...
        # Get the total number of individuals on the landscape cell
        on_cell <- sum( WVole[, xcol] == xdim & WVole[, ycol]  == ydim);
        # Only do something if on_cell is more than four
        if(on_cell > 4){
          # Get all of the occupants (by row number) on the cell
          occupants <- which(WVole[, xcol] == xdim & WVole[, ycol] == ydim);
          # Sample all but four random occupants to die
          rand_occ  <- sample(x = occupants, size = on_cell - 4);
          # Then add their death to the last column of WVole
          WVole[rand_occ, dcol] <- 1;
        }
      }
    }
    
    # Also after the density dependent death function we need to run the random "Mortality" percentage to see if the Water Voles just die
    Death_Chance       <-sample(x = 0:100, size = dim(WVole)[1], replace = TRUE);
    if(Mortality < Death_Chance){
      WVole[,dcol] <- 1;
    }
    return(WVole);
  }
  
  
  
  
  # And the final function for the moment is the Predation function for the American Mink
  predation <- function(AMink, xcol = 1, ycol = 2, rcol = 3, dcol = 4){
    predators   <- dim(AMink)[1]; # Predator number
    AMink[, dcol] <- 1;           # Assume dead until proven otherwise
    AMink[, rcol] <- 0;           # Assume not reproducing until proven otherwise
    for(p in 1:predators){       # For each predator (p) in the array
      xloc   <- AMink[p, xcol]; # Get the x and y locations
      yloc   <- AMink[p, ycol];
      N_prey <- sum( WVole[, xcol] == xloc & WVole[, ycol] == yloc);
      # ----- Let's take care of the predator first below
      if(N_prey > 0){
        AMink[p, dcol] <- 0;  # The predator lives
      }
      if(N_prey > 1){
        AMink[p, rcol] <- 1;  # The predator reproduces
      }
      # ----- Now let's take care of the prey
      if(N_prey > 0){ # If there are some prey, find them
        WVole <- which( inds[, xcol] == xloc & inds[, ycol] == yloc);
        if(N_prey > 2){ # But if there are more than 2, just eat 2
          WVole <- sample(x = prey, size = 2, replace = FALSE);
        }
        WVole[WVole, dcol] <- 1; # Record the prey as dead
      }
    } # We now know which Mink died, and which Water Voles died & reproduced
    # ---- Below removes predators that have died
    AMink         <- AMink[AMink[,dcol] == 0,] # Only survivors now left
    # ----- Below adds new predators based on the reproduction above    
    AMink_off     <- sum(AMink[, rcol]);
    new_AMink     <- array(data = 0, dim = c(AMink_off, dim(AMink)[2]));
    AMink[,1]        <- sample(x = 1:100, size = dim(pred)[1], replace = TRUE);
    AMink[,2]        <- sample(x = 1:150, size = dim(pred)[1], replace = TRUE);
    AMink         <- rbind(AMink, new_AMink);
    # ----- Now let's remove the prey that were eaten
    WVole         <- WVole[WVole[,dcol] == 0,]; # Only living prey left
    # Now need to return *both* the predator and prey arrays
    AMink_WVole <- list(AMink = AMink, WVole = WVole);
    return(AMink_WVole);
  }
  
  