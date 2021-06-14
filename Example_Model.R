#Time to start the creation of an individual based model for the Water Vole
#Before I construct the model using the actual vole data I am going to create a base model

#First things first I need to create a data array for the individual for use in the model
# Within this code the "5" refers to the number of individuals within the array
# The "3" refers to the number of characteristics we want to associate with each individual
inds <- array(data = 0, dim= c(5,3));
#This next set of code simply renames the columns for each characteristic to the correct name
colnames(inds) <- c("characteristic_1", "characteristic_2", "characteristic_3");
#This code renames the individuals to an appropriate naming scheme, may need to find way to do this automatically with more individuals
rownames(inds) <-c("ind_1", "ind_2", "ind_3", "ind_4", "ind_5");
#Now check the created array before proceeding
print(inds);

#The next set of code renames the characteristics to a more appropriate set of names for the example model
colnames(inds) <- c("body_mass", "x_loc", "y_loc");
#The next section of code is used to give each individual a random body mass distributed around a value of 23, with a Standard Deviation of 3
#This is to simulate the variance in body mass between individuals
inds[,1] <- rnorm(n = dim(inds)[1], mean = 23, sd= 3);

#dm(inds) as a command now returns the dimensions of the array created where element 1 (dim(inds)[1]) is the number of rows
# and element 2 is the number of columns (in the cast of this example it is 3) like co-ordinates
#Now we can allocate each of the individuals a randomly allocated x and y location in characteristics 2 and 3
#By sampling from a vector of numbers 1-8 with replacement (replacement allows individuals to occupy the same location)
inds[,2] <- sample(x = 1:8, size = dim(inds)[1], replace = TRUE);
inds[,3] <- sample(x = 1:8, size = dim(inds)[1], replace = TRUE);

#We can even plot the x and y locations to see how the individuals are spatially distributed
plot(x = inds[,2], y = inds[,3], pch= 20, cex= 4, xlim= c(1,8), ylim = c(1,8), xlab = "x location",
     mar = c(5, 5, 1, 1,), ylab = "y location", cex.lab = 1.5, cex.axis = 1.5);



#After allocating the required characteristics to the individuals we can now begin to mode the movement of the individuals
#We can create a system where the individuals can increase or decrease the x location by 1 to simulate movement
#To allow the model to let the individuals move into any of the surrounding cells we sample two random values from -1, 0, and 1 with replacement
#and equal probability. One for the x and one for the y.

#This section of code defines the maximum amount the individual can move in one time integer on each axis
#The second section of the code moves each individual by applying the x and y movement to each individual on their x and y location characteristic
x_move <- sample(x = c(-1, 0, 1), size = dim(inds)[1], replace = TRUE);
y_move <- sample(x = c(-1, 0, 1), size = dim(inds)[1], replace = TRUE);
inds[,2] <- inds[,2] + x_move
inds[,3] <- inds[,3]+ y_move

#Now we are going to take this movement idea and define it as a function in order to make the code
#later on in the model a lot cleaner and easier to manipulate
movement <- function(inds, xloc = 2, yloc = 3){
  total_inds <- dim(inds)[1]; #Get the number of individuals in inds
  move_dists <- c(-1, 0, 1); #Define the possible distances to move
  x_move <- sample(x = move_dists, size = total_inds, replace = TRUE);
  y_move <- sample(x = move_dists, size = total_inds, replace = TRUE);
  inds[, xloc] + x_move;
  inds[, yloc] + y_move;
  return(inds);
}

#All of that coding means that the following code moves all of the individuals using only one line of code
inds <- movement(inds)



#Now that we have that function it is time to create a loop function in order to move each individual the required amount of steps
#for the model to work as intended

#The first step in this is to define the total number of times we want the model to simulate
time_steps <- 13

#The next step in this is to create a loop that will continue the model over the 13 steps defined previously
#ts indicates the "Time Step" and will increase each time until the while command is no longer active
ts <- 0;
while(ts < time_steps) {
  inds <- movement(inds);
  ts <- ts + 1;
}

#This loop works, but has one fatal flaw, the individuals aren't prevented from moving outside the range
#The possibilities for dealing with this are many. Three that can be utilised are:
#Place them back on the boundary edge of the area (sticky landscape edge)
#Change their direction at the boundary edge (reflecting landscape edge)
#Have them move over to the opposite side of the landscape (Pacman, Mobius strip thing. Torus landscape)

#For now we're going to do a reflecting edge by adding new code to the movement function

movement <- function(inds, xloc = 2, yloc = 3, xmax = 8, ymax = 8) {
  total_inds <- dim(inds)[1]; #Get the number of individuals in inds
  move_dists <- c(-1, 0, 1); #Define the possible distances to move
  x_move <- sample(x= move_dists, size = total_inds, replace = TRUE);
  y_move <- sample(x= move_dists, size = total_inds, replace = TRUE);
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
  # With this code all individuals should stay on the landscape
  return (inds);
}
  
#Unfortunately with this we don't get any of the information on where the individuals started and ended and not any of the middle
#This information isn't exactly necessary, but if we wanted to record this data we could create a new list and store the inds array as a list element in each step

ts <- 0;
time_steps <- 20;
inds_hist <- NULL; #Here's the list of steps
while(ts < time_steps){
  inds <- movement(inds);
  ts   <- ts +1;
  inds_hist[[ts]] <- inds; #Add to the list of steps
}
print(inds);

#With this created list we can now see where individuals have been over the time steps by extracting from inds_hist and storing it in a new table
ind1_locs <- array(data = NA, dim =c(20, 3));
for(i in 1:20){
  ind1_locs[i, 1] <- i                     # save the time step
  ind1_locs[i, 2] <- inds_hist[[i]][1, 2]; # xloc for the time step
  ind1_locs[i, 3] <- inds_hist[[i]][1, 3]; # yloc for the time step
}
colnames(ind1_locs) <- c("time_step", "x_loc", "y_loc");
print(ind1_locs);


#With all of this code we now have a model that can store characteristics regarding each of the individuals
# and simulate the movement of these individuals within a set area that they cannot go outside of


#The next big step for the model is the simulation of the birth of new individuals
# This is done through th euse of a rule to determine when an individual reproduces
# First we assume that all individuals in the model are female, and that the number of birth
# events for an individual Bi is sampled from a Poisson distribution with 0.5 expected offspring per time step

#For this example model we will use an expected 0.5 expected offspring per time step
rpois(n = 5, lambda = 0.5);

#For the model itself we to create a place in the model to store these reproduction values
#A column called "repr" is added to the individuals which represents the number of offspring an individual has produced at a given time step

inds <- array(data = 0, dim = c(5,4));
colnames(inds) <- c("body_mass", "x_loc", "y_loc", "repr");
rownames(inds) <- c("ind_1", "ind_2", "ind_3", "ind_4", "ind_5");
inds[,1]       <- rnorm(n = dim(inds)[1], mean = 23, sd = 3);
inds[,2]       <- sample(x = 1:8, size = dim(inds)[1], replace = TRUE);
inds[,3]       <- sample(x = 1:8, size = dim(inds)[1], replace = TRUE);

#Now we create the birth function to both sample births for each individual and add new offspring at the same time
#Each individual will be first assinged a number of offspring with rpois, then new offspring will be added to the inds array

birth <- function(inds, lambda = 0.5, repr_col = 4){
  total_inds         <- dim(inds)[1]; #Get the number of individuals in inds array
  ind_cols           <- dim(inds)[2]; #Total inds columns
  inds[, repr_col]   <- rpois(n = total_inds, lambda = lambda);
  total_off          <- sum(inds[, repr_col]);
  # We now have the total number of new offspring; now add to inds
  new_inds     <- array(data = 0, dim = c(total_off, ind_cols));
  new_inds[,1] <- rnorm(n = dim(new_inds)[1], mean = 23, sd =3);
  new_inds[,2] <- sample(x = 1:8, size = dim(new_inds)[1], replace = TRUE)
  new_inds[,3] <- sample(x = 1:8, size = dim(new_inds)[1], replace = TRUE)
  # Our new offspring can now be attached in the inds array
  inds <- rbind(inds, new_inds);
  return(inds);
}

#The next section of code reads the inds into the birth function and stored in an overwritten inds with the new individuals in the array
inds <-birth(inds = inds);


#With this code we can now simulate movement and birth of individuals although this will result in unfettered growth
#The code below will result in the unrestricted growth, but shows that the birth code is working
ts         <-0;
time_steps <-10;
inds_hist  <-NULL;
while(ts < time_steps){
  inds            <-movement(inds);
  inds            <-birth(inds);
  ts              <-ts +1;
  inds_hist[[ts]] <- inds;
}

#Although we know that this will result in a ludicrous amount of growth it would help to know how much per time step
#The code underneath will tell us how many individuals are present in the model per time step under the term "Abundance"
ind_abund <- array(data = NA, dim = c(10,2));
for(i in 1:10){
  ind_abund[i, 1] <- i;
  ind_abund[i, 2] <- dim(inds_hist[[i]])[1];
}
colnames(ind_abund) <- c("time_step", "abundance");
print(ind_abund);
#We'll see that this just results in exponential growth, that is not only inaccurate but also biologically unrealistic
#So now we need to start killing some of them off

#Individual death can be simulated similarly to individual birth
#Here death is related to total population abundance which can be done in a variety of ways
#We could have a function that checks population abundance , assigns probability of individual death based on some carrying capacity,
#then realises death by a Bernoulli trial using that assigned probability
#Another, simpler option is to relate mortality to the number of individuals on a landscape cell.

#If we assume that each landscape cell has enough resources to feed one individual for one time step
#Then we might write a "death" function that causes mortality whenever there is more than one individual on the same landscape cell

inds <- array(data = 0, dim = c(5,5));
colnames(inds) <- c("body_mass", "x_loc", "y_loc", "repr", "death");
rownames(inds) <- c("ind_1", "ind_2", "ind_3", "ind_4", "ind_5");
inds[,1]       <- rnorm(n = dim(inds)[1], mean =23, sd = 3);
inds[,2]       <- sample(x = 1:8, size = dim(inds)[1], replace = TRUE);
inds[,3]       <- sample(x = 1:8, size = dim(inds)[1], replace = TRUE);

#Now we have to make a death function to use that will check if there is more than one individual on a cell
#and select one individual to survive, which is what the code below does

death <- function(inds, xlen = 8, ylen = 8, dcol = 5, xcol = 2, ycol = 3){
  for(xdim in 1:xlen){      # For each row `xdim` of the landscape...
    for(ydim in 1:ylen){  # For each col `ydim` of the landscape...
      # Get the total number of individuals on the landscape cell
      on_cell <- sum( inds[, xcol] == xdim & inds[, ycol]  == ydim);
      # Only do something if on_cell is more than one
      if(on_cell > 1){
        # Get all of the occupants (by row number) on the cell
        occupants <- which(inds[, xcol] == xdim & inds[, ycol] == ydim);
        # Sample all but one random occupant to die
        rand_occ  <- sample(x = occupants, size = on_cell - 1);
        # Then add their death to the last column of inds
        inds[rand_occ, dcol] <- 1;
      }
    }
  }
  return(inds);
}

#Now in order to ensure that the individuals that didn't die are retained in the inds array
inds <- inds[inds[, 5] == 0,];
print(inds);

#Now we can include the death function to simulate a population with movement, birth, and death
#The code below works for a total of 13 time steps

#This code initialises the individuals
inds             <- array(data = 0, dim = c(5,5));
colnames(inds)   <- c("body_mass", "x_loc", "y_loc", "repr", "death");
rownames(inds)   <- c("ind_1", "ind_2", "ind_3", "ind_4", "ind_5");
inds[,1]         <- rnorm(n = dim(inds)[1], mean = 23, sd = 3);
inds[,2]         <- sample(x = 1:8, size = dim(inds)[1], replace = TRUE)
inds[,3]         <- sample(x = 1:8, size = dim(inds)[1], replace = TRUE)
#This starts the simulation as before
ts         <- 0;
time_steps <- 20;
inds_hist  <- NULL;
while(ts < time_steps){
  inds            <- movement(inds);
  inds            <- birth(inds);
  inds            <- death(inds);
  inds            <- inds[inds[, 5] == 0,] #This code is from before to retain living individuals
  ts              <- ts + 1;
  inds_hist[[ts]] <- inds;
}

#THIS CODE IS NOW A WORKING INDIVIDUAL-BASED MODEL THAT MODELS POPULATION DISTRIBUTION, ABUNDANCE, BIRTH AND DEATH! :)
#Alterations can be made to the code to specify certain aspects of the model itself such as:
#Making body mass heritable in our model by adding code to the birth function
#Altering how changes in body mass affect who dies by adding code to the death function


#Before making these changes we're going to make one last adjustment to the model
#This change will be the introduction of a new array representing a predator
pred            <- array(data = 0, dim = c(5,5));
colnames(pred)  <- c("body_mass", "x_loc", "y_loc", "repr", "death");
rownames(pred)  <- c("pred_1", "pred_2", "pred_3", "pred_4", "pred_5");
pred[,1]        <- rnorm(n = dim(pred)[1], mean = 23, sd = 3);
pred[,2]        <- sample(x = 1:8, size = dim(pred)[1], replace = TRUE);
pred[,3]        <- sample(x = 1:8, size = dim(pred)[1], replace = TRUE);


#In the modelling of a new predator it is likely that you will want to create unique birth, death, and movement functions
#But for this example we are not going to do any new movement
#The code below however should allow for: Predators searching for prey, killing prey, reproducing, and removing consumed prey
#This complex bit of code is then encapsulated in the "predation" function
predation <- function(pred, inds, xcol = 2, ycol = 3, rcol = 4, dcol = 5){
  predators   <- dim(pred)[1]; # Predator number
  pred[, dcol] <- 1;           # Assume dead until proven otherwise
  pred[, rcol] <- 0;           # Assume not reproducing until proven otherwise
  for(p in 1:predators){       # For each predator (p) in the array
    xloc   <- pred[p, xcol]; # Get the x and y locations
    yloc   <- pred[p, ycol];
    N_prey <- sum( inds[, xcol] == xloc & inds[, ycol] == yloc);
    # ----- Let's take care of the predator first below
    if(N_prey > 0){
      pred[p, dcol] <- 0;  # The predator lives
    }
    if(N_prey > 1){
      pred[p, rcol] <- 1;  # The predator reproduces
    }
    # ----- Now let's take care of the prey
    if(N_prey > 0){ # If there are some prey, find them
      prey <- which( inds[, xcol] == xloc & inds[, ycol] == yloc);
      if(N_prey > 2){ # But if there are more than 2, just eat 2
        prey <- sample(x = prey, size = 2, replace = FALSE);
      }
      inds[prey, dcol] <- 1; # Record the prey as dead
    }
  } # We now know which inds died, and which prey died & reproduced
  # ---- Below removes predators that have died
  pred         <- pred[pred[,dcol] == 0,] # Only survivors now left
  # ----- Below adds new predators based on the reproduction above    
  pred_off     <- sum(pred[, rcol]);
  new_pred     <- array(data = 0, dim = c(pred_off, dim(pred)[2]));
  new_pred[,1] <- rnorm(n = dim(new_pred)[1], mean = 23, sd = 3);
  new_pred[,2] <- sample(x = 1:8, size = dim(new_pred)[1], replace = TRUE);
  new_pred[,3] <- sample(x = 1:8, size = dim(new_pred)[1], replace = TRUE);
  pred         <- rbind(pred, new_pred);
  # ----- Now let's remove the prey that were eaten
  inds         <- inds[inds[,dcol] == 0,]; # Only living prey left
  # Now need to return *both* the predator and prey arrays
  pred_prey <- list(pred = pred, inds = inds);
  return(pred_prey);
}





#WITH ALL OF THAT CODE WE CAN NOW PUT ALL OF IT TOGETHER INTO AN ACTUAL INDIVIDUAL BASED PREDATOR PREY SPATIALLY EXPLICIT MODEL!
#THE CODE BELOW RUNS THAT MODEL

# ----- Initialise individuals (prey)
inds           <- array(data = 0, dim = c(40, 5));
inds[,1]       <- rnorm(n = dim(inds)[1], mean = 23, sd = 3);
inds[,2]       <- sample(x = 1:8, size = dim(inds)[1], replace = TRUE);
inds[,3]       <- sample(x = 1:8, size = dim(inds)[1], replace = TRUE);
# ----- Initialise individuals (predator)
pred           <- array(data = 0, dim = c(20, 5));
pred[,1]       <- rnorm(n = dim(pred)[1], mean = 23, sd = 3);
pred[,2]       <- sample(x = 1:8, size = dim(pred)[1], replace = TRUE);
pred[,3]       <- sample(x = 1:8, size = dim(pred)[1], replace = TRUE);
# ---- Start the simulation as before
ts         <- 0;
time_steps <- 40;
inds_hist  <- NULL;
pred_hist  <- NULL;
while(ts < time_steps){
  pred            <- movement(pred);
  inds            <- movement(inds); # Note: I increased prey birth rate
  inds            <- birth(inds, lambda = 1.5);
  pred_prey_res   <- predation(pred = pred, inds = inds);
  pred            <- pred_prey_res$pred;
  inds            <- pred_prey_res$inds;
  inds            <- death(inds);
  inds            <- inds[inds[, 5] == 0,]; # Retain living
  ts              <- ts + 1; 
  inds_hist[[ts]] <- inds;
  pred_hist[[ts]] <- pred;
}

#HUZZAH!

#With the model displaying these now constructed we might now want to see certain outputs
#Whilst we could see the predators or individuals on specific time steps through code referring to the "_hist" lists we created earlier
#The code below is a modified version of the abundance code we wrote earlier that now includes the predators
ind_abund <- array(data = NA, dim = c(40, 3));
for(i in 1:40){
  ind_abund[i, 1] <- i;                      # Save the time step
  ind_abund[i, 2] <- dim(inds_hist[[i]])[1]; # rows in inds_hist[[i]]
  ind_abund[i, 3] <- dim(pred_hist[[i]])[1]; # rows in pred_hist[[i]]
}
colnames(ind_abund) <- c("time_step", "abundance", "predators");
print(ind_abund)

