### TOP ###
# Author: Mark A. Wright
# Please cite this Github repository if used for academic works
# Open to suggestions/improvements!



#### Step 1: Understand the Problem with Sample Data ####
# Read 'Sample_Maya_TR_export.csv' into R
setwd("") # Don't forget to set the directory to wherever you've downloaded the file
TRdata <- read.csv("Sample_Maya_TR_export.csv")

# In this example dataset of 300 animation frames, the 'jAx" function in the Maya XROMM Shelf changes the "XYZ" order of rotations when rotations about the z-axis (.rz) exceed 180 degrees
par(mfrow = c(3, 2))
labels <- c("X-Axis Rotations", "Y-Axis Rotations", "Z-Axis Rotations")
for(i in 1:3){
  hist(TRdata[,i+3], main = "", xlab = labels[i])
  plot(TRdata[,i+3] ~ seq(1,nrow(TRdata)), xlab = "Animation Frame", ylab = labels[i], pch = 16)
}
par(mfrow = c(1, 1))

# This error affects 29 frames out of 300 in this example dataset
paste(round(29/300,1), "% Error", sep = "")

# One way to correct this error is to convert XYZ Euler notation into quaternions, which is independent of axis order, and then convert back into XYZ Euler notation in a specified order. There are other ways to solve this error. Skip ahead to Step 2 for implementation.





#### Custom Functions ####
# The below functions have no dependencies (i.e. use only base R)
deg2rad <- function(deg) {(deg * pi) / (180)} # convert degrees into radians (Euler)
rad2deg <- function(rad) {(rad * 180) / (pi)} # convert radians into degrees (Euler)

eul2qua <- function(x, y, z){ # Convert Euler XYZ notation (radians) into quaternions
  # qw <- cos(x/2) * cos(y/2) * cos(z/2) + sin(x/2) * sin(y/2) * sin(z/2)
  # qx <- cos(x/2) * cos(y/2) * sin(z/2) - sin(x/2) * sin(y/2) * cos(z/2)
  # qy <- cos(x/2) * sin(y/2) * cos(z/2) + sin(x/2) * cos(y/2) * sin(z/2)
  # qz <- sin(x/2) * cos(y/2) * cos(z/2) - cos(x/2) * sin(y/2) * sin(z/2)
  qw <- cos(x/2) * cos(y/2) * cos(z/2) + sin(x/2) * sin(y/2) * sin(z/2)
  qx <- cos(x/2) * cos(y/2) * sin(z/2) - sin(x/2) * sin(y/2) * cos(z/2)
  qy <- cos(x/2) * sin(y/2) * cos(z/2) + sin(x/2) * cos(y/2) * sin(z/2)
  qz <- sin(x/2) * cos(y/2) * cos(z/2) - cos(x/2) * sin(y/2) * sin(z/2)
  output <- c(qw, qx, qy, qz)
  return(output)
}

qua2eul <- function(qw, qx, qy, qz){ # Convert quaternions into Euler XYZ notation (radians)
  t1 <- 2 * (qw * qz + qx * qy)
  t2 <- 1 - 2 * (qy * qy + qz * qz)
  x <- atan2(t1, t2)
  
  t3 <- 2 * (qw * qy - qz * qx)
  if(t3 > 1){
    t3 <- t3 + 1
  } else if(t3 < -1){
    t3 <- t3 - 1
  }
  y <- asin(t3)
  
  t4 <- 2 * (qw * qx + qy * qz)
  t5 <- 1 - 2 * (qx * qx + qy * qy)
  z <- atan2(t4, t5)
  
  output <- c(x, y, z)
  return(output)
}

# Put it all into one function for seamless integration with exported Maya Translation/Rotation (TR) files
MayaAngleCorrections <- function(TRfile.csv){
  rdata <- TRfile.csv
  
  # Convert Euler degree rotations into radians
  temp <- cbind(deg2rad(rdata$JointAxes_data.rx), deg2rad(rdata$JointAxes_data.ry), deg2rad(rdata$JointAxes_data.rz), rep(NA, nrow(rdata)), rep(NA, nrow(rdata)), rep(NA, nrow(rdata)), rep(NA, nrow(rdata)), rep(NA, nrow(rdata)), rep(NA, nrow(rdata)), rep(NA, nrow(rdata)))
  colnames(temp) <- c("rad.x", "rad.y", "rad.z", "q1", "q2", "q3", "q4", "rad.fix.x", "rad.fix.y", "rad.fix.z")
  
  # Convert radians into quaternions
  for(i in 1:nrow(temp)){
    temp[i,4:7] <- eul2qua(temp[i,1], temp[i,2], temp[i,3])
  }
  
  # Convert quaternions back into radians
  for(i in 1:nrow(temp)){
    temp[i,8:10] <- qua2eul(temp[i,4], temp[i,5], temp[i,6], temp[i,7])
  }
  
  # Convert radians back to Euler degrees
  rdata$JointAxes_data.rx <- rad2deg(temp[,8])
  rdata$JointAxes_data.ry <- rad2deg(temp[,9])
  rdata$JointAxes_data.rz <- rad2deg(temp[,10])
  
  remove(temp)
  return(rdata)
}

MakePosesContinuous <- function(TRfile.csv, start.row = NULL){
  rdata <- TRfile.csv
  temp <- rdata[,4:6]
  temp$Dist000 <- abs(rdata[,4]) * abs(rdata[,5]) * abs(rdata[,6])

  # If start.row is NULL, find the row with the closest rx,ry,rz to 0,0,0
  start <- NULL
  if(is.null(start.row)){
    start <- which(temp$Dist000 == min(temp$Dist000))
  } else {
    start <- start.row
  }

  # Moving in both directions from the starting row, "fix" values that are not continuous with the previous point (i.e. greater than 90 difference)
  # Do this for rx
  for(i in (start+1):nrow(temp)){
    if(abs(temp[i,1] - temp[i-1,1]) > 90){
      if(temp[i,1] < 0){
        temp[i,1] <- temp[i,1] + 360 # if negative, add 360
      } else {
        temp[i,1] <- temp[i,1] - 360 # if positive, subtract 360
      }
    }
  }
  for(i in (start-1):1){
    if(abs(temp[i,1] - temp[i+1,1]) > 90){
      if(temp[i,1] < 0){
        temp[i,1] <- temp[i,1] + 360
      } else {
        temp[i,1] <- temp[i,1] - 360
      }
    }
  }
  # Do this for ry
  for(i in (start+1):nrow(temp)){
    if(abs(temp[i,2] - temp[i-1,2]) > 90){
      if(temp[i,2] < 0){
        temp[i,2] <- temp[i,2] + 360
      } else {
        temp[i,2] <- temp[i,2] - 360
      }
    }
  }
  for(i in (start-1):1){
    if(abs(temp[i,2] - temp[i+1,2]) > 90){
      if(temp[i,2] < 0){
        temp[i,2] <- temp[i,2] + 360
      } else {
        temp[i,2] <- temp[i,2] - 360
      }
    }
  }
  # Do this for rz
  for(i in (start+1):nrow(temp)){
    if(abs(temp[i,3] - temp[i-1,3]) > 90){
      if(temp[i,3] < 0){
        temp[i,3] <- temp[i,3] + 360
      } else {
        temp[i,3] <- temp[i,3] - 360
      }
    }
  }
  for(i in (start-1):1){
    if(abs(temp[i,3] - temp[i+1,3]) > 90){
      if(temp[i,3] < 0){
        temp[i,3] <- temp[i,3] + 360
      } else {
        temp[i,3] <- temp[i,3] - 360
      }
    }
  }

  rdata$JointAxes_data.rx <- temp$JointAxes_data.rx
  rdata$JointAxes_data.ry <- temp$JointAxes_data.ry
  rdata$JointAxes_data.rz <- temp$JointAxes_data.rz

  remove(temp)

  return(rdata)
}





#### Step 2: Apply Corrections ####
# If you would like to use this function for your exported Translation/Rotation (TR) files from Maya, simply replace the sample .csv file here with your data file
TRdata <- read.csv("Sample_Maya_TR_export.csv") # NB: This step is repeated from above for clarity; 'TRdata' should already exist in your workspace

# Apply the correction
TRdata_corrected <- MayaAngleCorrections(TRdata)

# Check to confirm it worked
par(mfrow = c(3, 2))
labels <- c("X-Axis Rotations", "Y-Axis Rotations", "Z-Axis Rotations")
for(i in 1:3){
  hist(TRdata_corrected[,i+3], main = "", xlab = labels[i])
  plot(TRdata_corrected[,i+3] ~ seq(1,nrow(TRdata_corrected)), xlab = "Animation Frame", ylab = labels[i], pch = 16)
}
par(mfrow = c(1, 1))



# You can see here that X and Y rotations are fixed and now continuous with the animation frames immediately adjacent on either side
# However, because the pose has exceeded 180 degrees along the Z-axis, the "correct" way to now identify these frames using the same Euler XYZ rotation order lists them as negative values close to -180 (i.e. the "shortest" distance is to rotate negatively in the z direction)
# Depending on your dataset, it may be preferable to keep the values this way, or you may wish to describe them with continuity to the poses immediately adjacent to them (i.e. exceed +180 instead of flipping around the unit circle to be recorded as ~ -180)
# There is another function you can apply to achieve this goal

TRdata_corrected_continuous <- MakePosesContinuous(TRdata_corrected, start.row = 100)
# The 'start.row' parameter should be one frame of your dataset that has the "correct" or desired values for each X, Y, and Z
# If left blank, the function will make a guess, but it is better to put in a known value
# This function will not work as intended if any adjacent poses/frames contain greater than 90 degrees of rotation between them; it assumes that the recorded XRay data is of higher resolution than that as a threshold

# Check to confirmed it worked
par(mfrow = c(3, 2))
labels <- c("X-Axis Rotations", "Y-Axis Rotations", "Z-Axis Rotations")
for(i in 1:3){
  hist(TRdata_corrected_continuous[,i+3], main = "", xlab = labels[i])
  plot(TRdata_corrected_continuous[,i+3] ~ seq(1,nrow(TRdata_corrected_continuous)), xlab = "Animation Frame", ylab = labels[i], pch = 16)
}
par(mfrow = c(1, 1))



# The final step is to write your desired data corrections into a .csv file
# Both options below were left commented by default to ensure this step is performed intentionally

# Write the correct file(s):
# write.csv(TRdata_corrected, "TRdata_corrected.csv", row.names = FALSE)
# write.csv(TRdata_corrected_continuous, "TRdata_corrected_continuous.csv", row.names = FALSE)




#
