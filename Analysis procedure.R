# 16-05-2024
# Anastasia Makhnykina

# In this file you can read 12 steps of our analysis from raw data
# to graphs we use in the paper:

# Step 1 - Open file "Read_eddy.R" +
# Step 2 - Open file "Pulse and NP eddy.R" +
# Step 3 - Open file "Rain and pulses.R" +
# Step 4 - Open file "Seasons and pulses.R" +
# Step 5 - Open file "GPP vs Reco eddy.R" +
# Step 6 - Open file "SWC and ST space.R" +
# + additional Step - Open file "15% Threshold.R" +
# Step 7 - Open files "Eddy model 2.R" +  "Eddy model 3.R"

# Uncertainty found

# Step 8 - Open file "Read_chamber.R"
# Step 9 - Open file "Pulse and NP chamber.R" 
# Step 10 - Open file "GPP vs Rsoil chamber.R" 
# Step 11 - Open file "SWC ST space chamber.R"
# + additional Step - Open file "15% for Rsoil.R"
# Step 12 - Open file "Chamber model.R" 


 
#################### That's all! Mission completed! ######################

# I found one big uncertainty between our data frames for Pulse and non-Pulse time
# In your case the size of Pulse df = 1116 obs + 25 variables, Non-pulse df = 1806 obs + 25 variables
# In my case the size of Pulse df = 622 obs + 29 variables, Non-pulse df = 2300 obs + 29 variables

# Means we used different procedure to mark Pulse-time.

# I use the same Pulse-duration for all rain-sizes - 5 days for every pulse. 
# However, you divided the all pulses to 3 groups:
# Small pulses - 5-10 mm per day - duration 8 days
# Medium pulses - 10-20 mm per day - duration 14 days
# Large pulses - >20 mm per day - duration 20 days

# That's the reason why we have different results in some graphs

# To conclude - We should choose one procedure to classify the pulses. 
# After modelling part - you division shows better results and probably we should use this procedure
# I will change the other files and figures







