# AssistEDA
Package for Automated Exploratory Analysis
Download the package and source the Assist.R file.
Step 1: Go to ShinyWithShiny --> R --> AssistEDA.R
Step 2: After opening AssistEDA.R in R Studio, click on source on save and save the file.
Step 3: As you save a new tab opens in the chrome browser, there you can give the address of the
dataset you want to explore and see the results.
[[Note: If tab doesn't open in the chrome browser check in the AssistEDA.R the line 
runapp() should contain the IP address of the network [host = "198.XXX.X.XXX"] to which your system is connected.]]
 runApp(list(ui = ui, server = server),host="192.168.0.105",port=5013, launch.browser = TRUE)

[[Note: User have to click on respective tabs to see the results]]
