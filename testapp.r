

# Loading package to deploy app
library(rsconnect)

# Use credentials for deployment, run to authenticate (you should only need to do this once)
rsconnect::setAccountInfo(name='cd353g-dhruval-patel', # Add shinyapps username
                          token='22ED4D509C316C1C3A3D847F464C5809', # Add shinyapps token
                          secret='5XFhyGrfHYNj6zlwezN4A/SITfler+SpxCY1KUar') # Add shinyapps secret
# Warning: Do not publish these credentials publicly (e.g. on Github)

# Run to deploy app to the web
rsconnect::deployApp(appDir = "Socapp", # Replace with path to app folder
                     appName = "test") # Replace with app name

  