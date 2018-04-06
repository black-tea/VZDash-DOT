library(analogsea)
library(plumber)

install_package_secure <- function(droplet, pkg){
  analogsea::install_r_package(droplet, pkg, repo="https://cran.rstudio.com")
}


# Add ssh key to analogsea
analogsea::key_create(name = "home",
                      public_key = 'ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQCte7KKHNpDpRs3c5CsKXeRf5zcWEcmHbr8GP5H6baZu8AZvKYY0fyldZaypDhwqGfADcpIb0umSpYq0mXqZBL4wx1sIExRQhdo0MPJlypfZrl1SppUJlA0IxmGwUAywl+cDRVB9OMUy6QiUG7O+MpBUA4jfRJUjbeYHVAZ6yWJUJW3O/g+5x/+Atl7DZs+9vVmZYFWhxUubfyjhsmABgIkGAuzrGyi60LrquT1acD6VNJ7eyCytI7pImUhrOsLuve/2x5Zz/GOZcRRY6lPuU8y+oE78m49JmnVhNZM/kIBTmHlhVj/lJeSS5O/AoaFPb2akcquWdyc7Lm0JxloPE6OZeupx4P2l9GXvxMEvLCSzqdwKwi2uYq4HXk4laZcQgV1y37uQbztYATGDfPrq735YsxMdbAuxdZmCWtpc68VjQYnK1ZXDGmr7imxRaBjaGA5K66cg3ZSNog8h6ZdcH+u5ecO0FfaedQXs+jyHqMMyfpF8/Ag+YqCflcPGYNGq2lxawRFwHbm0HTSCzdQ3RSTv72m5obrfnvo6fKqEGxVFBKBaAapiHSRy9bu2xFqMr13s+vZraikEc+0d4tpZ2G73lph/tdOmcZwBCK/gBJnGkwGAGPsOJJS7eSpRDOjb6q3ZoKSY1iYvpkxzbnpVc6MGjGXaowD+Cg4v6ik7YV6cQ== timothy.c.black@gmail.com')

# Set default do options
options(do_size = "512mb")
options(do_region = "nyc1")
options(do_ssh_keys = 'home')

# Create a new droplet (uncomment it when you want to spin up a droplet)
#droplets()

drop <- plumber::do_provision()

#digital ocean personal access token
f9da0d8e5b0232ad179fa5a0dda9a931e1e8dff7e06a5ff647a5c35d651d89d2

# id of droplet on digital ocean
drop <- droplet(68824416)
drop2 <- droplet(88040305)
# Instructions for do_deploy_api
# do_deploy_api(droplet, path, localPath, port, forward = FALSE, preflight)
# https://www.rdocumentation.org/packages/plumber/versions/0.4.4/topics/do_deploy_api

# Mailgun Example
install_package_secure(drop, "htmltools")
plumber::do_deploy_api(drop, "mailgun", "C:/Users/Tim/Documents/GitHub/vzcd-shiny/mailgun_api", 8000)

plumber::do_deploy_api(drop, "mailgun", "mailgun_api", 8000)


