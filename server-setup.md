---
title: "Shiny Server Ubuntu 18.04 server setup"
author: "Hedley Stirrat"
date: "29/05/2020"
output:
  html_document:
    toc: true
---

# Initial server setup

Based on
https://www.digitalocean.com/community/tutorials/initial-server-setup-with-ubuntu-18-04

Server is Ubuntu 18.04.

## Create non-root user

`adduser hedley`

## Add sudo privileges

`usermod -aG sudo hedley`

## Set up firewall

### Allow SSH connections

`ufw allow OpenSSH`

### Enable firewall

`ufw enable`
`ufw status`

## Enable SSH access for non-root user

### Using SSH key auth

Copy files with ownership and permissions from root to user

`rsync --archive --chown=hedley:hedley ~/.ssh /home/hedley`

Exit root, log in as user.

# Install the things

## Install R

Based off
https://www.digitalocean.com/community/tutorials/how-to-install-r-on-ubuntu-18-04

Add GPG key

```
sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9
```

Add repo

```
sudo add-apt-repository 'deb https://cloud.r-project.org/bin/linux/ubuntu bionic-cran40/'
```

Check for updates in the package index

```
sudo apt update
```

Install r

```
sudo apt install r-base
```

Check it's working:

```
sudo -i R
```

## Install Nginx

https://www.digitalocean.com/community/tutorials/how-to-install-nginx-on-ubuntu-18-04

Install it:

```
sudo apt install nginx
```

Adjust firewall (enable HTTP and HTTPS on 80 and 443, respectively)

```
sudo ufw allow 'Nginx Full'
```

## Install Shiny

```
sudo su - -c "R -e \"install.packages('shiny', repos='http://cran.rstudio.com/')\""
```

## Install Shiny Server

https://rstudio.com/products/shiny/download-server/ubuntu/

Install GDebi which installs local deb packages:

```
sudo apt-get install gdebi-core
```

Download Shiny Server:

```
wget https://download3.rstudio.org/ubuntu-14.04/x86_64/shiny-server-1.5.13.944-amd64.deb
```

Install Shiny Server:

```
sudo gdebi shiny-server-1.5.13.944-amd64.deb
```

Check that Shiny Server is listening on port 3838:

```
sudo netstat -plunt | grep -i shiny
```

Allow traffic through to Shiny Server:

```
sudo ufw allow 3838
```

Verify it's working by going to 157.245.200.6:3838

## Clone the git repo into srv/shiny-server/

https://deanattali.com/2015/05/09/setup-rstudio-shiny-server-digital-ocean/#shiny-git

```
git config --global user.email "hedley.stirrat@gmail.com"
git config --global user.name "heds1"

cd /srv/shiny-server
sudo git init
```

Create shiny-server repo in browser, then

```
git remote add origin https://github.com/heds1/shiny-server.git
git add .
git commit -m 'initial commit'
git push -u origin master
```

TODO set up permissions so I don't have to sudo everything

pull to local, add shiny app, push back to remote, pull down from remote to
server...



## Configure Nginx as reverse proxy

```
sudo nano /etc/nginx/nginx.conf
```

Add this

```
http {
    ...
    # Map proxy settings for RStudio
    map $http_upgrade $connection_upgrade {
        default upgrade;
        '' close;
    }
}
```

## open default server block

```
sudo nano /etc/nginx/sites-enabled/default
```

https://deanattali.com/2015/05/09/setup-rstudio-shiny-server-digital-ocean/#custom-domain
https://www.digitalocean.com/community/tutorials/how-to-set-up-shiny-server-on-ubuntu-16-04

add above server {...}
```
map $http_upgrade $connection_upgrade {
  default upgrade;
  ''      close;
}
```

tell nginx to proxy pass to the shiny server (localhost:3838), i.e., route requests at '/' (root) to the shiny server.
add directly below server_name _;.
```
location / {
  proxy_pass http://127.0.0.1:3838/;
  proxy_http_version 1.1;
  proxy_set_header Upgrade $http_upgrade;
  proxy_set_header Connection $connection_upgrade;
  rewrite ^(/shiny/[^/]+)$ $1/ permanent;
}
```

Restart **service**
sudo service nginx restart

Test, navigate to http://apps.hedleystirrat.co.nz/

## Get a certificate

Add certbot repository:

```
sudo add-apt-repository ppa:certbot/certbot
```

Install certbot's nginx package:

```
sudo apt install python-certbot-nginx
```

Obtain certificate:

```
sudo certbot --nginx -d apps.hedleystirrat.co.nz
```


## Install other packages

### ggplot2

```
sudo su - -c "R -e \"install.packages('ggplot2', repos='http://cran.rstudio.com/')\""
```

### caret and other ML packages

```
sudo su - -c "R -e \"install.packages('caret', repos='http://cran.rstudio.com/')\""

sudo su - -c "R -e \"install.packages('randomForest', repos='http://cran.rstudio.com/')\""

sudo su - -c "R -e \"install.packages('e1071', repos='http://cran.rstudio.com/')\""
```

### 7 June 2020 lubridate, dplyr

```
sudo su - -c "R -e \"install.packages('lubridate', repos='http://cran.rstudio.com/')\""

sudo su - -c "R -e \"install.packages('dplyr', repos='http://cran.rstudio.com/')\""
```

### 2 July, packages for commuter app

```
sudo su - -c "R -e \"install.packages('shinyjs', repos='http://cran.rstudio.com/')\""
```

rgdal requires GDAL and PROJ libraries.

Install libgdal26 (found latest version for this OS by running `apt-cache search libgdal`):

```
sudo apt install libgdal-dev
```

I think that pulls in libproj too... so don't have to explicitly install it.

Some more difficulties were had with installing raster... Tried this...

```
sudo apt install r-api-4.0
```

After much searching,
it looks like my droplet simply didn't have enough RAM (it only has 1 GB) to
install it. Found that here:
https://community.rstudio.com/t/i-cant-seem-to-install-raster-package-on-ubuntu-16-04-server/31981/4

So I switched it off, and resized to 2 GB memory. Turned it back on, and tried again:

```
sudo su - -c "R -e \"install.packages('raster', repos='http://cran.rstudio.com/')\""
```

And it worked! Great. Now try leaflet:

```
sudo su - -c "R -e \"install.packages('leaflet', repos='http://cran.rstudio.com/')\""
```

And do rgdal now, since you forgot about it before :)

```
sudo su - -c "R -e \"install.packages('rgdal', repos='http://cran.rstudio.com/')\""
```

### 14 July 2020 add plotly package

```
sudo su - -c "R -e \"install.packages('plotly', repos='http://cran.rstudio.com/')\""
```

Also rebooted the servers at this point with `systemctl reboot -i`
