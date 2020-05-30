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

### Install it

```
sudo apt install nginx
```

### Adjust firewall

enable http and https on 80 and 443

```
sudo ufw allow 'Nginx Full'
```

## Install Shiny

```
sudo su - -c "R -e \"install.packages('shiny', repos='http://cran.rstudio.com/')\""
```

## Install Shiny Server

https://rstudio.com/products/shiny/download-server/ubuntu/

### Install GDebi which installs local deb packages

```
sudo apt-get install gdebi-core
```

### Download Shiny Server

```
wget https://download3.rstudio.org/ubuntu-14.04/x86_64/shiny-server-1.5.13.944-amd64.deb
```

### Install Shiny Server

```
sudo gdebi shiny-server-1.5.13.944-amd64.deb
```

## Check that Shiny Server is listening on port 3838

```
sudo netstat -plunt | grep -i shiny
```

## Allow traffic through to Shiny Server

```
sudo ufw allow 3838
```

## Verify it's working

Go to 157.245.200.6:3838

## Clone the git repo into srv/shiny-server/



```
git config --global user.email "hedley.stirrat@gmail.com"
git config --global user.name "heds1"

cd /srv/shiny-server
sudo git clone https://github.com/heds1/shinyapps.git
```