# Stow Maps
The following code helps deploy shiny-R based docker container on AWS EC-2 instance - ubuntu 18.04. 
This can be done in five steps:
1. Updating system files and install github
2. Cloning stow-maps repository into instance.
3. Downloading and installing Docker
4. Building docker image
5. Running docker container

Lets start
## 1. Updating system files and installing github
Run following commands
```
sudo apt update
sudo apt install git
```
## 2. Cloning stow-maps repository into instance
Run the following code:
```
git clone https://github.com/usamatrq94/stow-maps.git
```
Once done, next its time to update our AWS credentials inside dockerfile. Lets start:
```
cd /stow-maps
vim Dockerfile
```
Press `i` to insert text and enter provided `AWS_ACCESS_KEY_ID`, `AWS_SECRET_ACCESS_KEY` and `AWS_DEFAULT_REGION`. Once done press `esc` key followed by `:wq` to save and exit file.
Lets return to our main directory
```
cd ..
```
## 3. Downloading and installing Docker
Run the following code line by line to download docker from official repository
```
sudo apt-get update
sudo apt-get install apt-transport-https ca-certificates curl software-properties-common
curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo apt-key add –
sudo add-apt-repository "deb [arch=amd64] https://download.docker.com/linux/ubuntu  $(lsb_release -cs)  stable" 
sudo apt-get update
sudo apt-get install docker-ce
```
With this done, we need to manage permissions for our user. This will help avoid typing sudo whenever you run the docker command. We can do that using following code. 
```
sudo usermod -a -G docker ${USER}
```
Don't forget to replace `${USER}` with your username.
## 4. Building docker image
To build image, we need to first change our directory and head on to stow-maps folder.
```
cd stow-maps/
```
Once done, lets build docker image
```
docker build -t "stow-maps"  .
```
## 5. Running docker container
Run docker using:
```
docker run -p 3838:3838 stow-maps
```
The `-p` command is mapping shiny port 3838 to instance port 3838. 
Feel free to map 3838 onto any other port like 80 or 443. 
If the container is not working appropriately, use following code to debug container
```
docker run -it --entrypoint /bin/bash stow-maps -s
```
Once docker is running perfectly, we can now connect to the instance and excess port 3838. For example if your system DNS is <my-ip> excess it through port 3838 like <my-ip>:3838
This code is currently live at: [https://stowmaps-by.thatanalytics.studio/](https://stowmaps-by.thatanalytics.studio/)
