#!/bin/bash

# stop all running containers
docker stop $(docker ps -qa)

# remove all containers
docker rm $(docker ps -qa)
