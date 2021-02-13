#!/bin/bash
setfacl -m user:1000:r ${HOME}/.Xauthority
docker build -t winology .
exec docker run \
    -it \
    --rm \
    --name winology \
    --net=host \
    -e DISPLAY \
    -v ${HOME}/.Xauthority:/home/user/.Xauthority \
    winology \
    "$@"
