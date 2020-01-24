#lang at-exp racket

(displayln "Stopping any running mc-data containers") 
@system{
 docker rm $(docker stop $(docker ps -a -q --filter ancestor=mc-data --format="{{.ID}}"))
}

(displayln "Building mc-data image") 
@system{
  docker build -t mc-data .  
}

