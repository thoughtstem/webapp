#lang at-exp racket

;Starts Docker, Launches postgres, Seeds the Database, Drops into a bash console
@system{
  docker exec -it `docker ps -q --filter ancestor=mc-data --format="{{.ID}}"` bash 
}
