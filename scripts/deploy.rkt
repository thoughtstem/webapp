#lang at-exp racket

@system{
  docker tag mc-data srfoster/mc-data && docker login && docker push srfoster/mc-data
}
