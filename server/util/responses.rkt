#lang racket

(provide response/html
	 response/html/content)

(require 
  web-server/servlet
  json
  (only-in website/bootstrap xml->string content))

(define (response/html html)
  (response/full
    200 #"Success"
    (current-seconds) TEXT/HTML-MIME-TYPE
    '()
    (list 
      (string->bytes/utf-8 
        (xml->string html)))))

(define (response/html/content html)
  (response/html
    (content html)))

