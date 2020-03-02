#lang at-exp racket

;TODO: This hardcodes 7.5 as the Racket version, which is bad.  
;  But I haven't found a better way.
;  Whenever I try to run raco update --link ... in the docker container,
;  I get stuff like this
;  Uninstalling to prepare re-install of webapp
;  Moving webapp to trash: /root/.racket/7.5/pkgs/.trash/1580229532-0-webapp
;  rename-file-or-directory: cannot rename file or directory
;    source path: /root/.racket/7.5/pkgs/webapp
;    dest path: /root/.racket/7.5/pkgs/.trash/1580229532-0-webapp
;    system error: Invalid cross-device link; errno=18

(provide dev
	 host-port)

(require webapp/environment/util)

(define host-port (make-parameter 8080))

(define (dev . args)
  (define prefs-file
    (build-path (current-directory)
		".webapp-dev-prefs.rkt"))

  (when (file-exists? prefs-file)
    (displayln "Prefs file detected.  Loading...")
    (dynamic-require
      prefs-file
      #f)
    
    (displayln
      (~a "Host port is: " (host-port))))

  (define dev-pkgs 
    args)

  (displayln "Stopping any running containers")
  @system{
    @~a{docker stop @(get-cid)}
  }
  @system{
    @;@~a{docker rm $(docker stop $(docker ps -a -q --filter ancestor=@(pkg-name) --format="{{.ID}}"))}
    @;@~a{docker rm @(get-cid)}
    @~a{docker container prune --force}
  }

  (displayln "Starting new container") 
  (define (patch-in pkg)
    (displayln (~a "  Patching in " pkg))
    (define pkg-name (last (string-split pkg "/")))
    @~a{-v @|pkg|:/root/.racket/7.5/pkgs/@|pkg-name|})

  @system{
     @~a{docker run -dt -p @(~a (host-port) ":8080") -v @(~a (path->string (current-directory)) ":/" (pkg-name)) @(string-join (map patch-in dev-pkgs) " ") @(pkg-name)}
  }

  (displayln "Starting postgres")
  @system{
    @;@~a{docker exec $(docker ps -q --filter ancestor=@(pkg-name) --format="{{.ID}}") bash -c "/etc/init.d/postgresql start;"}
    @~a{docker exec @(get-cid) bash -c "/etc/init.d/postgresql start;"}
  }
)

