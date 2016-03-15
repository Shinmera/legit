## About Legit
This is an interface to the GIT binary to make controlling it from within CL much easier. I've had the need to do this kind of thing a couple of times now, so I might as well encapsulate it into a proper library. I don't know if this is ever going to reach full coverage of all features given GIT's immense size, but I will add features as they are needed. The low-level command API is fully mapped however.

## How To
You will need the `git` binary in your `PATH`. Once you got that, and have this system loaded via ASDF or Quicklisp, you can access all the git commands through functions.

    (legit:with-chdir ("some/git/dir")
      (legit:git-rev-parse "HEAD" :short T))

All commands have been hand-rewritten to work through the uniform and comfortable API we're used to from Lisp. There is an even more convenient (albeit incomplete) interface using `repository` instances.

    (let ((repository (make-instance 'legit:repository :location "some/git/dir")))
      (values (legit:commits repository)
              (legit:current-branch repository)
              (legit:remote-url repository)))

A lot of the information about a repository that you can access will automatically be cached until some destructive operation (like a `pull`) occurs, so that querying that information does not take so much time each request.

## Also See

* [Simple-inferiors](http://shinmera.github.io/simple-inferiors/) for running and controlling the git binary.
