cl-sip
======

ABOUT
-----

cl-sip is a Common Lisp library for working with the Session
Initiation Protocol (SIP) as defined in [RFC
3261](http://www.ietf.org/rfc/rfc3261.txt).

I started writing the library with 2 goals in mind: 

* Gaining experience with Common Lisp
* Building a library for use in SIP test tools

The library is a work in progress. The parsing code in msg.lisp is the
most complete by far and has decent unit test coverage. The code in
client.lisp is very raw -- it represents just ideas of what SIP
user-agent code might look like.

cl-sip is available from 
[http://www.github.com/mattkeller/cl-sip/](http://www.github.com/mattkeller/cl-sip/).

USING
-----

cl-sip uses [ASDF](http://www.cliki.net/asdf) to load its
dependencies. To install cl-sip, download the source and link the
included cl-sip.asd file into your ASDF central-registry
directory. You can then load cl-sip with:

    (asdf:oos 'asdf:load-op 'cl-sip)
    (in-package cl-sip.msg)

See the unit tests for hints about how to use the library. (Yes, I
should expand this description!)

PLATFORMS
---------

cl-sip has been developed on Linux using SBCL. At the very least the
unit tests pass with SBCL 1.0.32. 

LICENSE
-------

cl-sip is licensed with the LGPL.


AUTHOR
------

Matt Keller  
mattkeller@gmail.com  
[http://www.littleredbat.net/mk/](http://www.littleredbat.net/mk/)  




