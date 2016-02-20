Graphics Library
================

Requirements
-------------
The only required tool is [Stack](http://haskellstack.org).

It can be installed on OSX with Homebrew

    $ brew install haskell-stack

And on Ubuntu 14.04 with `apt`

    $ sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 575159689BEFB442
    $ echo 'deb http://download.fpcomplete.com/ubuntu trusty main'|sudo tee /etc/apt/sources.list.d/fpco.list
    $ sudo apt-get update && sudo apt-get install stack -y

For more installation instructions, check the [documentation](http://docs.haskellstack.org/en/stable/install_and_upgrade.html).

Usage
-----
To compile, run `make`.

To generate the NetPBM image file, run `make run`.

To generate a PNG image file, run `make convert`.

To generate [Haddock](https://www.haskell.org/haddock/doc/html/index.html) documentation which is similar to [JavaDoc](http://www.oracle.com/technetwork/articles/java/index-jsp-135444.html),
run `make doc`, and to view it run `make doc-view`
