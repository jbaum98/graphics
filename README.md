Graphics Library
================

Requirements
-------------
The only required tool is [Stack](http://haskellstack.org).

It can be installed on OSX with Homebrew

    $ brew install stack

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

To generate Haddock documentation which is similar to JDOC,
run `make doc`, and to view it run `make doc-view`
