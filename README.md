# HaNS

The Haskell Network Stack.

## Running Echo-Server Examples in Xen

*NOTE: This example is out of date*

You first need to set up a bridge device, `br0`; the echo servers are
configured to use this bridge in Xen. The configuration files are
`udp-test/udp-test.config` and `tcp-test/tcp-test.config`.

Assuming you are working in a Cabal sandbox at the top-level of the
repo, and have done `halvm-cabal install hans`, you can run the TCP
and UDP echo server examples with

    sudo xl create -c ../udp-test/udp-test.config

and

    sudo xl create -c ../tcpp-test/tcpp-test.config

where `-c` makes them print their log messages to standard out. They
are configured to use DHCP and will log the IP address they receive
after start up; they listen on port 9001.

You can then test them with NetCat, using

    nc -u <ip addr> 9001

for UDP and

    nc <ip addr> 9001

for TCP. Whatever you enter will be echoed back.
