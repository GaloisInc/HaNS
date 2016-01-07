#!/bin/bash

set -ev

sudo ip tuntap add dev tap1 mode tap user $USER
sudo ip link set dev tap1 master xenbr0
sudo ip link set dev tap1 up
