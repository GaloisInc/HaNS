#!/bin/bash

set -ev

bridge=$1
if [ -z "$bridge" ]; then
	bridge=xenbr0
fi

sudo ip tuntap add dev tap1 mode tap user $USER
sudo ip link set dev tap1 master $bridge
sudo ip link set dev tap1 up
