---
title: Fun with pong at Roland Garros
author: Clement Delafargue
tags: roland garros, company campus
---

I worked on a fun project with [Company Campus](companycamp.us) for the French
Open Roland Garros.

The goal was to let people play Pong on the Suzanne Langlen court giant
screen.

[![Pong Screen](/files/rg-1.jpg)](https://www.facebook.com/companycampus/photos/pb.402877119762888.-2207520000.1407399923./762498477134082/?type=3&theater)

Instead of playing with regular controllers, people just had to put a hand in
the air. The paddle position was mapped to the hand's position.

[![Game controllers](/files/rg-2.jpg)](https://www.facebook.com/companycampus/photos/pb.402877119762888.-2207520000.1407399923./762499367133993/?type=3&theater)

# The hardware

It's made of simple, easy-to-get parts.

The game server is a compact PC.

The screen is a 74m² 32/9 giant screen.

The game controllers are custom-made. Each one runs on a raspberry pi + an
arduino card for data acquisition. The frames were made by welding steel bars
and adding wood panels.

Both the game server and the controllers RasPis run on Archlinux.
Everything is automated thanks to `systemd` (everything had to be
plug-and-play). `systemd` services and network configuration helped me a great
deal in making everything resilient (NodeJS applications have this weird
tendency to not be reliable).

# The code

We used [`johnny-five`](https://github.com/rwaldron/johnny-five) to get the
data from the arduino.

The controllers stream the raw data from the ultrasound sensor to the game
server, in UDP. The CPU and RAM are a bit tight, so nothing else is done
there (switching from TCP to UDP and removing data normalization gave a huge
latency drop).

The game itself is written in nodeJS with a HTML display. Since the pong
physics are very straightforward, I was able to write it entirely in an
evented fashion. No game loop, yay (even the ball movements are rendered with
CSS transitions).

To normalize the data streamed by the controllers, `BaconJS` was of great help.

Here's the code which removes the errors and transforms the raw position in
centimeters into a relative position in a [0, 1] interval.

```javascript

var normalizedPosition =
rawPosition
    .filter(function(p) { return p >= min && p <= max;})
    .skipDuplicates(function(op, np) { return Math.abs(op - np) < 1; })
    .map(function(p) {
        return 1 - (p - min) / range;
    });

```


# Overall

This project was really fun. It's rare to have the opportunity to do stick
welding in my day-to-day job and to have your code displayed on a 74m² screen
right in the middle of Roland Garros :-)

