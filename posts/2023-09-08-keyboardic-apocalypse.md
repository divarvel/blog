---
title: "KeyBoardic Apocalypse; or, Downsizing of Primary Keyboard: An Annihilation of Key Switches and the Beginning of Merciless Configuration"
author: Clément Delafargue
tags: keyboard,  qmk
---

*The blog post was a pun on King Gizzard & the Lizard Wizard last record's name, but they released yet another one while I was writing this post. I guess that says a lot both about me, and about them.*

My journey with weird keyboards started many years ago, in 2009, with the acquisition of a [typematrix 2030](http://www.typematrix.com/2030/features.php) and switching to [bépo](https://bepo.fr/wiki/Accueil), an ergonomic keyboard layout designed for the French language.

I've used it for many years. I loved its orthogonal layout, and the fact that return and backspace were put under pointer fingers instead of straining my right pinky. At some point I stopped working with an external monitor and only worked on laptops, so I stopped using my typematrix. There were also issues with its skin (a kind of rubber layer that protects the keyboard).

Fast forward a couple years. I finally land a nice job working for a company based in Paris, and I have a proper office room at home. I wanted to try mechanical keyboards for some time, and I was particularly intrigued by ergodox keyboards: they are split in the middle, with two independent halves. This allows keeping the hands at shoulder width, tilting and tenting halves independently, etc. It also features thumb clusters: instead of using thumbs only for pressing space, they are now used for space, backspace, enter, esc, …. I looked a bit at how to build one and got overwhelmed, so I decided to get a pre-built one from [ergodox ez](https://ergodox-ez.com/). I'm super happy with it, even though i don't use thumb clusters to their full extent (they have 6 keys each, even though I only use 2), and it just takes a lot of place. Another issue I have with it is that it has unusual keycaps, so it's hard to find alternative keycap sets.

Fast forward a couple years (again). I stumble upon the iris keyboard. I like a few things about it: way smaller thumb clusters with a better repartition, and it only uses 1u keycaps, so ortholinar keysets work with it. It's also quite smaller than the ergodox, and works with usb c cables. I get an iris rev7 kit, which comes with hotswap sockets, no need to solder switches. I just solder a single rotary encoder on the top left.

Fast forward a couple months, I love my iris so much I wish I could bring it at work, but sadly it is still a bit too bulky. Then I remember those people talking about their fancy minimalist keyboards, amongst them, the [ferris sweep](https://github.com/davidphilipbarr/Sweep). At first I'm not too interested because it has only 34 keys and I can't even fit all letters in the bépo layout (namely m,z and w which are on the 6th column of the right hand). The idea still makes its way, because laptop keyboards start to make my wrists hurt. The ferris sweep is both small and low-profile thanks to the [kailh choc switches](https://splitkb.com/products/kailh-low-profile-choc-switches), which makes it perfect for carrying it with me.

I finally buy a kit:

- two PCB plates (the sweep bling variation, with support for hotswap sockets)
- 34 hotswap sockets
- 34 kailh choc sunset switches
- two elite-pi controllers

Assembling the kit was fun, soldering the switch sockets was ok. Soldering the controller sockets and the controllers themselves was a bit harder. Soldering the reset buttons was more challenging since they are very small and I was not experienced with SMD soldering.

I will detail further the layout I ended up with on the ferris. It's not too different from my iris layout or my laptop keyboard layouts, which allows me to switch keyboards with minimal pain.

I used the ferris as my primary keyboard for a couple weeks after assembling it, not only because I wanted to break it in, but also because I hurt my wrists and the high-profile of the iris was less comfortable than the ultra-thin ferris. 
I have since gone back to using the iris as my primary keyboard because it is still more convenient and doesn't require as much combos.

## Conclusion

Typematrix will always have a special place in my heart as my first non-standard keyboard, but it is too narrow for me, is fully orthogonal and not just ortholinear, the skins get dirty over time, it does not use mech switches. I have kept mine as a souvenir, but I don't plan on using it anymore.

The ergodox-ez was a great way to get started with split keyboards, but it is too big for my taste (and my hands), especially the thumb clusters. The specific keycaps also make it harder to customize. It has old micro controllers and is somewhat cumbersome to flash, even if the web interface makes editing layout easy.

The iris is wonderful keyboard. It is just the right size for me, especially the thumb keys. The rotary encoder for volume control is pure joy. Finally, it can be directly configured over USB via a web app. I love it.

Finally the ferris sweep is an uncompromising keyboard. I think fully adopting it allows to be the most efficient, but I did not want to give up on all my muscle memory, so I'm not using it to its full extent. That being said, it's a great travel keyboard and I'm happy to use it when I'm not at home. I cannot directly configure it over USB, but I use qmk configurator to generate config files, compile them and then copy the firmware over USB mass storage. It's a bit more convenient than what I had to do with the ergodox.

## Bépo adaptation

Once I had a working keyboard, it was time for tweaking the layout. Ferris offers only two 3x5 matrices, and 2x2 thumb keys. The rest is accesible through layers. While this works well for the qwerty layer where every letter is located on the 3x5 matrices, bépo makes full use of a regular keyboard, and moves some letters outside of it, in order to make space for commonly used diacritics like "é".

I started from the [default ferris layout](https://github.com/qmk/qmk_firmware/tree/master/keyboards/ferris/keymaps/default), with home row mods. 
I kept the home row mod idea, but departed significantly from the rest, since my goal was to keep using other keyboards, I wanted to minimize the amount of changes to a regular keyboard.

My goal was to make sure every letter was on the main layer. That means moving m,z,w around, by removing other characters. My choice went to à,è,^. Since m is the most used of the three, I decided to keep it on the same hand, so it replaced !. Then z went to the è, and w to à.

Then, I used a dedicated layer for the top row symbols and numbers. On a regular keyboard, symbols are directly accessible and numbers are accessed via shift. What I did instead was to put symbols on the home row and numbers on the top row. This allowed me to keep muscle memory. Finally, I put extra symbols on the bottom row (à,$,ç,è,^,=,%).

In order to stay close to my other keyboards, I put meta and alt-gr on the main layer, on thumb keys. I use those two so much it made sense to give them direct access.

I kept layer 7 almost as is, with return on the home row, left pointer, esc on the top row, left pointer, tab on the left thumb. I kept the reset button on the bottom row, right pinky since I ended up flashing my keyboard very often. The only addition was volume and playback control on the left half.

Apart from that, not a lot of changes, I kept the navigation layer as is (it provides arrows). I don't use mouse control on my keyboard, so I left the mouse layer unchanged.

