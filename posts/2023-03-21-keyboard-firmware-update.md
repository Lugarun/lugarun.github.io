---
title: Keyboard Firmware Update
---

This blog post is an update on my [split wireless keyboard journey](./2022-07-31-keyboard.html).
Not much has changed in terms of hardware other than that I have added the batteries to the keyboard halfs and made some wrist rests.
What has changed a ton is the firmware and keymap.
After a long period of the keyboard sitting on the corner of my desk, I have finally got them up and running with a keymap that I am happy using on a daily basis.

My keyboard only has 36 keys.
So lots of trickery is required to jam the functionality of 104 keys into this small package.
Most small keyboards do this through a combination of the following:

- *layers:* Each key on the keyboard does something different when a different 'layer' is activated. Layers are typically activated by holding down a key.
- *activation modes:* Keys can be assigned multiple functions based on how they are pressed. For example, a long hold might register as a shift keystroke where a tap might be the letter 'b,' and a double tap might register as 'B.'
- *chording:* In chording, pressing multiple keys simultaneously activates different functions based on which keys are pressed. This is how [Stenotype](https://en.wikipedia.org/wiki/Stenotype) keyboards work.
- *tap dance:* Tap dance is activation modes taken so far as to be something else. Keys are pressed multiple times to access different functionality as the main method of multiplexing the functionality of a key. Ben Vallack has a great [video](https://www.youtube.com/watch?v=XBV0piKtNjI) about what you can do with this method.

After experimentation with my layer-based keymap design that mostly relied on shift key style layers, I discovered home row mods.
Precondition has a great [blog post](https://precondition.github.io/home-row-mods) going into the details on what home row mods are and how you might want to use them.
This seemed like a great idea; however, home row mods require some careful fine-tuning to ensure that the timing doesn't result in lots of accidental modifier activations.
Thankfully, there is a preconfigured keymap called [Miryoku](https://github.com/manna-harbour/miryoku/) that has good default values that we can start to experiment with.
Miryoku doesn't have an implementation for the [BlueMicro_BLE](http://bluemicro.jpconstantineau.com/) firmware I was using in the previous post; however, it does have one for [ZMK](https://zmk.dev/).

Switching to ZMK with the Adafruit Feather nRF52 microcontrollers turned out to be a challenge for someone not familiar with the ecosystem.
ZMK doesn't natively support the Feather; however, there are unmerged [pull requests](https://github.com/zmkfirmware/zmk/pull/1465) which seem to have gotten Feathers working.
ZMK is set up so you can easily create your configuration in a separate git repo with GitHub actions to build against the current ZMK repo.
The Miryoku ZMK implementation is simply one of these ZMK configuration repos for you to work with.
It turned out to be pretty straightforward to integrate [tmbutcher's](https://github.com/tmbutcher) configuration into my [Miryoku ZMK configuration](https://github.com/Lugarun/miryoku_zmk).
Welll, it turned out to be pretty straightforward after a month of on-and-off debugging that I am still working on forgetting.

Miryoku has so far been so amazing that I have become hesitant to change anything about it.
The home row mod timings are so good that from the get-go I have had negligible amounts of miss-fires.
As with any few key layout, it takes some time to get used to navigating programs like [tmux](https://github.com/tmux/tmux), [kakoune](https://kakoune.org/), or a [tiling window manager](https://xmonad.org/) but the home row mods feel so much better than constant pinky workouts.
One thing I do want to eventually add is a gaming layer, but for now, I haven't been playing games much, and when I do, it is Rocket League with a controller.

ZMK has so far been a joy to work with.
The next big ZMK steps for me are to fix a Bluetooth connection bug and to work out display support, but I will leave those for another day.

