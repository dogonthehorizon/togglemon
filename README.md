# togglemon

A small program for toggling monitors in Linux. Ideal use case is a laptop
with a single video card and single external display. Works best when triggered
via a udev event for automatic display management.

## Motivation

I used to have a few shell scripts to manually toggle the displays using
xrandr, but they were rather brittle and depended on plugging the monitor into
the same port on my laptop.

Rather than deal with this frustration, an overengineered program was written
to make sure the minor annoyance of running a script was never encountered
again. Progress!

## Figuring out how this all works

Displays appear to have a directory in `sys/class/drm` and follow the pattern
`videoCard-display-number`. That is, the first DisplayPort peripheral for
a laptop's video card would look like `card0-DP-1`. The internal display
_appears_ to have `e` prepended to the display type (e.g. `card0-eDP-1`).
The contents of this directory include a few interesting things, the chiefmost
being `enabled` and `status` files.

The `enabled` file tells you if the display is currently in use, while the
`status` file tells you if the display is connected or not. The absence of
these files implies the display cannot be used for output.

`enabled` can have two states:

  - disabled
  - enabled

`status` can have two states:

  - connected
  - disconnected

The basic strategy would seem to be:

  - Get the contents of `sys/class/drm`
  - For each entry:
    - Check contents of `status` and `enabled` files
    - Build DisplayMatrix object (or some such)
  - Given a DisplayMatrix:
    - Find the display that is connected but disabled
    - Find the display that is connected and enabled
    - Construct an xrandr command to toggle them
    - Execute command
