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
This [`sysfs`] directory is managed by the [Direct Rendering Manager]
(DRM for short, unfortunate as that acronym is). The contents of this directory
include a few interesting files:

| File    | Purpose                                    |
|---------|--------------------------------------------|
| status  | is the display currently connected?        |
| enabled | is the display currently in use?           |
| edid    | binary file containing display information |

`enabled` can have two states:

  - disabled
  - enabled

`status` can have two states:

  - connected
  - disconnected

The `edid` file has it's [own format][edid].

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

## Iteration on Initial Design

With the ability to parse [edid] information it would be great to have a config
format that would allow one to set xrandr display options per-display,
irrespective of which port the display is plugged into.

Depending on how much information is provided in the edid file for each monitor,
it may be possible to achieve this in the following way.

Implement something like `~/.config/togglemon/known_displays.yaml` similar in
spirit to the `known_hosts` file in SSH. This file contains the parsed edid
information of each monitor that it's seen and gives it some kind of generated,
human readable name (similar to how docker randomly names it's containers).

The user then would make their own config file (perhaps
`~/.config/togglemon/config.yaml`) that would reference these displays and the
xrandr options they wish to be executed.

Maybe something like this:

```yaml
# ~/.config/togglemon/known_displays.yaml
prancing-pangolin:
  manufacturerId: SHP
  manufacturerProductCode: 1111
  serialNumber: 123412341234
  weekOfManufacture: 52
  yearOfManufacture: 2018
  edidVersion: 1.4
# Who knows if all that information would be useful, but it would increase
# the likelihood that we would get a unique monitor from all that information.
# For example: my current laptop display does not appear to have a serial
# number (it appears zeroed out), so it's likely we can't depend on a single
# field being setup correctly.
```

```yaml
# ~/.config/togglemon/config.yaml
prancing-pangolin:
  xrandrOpts:
    - auto
    - position 0x0
```

## Thinking Further Afield

- At what point is it possible to dump xrandr entirely?

[Direct Rendering Manager]: https://en.wikipedia.org/wiki/Direct_Rendering_Manager
[`sysfs`]: https://en.wikipedia.org/wiki/Sysfs
[edid]: https://en.wikipedia.org/wiki/Extended_Display_Identification_Data#EDID_1.4_data_format
