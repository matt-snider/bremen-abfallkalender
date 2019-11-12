# bremer-abfallkalender

Find information on the next garbage/recycling pickup!

This is a _very_ simple interface to the [Bremer Stadtreinigung's Abfallkalender](https://www.die-bremer-stadtreinigung.de/detail.php?gsid=bremen206.c.10946.de)

## Usage

```sh
$ bremer-abfallkalender list Meine-Strasse 42

2018-07-05 - Restmüll / Bioabfall
2018-07-12 - Papier / Gelber Sack
2018-07-19 - Restmüll / Bioabfall
...

$ bremer-abfallkalender next Meine-Strasse 42
2018-07-05 - Restmüll / Bioabfall

$ bremer-abfallkalender next papier Meine-Strasse 42
2018-07-12 - Papier / Gelber Sack
```

## Development

During development, run the tool with stack:

```sh
$ stack run -- list Meine-Strasse 42
```

## Building

To build the tool run:

```sh
$ stack build
```

The build artifact will be output under `./.stack-work/dist/.../build/bremer-abfallkalender/`

