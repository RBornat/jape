# Jape Flatpak Builder

Parent project: [https://github.com/RBornat/jape](https://github.com/RBornat/jape)


## Requirements
- flatpak
- flatpak-builder
- wget

## Build

`./build.sh`

## Usage

### Install

```
flatpak install org.freedesktop.Platform//21.08
flatpak --user install jape.flatpak
flatpak run uk.org.jape
```

### Uninstall

`flatpak uninstall uk.org.jape`

## Example

A working Flatpak example of Jape is available on [https://binsky.org/flatpak-jape/](https://binsky.org/flatpak-jape/)
