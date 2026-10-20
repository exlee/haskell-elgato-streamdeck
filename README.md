# elgato-streamdeck

Haskell port of the Rust crate
[elgato-streamdeck](https://github.com/OpenActionAPI/rust-elgato-streamdeck) 0.13.2,
ported from its OCaml translation.

- `libhidapi` and `libturbojpeg` load at runtime through `dlopen`
  (`System.Posix.DynamicLinker`) and `foreign import ccall "dynamic"`.
  Override paths with `STREAMDECK_HIDAPI_LIB` and `STREAMDECK_TURBOJPEG_LIB`.
- Rotation, mirroring, triangle-filter resizing and BMP are pure Haskell.
- `System.Hardware.StreamDeck.Async` replaces the Rust `async` feature (tokio).
  Device access is serialized by an STM lock; blocking hidapi calls are safe
  foreign calls, so other threads keep running.
- Depends only on GHC boot packages.

| Rust | Haskell |
|---|---|
| `info::Kind` | `Kind` |
| `StreamDeck` | `StreamDeck` |
| `DeviceStateReader` | `Reader` (`getReader`, `readerRead`) |
| `StreamDeckInput` / `DeviceStateUpdate` | `Input` / `Update` |
| `images::{convert_image, ImageRect}` | `convertImage`, `ImageRect` |
| `image::DynamicImage` | `Image` (RGB8) |
| `AsyncStreamDeck` | `System.Hardware.StreamDeck.Async.AsyncStreamDeck` |

`System.Hardware.StreamDeck` re-exports `Device`, which defines `product`,
`close` and `flush`; import it qualified or hide `Prelude.product`.

```sh
brew install hidapi jpeg-turbo
cabal build all && cabal test
cabal run list-devices
cabal run simple   # writes to the device
cabal run async    # concurrent version
```

Run the examples from the package root: they load
`examples/no-place-like-localhost.jpg`.

Linux udev rules: `40-streamdeck.rules`, as in the Rust repository.

## Install

From Hackage, once published: add `elgato-streamdeck` to `build-depends`.
From Git, in `cabal.project`:

```cabal
source-repository-package
  type: git
  location: https://github.com/exlee/haskell-elgato-streamdeck.git
  tag: v0.13.2
```

Runtime libraries: `libhidapi` and `libturbojpeg`.

- macOS: `brew install hidapi jpeg-turbo`
- Debian/Ubuntu: `apt install libhidapi-hidraw0 libturbojpeg`, then copy
  `40-streamdeck.rules` to `/etc/udev/rules.d/` and replug the device.

## License

MPL-2.0, the license of the Rust original. The Rust crate is by the
[elgato-streamdeck contributors](https://github.com/OpenActionAPI/rust-elgato-streamdeck/graphs/contributors).
This port is a Modification of that Covered Software and is distributed under
the same terms; see `LICENSE`.
