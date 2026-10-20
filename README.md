# elgato-streamdeck

elgato-streamdeck is a Haskell library for Elgato Stream Deck devices.
elgato-streamdeck is a port of the Rust crate
[elgato-streamdeck](https://github.com/OpenActionAPI/rust-elgato-streamdeck)
0.13.2. The port was translated from the OCaml translation of the Rust crate.

- elgato-streamdeck loads `libhidapi` and `libturbojpeg` at runtime with
  `dlopen` (`System.Posix.DynamicLinker`) and calls both libraries through
  `foreign import ccall "dynamic"`. The build does not need C headers.
- To load `libhidapi` or `libturbojpeg` from another path, set
  `STREAMDECK_HIDAPI_LIB` or `STREAMDECK_TURBOJPEG_LIB`.
- Image rotation, mirroring, triangle-filter resizing and BMP encoding are
  implemented in Haskell.
- `System.Hardware.StreamDeck.Async` replaces the `async` feature (tokio) of the
  Rust crate. An STM lock serializes device access. Blocking hidapi calls are
  safe foreign calls, so other Haskell threads keep running during a hidapi call.
- elgato-streamdeck depends only on GHC boot packages.

## Install

elgato-streamdeck is not on Hackage yet. To use elgato-streamdeck from Git, add
the following to `cabal.project`:

```cabal
source-repository-package
  type: git
  location: https://github.com/exlee/haskell-elgato-streamdeck.git
  tag: v0.13.2
```

Then add `elgato-streamdeck` to `build-depends`.

Install the runtime libraries `libhidapi` and `libturbojpeg`:

- macOS: `brew install hidapi jpeg-turbo`
- Debian and Ubuntu: `apt install libhidapi-hidraw0 libturbojpeg`

On Linux, copy `40-streamdeck.rules` to `/etc/udev/rules.d/`, then unplug the
Stream Deck and plug the Stream Deck in again. `40-streamdeck.rules` is the
udev rules file of the Rust crate.

## Usage

`System.Hardware.StreamDeck` re-exports `System.Hardware.StreamDeck.Device`.
`System.Hardware.StreamDeck.Device` defines `product`, `close` and `flush`, so
import `System.Hardware.StreamDeck` qualified or hide `Prelude.product`.

| Rust | Haskell |
|---|---|
| `info::Kind` | `Kind` |
| `StreamDeck` | `StreamDeck` |
| `DeviceStateReader` | `Reader` (`getReader`, `readerRead`) |
| `StreamDeckInput` / `DeviceStateUpdate` | `Input` / `Update` |
| `images::{convert_image, ImageRect}` | `convertImage`, `ImageRect` |
| `image::DynamicImage` | `Image` (RGB8) |
| `AsyncStreamDeck` | `System.Hardware.StreamDeck.Async.AsyncStreamDeck` |

## Build and examples

```sh
cabal build all && cabal test
cabal run list-devices   # lists connected Stream Deck devices
cabal run simple         # sets brightness and key images, then prints key input
cabal run async          # same as simple, with System.Hardware.StreamDeck.Async
```

Run the examples from the package root directory. The examples load
`examples/no-place-like-localhost.jpg`.

## License

MPL-2.0, the license of the Rust crate. The Rust crate is by the
[elgato-streamdeck contributors](https://github.com/OpenActionAPI/rust-elgato-streamdeck/graphs/contributors).
elgato-streamdeck is a Modification of the Rust crate, as defined by MPL-2.0,
and is distributed under the same terms. See `LICENSE`.
