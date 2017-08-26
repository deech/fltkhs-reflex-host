# fltkhs-reflex-host

This is a port of the [Reflex host example][1] to [FLTKHS][2], a Haskell binding to the [FLTK][3] GUI toolkit.

![Screenshot](https://github.com/deech/fltkhs-reflex-host/blob/master/fltkhs-reflex-host.gif)

## Installation

To install just do:

```
> git clone http://github.com/deech/fltkhs-reflex-host
> stack build --flag fltkhs:bundled
> stack exec fltkhs-reflex-host
```

  [1]: https://github.com/reflex-frp/reflex-platform/blob/develop/examples/host.hs
  [2]: https://hackage.haskell.org/package/fltkhs
  [3]: http://fltk.org
  [4]: https://github.com/deech/fltkhs#quick-install
