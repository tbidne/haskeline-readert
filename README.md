<div align="center">

# haskeline-readert

[![ci](http://img.shields.io/github/actions/workflow/status/tbidne/haskeline-readert/ci.yaml?branch=main)](https://github.com/tbidne/haskeline-readert/actions/workflows/ci.yaml)
[![MIT](https://img.shields.io/github/license/tbidne/haskeline-readert?color=blue)](https://opensource.org/licenses/MIT)

</div>

---

Shows example usage for `haskeline`'s `ReaderT` interface i.e.

```haskell
-- InputT is the core haskeline type.
toReaderT :: InputT m a -> ReaderT (InputTEnv m) m a
fromReaderT :: ReaderT (InputTEnv m) m a -> InputT m a
```

with typeclasses (`mtl` style) and effects (`bluefin` and `effectful`).
