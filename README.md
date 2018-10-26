# &#8477;P<sup>2</sup>-Asteroids
This is a very minimalistic implementation of the video game
classic [Asteroids][A] &ndash; with one notably exception:
it is played on the [real projective plane &#8477;P<sup>2</sup>][RP2]
instead of the torus (as usual).

It's implemented in [Haskell][H], yet it is supposed to be run in
the browser. This is accomplished by the [haste compiler][Hc] which
is one of the available Haskell to Javascript compilers (see the
[HaskellWiki][hs2js] for alternatives). (In particular this code
will _not_ work with a vanilla GHC installation!)

You can see it in action (and, of course, play it) [here][demo].

[A]: https://en.wikipedia.org/wiki/Asteroids_(video_game)
[RP2]: https://en.wikipedia.org/wiki/Real_projective_plane
[H]: https://www.haskell.org/
[Hc]: https://haste-lang.org/
[hs2js]: https://wiki.haskell.org/The_JavaScript_Problem#Haskell_-.3E_JS
[demo]: https://homepages.uni-regensburg.de/~prj05723/rp2-asteroids/


## Controls
* Left/Right arrow keys: rotate ship
* Up arrow key: boost
* Space: shoot


## Build instructions
Get `hastec` and `haste-cabal`, the haste compiler and patched cabal
for use with haste, respectively. The easiest way to do this, is via
the "ready to use" [binary packages][hbin].

Then you can just use `(haste-)cabal` as usual:
```
git clone "https://github.com/J0J0/rp2-asteroids.git"
cd rp2-asteroids
path/to/haste-cabal update
path/to/haste-cabal build --hastec-options='--opt-all'
```

This will produce a javascript file `rp2-asteroids.js` that can be
used by any html file providing a (big enough) `canvas` element
with `id` set to `game-canvas` (and statically assigned `width`/`height`
html attributes). See `RP2-Asteroids.html` for an example.

[hbin]: https://haste-lang.org/downloads/
