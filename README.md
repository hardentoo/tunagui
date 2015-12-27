# tunagui
GUI (Graphical User Interface) by Haskell

## Build

### Dependency on ...

#### 1) SDL2

* [SDL2](https://www.libsdl.org/download-2.0.php)

* Haskell binding

Requires [sdl2 binding of haskell](http://hackage.haskell.org/package/sdl2) (sdl2 >= 2)

#### 2) sdl2-ttf

* sdl2-ttf

* Haskell binding

This library depends on sdl2-ttf binding of haskell
 'git@github.com:sbidin/sdl2-ttf.git'

please copy sdl2-ttf/src/SDL to tunagui/src directory

```
git clone 'git@github.com:sbidin/sdl2-ttf.git'

cp -r sdl2-ttf/src/SDL tunagui/src/
```

### Using Stack

```
extra-deps:
- sdl2-2.1.0
```
