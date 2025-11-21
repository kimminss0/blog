# Personal Blog

Personal website built with [Hakyll], a static site generator written in
Haskell. Hosted at [blog.mskim.org].

[Hakyll]: https://jaspervdj.be/hakyll/
[blog.mskim.org]: https://blog.mskim.org

## How to Build

### Prerequisites

- `cabal`: It is recommended to use GHCup for installation.
- `imagemagick`: Required to convert LaTeX to PNG
- `ghostscript`: Required to convert LaTeX to PNG
- `texlive`: Required to convert LaTeX to PNG

### Build Steps

To build this site locally, follow these steps:

```sh
$ cabal build
$ cabal exec site build
```

Please note that the compilation process may take some time.

To preview the website locally, start the server:

```sh
$ cabal exec site watch
```

Create an executable `deploy.sh` at the project root and put your own deployment logic in it.

To deploy the website, run:

```sh
$ cabal exec site deploy
```

## Note

Please note that the markdown syntax used for this site may differ from
GitHub-flavored markdown. As a result, certain elements like images or LaTeX
syntax might not render correctly when viewed on other platforms, such as
GitHub markdown preview, rather than pandoc.

