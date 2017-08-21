# SPA SEO

SPA and SEO: Is Googlebot able to render a Single Page Application with Ajax calls?

## [Demo with Time-traveling Debugger](http://elm-spa-seo-testing.surge.sh/)

## How it works

Check out [the full writeup](https://medium.com/@l.mugnaini/spa-and-seo-is-googlebot-able-to-render-a-single-page-application-1f74e706ab11)!

## Getting started

If you don't already have `elm` and `elm-live`:

> npm install -g elm elm-live

Then, to build everything:

> elm-live --dir=build --output=build/main.js src/main.elm --pushstate --open --debug

(Leave off the `--debug` if you don't want the time-traveling debugger.)

## To deploy

> cd build
> surge

## Links

* [Search Result](https://www.google.com/search?q=site:elm-spa-seo-testing.surge.sh)
* [Code](https://github.com/lucamug/elm-spa-seo-testing)
