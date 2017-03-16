# [elm-hn](https://elm-hn.surge.sh)

Hacker News clone powered by [Elm](http://elm-lang.org/) + [elm-navigation](https://github.com/elm-lang/navigation) + [GraphQL](https://www.graphqlhub.com/).

[![elm-hn screenshot](https://github.com/alpacaaa/elm-hn/raw/master/screenshot.png "Homepage")](https://elm-hn.surge.sh)

Live version: https://elm-hn.surge.sh

## Features

* Supports display of all item types:
  [stories](https://elm-hn.surge.sh/story/8863),
  [jobs](https://elm-hn.surge.sh/story/8426937) and
  [polls](https://elm-hn.surge.sh/story/126809)
* Basic [user profiles](https://elm-hn.surge.sh/user/patio11)
* Collapsible comment threads, with child counts

It might take a few seconds to get a response from the server, so old tight! All errors are displayed on screen.

The backend api is a bit quirky in that it doesn't return all the results and has some inconsistencies in the data shown. Because of this, some of the JSON decoders are quite tricky and studying them can be a good exercise for new comers. The point of this projects is to work on a real Elm app so I'm not too worried about what gets returned by the api.

## Running

Make sure you have `create-elm-app` installed:

```
npm install -g create-elm-app
```

Run dev server with:
```
elm-app start
```

Build in `dist` folder:
```
elm-app build
```
----
Inspired by [react-hn](https://react-hn.appspot.com)