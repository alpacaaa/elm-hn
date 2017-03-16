# [elm-hn](https://elm-hn.surge.sh)

Hacker News clone powered by [Elm](http://elm-lang.org/) + [elm-navigation](https://github.com/elm-lang/navigation) + [GraphQL](https://www.graphqlhub.com/).

[![elm-hn screenshot](https://github.com/alpacaaa/elm-hn/raw/master/screenshot.png "Homepage")](https://elm-hn.surge.sh)

Live version: https://elm-hn.surge.sh

## Features

* Supports display of all item types:
  [stories](https://elm-hn.surge.sh/story/8863),
  [jobs](https://react-hn.appspot.com/story/8426937),
  [polls](https://react-hn.appspot.com/story/126809) and
  [comments](https://react-hn.appspot.com/#/comment/8054455)

It might take a few seconds to get a response from the server, so old tight! All errors are displayed on screen.

The backend api is a bit quirky in that it doesn't return all the results and has some inconsistencies in the data shown. Because of this, some of the JSON decoders are quite tricky and studying them can be a good exercise for new comers. The point of this projects is to work on a real Elm app so I'm not too worried about what gets returned by the api.

Inspired by [react-hn](https://react-hn.appspot.com)