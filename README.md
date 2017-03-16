# [elm-hn](https://elm-hn.surge.sh)

Hacker News clone powered by Elm + elm-navigation + GraphQL.

[![elm-hn screenshot](https://github.com/alpacaaa/elm-hn/raw/master/screenshot.png "Homepage")](https://elm-hn.surge.sh)


It might take a few seconds to get a response from the server, so old tight! All errors are displayed on screen.

The backend api is a bit quirky in that it doesn't return all the results and has some inconsistencies in the data shown. Because of this, some of the JSON decoders are quite tricky and studying them can be a good exercise for new comers. The point of this projects is to work on a real Elm app so I'm not too worried about what gets returned by the api.

Inspired by [react-hn](https://react-hn.appspot.com)