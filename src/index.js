require('./main.css');

var firebase = require('firebase')

firebase.initializeApp({
  databaseURL: 'https://hacker-news.firebaseio.com/'
})

var db = firebase.database()
// db.ref('/v0/topstories').on('value', function(data) {
//   console.log(data.val())
// })

var Elm = require('./Main.elm');

var root = document.getElementById('root');

var elmApp = Elm.Main.embed(root);

elmApp.ports.firebaseOut.subscribe(function(channel) {
  db.ref('/v0/' + channel).on('value', function(data) {
    elmApp.ports.firebaseIn.send({
      channel: channel,
      data: data.val()
    })
  })
})
