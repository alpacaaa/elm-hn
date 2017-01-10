require('./main.css');

var firebase = require('firebase')

var app = firebase.initializeApp({
  databaseURL: 'https://hacker-news.firebaseio.com/'
})

var db = firebase.database()
db.ref('/v0/topstories').on('value', function(data) {
  console.log(data.val())
})

var Elm = require('./Main.elm');

var root = document.getElementById('root');

Elm.Main.embed(root);
