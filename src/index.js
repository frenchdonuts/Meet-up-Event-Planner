var Elm = require('./Main');
var meetupPlanner = Elm.fullscreen(Elm.Main, {
    swap: false
});

// Autofocus when Page changes
meetupPlanner.ports.focus.subscribe(function(unused) {
    setTimeout(function() {

        var firstElement = document.getElementById('main').querySelector('.focus-field');

        if (firstElement) {

            firstElement.focus();
        }
    }, 500);
});
