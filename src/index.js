var Elm = require('./Main');
var meetupPlanner = Elm.Main.fullscreen();

// Autofocus when Page changes
meetupPlanner.ports.focusOnFirstInputAboveFold.subscribe(function(unused) {
    setTimeout(function() {

        var firstElement = document.querySelector('.focus-field');

        if (firstElement) {

            firstElement.focus();
        }
    }, 500);
});
