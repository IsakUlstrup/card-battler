import { Elm } from "./src/Main.elm";

// Get state from localstorage
const storedCards = localStorage.getItem('cards');
// console.log("Retrieved state: ", storedCards);


// Elm flags object
const flags = {
    timestamp: Date.now()
    , cards: storedCards
}


// Elm init
const app = Elm.Main.init({
    node: document.getElementById("app")
    , flags: flags
});


// storeCards port listener
app.ports.storeCards.subscribe(function (cards) {
    localStorage.setItem('cards', cards);
    // console.log("Saved cards: ", cards);
});