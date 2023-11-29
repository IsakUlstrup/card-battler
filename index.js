import { Elm } from "./src/Main.elm";

const app = Elm.Main.init({
    node: document.getElementById("app")
    , flags: Date.now()
});

app.ports.storeCards.subscribe(function (cards) {
    const cardsJson = JSON.stringify(cards);
    localStorage.setItem('cards', cardsJson);
    console.log("Saved cards: ", JSON.stringify(cards));
});