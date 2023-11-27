import { Elm } from "./src/Main.elm";

const app = Elm.Main.init({
    node: document.getElementById("app")
    , flags: Date.now()
});

app.ports.storeCards.subscribe(function (cards) {
    console.log("Cards: ", JSON.stringify(cards));
});