import { Elm } from "./src/Main.elm";
const deckstrings = require("deckstrings");

const node = document.getElementById("elm");
const flags = {};
const app = Elm.Main.init({ node: node, flags: flags });

app.ports.decodeDeck.subscribe(function (deckstring) {
    requestAnimationFrame(function () {
        const deck = deckstrings.decode(deckstring);
        console.log("deck", deck);
        app.ports.deckDecoded.send(deck);
    });
});
