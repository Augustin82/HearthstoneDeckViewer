import { Elm } from "./src/Main.elm";
const deckstrings = require("deckstrings");

const node = document.getElementById("elm");
const flags = {};
const app = Elm.Main.init({ node: node, flags: flags });

app.ports.decodeDeck.subscribe(function (deckstring) {
    const deck = deckstrings.decode(deckstring);
    console.log("deck", deck);
    app.ports.deckDecoded.send({ deck, deckstring });
});

app.ports.copyToClipboard.subscribe(function (textToCopy) {
    const copyTextarea = document.createElement("textarea");
    copyTextarea.value = textToCopy;
    copyTextarea.setAttribute("type", "hidden");
    copyTextarea.textContent = textToCopy;
    document.body.appendChild(copyTextarea);
    copyTextarea.select();

    try {
        const successful = document.execCommand("copy");
        if (!successful) {
            throw new Error("Copy not successful");
        }
    } catch (err) {
        console.log("Copy failed");
    }
});

app.ports.fixTooltipPlacement.subscribe(function (id) {
    requestAnimationFrame(function () {
        const screenPadding = 16;
        const tooltip = document.getElementById(id);
        const rect = tooltip.getBoundingClientRect();

        console.log(rect);
        if (rect.x < 0) {
            tooltip.style.transform = `translateX(${
                -rect.x + screenPadding
            }px)`;
        } else if (rect.x + 450 > window.outerWidth) {
            tooltip.style.transform = `translateX(${
                window.outerWidth - rect.x - screenPadding
            }px)`;
        }
    });
});
