import { Elm } from "./src/Main.elm";
const deckstrings = require("deckstrings");

const node = document.getElementById("elm");
const flags = {};
const app = Elm.Main.init({ node: node, flags: flags });

app.ports.decodeDeck.subscribe(function ([deckstring, title]) {
    const deck = deckstrings.decode(deckstring);
    app.ports.deckDecoded.send({ deck, deckstring, title });
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
        const columnWidth = 250;
        const contentRadius = 15;
        const tooltipContent = document.getElementById(id + "-content");
        const tooltipPointer = document.getElementById(id + "-pointer");
        if (!tooltipContent || !tooltipPointer) {
            return;
        }
        const tooltipContentRect = tooltipContent.getBoundingClientRect();
        const tooltipContentLeft = tooltipContentRect.x;
        const tooltipContentTop = tooltipContentRect.y;
        const tooltipContentHeight = tooltipContentRect.height;
        const tooltipContentBottom = tooltipContentTop + tooltipContentHeight;
        const tooltipContentWidth = tooltipContentRect.width;
        const tooltipPointerRect = tooltipPointer.getBoundingClientRect();
        const tooltipPointerWidth = tooltipPointerRect.width;
        const tooltipPointerHeight = tooltipPointerRect.height;
        const tooltipPointerTop = tooltipPointerRect.y;
        const tooltipPointerBottom = tooltipPointerTop + tooltipPointerHeight;
        let contentOffsetX = 0,
            contentOffsetY = 0;
        let pointerOffsetX = 0,
            pointerScaleX = 1;

        // check if the tooltip needs to be flipped from right to left
        if (
            tooltipContentLeft + tooltipContentWidth + screenPadding >
            window.innerWidth
        ) {
            // if the tooltip overflows to the right, it is moved to the left, but cannot overflow to the left
            contentOffsetX = tooltipContentWidth + columnWidth;
            if (tooltipContentLeft < contentOffsetX) {
                contentOffsetX = tooltipContentLeft - screenPadding;
            }
            pointerScaleX = -1;
            pointerOffsetX =
                tooltipContentWidth - contentOffsetX + tooltipPointerWidth;
        }

        // check if the tooltip needs to be moved up
        const overflowY =
            tooltipContentBottom + screenPadding - window.innerHeight;
        if (overflowY > 0) {
            contentOffsetY = -overflowY;
            const pointerPositionWRTContent =
                tooltipPointerBottom - (tooltipContentBottom + contentOffsetY);
            if (pointerPositionWRTContent > 0) {
                contentOffsetY =
                    contentOffsetY + pointerPositionWRTContent + contentRadius;
            }
        }

        const transformContent = `translate(${
            contentOffsetX * -1
        }px, ${contentOffsetY}px)`;
        tooltipContent.style.transform = transformContent;

        const transformPointer = `translate(${pointerOffsetX}px, 16px) scaleX(${pointerScaleX})`;
        tooltipPointer.style.transform = transformPointer;
    });
});
