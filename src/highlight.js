export const highlight = (selector) => {
    for (const e of document.querySelectorAll(selector)) {
        e.classList.add("highlight");
    }
}

export const lowlight = (selector) => {
    for (const e of document.querySelectorAll(selector)) {
        e.classList.remove("highlight");
    }
};